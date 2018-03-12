#pragma once

#include "BlockList.h"

#include <atomic>
#include <exception>
#include <iterator>
#include <limits>
#include <list>
#include <mutex>
#include <unordered_map>
#include <vector>

#include <tbb/concurrent_hash_map.h>

namespace souffle {

typedef uint8_t rank_t;
/* technically uint56_t, but, doesn't exist. Just be careful about storing > 2^56 elements. */
typedef uint64_t parent_t;

// number of bits that the rank is
constexpr uint8_t split_size = 8u;
// block_t & rank_mask extracts the rank
constexpr block_t rank_mask = (1ul << split_size) - 1;

/**
 * Structure that emulates a Disjoint Set, i.e. a data structure that supports efficient union-find operations
 */
class DisjointSet {
    template <typename TupleType>
    friend class BinaryRelation;

    /* store blocks of atomics */
    BlockList<std::atomic<block_t>> a_blocks;

public:
    DisjointSet(){};

    // Not a thread safe operation
    DisjointSet& operator=(const DisjointSet& old) {
        if (this == &old) return *this;

        a_blocks = old.a_blocks;

        return *this;
    }

    inline size_t size() const {
        auto sz = a_blocks.size();
        return sz;
    };

    /**
     * Yield reference to the node by its node index
     * @param node node to be searched
     * @return the parent block of the specified node
     */
    inline std::atomic<block_t>& get(parent_t node) const {
        auto& ret = a_blocks.get(node);
        return ret;
    };

    /**
     * Equivalent to the find() function in union/find
     * Find the highest ancestor of the provided node - flattening as we go
     * @param x the node to find the parent of, whilst flattening its set-tree
     * @return The parent of x
     */
    parent_t findNode(parent_t x) {
        // while x's parent is not itself
        while (x != b2p(get(x))) {
            block_t xState = get(x);
            // yield x's parent's parent
            parent_t newParent = b2p(get(b2p(xState)));
            // construct block out of the original rank and the new parent
            block_t newState = pr2b(newParent, b2r(xState));

            this->get(x).compare_exchange_strong(xState, newState);

            x = newParent;
        }
        return x;
    }

    /**
     * Read only version of findNode
     * i.e. it doesn't compress the tree when searching - it only finds the top representative
     * @param x the node to find the rep of
     * @return the representative that is found
     */
    parent_t readOnlyFindNode(parent_t x) const {
        block_t ii = get(x);
        parent_t p = b2p(ii);
        if (x == p) return x;
        return readOnlyFindNode(p);
    }

private:
    /**
     * Update the root of the tree of which x is, to have y as the base instead
     * @param x : old root
     * @param oldrank : old root rank
     * @param y : new root
     * @param newrank : new root rank
     * @return Whether the update succeeded (fails if another root update/union has been perfomed in the
     * interim)
     */
    bool updateRoot(const parent_t x, const rank_t oldrank, const parent_t y, const rank_t newrank) {
        block_t oldState = get(x);
        parent_t nextN = b2p(oldState);
        rank_t rankN = b2r(oldState);

        if (nextN != x || rankN != oldrank) return false;
        // set the parent and rank of the new record
        block_t newVal = pr2b(y, newrank);

        return this->get(x).compare_exchange_strong(oldState, newVal);
    }

public:
    /**
     * Clears the DisjointSet of all nodes
     * Invalidates all iterators
     */
    void clear() {
        a_blocks.clear();
    }

    /**
     * Check whether the two indices are in the same set
     * @param x node to be checked
     * @param y node to be checked
     * @return where the two indices are in the same set
     */
    bool sameSet(parent_t x, parent_t y) {
        while (true) {
            x = findNode(x);
            y = findNode(y);
            if (x == y) return true;
            // if x's parent is itself, they are not the same set
            if (b2p(get(x)) == x) return false;
        }
    }

    /**
     * Union the two specified index nodes
     * @param x node to be unioned
     * @param y node to be unioned
     */
    void unionNodes(parent_t x, parent_t y) {
        while (true) {
            x = findNode(x);
            y = findNode(y);

            // no need to union if both already in same set
            if (x == y) return;

            rank_t xrank = b2r(get(x));
            rank_t yrank = b2r(get(y));

            // if x comes before y (better rank or earlier & equal node)
            if (xrank > yrank || ((xrank == yrank) && x > y)) {
                std::swap(x, y);
                std::swap(xrank, yrank);
            }
            // join the trees together
            // perhaps we can optimise the use of compare_exchange_strong here, as we're in a pessimistic loop
            if (!updateRoot(x, xrank, y, yrank)) continue;
            // make sure that the ranks are orderable
            if (xrank == yrank) updateRoot(y, yrank, y, yrank + 1);
            break;
        }
    }

    /**
     * Create a node with its parent as itself, rank 0
     * @return the newly created block
     */
    inline block_t makeNode() {
        // make node and find out where we've added it
        size_t nodeDetails = a_blocks.createNode();

        a_blocks.get(nodeDetails).store(pr2b(nodeDetails, 0));

        return a_blocks.get(nodeDetails).load();
    };

    /**
     * Extract parent from block
     * @param inblock the block to be masked
     * @return The parent_t contained in the upper half of block_t
     */
    static inline parent_t b2p(const block_t inblock) {
        return (parent_t)(inblock >> split_size);
    };

    /**
     * Extract rank from block
     * @param inblock the block to be masked
     * @return the rank_t contained in the lower half of block_t
     */
    static inline rank_t b2r(const block_t inblock) {
        return (rank_t)(inblock & rank_mask);
    };

    /**
     * Yield a block given parent and rank
     * @param parent the top half bits
     * @param rank the lower half bits
     * @return the resultant block after merge
     */
    static inline block_t pr2b(const parent_t parent, const rank_t rank) {
        return (((block_t)parent) << split_size) | rank;
    };
};

template <typename SparseDomain>
class SparseDisjointSet {
    DisjointSet ds;

    template <typename TupleType>
    friend class BinaryRelation;

    // mapping from souffle val to union-find dense value
    typedef tbb::concurrent_hash_map<SparseDomain, parent_t> SparseMap;
    SparseMap sparseToDenseMap;
    // mapping from union-find val to souffle, union-find encoded as index
    souffle::BlockList<SparseDomain> denseToSparseMap;

private:
    /**
     * Retrieve dense encoding, adding it in if non-existent
     * @param in the sparse value
     * @return the corresponding dense value
     */
    parent_t toDense(const SparseDomain in) {
        typename SparseMap::accessor a;
        bool newItem = sparseToDenseMap.insert(a, in);
        // create mapping if doesn't exist
        if (newItem) {
            // call b2p on this (makeNode returns a block_t, and we only care about its index)
            parent_t dense = ds.b2p(ds.makeNode());
            a->second = dense;

            denseToSparseMap.insertAt(dense, in);
        }

        return a->second;
    }

public:
    // warning! not thread safe, do not perform copy operations
    // XXX: is this actually a copy-op? Doesn't look like it...
    SparseDisjointSet& operator=(const SparseDisjointSet& old) {
        if (&old == this) return *this;

        ds = old.ds;
        sparseToDenseMap = old.sparseToDenseMap;
        denseToSparseMap = old.denseToSparseMap;

        return *this;
    }

    /**
     * For the given dense value, return the associated sparse value
     *   Undefined behaviour if dense value not in set
     * @param in the supplied dense value
     * @return the sparse value from the denseToSparseMap
     */
    inline const SparseDomain toSparse(const parent_t in) const {
        return denseToSparseMap.get(in);
    };

    /* a wrapper to enable checking in the sparse set - however also adds them if not already existing */
    inline bool sameSet(SparseDomain x, SparseDomain y) {
        return ds.sameSet(toDense(x), toDense(y));
    };
    /* simply a wrapper to findNode, that does not affect the structure of the disjoint set */
    inline SparseDomain readOnlyFindNode(SparseDomain x) {
        // replaced toDense to readonly vv
        // undefined if x does not exist.
        typename SparseMap::const_accessor a;
        sparseToDenseMap.find(a, x);
        return toSparse(ds.readOnlyFindNode(a->second));
    };
    /* finds the node in the underlying disjoint set, adding the node if non-existent */
    inline SparseDomain findNode(SparseDomain x) {
        return toSparse(ds.findNode(toDense(x)));
    };
    /* union the nodes, add if not existing */
    inline void unionNodes(SparseDomain x, SparseDomain y) {
        ds.unionNodes(toDense(x), toDense(y));
    };

    inline std::size_t size() {
        return ds.size();
    };

    // TODO: documentation
    void clear() {
        ds.clear();
        sparseToDenseMap.clear();
    }

    /* wrapper for node creation */
    inline void makeNode(SparseDomain val) {
        // dense has the behaviour of creating if not exists.
        toDense(val);
    };

    /* whether we the supplied node exists */
    inline bool nodeExists(const SparseDomain val) const {
        return sparseToDenseMap.count(val);
    };

    inline bool contains(SparseDomain v1, SparseDomain v2) {
        if (nodeExists(v1) && nodeExists(v2)) {
            return sameSet(v1, v2);
        }
        return false;
    }
};
}
