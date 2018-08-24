
#pragma once

#include "BlockList.h"
#include "PiggyList.h"

#include <atomic>
#include <exception>
#include <iterator>
#include <limits>
#include <list>
#include <mutex>
#include <unordered_map>
#include <vector>

#define BTREESTUFF
#ifndef BTREESTUFF
    #define SPARSETBB

    #ifdef SPARSETBB
        #include <tbb/concurrent_hash_map.h>
    #else
        #define CUCKOOHASH
        #include <libcuckoo/cuckoohash_map.hh>
    //#include <junction/ConcurrentMap_Leapfrog.h>
    #endif
#else
//    #include "eqrelbtree.h"
#endif

// branch predictor hacks
#define unlikely(x) __builtin_expect((x), 0)
#define likely(x) __builtin_expect((x), 1)

namespace souffle {

typedef uint8_t rank_t;
/* technically uint56_t, but, doesn't exist. Just be careful about storing > 2^56 elements. */
typedef uint64_t parent_t;

// number of bits that the rank is
constexpr uint8_t split_size = 8u;

// block_t stores parent in the upper half, rank in the lower half
typedef uint64_t block_t;
// block_t & rank_mask extracts the rank
constexpr block_t rank_mask = (1ul << split_size) - 1;

/**
 * Structure that emulates a Disjoint Set, i.e. a data structure that supports efficient union-find operations
 */
class DisjointSet {
    template <typename TupleType>
    friend class BinaryRelation;

#if defined SPARSETBB || defined BTREESTUFF
    //PiggyList<std::atomic<block_t>> a_blocks;
    BlockList<std::atomic<block_t>> a_blocks;
#else
    /* store blocks of atomics - needs removal for speculative inserts */
    BlockListRemovable<std::atomic<block_t>> a_blocks;
#endif

public:
    DisjointSet(){};

    inline size_t size() {
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

#if defined CUCKOOHASH
    inline void delNode(parent_t node) {
        a_blocks.remove(node);
    }
#endif

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

} // end namespace souffle

