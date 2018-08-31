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

#include "DisjointSet.h"

#include "eqrelbtree.h"

// branch predictor hacks
#define unlikely(x) __builtin_expect((x), 0)
#define likely(x) __builtin_expect((x), 1)

namespace souffle {

template<typename StorePair>
struct EqrelMapComparator {
    int operator()(const StorePair a, const StorePair b) {
        if (a.first < b.first) return -1;
        else if (b.first < a.first) return 1;
        else return 0;
    }   

    bool less(const StorePair a, const StorePair b) {
        return operator()(a,b) < 0;
    }

    bool equal(const StorePair a, const StorePair b) {
        return operator()(a,b) == 0;
    }
};

template <typename SparseDomain>
class SparseDisjointSet {
    DisjointSet ds;

    template <typename TupleType>
    friend class BinaryRelation;

#if defined BTREESTUFF
    typedef std::pair<SparseDomain, parent_t> PairStore;
    typedef souffle::IncrementingBTreeSet<PairStore, EqrelMapComparator<PairStore>> SparseMap;
    typedef souffle::BlockList<SparseDomain> DenseMap;
    typename SparseMap::operation_hints last_ins;
#elif defined SPARSETBB
    typedef tbb::concurrent_hash_map<SparseDomain, parent_t> SparseMap;
    typedef souffle::BlockList<SparseDomain> DenseMap;
#else
    typedef cuckoohash_map<SparseDomain, parent_t> SparseMap;
    typedef souffle::BlockList<SparseDomain> DenseMap;
#endif
    SparseMap sparseToDenseMap;
    // mapping from union-find val to souffle, union-find encoded as index
    DenseMap denseToSparseMap;

private:
    /**
     * Retrieve dense encoding, adding it in if non-existent
     * @param in the sparse value
     * @return the corresponding dense value
     */
    parent_t toDense(const SparseDomain in) {

#if defined BTREESTUFF
        // TODO:
        //parent_t densi = sparseToDenseMap.insert({in, 0}, last_ins, denseToSparseMap, ds);
        parent_t densi = sparseToDenseMap.insert({in, 0}, last_ins, [&](typename PairStore::first_type d){ 
                size_t c2 = this->ds.makeNode(); 
                this->denseToSparseMap.insertAt(c2, d);
                return c2;
        });
        // TODO: make the node in the dj set
        //denseToSparseMap.insertAt(densi, in);
        return densi;
#elif defined SPARSETBB

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
#else
        parent_t dense;
        bool exists = sparseToDenseMap.find(in, dense);
        if (!exists) {
            // attempt to insert a new val
            dense = ds.b2p(ds.makeNode());
            sparseToDenseMap.upsert(in, [this,&dense](parent_t& v) { this->ds.delNode(dense); dense=v; }, dense);
            denseToSparseMap.insertAt(dense, in);
        }
        return dense;
#endif

    }

public:
    // warning! not thread safe, do not perform copy operations
    // XXX: is this actually a copy-op? Doesn't look like it...
    SparseDisjointSet& operator=(const SparseDisjointSet& old) {
        if (&old == this) return *this;

        ds = old.ds;
        sparseToDenseMap = old.sparseToDenseMap;
        //denseToSparseMap = old.denseToSparseMap;

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
#if defined BTREESTUFF
        //...
        sparseToDenseMap.clear();
#elif defined SPARSETBB
        sparseToDenseMap.clear();
#else
        // we can't clear the junction map.. so lets indiana jones it
        SparseMap replacer;
        std::swap(sparseToDenseMap, replacer);
#endif
        denseToSparseMap.clear();
    }

    /* wrapper for node creation */
    inline void makeNode(SparseDomain val) {
        // dense has the behaviour of creating if not exists.
        toDense(val);
    };

    /* whether we the supplied node exists */
    inline bool nodeExists(const SparseDomain val) const {
#if defined SPARSETBB
        return sparseToDenseMap.count(val);
#elif defined BTREESTUFF
        return sparseToDenseMap.contains({val, 0});
#else
        return sparseToDenseMap.contains(val);
#endif
    };

    inline bool contains(SparseDomain v1, SparseDomain v2) {
        if (nodeExists(v1) && nodeExists(v2)) {
            return sameSet(v1, v2);
        }
        return false;
    }
};
}
