/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/***************************************************************************
 *
 * @file RamIndexAnalysis.h
 *
 * Computes indexes for relations in a translation unit
 *
 ***************************************************************************/

#pragma once

#include "RamAnalysis.h"
#include "RamOperation.h"
#include "RamRelation.h"
#include "RamStatement.h"
#include "RamTypes.h"
#include "Util.h"
#include <cassert>
#include <cstdlib>
#include <functional>
#include <iosfwd>
#include <limits>
#include <map>
#include <memory>
#include <set>
#include <string>
#include <utility>
#include <vector>

// define if enable unit tests
#define M_UNIT_TEST

namespace souffle {

class RamTranslationUnit;

/** search signature of a RAM operation; each bit represents an attribute of a relation.
 * A one represents that the attribute has an assigned value; a zero represents that
 * no value exists (i.e. attribute is unbounded) in the search. */
class SearchSignature {
public:
    explicit SearchSignature(size_t arity, uint64_t mask) : bits(arity, false) {
        size_t len = bits.size();
        for (size_t i = 0; i < len; ++i) {
            bits[i] = (mask % 2);
            mask /= 2;
        }
    }
    // comparison operators
    inline bool operator<(const SearchSignature& other) const {
        assert(bits.size() == other.bits.size());
        size_t len = bits.size();
        for (size_t i = 0; i < len; ++i) {
            size_t index = len - i - 1;  // get the most significant bit

            // if our bit is not set and other's bit is set then it is smaller
            if (bits[index] < other.bits[index]) {
                return true;
            }
            // if other's bit is set and ours is not set then it is larger
            else if (bits[index] > other.bits[index]) {
                return false;
            }
        }
        return false;
    }
    inline bool operator>(const SearchSignature& other) const {
        return other < *this;
    }
    inline bool operator==(const SearchSignature& other) const {
        assert(bits.size() == other.bits.size());
        return bits == other.bits;
    }
    inline bool operator!=(const SearchSignature& other) const {
        return !(other == *this);
    }

    // bitwise assignment operators
    inline SearchSignature& operator|=(const SearchSignature& other) {
        assert(bits.size() == other.bits.size());
        std::transform(other.bits.begin(), other.bits.end(), bits.begin(), bits.begin(), std::bit_or<bool>());
        return *this;
    }
    inline SearchSignature& operator&=(const SearchSignature& other) {
        assert(bits.size() == other.bits.size());
        std::transform(
                other.bits.begin(), other.bits.end(), bits.begin(), bits.begin(), std::bit_and<bool>());
        return *this;
    }
    inline SearchSignature& operator^=(const SearchSignature& other) {
        assert(bits.size() == other.bits.size());
        std::transform(
                other.bits.begin(), other.bits.end(), bits.begin(), bits.begin(), std::bit_xor<bool>());
        return *this;
    }
    inline SearchSignature& operator-=(const SearchSignature& other) {
        assert(bits.size() == other.bits.size());
        for (size_t i = 0; i < bits.size(); ++i) {
            bits[i] = bits[i] && !other.bits[i];
        }
        return *this;
    }
    inline SearchSignature& operator<<=(const size_t n) {
        size_t original_size = bits.size();
        bits.insert(bits.begin(), n, 0);
        bits.erase(bits.end() - n, bits.end());
        assert(bits.size() == original_size);
        return *this;
    }
    inline SearchSignature& operator>>=(const size_t n) {
        size_t original_size = bits.size();
        bits.resize(bits.size() + n, 0);
        bits.erase(bits.begin(), bits.begin() + n);
        assert(bits.size() == original_size);
        return *this;
    }
    // set a bit
    inline SearchSignature& set(size_t pos, bool val = true) {
        assert(pos < bits.size());
        bits[pos] = val;
        return *this;
    }

    // bitwise operators
    inline SearchSignature operator~() const {
        SearchSignature s(*this);
        s.bits.flip();
        return s;
    }
    inline SearchSignature operator|(const SearchSignature& other) const {
        SearchSignature s(*this);
        return s |= other;
    }
    inline SearchSignature operator&(const SearchSignature& other) const {
        SearchSignature s(*this);
        return s &= other;
    }
    inline SearchSignature operator^(const SearchSignature& other) const {
        SearchSignature s(*this);
        return s ^= other;
    }
    inline SearchSignature operator-(const SearchSignature& other) const {
        SearchSignature s(*this);
        return s -= other;
    }
    inline SearchSignature operator<<(const size_t n) const {
        SearchSignature s(*this);
        return s <<= n;
    }
    inline SearchSignature operator>>(const size_t n) const {
        SearchSignature s(*this);
        return s >>= n;
    }

    inline size_t arity() const {
        return bits.size();
    }
    friend std::ostream& operator<<(std::ostream& out, const SearchSignature& signature);

private:
    std::vector<bool> bits;
};

inline std::ostream& operator<<(std::ostream& out, const SearchSignature& signature) {
    size_t len = signature.bits.size();
    for (size_t i = 0; i < len; ++i) {
        out << signature.bits[len - 1 - i];
    }
    return out;
}

/**
 * @class MaxMatching
 * @Brief Computes a maximum matching with Hopcroft-Karp algorithm
 *
 * This class is a helper class for RamIndexAnalysis.
 *
 * This implements a standard maximum matching algorithm for a bi-partite graph
 * also known as a marriage problem. Given a set of edges in a bi-partite graph
 * select a subset of edges that each node in the bi-partite graph has at most
 * one adjacent edge associated with.
 *
 * The nodes of the bi-partite graph represent index-signatures stemming from
 * RAM operations and RAM existence checks for a relation. A relation between
 * two nodes represent whether an index operation subsumes another operation.
 *
 * Source: http://en.wikipedia.org/wiki/Hopcroft%E2%80%93Karp_algorithm#Pseudocode
 */
class MaxMatching {
public:
    using Node = uint32_t;
    /* The nodes of the bi-partite graph are index signatures of RAM operation */
    using Nodes = std::set<Node, std::greater<Node>>;
    /* Distance between nodes */
    using Distance = int;
    /**
     * Matching represent a solution of the matching, i.e., which node in the bi-partite
     * graph maps to another node. If no map exist for a node, there is no adjacent edge
     * exists for that node.
     */
    using Matchings = std::map<Node, Node, std::greater<Node>>;

    /* Node constant representing no match */
    const Node NullVertex = 0;

    /* Infinite distance */
    const Distance InfiniteDistance = -1;

    /**
     * @Brief solve the maximum matching problem
     * @result returns the matching
     */
    const Matchings& solve();

    /**
     * @Brief get number of matches in the solution
     * @return number of matches
     */
    int getNumMatchings() const {
        return match.size() / 2;
    }

    /**
     * @Brief add an edge to the bi-partite graph
     * @param u search signature
     * @param v subsuming search signature
     */
    void addEdge(Node u, Node v);

protected:
    /**
     * @Brief get match for a search signature
     * @param v search signature
     */
    Node getMatch(Node v);

    /**
     * @Brief get distance of a node
     */
    Distance getDistance(Node v);

    /**
     * @Brief perform a breadth first search in the graph
     */
    bool bfSearch();

    /**
     * @Brief perform a depth first search in the graph
     * @param u search signature
     */
    bool dfSearch(Node u);

private:
    /**
     * Edges in the bi-partite graph
     */
    using Edges = std::set<Node>;
    /**
     * Bi-partite graph of instance
     */
    using Graph = std::map<Node, Edges>;
    /**
     * distance function of nodes
     */
    using DistanceMap = std::map<Node, Distance>;

    Matchings match;
    Graph graph;
    DistanceMap distance;
};

/**
 * @class MinIndexSelection
 * @Brief computes the minimal index cover for a relation
 *        in a RAM Program.
 *
 * If the indexes of a relation can cover several searches, the minimal
 * set of indexes is computed by Dilworth's problem. See
 *
 * "Automatic Index Selection for Large-Scale Datalog Computation"
 * http://www.vldb.org/pvldb/vol12/p141-subotic.pdf
 *
 */

class MinIndexSelection {
public:
    using AttributeIndex = uint32_t;
    using SignatureIndexMap = std::map<SearchSignature, AttributeIndex>;
    using IndexSignatureMap = std::map<AttributeIndex, SearchSignature>;
    using LexOrder = std::vector<AttributeIndex>;
    using OrderCollection = std::vector<LexOrder>;
    using Chain = std::set<SearchSignature>;
    using ChainOrderMap = std::vector<Chain>;
    using SearchSet = std::set<SearchSignature>;

    /** @Brief Add new key to an Index Set */
    inline void addSearch(SearchSignature cols) {
        SearchSignature empty(cols.arity(), 0);
        if (cols != empty) {
            searches.insert(cols);
        }
    }

    MinIndexSelection() = default;
    ~MinIndexSelection() = default;

    /** @Brief Get searches **/
    const SearchSet& getSearches() const {
        return searches;
    }

    /** @Brief Get index for a search */
    const LexOrder& getLexOrder(SearchSignature cols) const {
        int idx = map(cols);
        return orders[idx];
    }

    /** @Brief Get index for a search */
    int getLexOrderNum(SearchSignature cols) const {
        return map(cols);
    }

    /** @Brief Get all indexes */
    const OrderCollection getAllOrders() const {
        return orders;
    }

    /** @Brief Get all chains */
    const ChainOrderMap getAllChains() const {
        return chainToOrder;
    }

    /** @Brief check whether number of bits in k is not equal
        to number of columns in lexicographical order */
    bool isSubset(SearchSignature cols) const {
        int idx = map(cols);
        return card(cols) < orders[idx].size();
    }

    /** @Brief map the keys in the key set to lexicographical order */
    void solve();

    /** @Brief insert a total order index
     *  @param size of the index
     */
    void insertDefaultTotalIndex(size_t arity) {
        Chain chain = std::set<SearchSignature>();
        SearchSignature fullIndexKey(arity, 0);
        fullIndexKey = ~fullIndexKey;
        chain.insert(fullIndexKey);
        chainToOrder.push_back(std::move(chain));
        LexOrder totalOrder;
        for (size_t i = 0; i < arity; ++i) {
            totalOrder.push_back(i);
        }
        orders.push_back(std::move(totalOrder));
    }

protected:
    SignatureIndexMap signatureToIndexA;  // mapping of a SearchSignature on A to its unique index
    SignatureIndexMap signatureToIndexB;  // mapping of a SearchSignature on B to its unique index
    IndexSignatureMap indexToSignature;   // mapping of a unique index to its SearchSignature
    SearchSet searches;                   // set of search patterns on table
    OrderCollection orders;               // collection of lexicographical orders
    ChainOrderMap chainToOrder;           // maps order index to set of searches covered by chain
    MaxMatching matching;                 // matching problem for finding minimal number of orders

    /** @Brief count the number of bits in key */
    static size_t card(SearchSignature cols) {
        size_t sz = 0;
        SearchSignature empty(cols.arity(), 0);
        SearchSignature one(cols.arity(), 1);
        for (size_t i = 0; i < cols.arity(); i++) {
            if ((cols & one) != empty) {
                sz++;
            }
            cols >>= 1;
        }
        return sz;
    }

    /** @Brief maps search columns to an lexicographical order (labeled by a number) */
    int map(SearchSignature cols) const {
        assert(orders.size() == chainToOrder.size() && "Order and Chain Sizes do not match!!");
        int i = 0;
        for (auto it = chainToOrder.begin(); it != chainToOrder.end(); ++it, ++i) {
            if (it->find(cols) != it->end()) {
                assert((size_t)i < orders.size());
                return i;
            }
        }
        fatal("cannot find matching lexicographical order");
    }

    /** @Brief determine if key a is a strict subset of key b*/
    static bool isStrictSubset(SearchSignature a, SearchSignature b) {
        auto tt = SearchSignature(a.arity(), 0);
        // tt |= std::numeric_limits<uint64_t>::max();
        tt = ~tt;
        return (~(a) | (b)) == tt && a != b;
    }

    /** @Brief insert an index based on the delta */
    void insertIndex(LexOrder& ids, SearchSignature delta) {
        SearchSignature empty(delta.arity(), 0);
        SearchSignature mask(delta.arity(), 0);

        for (size_t pos = 0; pos < delta.arity(); pos++) {
            mask = SearchSignature(delta.arity(), 0);
            mask.set(pos);
            SearchSignature result = (delta) & (mask);
            if (result != empty) {
                ids.push_back(pos);
            }
        }
    }

    /** @Brief get a chain from a matching
     *  @param Starting node of a chain
     *  @param Matching
     *  @result A minimal chain
     * given an unmapped node from set A
     * we follow it from set B until it cannot be matched from B
     * if not matched from B then umn is a chain.
     */
    Chain getChain(const SearchSignature umn, const MaxMatching::Matchings& match);

    /** @Brief get all chains from the matching */
    const ChainOrderMap getChainsFromMatching(const MaxMatching::Matchings& match, const SearchSet& nodes);

    /** @Brief get all nodes which are unmatched from A-> B */
    const SearchSet getUnmatchedKeys(const MaxMatching::Matchings& match, const SearchSet& nodes) {
        SearchSet unmatched;

        // For all nodes n such that n is not in match
        for (auto node : nodes) {
            if (match.find(signatureToIndexA[node]) == match.end()) {
                unmatched.insert(node);
            }
        }
        return unmatched;
    }
};

/**
 * @class RamIndexAnalyis
 * @Brief Analysis pass computing the index sets of RAM relations
 */
class RamIndexAnalysis : public RamAnalysis {
public:
    RamIndexAnalysis(const char* id) : RamAnalysis(id) {}

    static constexpr const char* name = "index-analysis";

    void run(const RamTranslationUnit& translationUnit) override;

    void print(std::ostream& os) const override;

    /**
     * @Brief get the minimal index cover for a relation
     * @param relation
     * @result set of indexes of the minimal index cover
     */
    MinIndexSelection& getIndexes(const RamRelation& rel);

    /**
     * @Brief get the minimal index cover for a relation
     * @param relation name
     * @result set of indexes of the minimal index cover
     */
    MinIndexSelection& getIndexes(const std::string& relName);

    /**
     * @Brief Get index signature for an Ram IndexOperation operation
     * @param  Index-relation-search operation
     * @result Index signature of operation
     */
    SearchSignature getSearchSignature(const RamIndexOperation* search) const;

    /**
     * @Brief Get the index signature for an existence check
     * @param Existence check
     * @result index signature of existence check
     */
    SearchSignature getSearchSignature(const RamExistenceCheck* existCheck) const;

    /**
     * @Brief Get the index signature for a provenance existence check
     * @param Provenance-existence check
     * @result index signature of provenance-existence check
     */
    SearchSignature getSearchSignature(const RamProvenanceExistenceCheck* existCheck) const;

    /**
     * @Brief Get the default index signature for a relation (the total-order index)
     * @param ramRel RAM-relation
     * @result total full-signature of the relation
     */
    SearchSignature getSearchSignature(const RamRelation* ramRel) const;

    /**
     * @Brief index signature of existence check resembles a total index
     * @param (provenance) existence check
     *
     * isTotalSignature returns true if all elements of a tuple are used for the
     * the existence check.
     */
    bool isTotalSignature(const RamAbstractExistenceCheck* existCheck) const;

private:
    /**
     * minimal index cover for relations, i.e., maps a relation to a set of indexes
     */
    std::map<const RamRelation*, MinIndexSelection> minIndexCover;
};

}  // end of namespace souffle
