/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file IndexAnalysis.cpp
 *
 * Computes indexes for relations in a translation unit
 *
 ***********************************************************************/

#include "RamIndexAnalysis.h"
#include "RamCondition.h"
#include "RamNode.h"
#include "RamOperation.h"
#include "RamTranslationUnit.h"
#include "RamVisitor.h"
#include <cstdint>
#include <cstdlib>
#include <iostream>
#include <iterator>
#include <queue>

namespace souffle {

void MaxMatching::addEdge(SearchSignature u, SearchSignature v) {
    if (graph.find(u) == graph.end()) {
        Edges vals;
        vals.insert(v);
        graph.insert(make_pair(u, vals));
    } else {
        graph[u].insert(v);
    }
}

SearchSignature MaxMatching::getMatch(SearchSignature v) {
    auto it = match.find(v);
    if (it == match.end()) {
        return SearchSignature(v.arity(), RIA_NIL);
    }
    return it->second;
}

int MaxMatching::getDistance(SearchSignature v) {
    auto it = distance.find(v);
    if (it == distance.end()) {
        return RIA_INF;
    }
    return it->second;
}

bool MaxMatching::bfSearch(size_t arity) {
    std::queue<SearchSignature> bfQueue;
    // Build layers
    for (auto& it : graph) {
        arity = it.first.arity();
        if (getMatch(it.first) == SearchSignature(arity, RIA_NIL)) {
            distance[it.first] = 0;
            bfQueue.push(it.first);
        } else {
            distance[it.first] = RIA_INF;
        }
    }

    distance.insert({SearchSignature(arity, RIA_NIL), RIA_INF});
    while (!bfQueue.empty()) {
        SearchSignature u = bfQueue.front();
        bfQueue.pop();
        assert(u != SearchSignature(arity, RIA_NIL));
        const Edges& children = graph[u];
        for (auto it : children) {
            SearchSignature mv = getMatch(it);
            if (getDistance(mv) == RIA_INF) {
                distance[mv] = getDistance(u) + 1;
                if (mv != SearchSignature(arity, RIA_NIL)) {
                    bfQueue.push(mv);
                }
            }
        }
    }
    return (getDistance(SearchSignature(arity, 0)) != RIA_INF);
}

bool MaxMatching::dfSearch(SearchSignature u) {
    SearchSignature zero(u.arity(), 0);
    if (u != zero) {
        Edges& children = graph[u];
        for (auto v : children) {
            if (getDistance(getMatch(v)) == getDistance(u) + 1) {
                if (dfSearch(getMatch(v))) {
                    match.insert({u, v});
                    match.insert({v, u});
                    return true;
                }
            }
        }

        distance[u] = RIA_INF;
        return false;
    }
    return true;
}

const MaxMatching::Matchings& MaxMatching::solve(size_t arity) {
    while (bfSearch(arity)) {
        for (auto& it : graph) {
            if (getMatch(it.first) == SearchSignature(arity, RIA_NIL)) {
                dfSearch(it.first);
            }
        }
    }
    return match;
}

void MinIndexSelection::solve() {
    // map the keys in the key set to lexicographical order
    if (searches.empty()) {
        return;
    }

    // Construct the matching poblem
    for (auto search : searches) {
        // For this node check if other nodes are strict subsets
        for (auto itt : searches) {
            if (isStrictSubset(search, itt)) {
                matching.addEdge(search, toB(itt));
            }
        }
    }

    // Perform the Hopcroft-Karp on the graph and receive matchings (mapped A->B and B->A)
    // Assume: alg.calculate is not called on an empty graph
    assert(!searches.empty());
    size_t arity = searches.begin()->arity();
    const MaxMatching::Matchings& matchings = matching.solve(arity);

    // Extract the chains given the nodes and matchings
    const ChainOrderMap chains = getChainsFromMatching(matchings, searches);

    // Should never get no chains back as we never call calculate on an empty graph
    assert(!chains.empty());
    for (const auto& chain : chains) {
        std::vector<int> ids;
        SearchSignature initDelta = *(chain.begin());
        insertIndex(ids, initDelta);

        for (auto iit = chain.begin(); next(iit) != chain.end(); ++iit) {
            SearchSignature delta = *(next(iit)) - *iit;
            insertIndex(ids, delta);
        }

        assert(!ids.empty());

        orders.push_back(ids);
    }

    // Construct the matching poblem
    for (auto search : searches) {
        int idx = map(search);
        size_t l = card(search);
        SearchSignature k(search.arity(), 0);
        for (size_t i = 0; i < l; i++) {
            k.set(orders[idx][i]);
        }
        assert(k == search && "incorrect lexicographical order");
    }
}

MinIndexSelection::Chain MinIndexSelection::getChain(
        const SearchSignature umn, const MaxMatching::Matchings& match) {
    SearchSignature start = umn;  // start at an unmatched node
    Chain chain;
    // given an unmapped node from set A we follow it from set B until it cannot be matched from B
    //  if not mateched from B then umn is a chain
    //
    // Assume : no circular mappings, i.e. a in A -> b in B -> ........ -> a in A is not allowed.
    // Given this, the loop will terminate
    while (true) {
        auto mit = match.find(toB(start));  // we start from B side
        chain.insert(start);

        if (mit == match.end()) {
            return chain;
        }

        SearchSignature a = mit->second;
        chain.insert(a);
        start = a;
    }
}

const MinIndexSelection::ChainOrderMap MinIndexSelection::getChainsFromMatching(
        const MaxMatching::Matchings& match, const SearchSet& nodes) {
    assert(!nodes.empty());

    // Get all unmatched nodes from A
    const SearchSet& umKeys = getUnmatchedKeys(match, nodes);

    // Case: if no unmatched nodes then we have an anti-chain
    if (umKeys.empty()) {
        for (auto node : nodes) {
            std::set<SearchSignature> a;
            a.insert(node);
            chainToOrder.push_back(a);
            return chainToOrder;
        }
    }

    assert(!umKeys.empty());

    // A worklist of used nodes
    SearchSet usedKeys;

    // Case: nodes < umKeys or if nodes == umKeys then anti chain - this is handled by this loop
    for (auto umKey : umKeys) {
        Chain c = getChain(umKey, match);
        assert(!c.empty());
        chainToOrder.push_back(c);
    }

    assert(!chainToOrder.empty());

    return chainToOrder;
}

void RamIndexAnalysis::run(const RamTranslationUnit& translationUnit) {
    // After complete:
    // 1. All relations should have at least one index (for full-order search).
    // 2. Two relations involved in a swap operation will have same set of indices.
    // 3. A 0-arity relation will have only one index where LexOrder is defined as empty. A comparator using
    // an empty order should regard all elements as equal and therefore only allow one arbitrary tuple to be
    // inserted.
    //
    // TODO:
    // 0-arity relation in a provenance program still need to be revisited.

    // visit all nodes to collect searches of each relation
    visitDepthFirst(translationUnit.getProgram(), [&](const RamNode& node) {
        if (const auto* indexSearch = dynamic_cast<const RamIndexOperation*>(&node)) {
            MinIndexSelection& indexes = getIndexes(indexSearch->getRelation());
            indexes.addSearch(getSearchSignature(indexSearch));
        } else if (const auto* exists = dynamic_cast<const RamExistenceCheck*>(&node)) {
            MinIndexSelection& indexes = getIndexes(exists->getRelation());
            indexes.addSearch(getSearchSignature(exists));
        } else if (const auto* provExists = dynamic_cast<const RamProvenanceExistenceCheck*>(&node)) {
            MinIndexSelection& indexes = getIndexes(provExists->getRelation());
            indexes.addSearch(getSearchSignature(provExists));
        } else if (const auto* ramRel = dynamic_cast<const RamRelation*>(&node)) {
            MinIndexSelection& indexes = getIndexes(*ramRel);
            indexes.addSearch(getSearchSignature(ramRel));
        }
    });

    // A swap happen between rel A and rel B indicates A should include all indices of B, vice versa.
    visitDepthFirst(translationUnit.getProgram(), [&](const RamSwap& swap) {
        // Note: this naive approach will not work if there exists chain or cyclic swapping.
        // e.g.  swap(relA, relB) swap(relB, relC) swap(relC, relA)
        // One need to keep merging the search set until a fixed point where no more index is introduced
        // in any of the relation in a complete iteration.
        //
        // Currently RAM does not have such situation.
        const RamRelation& relA = swap.getFirstRelation();
        const RamRelation& relB = swap.getSecondRelation();

        MinIndexSelection& indexesA = getIndexes(relA);
        MinIndexSelection& indexesB = getIndexes(relB);
        // Add all searchSignature of A into B
        for (const auto& signature : indexesA.getSearches()) {
            indexesB.addSearch(signature);
        }

        // Add all searchSignature of B into A
        for (const auto& signature : indexesB.getSearches()) {
            indexesA.addSearch(signature);
        }
    });

    // find optimal indexes for relations
    for (auto& cur : minIndexCover) {
        MinIndexSelection& indexes = cur.second;
        indexes.solve();
    }

    // Only case where indexSet is still empty is when relation has arity == 0
    for (auto& cur : minIndexCover) {
        MinIndexSelection& indexes = cur.second;
        if (indexes.getAllOrders().empty()) {
            indexes.insertDefaultTotalIndex(0);
        }
    }
}

MinIndexSelection& RamIndexAnalysis::getIndexes(const RamRelation& rel) {
    auto pos = minIndexCover.find(&rel);
    if (pos != minIndexCover.end()) {
        return pos->second;
    } else {
        auto ret = minIndexCover.insert(std::make_pair(&rel, MinIndexSelection()));
        assert(ret.second);
        return ret.first->second;
    }
}

void RamIndexAnalysis::print(std::ostream& os) const {
    for (auto& cur : minIndexCover) {
        const RamRelation& rel = *cur.first;
        const MinIndexSelection& indexes = cur.second;
        const std::string& relName = rel.getName();

        /* Print searches */
        os << "Relation " << relName << "\n";
        os << "\tNumber of Primitive Searches: " << indexes.getSearches().size() << "\n";

        const auto& attrib = rel.getAttributeNames();
        uint32_t arity = rel.getArity();
        SearchSignature zero(arity, 0);

        /* print searches */
        for (auto& cols : indexes.getSearches()) {
            os << "\t\t";
            for (uint32_t i = 0; i < arity; i++) {
                if ((cols & SearchSignature(arity, 1UL << i)) != zero) {
                    os << attrib[i] << " ";
                }
            }
            os << "\n";
        }

        os << "\tNumber of Indexes: " << indexes.getAllOrders().size() << "\n";
        for (auto& order : indexes.getAllOrders()) {
            os << "\t\t";
            for (auto& i : order) {
                os << attrib[i] << " ";
            }
            os << "\n";
        }
    }
}

namespace {
template <typename Iter>
SearchSignature searchSignature(size_t arity, Iter const& bgn, Iter const& end) {
    SearchSignature keys(arity, 0);

    size_t i = 0;
    for (auto cur = bgn; cur != end; ++cur, ++i) {
        if (!isRamUndefValue(*cur)) {
            keys.set(i);
        }
    }
    return keys;
}

template <typename Seq>
SearchSignature searchSignature(size_t arity, Seq const& xs) {
    return searchSignature(arity, xs.begin(), xs.end());
}
}  // namespace

SearchSignature RamIndexAnalysis::getSearchSignature(const RamIndexOperation* search) const {
    size_t arity = search->getRelation().getArity();
    for (size_t i = 0; i < arity; ++i) {
        if (*(search->getRangePattern().first[i]) != *(search->getRangePattern().second[i])) {
            fatal("Lower and upper bounds do not match when retrieving search signature.");
        }
    }
    return searchSignature(arity, search->getRangePattern().second);
}

SearchSignature RamIndexAnalysis::getSearchSignature(
        const RamProvenanceExistenceCheck* provExistCheck) const {
    const auto values = provExistCheck->getValues();
    auto auxiliaryArity = provExistCheck->getRelation().getAuxiliaryArity();

    // values.size() - auxiliaryArity because we discard the height annotations
    auto const numSig = values.size() - auxiliaryArity;
    return searchSignature(auxiliaryArity, values.begin(), values.begin() + numSig);
}

SearchSignature RamIndexAnalysis::getSearchSignature(const RamExistenceCheck* existCheck) const {
    return searchSignature(existCheck->getRelation().getArity(), existCheck->getValues());
}

SearchSignature RamIndexAnalysis::getSearchSignature(const RamRelation* ramRel) const {
    SearchSignature s(ramRel->getArity(), 0);
    s = ~s;
    return s;
}

bool RamIndexAnalysis::isTotalSignature(const RamAbstractExistenceCheck* existCheck) const {
    for (const auto& cur : existCheck->getValues()) {
        if (isRamUndefValue(cur)) {
            return false;
        }
    }
    return true;
}

}  // end of namespace souffle
