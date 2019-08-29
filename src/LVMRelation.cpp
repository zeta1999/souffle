/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file LVMRelation.cpp
 *
 * Implement LVM Relations
 *
 ***********************************************************************/

#include "LVMRelation.h"
#include "BTree.h"
#include "Brie.h"
#include "EquivalenceRelation.h"
#include "Util.h"

namespace souffle {

LVMRelation::LVMRelation(std::size_t arity, const std::string& name,
        const std::vector<std::string>& attributeTypes, const MinIndexSelection& orderSet,
        IndexFactory factory)
        : relName(name), arity(arity), attributeTypes(attributeTypes) {
    for (auto order : orderSet.getAllOrders()) {
        // Expand the order to a total order
        std::set<int> set;
        for (const auto& i : order) {
            set.insert(i);
        }
        for (std::size_t i = 0; i < arity; ++i) {
            if (set.find(i) == set.end()) {
                order.push_back(i);
            }
        }
        indexes.push_back(factory(Order(order)));
    }

    // Use the first index as default main index
    main = indexes[0].get();
}

void LVMRelation::removeIndex(const size_t& indexPos) {
    // All but one index can be removed, default full index can't be removed.
    assert(indexes.size() > 1 || indexPos != 0);
    indexes[indexPos].reset(nullptr);
}

IndexViewPtr LVMRelation::getView(const size_t& indexPos) const {
    assert(indexPos < indexes.size());
    return indexes[indexPos]->createView();
}

bool LVMRelation::insert(const TupleRef& tuple) {
    if (!main->insert(tuple)) return false;
    for (const auto& cur : indexes) {
        if (cur.get() == main) continue;
        cur->insert(tuple);
    }
    return true;
}

void LVMRelation::insert(const LVMRelation& other) {
    // TODO: cover this in a smarter way
    for (const auto& cur : other.scan()) {
        insert(cur);
    }
}

bool LVMRelation::contains(const TupleRef& tuple) const {
    return main->contains(tuple);
}

bool LVMRelation::contains(const size_t& indexPos, const TupleRef& low, const TupleRef& high) const {
    return indexes[indexPos]->contains(low, high);
}

Stream LVMRelation::scan() const {
    return main->scan();
}

PartitionedStream LVMRelation::partitionScan(size_t partitionCount) const {
    return main->partitionScan(partitionCount);
}

Stream LVMRelation::range(const size_t& indexPos, const TupleRef& low, const TupleRef& high) const {
    auto& pos = indexes[indexPos];
    return pos->range(low, high);
}

PartitionedStream LVMRelation::partitionRange(
        const size_t& indexPos, const TupleRef& low, const TupleRef& high, size_t partitionCount) const {
    auto& pos = indexes[indexPos];
    return pos->partitionRange(low, high, partitionCount);
}

void LVMRelation::swap(LVMRelation& other) {
    indexes.swap(other.indexes);
}

size_t LVMRelation::getLevel() const {
    return this->level;
}

const std::string& LVMRelation::getName() const {
    return this->relName;
}

const std::vector<std::string>& LVMRelation::getAttributeTypeQualifiers() const {
    return this->attributeTypes;
}

size_t LVMRelation::size() const {
    return main->size();
}

bool LVMRelation::empty() const {
    return main->empty();
}

void LVMRelation::purge() {
    for (auto& index : indexes) {
        index->clear();
    }
}

bool LVMRelation::exists(const TupleRef& tuple) const {
    return main->contains(tuple);
}

void LVMRelation::extend(const LVMRelation& rel) {}

LVMEqRelation::LVMEqRelation(size_t arity, const std::string& name,
        const std::vector<std::string>& attributeTypes, const MinIndexSelection& orderSet)
        : LVMRelation(arity, name, attributeTypes, orderSet, createEqrelIndex) {
    // EqivalenceRelation should have only index.
    assert(this->indexes.size() == 1);
}

void LVMEqRelation::extend(const LVMRelation& rel) {
    auto otherEqRel = dynamic_cast<const LVMEqRelation*>(&rel);
    assert(otherEqRel != nullptr && "A eqRel can only merge with another eqRel");
    this->main->extend(otherEqRel->main);
}

LVMIndirectRelation::LVMIndirectRelation(size_t arity, const std::string& name,
        const std::vector<std::string>& attributeTypes, const MinIndexSelection& orderSet)
        : LVMRelation(arity, name, attributeTypes, orderSet, createIndirectIndex) {}

bool LVMIndirectRelation::insert(const TupleRef& tuple) {
    if (main->contains(tuple)) {
        return false;
    }

    int blockIndex = num_tuples / (BLOCK_SIZE / arity);
    int tupleIndex = (num_tuples % (BLOCK_SIZE / arity)) * arity;

    if (tupleIndex == 0) {
        blockList.push_back(std::make_unique<RamDomain[]>(BLOCK_SIZE));
    }

    RamDomain* newTuple = &blockList[blockIndex][tupleIndex];
    for (size_t i = 0; i < arity; ++i) {
        newTuple[i] = tuple[i];
    }

    // update all indexes with new tuple
    for (auto& cur : indexes) {
        cur->insert(TupleRef(newTuple, arity));
    }

    // increment relation size
    num_tuples++;

    return true;
}

bool LVMIndirectRelation::insert(const RamDomain* tuple) {
    return this->insert(TupleRef(tuple, arity));
}

void LVMIndirectRelation::purge() {
    blockList.clear();
    for (auto& cur : indexes) {
        cur->clear();
    }
    num_tuples = 0;
}

}  // namespace souffle
