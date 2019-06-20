/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */
#include <LVMRelation.h>

namespace souffle {

LVMRelation ::LVMRelation(std::size_t arity, std::string& name, std::vector<std::string> attributeTypes,
        const MinIndexSelection& orderSet, IndexFactory factory)
        : arity(arity), relName(name), attributeTypes(std::move(attributeTypes)) {
    for (auto& order : orderSet.getAllOrders()) {
        assert(order.size() == arity && "Order == Arity failed");
        indexes.push_back(factory(Order(order)));
    }

    // Use the first index as default main index
    // TODO make sure first index is full index
    main = indexes[0].get();
}

void LVMRelation::removeIndex(const size_t& indexPos) {
    // All but one index can be removed, default full index can't be removed.
    assert(indexes.size() > 1 || indexPos != 0);
    indexes[indexPos].reset(nullptr);
}

bool LVMRelation::insert(TupleRef tuple) {
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

bool LVMRelation::contains(TupleRef tuple) const {
    return main->contains(tuple);
}

Stream LVMRelation::scan() const {
    return main->scan();
}

Stream LVMRelation::range(const size_t& indexPos, TupleRef low, TupleRef high) const {
    auto& pos = indexes[indexPos];
    return pos->range(low, high);
}

void LVMRelation::clear() {
    for (const auto& cur : indexes) {
        cur->clear();
    }
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

size_t LVMRelation::getArity() const {
    return this->arity;
}

size_t LVMRelation::size() const {
    return main->size();
}

bool LVMRelation::empty() const {
    return main->size() == 0;
}

void LVMRelation::purge() {
    for (auto& index : indexes) {
        index->clear();
    }
}

bool LVMRelation::exists(const TupleRef& tuple) const {
    return main->contains(tuple);
}

void extend(const LVMRelation& rel) {}

}  // namespace souffle
