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
#include "CompiledIndexUtils.h"
#include "Util.h"

namespace souffle {

std::ostream& operator<<(std::ostream& out, TupleRef ref) {
    out << "[";
    for (std::size_t i = 0; i < ref.size(); i++) {
        if (i > 0) {
            out << ',';
        }
        out << ref[i];
    }
    return out << "]";
}

Order Order::create(size_t arity) {
    Order res;
    res.order.resize(arity);
    for (size_t i = 0; i < arity; i++) {
        res.order[i] = i;
    }
    return res;
}

std::size_t Order::size() const {
    return order.size();
}

bool Order::valid() const {
    // Check that all indices are in range.
    for (int i : order) {
        if (i < 0 || i >= int(order.size())) {
            return false;
        }
    }
    // Check that there are no duplicates.
    for (std::size_t i = 0; i < order.size(); i++) {
        for (std::size_t j = i + 1; j < order.size(); j++) {
            if (order[i] == order[j]) {
                return false;
            }
        }
    }
    return true;
}

bool Order::operator==(const Order& other) const {
    return order == other.order;
}

bool Order::operator!=(const Order& other) const {
    return !(*this == other);
}

bool Order::operator<(const Order& other) const {
    return order < other.order;
}

std::ostream& operator<<(std::ostream& out, const Order& order) {
    return out << "[" << join(order.order) << "]";
}

// An index wrapper for nullary indexes. For those, no complex
// nested data structure is required.
class NullaryIndex : public Index {
    // indicates whether the one single element is present or not.
    bool present;

    // a source adaptation, iterating through the optionally present
    // entry in this relation.
    class Source : public Stream::Source {
        bool present;

    public:
        Source(bool present) : present(present) {}
        int load(TupleRef* buffer, int max) override {
            if (!present) {
                return 0;
            }
            buffer[0] = TupleRef(nullptr, 0);
            present = false;
            return 1;
        }
    };

public:
    size_t arity() const override {
        return 0;
    }

    bool empty() const override {
        return !present;
    }

    std::size_t size() const override {
        return present ? 1 : 0;
    }

    bool insert(const TupleRef& tuple) override {
        assert(tuple.size() == 0);
        bool res = present;
        present = true;
        return res;
    }

    void insert(const Index& src) override {
        assert(src.arity() == 0);
        present = present | !src.empty();
    }

    bool contains(const TupleRef& tuple) const override {
        assert(tuple.size() == 0);
        return present;
    }

    Stream scan() const override {
        return std::make_unique<Source>(present);
    }

    Stream range(const TupleRef& low, const TupleRef& high) const override {
        return scan();
    }

    void clear() override {
        present = false;
    }
};

/**
 * A generic data structure index adapter handling the boundary
 * level order conversion as well as iteration through nested
 * data structures.
 *
 * @tparam Structure the structure to be utilized
 */
template <typename Structure>
class GenericIndex : public Index {
    using Entry = typename Structure::element_type;
    static constexpr int Arity = Entry::arity;

    // the order to be simulated
    Order order;

    // the internal data structure
    Structure data;

    // a source adapter for streaming through data
    class Source : public Stream::Source {
        const Order& order;

        // the begin and end of the stream
        using iter = typename Structure::iterator;
        iter cur;
        iter end;

        // an internal buffer for re-ordered elements
        std::array<Entry, Stream::BUFFER_SIZE> buffer;

    public:
        Source(const Order& order, iter begin, iter end) : order(order), cur(begin), end(end) {}

        int load(TupleRef* out, int max) override {
            int c = 0;
            while (cur != end && c < max) {
                buffer[c] = order.decode(*cur);
                out[c] = buffer[c];
                ++cur;
                ++c;
            }
            return c;
        }
    };

public:
    GenericIndex(const Order& order) : order(order) {}

    size_t arity() const override {
        return Arity;
    }

    bool empty() const override {
        return data.empty();
    }

    std::size_t size() const override {
        return data.size();
    }

    bool insert(const TupleRef& tuple) override {
        return data.insert(order.encode(tuple.asTuple<Arity>()));
    }

    void insert(const Index& src) override {
        // TODO: make smarter
        for (const auto& cur : src.scan()) {
            insert(cur);
        }
    }

    bool contains(const TupleRef& tuple) const override {
        return data.contains(order.encode(tuple.asTuple<Arity>()));
    }

    Stream scan() const override {
        return std::make_unique<Source>(order, data.begin(), data.end());
    }

    Stream range(const TupleRef& low, const TupleRef& high) const override {
        Entry a = order.encode(low.asTuple<Arity>());
        Entry b = order.encode(high.asTuple<Arity>());
        // Transfer upper_bound to a equivalent lower bound
        for (size_t i = Arity - 1; i >= 0; --i) {
            if (a[i] == b[i] && b[i] != MIN_RAM_DOMAIN) {
                b[i] += 1;
                break;
            }
        }
        return std::make_unique<Source>(order, data.lower_bound(a), data.lower_bound(b));
    }

    void clear() override {
        data.clear();
    }
};

// The comparator to be used for B-tree nodes.
template <std::size_t Arity>
using comparator = typename ram::index_utils::get_full_index<Arity>::type::comparator;

/**
 * A index adapter for B-trees, using the generic index adapter.
 */
template <std::size_t Arity>
class BTreeIndex : public GenericIndex<btree_set<ram::Tuple<RamDomain, Arity>, comparator<Arity>>> {
public:
    using GenericIndex<btree_set<ram::Tuple<RamDomain, Arity>, comparator<Arity>>>::GenericIndex;
};

/**
 * A index adapter for Bries, using the generic index adapter.
 */
template <std::size_t Arity>
class BrieIndex : public GenericIndex<Trie<Arity>> {
public:
    using GenericIndex<Trie<Arity>>::GenericIndex;
};

std::unique_ptr<Index> createBTreeIndex(const Order& order) {
    switch (order.size()) {
        case 0:
            return std::make_unique<NullaryIndex>();
        case 1:
            return std::make_unique<BTreeIndex<1>>(order);
        case 2:
            return std::make_unique<BTreeIndex<2>>(order);
        case 3:
            return std::make_unique<BTreeIndex<3>>(order);
        case 4:
            return std::make_unique<BTreeIndex<4>>(order);
        case 5:
            return std::make_unique<BTreeIndex<5>>(order);
        case 6:
            return std::make_unique<BTreeIndex<6>>(order);
        case 7:
            return std::make_unique<BTreeIndex<7>>(order);
        case 8:
            return std::make_unique<BTreeIndex<8>>(order);
        case 9:
            return std::make_unique<BTreeIndex<9>>(order);
        case 10:
            return std::make_unique<BTreeIndex<10>>(order);
        case 11:
            return std::make_unique<BTreeIndex<11>>(order);
        case 12:
            return std::make_unique<BTreeIndex<12>>(order);
    }
    assert(false && "Requested arity not yet supported. Feel free to add it.");
}

std::unique_ptr<Index> createBrieIndex(const Order& order) {
    switch (order.size()) {
        case 0:
            return std::make_unique<NullaryIndex>();
        case 1:
            return std::make_unique<BrieIndex<1>>(order);
        case 2:
            return std::make_unique<BrieIndex<2>>(order);
        case 3:
            return std::make_unique<BrieIndex<3>>(order);
        case 4:
            return std::make_unique<BrieIndex<4>>(order);
        case 5:
            return std::make_unique<BrieIndex<5>>(order);
        case 6:
            return std::make_unique<BrieIndex<6>>(order);
        case 7:
            return std::make_unique<BrieIndex<7>>(order);
        case 8:
            return std::make_unique<BrieIndex<8>>(order);
        case 9:
            return std::make_unique<BrieIndex<9>>(order);
        case 10:
            return std::make_unique<BrieIndex<10>>(order);
        case 11:
            return std::make_unique<BrieIndex<11>>(order);
        case 12:
            return std::make_unique<BrieIndex<12>>(order);
    }
    assert(false && "Requested arity not yet supported. Feel free to add it.");
}

LVMRelation ::LVMRelation(std::size_t arity, const std::string& name,
        std::vector<std::string>&& attributeTypes, const MinIndexSelection& orderSet, IndexFactory factory)
        : relName(name), arity(arity), attributeTypes(std::move(attributeTypes)) {
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

Stream LVMRelation::scan() const {
    return main->scan();
}

Stream LVMRelation::range(const size_t& indexPos, const TupleRef& low, const TupleRef& high) const {
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

LVMEqRelation::LVMEqRelation(size_t arity, const std::string& name, std::vector<std::string>&& attributeTypes,
        const MinIndexSelection& orderSet)
        : LVMRelation(arity, name, std::move(attributeTypes), orderSet) {}

bool LVMEqRelation::insert(const TupleRef& tuple) {
    // TODO: (pnappa) an eqrel check here is all that appears to be needed for implicit additions
    // TODO: future optimisation would require this as a member datatype
    // brave soul required to pass this quest
    // // specialisation for eqrel defs
    // std::unique_ptr<binaryrelation> eqreltuples;
    // in addition, it requires insert functions to insert into that, and functions
    // which allow reading of stored values must be changed to accommodate.
    // e.g. insert =>  eqRelTuples->insert(tuple[0], tuple[1]);

    // for now, we just have a naive & extremely slow version, otherwise known as a O(n^2) insertion
    // ):

    for (auto& newTuple : extend(tuple)) {
        LVMRelation::insert(TupleRef(newTuple, arity));
        delete[] newTuple;
    }
    return true;
}

std::vector<RamDomain*> LVMEqRelation::extend(const TupleRef& tuple) {
    std::vector<RamDomain*> newTuples;

    newTuples.push_back(new RamDomain[2]{tuple[0], tuple[0]});
    newTuples.push_back(new RamDomain[2]{tuple[0], tuple[1]});
    newTuples.push_back(new RamDomain[2]{tuple[1], tuple[0]});
    newTuples.push_back(new RamDomain[2]{tuple[1], tuple[1]});

    std::vector<const RamDomain*> relevantStored;
    for (const TupleRef& vals : this->scan()) {
        if (vals[0] == tuple[0] || vals[0] == tuple[1] || vals[1] == tuple[0] || vals[1] == tuple[1]) {
            relevantStored.push_back(new RamDomain[2]{vals[0], vals[1]});
        }
    }

    for (auto& vals : relevantStored) {
        newTuples.push_back(new RamDomain[2]{vals[0], tuple[0]});
        newTuples.push_back(new RamDomain[2]{vals[0], tuple[1]});
        newTuples.push_back(new RamDomain[2]{vals[1], tuple[0]});
        newTuples.push_back(new RamDomain[2]{vals[1], tuple[1]});
        newTuples.push_back(new RamDomain[2]{tuple[0], vals[0]});
        newTuples.push_back(new RamDomain[2]{tuple[0], vals[1]});
        newTuples.push_back(new RamDomain[2]{tuple[1], vals[0]});
        newTuples.push_back(new RamDomain[2]{tuple[1], vals[1]});
        delete[] vals;
    }
    return newTuples;
}

void LVMEqRelation::extend(const LVMRelation& rel) {
    std::vector<RamDomain*> newTuples;
    // store all values that will be implicitly relevant to the those that we will insert
    for (const auto& tuple : rel.scan()) {
        for (auto& newTuple : extend(tuple)) {
            newTuples.push_back(newTuple);
        }
    }
    for (const auto& newTuple : newTuples) {
        LVMRelation::insert(TupleRef(newTuple, arity));
        delete[] newTuple;
    }
}

}  // namespace souffle

