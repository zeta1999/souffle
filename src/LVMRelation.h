/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file LVMRelation.h
 *
 * Defines LVM Relations
 *
 ***********************************************************************/

#pragma once

#include "LVMIndex.h"
#include "ParallelUtils.h"
#include "RamIndexAnalysis.h"
#include "RamTypes.h"

#include <deque>
#include <map>
#include <memory>
#include <utility>
#include <vector>

namespace souffle {

class LVMRelation {
    using LexOrder = std::vector<int>;

public:
    using iterator = LVMIndex::iterator;

    LVMRelation(size_t relArity, size_t numberOfHeights, const MinIndexSelection* orderSet, std::string& relName,
            std::vector<std::string>& attributeTypes)
            : arity(relArity), numberOfHeights(numberOfHeights), orderSet(orderSet), relName(relName), attributeTypeQualifiers(attributeTypes) {

    }

    LVMRelation(const LVMRelation& other) = delete;

    virtual ~LVMRelation() = default;

    /** Get AttributeType for the relation */
    std::vector<std::string>& getAttributeTypeQualifiers() {
        return attributeTypeQualifiers;
    }

    /** Return relation name */
    const std::string& getName() const {
        return relName;
    }

    /** Set relation level */
    void setLevel(size_t level) {
        this->level = level;
    }

    /** Get relation level */
    size_t getLevel() {
        return level;
    }

    /** Get arity of relation */
    size_t getArity() const {
        return arity;
    }

    /** Get arity of relation */
	size_t getNumberOfHeights() const {
		return numberOfHeights;
	}

    /** Gets the number of contained tuples */
    virtual size_t size() const {
        return num_tuples;
    }

    /** Check whether relation is empty */
    virtual bool empty() const {
        return num_tuples == 0;
    }

    /** Insert tuple */
    virtual void insert(const RamDomain* tuple) = 0;

    /** Merge another relation into this relation */
    virtual void insert(const LVMRelation& other) = 0;

    /** Purge table */
    virtual void purge() = 0;

    /** check whether a tuple exists in the relation */
    virtual bool exists(const RamDomain* tuple) const = 0;

    /** Iterator for relation, uses full-order index as default */
    virtual iterator begin() const = 0;

    virtual iterator end() const = 0;

    /** Return range iterator */
    virtual std::pair<iterator, iterator> lowerUpperBound(
            const RamDomain* low, const RamDomain* high, size_t indexPosition) const = 0;

    /** Extend tuple */
    virtual std::vector<RamDomain*> extend(const RamDomain* tuple) = 0;

    /** Extend relation */
    virtual void extend(const LVMRelation& rel) = 0;

protected:
    /** Relation level */
    size_t level = 0;

    /** Arity of relation */
    const size_t arity;

    /** Number of height parameters of relation */
	const size_t numberOfHeights;

    /** Number of tuples in relation */
    size_t num_tuples = 0;

    /** IndexSet */
    const MinIndexSelection* orderSet;

    /** Relation name */
    const std::string relName;

    /** Type of attributes */
    std::vector<std::string> attributeTypeQualifiers;
};

/**
 * Interpreter Relation
 */
class LVMIndirectRelation : public LVMRelation {
public:
    LVMIndirectRelation(size_t relArity, size_t numberOfHeights, const MinIndexSelection* orderSet, std::string& relName,
            std::vector<std::string>& attributeTypes)
            : LVMRelation(relArity, numberOfHeights, orderSet, relName, attributeTypes) {
        for (auto& order : orderSet->getAllOrders()) {
            indices.push_back(LVMIndex(order));
        }
    }

    /** Insert tuple */
    void insert(const RamDomain* tuple) override {
        assert(tuple);

        // make existence check
        if (exists(tuple)) {
            return;
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
        for (auto& cur : indices) {
            cur.insert(newTuple);
        }

        // increment relation size
        num_tuples++;
    }

    /** Merge another relation into this relation */
    void insert(const LVMRelation& other) override {
        assert(getArity() == other.getArity());
        for (const auto& cur : other) {
            insert(cur);
        }
    }

    /** Purge table */
    void purge() override {
        blockList.clear();
        for (auto& cur : indices) {
            cur.purge();
        }
        num_tuples = 0;
    }

    /** check whether a tuple exists in the relation */
    bool exists(const RamDomain* tuple) const override {
        LVMIndex* index = getIndex(getTotalIndexKey());
        return index->exists(tuple);
    }

    /** Iterator for relation, uses full-order index as default */
    iterator begin() const override {
        return indices[0].begin();
    }

    iterator end() const override {
        return indices[0].end();
    }

    /** Return range iterator */
    std::pair<iterator, iterator> lowerUpperBound(
            const RamDomain* low, const RamDomain* high, size_t indexPosition) const override {
        auto idx = this->getIndexByPos(indexPosition);
        return idx->lowerUpperBound(low, high);
    }

    /** Extend tuple */
    std::vector<RamDomain*> extend(const RamDomain* tuple) override {
        std::vector<RamDomain*> newTuples;

        // A standard relation does not generate extra new knowledge on insertion.
        newTuples.push_back(new RamDomain[2]{tuple[0], tuple[1]});

        return newTuples;
    }

    /** Extend relation */
    void extend(const LVMRelation& rel) override {}

    /** get index for a given search signature. Order are encoded as bits for each column */
    LVMIndex* getIndex(const SearchSignature& col) const {
        // Special case in provenance program, a 0 searchSignature is considered as a full search
        if (col == 0) {
            return getIndex(getTotalIndexKey());
        }
        return getIndexByPos(orderSet->getLexOrderNum(col));
    }

    /** get index for a given order. Order are encoded as bits for each column */
    LVMIndex* getIndexByPos(int idx) const {
        return &indices[idx];
    }

    /** Obtains a full index-key for this relation */
    SearchSignature getTotalIndexKey() const {
        return (1 << (getArity())) - 1;
    }

private:
    /** Size of blocks containing tuples */
    static const int BLOCK_SIZE = 1024;

    std::deque<std::unique_ptr<RamDomain[]>> blockList;

    /** List of indices */
    mutable std::vector<LVMIndex> indices;
};

/**
 * Interpreter Nullary relation
 */

class LVMNullaryRelation : public LVMRelation {
public:
    LVMNullaryRelation(std::string relName, std::vector<std::string>& attributeTypes)
            : LVMRelation(0, 0, nullptr, relName, attributeTypes), nullaryIndex(std::vector<int>()) {}
    		//TODO (sarah) what to putfor numberOfHeights here?

    /** Insert tuple into nullary relation */
    void insert(const RamDomain* tuple) override {
        if (!inserted) {
            nullaryIndex.insert(tuple);
        }
        inserted = true;
    }

    /** Merge another relation into this relation */
    void insert(const LVMRelation& other) override {
        if (!other.empty() && !inserted) {
            insert(nullptr);
        }
    }

    /** Size of nullary is either 0 or 1 */
    size_t size() const override {
        return inserted == true ? 1 : 0;
    }

    bool empty() const override {
        return !inserted;
    }

    /** Purge table */
    void purge() override {
        inserted = false;
    }

    /** check whether a tuple exists in the relation */
    bool exists(const RamDomain* tuple) const override {
        return inserted;
    }

    /** Iterator for relation, uses full-order index as default */
    iterator begin() const override {
        return nullaryIndex.begin();
    }

    iterator end() const override {
        return nullaryIndex.end();
    }

    /** Return range iterator */
    std::pair<iterator, iterator> lowerUpperBound(
            const RamDomain* low, const RamDomain* high, size_t indexPosition) const override {
        return std::make_pair(begin(), end());
    }

    /** Extend tuple */
    std::vector<RamDomain*> extend(const RamDomain* tuple) override {
        std::vector<RamDomain*> newTuples;

        // A standard relation does not generate extra new knowledge on insertion.
        newTuples.push_back(new RamDomain[2]{tuple[0], tuple[1]});

        return newTuples;
    }

    /** Extend relation */
    void extend(const LVMRelation& rel) override {}

private:
    /** Nullary can hold only one tuple */
    bool inserted = false;


    /** Nullary index with empty search signature */
    LVMIndex nullaryIndex;
};

/**
 * Interpreter Equivalence Relation
 */

class LVMEqRelation : public LVMIndirectRelation {
public:
    LVMEqRelation(size_t relArity, size_t numberOfHeights, const MinIndexSelection* orderSet, std::string relName,
            std::vector<std::string>& attributeTypes)
            : LVMIndirectRelation(relArity, numberOfHeights, orderSet, relName, attributeTypes) {}

    /** Insert tuple */
    void insert(const RamDomain* tuple) override {
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

        for (auto* newTuple : extend(tuple)) {
            LVMIndirectRelation::insert(newTuple);
            delete[] newTuple;
        }
    }

    /** Find the new knowledge generated by inserting a tuple */
    std::vector<RamDomain*> extend(const RamDomain* tuple) override {
        std::vector<RamDomain*> newTuples;

        newTuples.push_back(new RamDomain[2]{tuple[0], tuple[0]});
        newTuples.push_back(new RamDomain[2]{tuple[0], tuple[1]});
        newTuples.push_back(new RamDomain[2]{tuple[1], tuple[0]});
        newTuples.push_back(new RamDomain[2]{tuple[1], tuple[1]});

        std::vector<const RamDomain*> relevantStored;
        for (const RamDomain* vals : *this) {
            if (vals[0] == tuple[0] || vals[0] == tuple[1] || vals[1] == tuple[0] || vals[1] == tuple[1]) {
                relevantStored.push_back(vals);
            }
        }

        for (const auto vals : relevantStored) {
            newTuples.push_back(new RamDomain[2]{vals[0], tuple[0]});
            newTuples.push_back(new RamDomain[2]{vals[0], tuple[1]});
            newTuples.push_back(new RamDomain[2]{vals[1], tuple[0]});
            newTuples.push_back(new RamDomain[2]{vals[1], tuple[1]});
            newTuples.push_back(new RamDomain[2]{tuple[0], vals[0]});
            newTuples.push_back(new RamDomain[2]{tuple[0], vals[1]});
            newTuples.push_back(new RamDomain[2]{tuple[1], vals[0]});
            newTuples.push_back(new RamDomain[2]{tuple[1], vals[1]});
        }

        return newTuples;
    }
    /** Extend this relation with new knowledge generated by inserting all tuples from a relation */
    void extend(const LVMRelation& rel) override {
        std::vector<RamDomain*> newTuples;
        // store all values that will be implicitly relevant to the those that we will insert
        for (const auto* tuple : rel) {
            for (auto* newTuple : extend(tuple)) {
                newTuples.push_back(newTuple);
            }
        }
        for (const auto* newTuple : newTuples) {
            LVMIndirectRelation::insert(newTuple);
            delete[] newTuple;
        }
    }
};

}  // end of namespace souffle
