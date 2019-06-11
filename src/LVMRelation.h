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

    using iterator = LVMIndex::iterator;

public:
    LVMRelation(size_t relArity, const MinIndexSelection* orderSet, std::string& relName,
            std::vector<std::string>& attributeTypes)
            : arity(relArity), orderSet(orderSet), relName(relName), attributeTypeQualifiers(attributeTypes) {
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

    /** Get arity of relation */
    virtual size_t getArity() const {
        return arity;
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
            const RamDomain* low, const RamDomain* high, size_t indexPosition) = 0;

    /** Extend tuple */
    virtual std::vector<RamDomain*> extend(const RamDomain* tuple) = 0;

    /** Extend relation */
    virtual void extend(const LVMRelation& rel) = 0;

protected:
    /** Arity of relation */
    const size_t arity;

    /** Number of tuples in relation */
    size_t num_tuples = 0;

    /** List of indices */
    mutable std::vector<LVMIndex> indices;

    /** IndexSet */
    const MinIndexSelection* orderSet;

    /** Type of attributes */
    std::vector<std::string> attributeTypeQualifiers;

    /** Relation name */
    const std::string relName;
};

/**
 * Interpreter Relation
 */
class LVMIndirectRelation : public LVMRelation {
    using LexOrder = std::vector<int>;

    using iterator = LVMIndex::iterator;

public:
    LVMIndirectRelation(size_t relArity, const MinIndexSelection* orderSet, std::string& relName,
            std::vector<std::string>& attributeTypes)
            : LVMRelation(relArity, orderSet, relName, attributeTypes) {
        for (auto& order : orderSet->getAllOrders()) {
            indices.push_back(LVMIndex(order));
        }
    }

    LVMIndirectRelation(const LVMRelation& other) = delete;

    virtual ~LVMIndirectRelation() = default;

    /** Insert tuple */
    virtual void insert(const RamDomain* tuple) override {
        assert(tuple);

        // make existence check
        if (exists(tuple)) {
            return;
        }

        // check for null-arity
        if (arity == 0) {
            indices[0].insert(tuple);
            num_tuples = 1;
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
    virtual void insert(const LVMRelation& other) override {
        assert(getArity() == other.getArity());
        for (const auto& cur : other) {
            insert(cur);
        }
    }

    /** Purge table */
    virtual void purge() override {
        blockList.clear();
        for (auto& cur : indices) {
            cur.purge();
        }
        num_tuples = 0;
    }

    /** check whether a tuple exists in the relation */
    virtual bool exists(const RamDomain* tuple) const override {
        LVMIndex* index = getIndex(getTotalIndexKey());
        return index->exists(tuple);
    }

    /** Iterator for relation, uses full-order index as default */
    virtual iterator begin() const override {
        return indices[0].begin();
    }

    virtual iterator end() const override {
        return indices[0].end();
    }

    /** Return range iterator */
    virtual std::pair<iterator, iterator> lowerUpperBound(
            const RamDomain* low, const RamDomain* high, size_t indexPosition) override {
        auto idx = this->getIndexByPos(indexPosition);
        return idx->lowerUpperBound(low, high);
    }

    /** Extend tuple */
    virtual std::vector<RamDomain*> extend(const RamDomain* tuple) override {
        std::vector<RamDomain*> newTuples;

        // A standard relation does not generate extra new knowledge on insertion.
        newTuples.push_back(new RamDomain[2]{tuple[0], tuple[1]});

        return newTuples;
    }

    /** Extend relation */
    virtual void extend(const LVMRelation& rel) override {}

    /** get index for a given search signature. Order are encoded as bits for each column */
    LVMIndex* getIndex(const SearchSignature& col) const {
        // Special case in provenance program, a 0 searchSignature is considered as a full search
        if (col == 0 && arity != 0) {
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
};

/**
 * Interpreter Equivalence Relation
 */

class LVMEqRelation : public LVMIndirectRelation {
public:
    LVMEqRelation(size_t relArity, const MinIndexSelection* orderSet, std::string relName, std::vector<std::string>& attributeTypes)
            : LVMIndirectRelation(relArity, orderSet, relName, attributeTypes) {}

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
            LVMRelation::insert(newTuple);
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
            LVMRelation::insert(newTuple);
            delete[] newTuple;
        }
    }
};

}  // end of namespace souffle
