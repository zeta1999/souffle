/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RAMI.h
 *
 * Declares the RAMI (RamInterpreter) class for executing RAM programs.
 *
 ***********************************************************************/

#pragma once

#include "AstVisitor.h"
#include "LVMGenerator.h"
#include "RAMIContext.h"
#include "RAMIInterface.h"
#include "RAMIRelation.h"
#include "RamCondition.h"
#include "RamRelation.h"
#include "RamStatement.h"
#include "RamTranslationUnit.h"
#include "RamTypes.h"
#include "RelationRepresentation.h"

#include <atomic>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>
#include <dlfcn.h>

namespace souffle {

class RMAIProgInterface;
class RamOperation;
class RamExpression;
class SymbolTable;

/**
 * This class contains functions for views (Hints) analysis and creation.
 */
class RAMIPreamble {
public:
    /**
     * Add outer-most filter operation which require a view.
     */
    void addOuterFilterOperation(const RamCondition* node) {
        outerOperations.push_back(node);
    }

    /**
     * Add existence check in outer-most filter operation.
     */
    void addViewForOuterFilter(const RamNode* node) {
        assert(dynamic_cast<const RamExistenceCheck*>(node) ||
                dynamic_cast<const RamProvenanceExistenceCheck*>(node));
        viewsForFilterOperation.push_back(node);
    }

    /**
     * Add nested operation which require a view (Hints).
     * This node cannot come from the outer-most filter operation.
     */
    void addViewForNestedOperation(const RamNode* op) {
        assert(dynamic_cast<const RamIndexOperation*>(op) || dynamic_cast<const RamExistenceCheck*>(op) ||
                dynamic_cast<const RamProvenanceExistenceCheck*>(op));
        viewsForNestedOperation.push_back(op);
    }

    /**
     * Add a list of nested operations which require a view.
     */
    void addNestedOperationList(const std::vector<const RamNode*>& nodes) {
        for (auto& node : nodes) {
            addViewForNestedOperation(node);
        }
    }

    /**
     * Return outer-most filter operations.
     */
    const std::vector<const RamCondition*>& getOuterFilterOperation() {
        return outerOperations;
    }

    /**
     * Return views for outer-most filter operations.
     */
    const std::vector<const RamNode*>& getViewsInOuterOperation() {
        return viewsForFilterOperation;
    }

    /**
     * Return nested operations
     */
    std::vector<const RamNode*> getViewsInNestedOperation() {
        return viewsForNestedOperation;
    }

    /**
     * Find all operations under the root node that require a view.
     *
     * In particular, find all index operation and existence check operation.
     */
    std::vector<const RamNode*> findAllViews(const RamNode& node) {
        std::vector<const RamNode*> res;
        visitDepthFirst(node, [&](const RamNode& node) {
            if (requireView(&node) == true) {
                res.push_back(&node);
            };
        });
        return res;
    }

    /**
     * Return true if the given operation requires a view.
     */
    bool requireView(const RamNode* node) {
        if (dynamic_cast<const RamExistenceCheck*>(node)) {
            return true;
        } else if (dynamic_cast<const RamProvenanceExistenceCheck*>(node)) {
            return true;
        } else if (dynamic_cast<const RamIndexOperation*>(node)) {
            return true;
        }
        return false;
    }

    void reset() {
        outerOperations.clear();
        viewsForFilterOperation.clear();
        viewsForNestedOperation.clear();
    }

    /**
     * Same as toConjunctionList defined in RamOperation. But does not clone new node,
     * only holds a list of raw pointers to the original node.
     */
    inline std::vector<const RamCondition*> toConjunctionList(const RamCondition* condition) {
        std::vector<const RamCondition*> list;
        std::stack<const RamCondition*> stack;
        if (condition != nullptr) {
            stack.push(condition);
            while (!stack.empty()) {
                condition = stack.top();
                stack.pop();
                if (const auto* ramConj = dynamic_cast<const RamConjunction*>(condition)) {
                    stack.push(&ramConj->getLHS());
                    stack.push(&ramConj->getRHS());
                } else {
                    list.emplace_back(condition);
                }
            }
        }
        return list;
    }

private:
    std::vector<const RamCondition*> outerOperations;
    std::vector<const RamNode*> viewsForFilterOperation;
    std::vector<const RamNode*> viewsForNestedOperation;
};

/**
 * Interpreter executing a RAM translation unit
 */

class RAMI : public RAMIInterface {
public:
    RAMI(RamTranslationUnit& tUnit)
            : RAMIInterface(tUnit), profiling_enabled(Global::config().has("profile")),
              isProvenance(Global::config().has("provenance")) {
        threadsNum = std::stoi(Global::config().get("jobs"));
#ifdef _OPENMP
        if (threadsNum > 1) {
            omp_set_num_threads(threadsNum);
        }
#endif
    }
    ~RAMI() {
        for (auto& x : environment) {
            delete x.second;
        }
    }

    /** Execute main program */
    void executeMain() override;

    /* Execute subroutine */
    void executeSubroutine(const std::string& name, const std::vector<RamDomain>& arguments,
            std::vector<RamDomain>& returnValues, std::vector<bool>& returnErrors) override;

protected:
    /** Evaluate value */
    RamDomain evalExpr(const RamExpression& value, const RAMIContext& ctxt = RAMIContext());

    /** Evaluate operation */
    void evalOp(const RamOperation& op, RAMIContext& ctxt);

    /** Evaluate conditions */
    bool evalCond(const RamCondition& cond, RAMIContext&);

    /** Evaluate statement */
    void evalStmt(const RamStatement& stmt, RAMIContext&);

    /** Get symbol table */
    SymbolTable& getSymbolTable() {
        return translationUnit.getSymbolTable();
    }

    /** Get counter */
    int getCounter() const {
        return counter;
    }

    /** Get Iteration Number */
    size_t getIterationNumber() const {
        return iteration;
    }

    /** Increment counter */
    int incCounter() {
        return counter++;
    }

    /** Increment iteration number */
    void incIterationNumber() {
        iteration++;
    }

    /** Reset iteration number */
    void resetIterationNumber() {
        iteration = 0;
    }

    void createRelation(const RamRelation& id, const MinIndexSelection* orderSet) {
        RelationHandle res;
        assert(environment.find(id.getName()) == environment.end());
        if (id.getRepresentation() == RelationRepresentation::EQREL) {
            res = std::make_unique<RAMIEqRelation>(
                    id.getArity(), id.getName(), std::vector<std::string>(), *orderSet);
        } else {
            if (isProvenance == true) {
                res = std::make_unique<RAMIRelation>(id.getArity(), id.getName(), std::vector<std::string>(),
                        *orderSet, createBTreeProvenanceIndex);
            } else {
                res = std::make_unique<RAMIRelation>(
                        id.getArity(), id.getName(), std::vector<std::string>(), *orderSet);
            }
        }
        environment[id.getName()] = new RelationHandle(std::move(res));
    }

private:
    /** Get relation */
    RelationHandle& getRelationHandle(const std::string& name) {
        // look up relation
        auto pos = environment.find(name);
        assert(pos != environment.end());
        return *pos->second;
    }

    /** Create views */
    void createViews(const std::vector<const RamNode*>& nodes, RAMIContext& ctxt) {
        for (auto node : nodes) {
            if (auto exists = dynamic_cast<const RamExistenceCheck*>(node)) {
                auto& rel = getRelation(exists->getRelation());
                auto indexPos = ctxt.getIndexPos(*exists, isa);
                ctxt.addNewView(rel.getView(indexPos), exists);
            } else if (auto provExists = dynamic_cast<const RamProvenanceExistenceCheck*>(node)) {
                auto& rel = getRelation(provExists->getRelation());
                auto indexPos = ctxt.getIndexPos(*provExists, isa);
                ctxt.addNewView(rel.getView(indexPos), provExists);
            } else if (auto index = dynamic_cast<const RamIndexOperation*>(node)) {
                auto& rel = getRelation(index->getRelation());
                auto indexPos = ctxt.getIndexPos(*index, isa);
                ctxt.addNewView(rel.getView(indexPos), index);
            } else {
                assert(false && "Operation does not need a view!");
            }
        }
    }

public:
    /** Get relation */
    inline RAMIRelation& getRelation(const RamRelation& id) {
        if (id.relation) return **static_cast<RelationHandle*>(id.relation);
        auto& handle = getRelationHandle(id.getName());
        id.relation = &handle;
        return *handle;
    }

    /** Drop relation */
    void dropRelation(const RamRelation& id) {
        RAMIRelation& rel = getRelation(id);
        environment.erase(id.getName());
        delete &rel;
    }

    /** Swap relation */
    void swapRelation(const RamRelation& ramRel1, const RamRelation& ramRel2) {
        RelationHandle& rel1 = getRelationHandle(ramRel1.getName());
        RelationHandle& rel2 = getRelationHandle(ramRel2.getName());
        std::swap(rel1, rel2);
    }

private:
    friend RAMIProgInterface;

    /** relation environment type */
    using relation_map = std::map<std::string, RelationHandle*>;

    /** Get relation map */
    virtual std::map<std::string, RelationHandle*>& getRelationMap() override {
        return environment;
    }

    /** counters for atom profiling */
    std::map<std::string, std::map<size_t, size_t>> frequencies;

    /** counters for non-existence checks */
    std::map<std::string, std::atomic<size_t>> reads;

    /** counter for $ operator */
    std::atomic<RamDomain> counter{0};

    /** iteration number (in a fix-point calculation) */
    size_t iteration = 0;

    /** SymbolTable for views access */
    size_t viewId = 0;

    /** Relation Environment */
    relation_map environment;

    bool profiling_enabled;

    bool isProvenance;

    RAMIPreamble preamble;

    size_t threadsNum;
};

}  // end of namespace souffle
