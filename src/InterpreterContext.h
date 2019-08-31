/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterContext.h
 *
 * Defines Interpreter interpreter context
 *
 ***********************************************************************/

#pragma once

#include "InterpreterIndex.h"
#include "RamIndexAnalysis.h"
#include "RamNode.h"
#include "RamTypes.h"
#include <cassert>
#include <memory>
#include <unordered_map>
#include <vector>

namespace souffle {

/**
 * Evaluation context for Interpreter operations
 */
class InterpreterContext {
    std::vector<const RamDomain*> data;
    std::vector<RamDomain>* returnValues = nullptr;
    std::vector<bool>* returnErrors = nullptr;
    const std::vector<RamDomain>* args = nullptr;
    std::vector<std::unique_ptr<RamDomain[]>> allocatedDataContainer;
    std::vector<std::unique_ptr<IndexView>> views;
    std::unordered_map<const RamNode*, size_t> viewTable;
    std::unordered_map<const RamNode*, size_t> indexPositionCache;

    // caching the last requester for a view
    const RamNode* lastRequester = nullptr;
    size_t lastView = 0;

public:
    InterpreterContext(size_t size = 0) : data(size) {}
    InterpreterContext(InterpreterContext& ctxt)
            : data(0), returnValues(ctxt.returnValues), returnErrors(ctxt.returnErrors), args(ctxt.args) {}
    virtual ~InterpreterContext() = default;

    const RamDomain*& operator[](size_t index) {
        if (index >= data.size()) {
            data.resize((index + 1));
        }
        return data[index];
    }

    const RamDomain* const& operator[](size_t index) const {
        return data[index];
    }

    /** Allocate a tuple.
     *  allocatedDataContainer has the ownership of those tuples. */
    RamDomain* allocateNewTuple(size_t size) {
        std::unique_ptr<RamDomain[]> newTuple(new RamDomain[size]);
        allocatedDataContainer.push_back(std::move(newTuple));

        // Return the reference as raw pointer.
        return allocatedDataContainer.back().get();
    }

    std::vector<RamDomain>& getReturnValues() const {
        return *returnValues;
    }

    void setReturnValues(std::vector<RamDomain>& retVals) {
        returnValues = &retVals;
    }

    void addReturnValue(RamDomain val, bool err = false) {
        assert(returnValues != nullptr && returnErrors != nullptr);
        returnValues->push_back(val);
        returnErrors->push_back(err);
    }

    std::vector<bool>& getReturnErrors() const {
        return *returnErrors;
    }

    void setReturnErrors(std::vector<bool>& retErrs) {
        returnErrors = &retErrs;
    }

    const std::vector<RamDomain>& getArguments() const {
        return *args;
    }

    void setArguments(const std::vector<RamDomain>& a) {
        args = &a;
    }

    RamDomain getArgument(size_t i) const {
        assert(args != nullptr && i < args->size() && "argument out of range");
        return (*args)[i];
    }

    size_t addNewView(std::unique_ptr<IndexView> view, const RamNode* node) {
        auto pos = viewTable.find(node);
        if (pos != viewTable.end()) {
            views[pos->second] = std::move(view);
            return pos->second;
        }
        views.push_back(std::move(view));
        viewTable[node] = views.size() - 1;
        return views.size() - 1;
    }

    IndexView& getView(const RamNode* node) {
        if (node == lastRequester) {
            return *views[lastView];
        }
        auto pos = viewTable.find(node);
        lastView = pos->second;
        lastRequester = node;
        return *views[lastView];
    }

    /** Get the index position in a relation based on the SearchSignature */
    template <class RamNode>
    size_t getIndexPos(const RamNode& node, RamIndexAnalysis* isa) {
        size_t indexPos = 0;
        auto ret = indexPositionCache.find((RamNode*)&node);
        if (ret != indexPositionCache.end()) {
            indexPos = ret->second;
        } else {
            /** If index position is not in the cache yet, consult RamIndexAnalysis
             * and store the position in the cache for fast lookup next time.
             */
            const MinIndexSelection& orderSet = isa->getIndexes(node.getRelation());
            SearchSignature signature = isa->getSearchSignature(&node);
            // A zero signature is equivalent as a full order signature.
            if (signature == 0) {
                signature = (1 << node.getRelation().getArity()) - 1;
            }
            indexPos = orderSet.getLexOrderNum(signature);
            indexPositionCache[&node] = indexPos;
        }
        return indexPos;
    };
};

}  // end of namespace souffle
