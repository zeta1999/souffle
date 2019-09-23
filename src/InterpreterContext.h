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
#include "InterpreterRelation.h"
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
    using ViewPtr = std::unique_ptr<IndexView>;
    std::vector<const RamDomain*> data;
    std::vector<RamDomain>* returnValues = nullptr;
    std::vector<bool>* returnErrors = nullptr;
    const std::vector<RamDomain>* args = nullptr;
    std::vector<std::unique_ptr<RamDomain[]>> allocatedDataContainer;
    std::vector<std::unique_ptr<IndexView>> views;

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

    void createView(const InterpreterRelation& rel, size_t indexPos, size_t viewPos) {
        ViewPtr view;
        if (views.size() < viewPos + 1) {
            views.resize(viewPos + 1);
        }
        views[viewPos] = rel.getView(indexPos);
    }

    ViewPtr& getView(size_t id) {
        assert(id < views.size());
        return views[id];
    }
};

}  // end of namespace souffle
