/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ExecutionPlan.h
 *
 * Defines AST Clauses
 *
 ***********************************************************************/

#pragma once

#include "ast/ExecutionOrder.h"
#include "ast/Node.h"
#include "ast/NodeMapper.h"
#include "utility/ContainerUtil.h"
#include "utility/StreamUtil.h"
#include <algorithm>
#include <map>
#include <memory>
#include <ostream>
#include <utility>
#include <vector>

namespace souffle {

/**
 * The class utilized to model user-defined execution plans for various
 * versions of clauses.
 */
class AstExecutionPlan : public AstNode {
public:
    /** updates execution order for rule version */
    void setOrderFor(int version, Own<AstExecutionOrder> plan) {
        plans[version] = std::move(plan);
    }

    /** get orders */
    std::map<int, const AstExecutionOrder*> getOrders() const {
        std::map<int, const AstExecutionOrder*> result;
        for (auto& plan : plans) {
            result.insert(std::make_pair(plan.first, plan.second.get()));
        }
        return result;
    }

    AstExecutionPlan* clone() const override {
        auto res = new AstExecutionPlan();
        res->setSrcLoc(getSrcLoc());
        for (auto& plan : plans) {
            res->setOrderFor(plan.first, Own<AstExecutionOrder>(plan.second->clone()));
        }
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& plan : plans) {
            plan.second = map(std::move(plan.second));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> childNodes;
        for (auto& plan : plans) {
            childNodes.push_back(plan.second.get());
        }
        return childNodes;
    }

protected:
    void print(std::ostream& out) const override {
        if (!plans.empty()) {
            out << " .plan ";
            out << join(plans, ", ",
                    [](std::ostream& os, const auto& arg) { os << arg.first << ":" << *arg.second; });
        }
    }

    bool equal(const AstNode& node) const override {
        const auto& other = static_cast<const AstExecutionPlan&>(node);
        return equal_targets(plans, other.plans);
    }

private:
    /** mapping versions of clauses to execution plans */
    std::map<int, Own<AstExecutionOrder>> plans;
};

}  // end of namespace souffle
