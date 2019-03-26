/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamOperation.cpp
 *
 * Implements the operation of a relational algebra query consisting of
 * Search/Scan and a Project operation. The Search/Scan operation traverses
 * a table and/or check for condition of each tuple and/or uses an index.
 *
 ***********************************************************************/

#include "RamOperation.h"
#include "BinaryConstraintOps.h"
#include "RamCondition.h"
#include "RamConditionLevel.h"
#include "RamConstValue.h"
#include "RamExpression.h"
#include "RamExpressionLevel.h"
#include <algorithm>
#include <cstddef>
#include <iostream>
#include <memory>
#include <string>

namespace souffle {

/** get indexable element */
std::unique_ptr<RamExpression> RamAggregate::getIndexElement(
        RamCondition* condition, size_t& element, size_t level) {
    if (auto* binRelOp = dynamic_cast<RamConstraint*>(condition)) {
        if (binRelOp->getOperator() == BinaryConstraintOp::EQ) {
            if (auto* lhs = dynamic_cast<RamElementAccess*>(binRelOp->getLHS())) {
                RamExpression* rhs = binRelOp->getRHS();
                if (lhs->getIdentifier() == level &&
                        (RamConstValueAnalysis().isConstant(rhs) ||
                                RamExpressionLevelAnalysis().getLevel(rhs) < level)) {
                    element = lhs->getElement();
                    return std::unique_ptr<RamExpression>(rhs->clone());
                }
            }
            if (auto* rhs = dynamic_cast<RamElementAccess*>(binRelOp->getRHS())) {
                RamExpression* lhs = binRelOp->getLHS();
                if (rhs->getIdentifier() == level &&
                        (RamConstValueAnalysis().isConstant(lhs) ||
                                RamExpressionLevelAnalysis().getLevel(lhs) < level)) {
                    element = rhs->getElement();
                    return std::unique_ptr<RamExpression>(lhs->clone());
                }
            }
        }
    }
    return std::unique_ptr<RamExpression>(nullptr);
}

/** add condition */
void RamAggregate::addCondition(std::unique_ptr<RamCondition> newCondition) {
    assert(RamConditionLevelAnalysis().getLevel(newCondition.get()) == getIdentifier());

    // use condition to narrow scan if possible
    size_t element = 0;
    if (std::unique_ptr<RamExpression> value =
                    getIndexElement(newCondition.get(), element, getIdentifier())) {
        if (element > 0 || relationRef->get()->getName().find("__agg") == std::string::npos) {
            keys |= (1 << element);
            if (pattern[element] == nullptr) {
                pattern[element] = std::move(value);
            } else {
                std::unique_ptr<RamExpression> field(new RamElementAccess(getIdentifier(), element));

                auto addCondition = [&](std::unique_ptr<RamCondition> c) {
                    assert(RamConditionLevelAnalysis().getLevel(c.get()) == getIdentifier());
                    if (condition != nullptr) {
                        condition = std::make_unique<RamConjunction>(std::move(condition), std::move(c));
                    } else {
                        condition = std::move(c);
                    }
                };

                addCondition(std::make_unique<RamConstraint>(
                        BinaryConstraintOp::EQ, std::move(field), std::move(value)));
            }
        } else {
            std::unique_ptr<RamExpression> field(new RamElementAccess(getIdentifier(), element));
            std::unique_ptr<RamCondition> eq(
                    new RamConstraint(BinaryConstraintOp::EQ, std::move(field), std::move(value)));
            if (condition != nullptr) {
                condition = std::make_unique<RamConjunction>(std::move(condition), std::move(eq));
            } else {
                condition.swap(eq);
            }
        }
    }
}

}  // end of namespace souffle
