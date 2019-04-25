/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamTransforms.cpp
 *
 * Implementation of RAM transformation passes.
 *
 ***********************************************************************/

#include "RamTransforms.h"
#include "BinaryConstraintOps.h"
#include "RamCondition.h"
#include "RamExpression.h"
#include "RamNode.h"
#include "RamOperation.h"
#include "RamProgram.h"
#include "RamRelation.h"
#include "RamStatement.h"
#include "RamTypes.h"
#include "RamVisitor.h"
#include <utility>
#include <vector>

namespace souffle {

namespace {

std::vector<std::unique_ptr<RamCondition>> getConditions(const RamCondition* condition) {
    std::vector<std::unique_ptr<RamCondition>> conditions;
    while (condition != nullptr) {
        if (const auto* ramConj = dynamic_cast<const RamConjunction*>(condition)) {
            conditions.emplace_back(ramConj->getRHS().clone());
            condition = &ramConj->getLHS();
        } else {
            conditions.emplace_back(condition->clone());
            break;
        }
    }
    return conditions;
}

}  // namespace

/**
 * levels filter operations: there are two types - filter operations
 * that are dependent on tuples (i.e. RamSearch) and others that
 * are not. Depending on the type, a different transformation is
 * required.
 */
bool HoistConditionsTransformer::hoistConditions(RamProgram& program) {
    // flag to determine whether the RAM program has changed
    bool changed = false;

    // helper for collecting conditions from filter operations
    auto addCondition = [](std::unique_ptr<RamCondition> condition,
                                RamCondition* c) -> std::unique_ptr<RamCondition> {
        if (condition == nullptr) {
            return std::unique_ptr<RamCondition>(c);
        } else {
            return std::make_unique<RamConjunction>(std::move(condition), std::unique_ptr<RamCondition>(c));
        }
    };

    // insert a new filter
    auto insertFilter = [](RamOperation* op, std::unique_ptr<RamCondition>& condition) {
        op->apply(makeLambdaRamMapper([&](std::unique_ptr<RamNode> node) -> std::unique_ptr<RamNode> {
            if (nullptr != dynamic_cast<RamOperation*>(node.get())) {
                return std::make_unique<RamFilter>(std::move(condition),
                        std::unique_ptr<RamOperation>(dynamic_cast<RamOperation*>(node.release())));
            }
            return node;
        }));
    };

    // hoist conditions to the most outer scope if they
    // don't depend on RamSearches
    visitDepthFirst(program, [&](const RamQuery& query) {
        std::unique_ptr<RamCondition> newCondition;
        std::function<std::unique_ptr<RamNode>(std::unique_ptr<RamNode>)> filterRewriter =
                [&](std::unique_ptr<RamNode> node) -> std::unique_ptr<RamNode> {
            if (auto* filter = dynamic_cast<RamFilter*>(node.get())) {
                const RamCondition& condition = filter->getCondition();
                // if filter condition is independent of any RamSearch,
                // delete the filter operation and collect condition
                if (rcla->getLevel(&condition) == -1) {
                    changed = true;
                    newCondition = addCondition(std::move(newCondition), condition.clone());
                    node->apply(makeLambdaRamMapper(filterRewriter));
                    return std::unique_ptr<RamOperation>(filter->getOperation().clone());
                }
            }
            node->apply(makeLambdaRamMapper(filterRewriter));
            return node;
        };
        ((RamNode*)&query)->apply(makeLambdaRamMapper(filterRewriter));
        if (newCondition != nullptr) {
            // insert new filter operation at outer-most level of the query
            changed = true;
            insertFilter((RamOperation*)&query, newCondition);
        }
    });

    // hoist conditions for each RamSearch operation
    visitDepthFirst(program, [&](const RamSearch& search) {
        std::unique_ptr<RamCondition> newCondition;
        std::function<std::unique_ptr<RamNode>(std::unique_ptr<RamNode>)> filterRewriter =
                [&](std::unique_ptr<RamNode> node) -> std::unique_ptr<RamNode> {
            if (auto* filter = dynamic_cast<RamFilter*>(node.get())) {
                const RamCondition& condition = filter->getCondition();
                // if filter condition matches level of RamSearch,
                // delete the filter operation and collect condition
                if (rcla->getLevel(&condition) == search.getIdentifier()) {
                    changed = true;
                    newCondition = addCondition(std::move(newCondition), condition.clone());
                    node->apply(makeLambdaRamMapper(filterRewriter));
                    return std::unique_ptr<RamOperation>(filter->getOperation().clone());
                }
            }
            node->apply(makeLambdaRamMapper(filterRewriter));
            return node;
        };
        ((RamNode*)&search)->apply(makeLambdaRamMapper(filterRewriter));
        if (newCondition != nullptr) {
            // insert new filter operation after the search operation
            changed = true;
            insertFilter((RamOperation*)&search, newCondition);
        }
    });
    return changed;
}

/**
 */

/** Get expression of an equivalence constraint of the format t1.x = <expression> or <expression> = t1.x */
std::unique_ptr<RamExpression> MakeIndexTransformer::getExpression(
        RamCondition* c, size_t& element, int identifier) {
    if (auto* binRelOp = dynamic_cast<RamConstraint*>(c)) {
        if (binRelOp->getOperator() == BinaryConstraintOp::EQ) {
            if (const RamElementAccess* lhs = dynamic_cast<RamElementAccess*>(binRelOp->getLHS())) {
                RamExpression* rhs = binRelOp->getRHS();
                if (lhs->getIdentifier() == identifier &&
                        (rcva->isConstant(rhs) || rvla->getLevel(rhs) < identifier)) {
                    element = lhs->getElement();
                    return std::unique_ptr<RamExpression>(rhs->clone());
                }
            }
            if (const RamElementAccess* rhs = dynamic_cast<RamElementAccess*>(binRelOp->getRHS())) {
                RamExpression* lhs = binRelOp->getLHS();
                if (rhs->getIdentifier() == identifier &&
                        (rcva->isConstant(lhs) || rvla->getLevel(lhs) < identifier)) {
                    element = rhs->getElement();
                    return std::unique_ptr<RamExpression>(lhs->clone());
                }
            }
        }
    }
    return nullptr;
}

std::unique_ptr<RamCondition> MakeIndexTransformer::constructPattern(
        std::vector<std::unique_ptr<RamExpression>>& queryPattern, bool& indexable,
        std::vector<std::unique_ptr<RamCondition>> conditionList, int identifier) {
    // Remaining conditions which cannot be handled by an index
    std::unique_ptr<RamCondition> condition;
    auto addCondition = [&](std::unique_ptr<RamCondition> c) {
        if (condition != nullptr) {
            condition = std::make_unique<RamConjunction>(std::move(condition), std::move(c));
        } else {
            condition = std::move(c);
        }
    };

    // Build query pattern and remaining condition
    for (auto& cond : conditionList) {
        size_t element = 0;
        if (std::unique_ptr<RamExpression> value = getExpression(cond.get(), element, identifier)) {
            if (queryPattern[element] == nullptr) {
                indexable = true;
                queryPattern[element] = std::move(value);
            } else {
                // TODO: This case is a recursive case introducing a new filter operation
                // at upper level, i.e., if queryPattern[element] == value ...
                // and apply indexing recursively to the rewritten program.
                // At the moment we just another local condition which is sub-optimal
                // Note sure whether there are cases in practice that would improve the performance
                addCondition(std::make_unique<RamConstraint>(BinaryConstraintOp::EQ, std::move(value),
                        std::unique_ptr<RamExpression>(queryPattern[element]->clone())));
            }
        } else {
            addCondition(std::move(cond));
        }
    }
    return condition;
}

/* Find the query pattern for an indexable scan operation and rewrite it to an IndexScan */
std::unique_ptr<RamOperation> MakeIndexTransformer::rewriteAggregate(const RamAggregate* agg) {
    if (agg->getCondition() != nullptr) {
        const RamRelation& rel = agg->getRelation();
        int identifier = agg->getIdentifier();
        std::vector<std::unique_ptr<RamExpression>> queryPattern(rel.getArity());
        bool indexable = false;
        std::unique_ptr<RamCondition> condition =
                constructPattern(queryPattern, indexable, getConditions(agg->getCondition()), identifier);
        if (indexable) {
            std::unique_ptr<RamExpression> expr;
            if (agg->getExpression() != nullptr) {
                expr = std::unique_ptr<RamExpression>(agg->getExpression()->clone());
            }
            return std::make_unique<RamAggregate>(std::unique_ptr<RamOperation>(agg->getOperation().clone()),
                    agg->getFunction(), std::make_unique<RamRelationReference>(&rel), std::move(expr),
                    std::move(condition), std::move(queryPattern), agg->getIdentifier());
        }
    }
    return nullptr;
}
/* Find the query pattern for an indexable scan operation and rewrite it to an IndexScan */
std::unique_ptr<RamOperation> MakeIndexTransformer::rewriteScan(const RamScan* scan) {
    if (const auto* filter = dynamic_cast<const RamFilter*>(&scan->getOperation())) {
        const RamRelation& rel = scan->getRelation();
        const int identifier = scan->getIdentifier();
        std::vector<std::unique_ptr<RamExpression>> queryPattern(rel.getArity());
        bool indexable = false;
        std::unique_ptr<RamCondition> condition =
                constructPattern(queryPattern, indexable, getConditions(&filter->getCondition()), identifier);
        if (indexable) {
            return std::make_unique<RamIndexScan>(std::make_unique<RamRelationReference>(&rel), identifier,
                    std::move(queryPattern),
                    condition == nullptr
                            ? std::unique_ptr<RamOperation>(filter->getOperation().clone())
                            : std::make_unique<RamFilter>(std::move(condition),
                                      std::unique_ptr<RamOperation>(filter->getOperation().clone())),
                    scan->getProfileText());
        }
    }
    return nullptr;
}

bool MakeIndexTransformer::makeIndex(RamProgram& program) {
    bool changed = false;
    visitDepthFirst(program, [&](const RamQuery& query) {
        std::function<std::unique_ptr<RamNode>(std::unique_ptr<RamNode>)> scanRewriter =
                [&](std::unique_ptr<RamNode> node) -> std::unique_ptr<RamNode> {
            if (const RamScan* scan = dynamic_cast<RamScan*>(node.get())) {
                if (std::unique_ptr<RamOperation> op = rewriteScan(scan)) {
                    changed = true;
                    node = std::move(op);
                }
            } else if (const RamAggregate* agg = dynamic_cast<RamAggregate*>(node.get())) {
                if (std::unique_ptr<RamOperation> op = rewriteAggregate(agg)) {
                    changed = true;
                    node = std::move(op);
                }
            }
            node->apply(makeLambdaRamMapper(scanRewriter));
            return node;
        };
        ((RamNode*)&query)->apply(makeLambdaRamMapper(scanRewriter));
    });
    return changed;
}

/** rewrite IndexScan to a filter/existence check if the IndexScan's tuple
 * is not used in a consecutive RAM operation */
std::unique_ptr<RamOperation> IfConversionTransformer::rewriteIndexScan(const RamIndexScan* indexScan) {
    // check whether tuple is used in subsequent operations
    bool tupleNotUsed = true;
    visitDepthFirst(*indexScan, [&](const RamNode& node) {
        if (const RamElementAccess* element = dynamic_cast<const RamElementAccess*>(&node)) {
            if (element->getIdentifier() == indexScan->getIdentifier()) {
                tupleNotUsed = false;
            }
        } else if (const RamUnpackRecord* unpack = dynamic_cast<const RamUnpackRecord*>(&node)) {
            if (unpack->getReferenceLevel() == indexScan->getIdentifier()) {
                tupleNotUsed = false;
            }
        }
    });

    // if not used, transform the IndexScan operation to an existence check
    if (tupleNotUsed) {
        // replace IndexScan with an Filter/Existence check
        std::vector<std::unique_ptr<RamExpression>> newValues;
        for (auto& cur : indexScan->getRangePattern()) {
            RamExpression* val = nullptr;
            if (cur != nullptr) {
                val = cur->clone();
            }
            newValues.emplace_back(val);
        }
        return std::make_unique<RamFilter>(
                std::make_unique<RamExistenceCheck>(
                        std::make_unique<RamRelationReference>(&indexScan->getRelation()),
                        std::move(newValues)),
                std::unique_ptr<RamOperation>(indexScan->getOperation().clone()),
                indexScan->getProfileText());
    }
    return nullptr;
}

/** Search for queries and rewrite their IndexScan operations if possible */
bool IfConversionTransformer::convertIndexScans(RamProgram& program) {
    bool changed = false;
    visitDepthFirst(program, [&](const RamQuery& query) {
        std::function<std::unique_ptr<RamNode>(std::unique_ptr<RamNode>)> scanRewriter =
                [&](std::unique_ptr<RamNode> node) -> std::unique_ptr<RamNode> {
            if (const RamIndexScan* scan = dynamic_cast<RamIndexScan*>(node.get())) {
                if (std::unique_ptr<RamOperation> op = rewriteIndexScan(scan)) {
                    changed = true;
                    node = std::move(op);
                }
            }
            node->apply(makeLambdaRamMapper(scanRewriter));
            return node;
        };
        ((RamNode*)&query)->apply(makeLambdaRamMapper(scanRewriter));
    });
    return changed;
}

}  // end of namespace souffle
