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
bool LevelConditionsTransformer::levelConditions(RamProgram& program) {
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
 * createIndices assumes that the RAM has been levelled before, and
 * that the conditions that could be used for an index are located
 * immediately after the scan or aggregrate information
 *
 *  QUERY
 *   ...
 *   FOR t1 in A
 *    IF t1.x = 10 /\ t1.y = 20 /\ C
 *     ...
 *
 * will be rewritten to
 *
 *  QUERY
 *   ...
 *    SEARCH t1 in A INDEX (10, 20)
 *     IF C
 *      ...
 */

/** Get expression of an equivalence constraint of the format t1.x = <expression> or <expression> = t1.x */
std::unique_ptr<RamExpression> CreateIndicesTransformer::getExpression(
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

std::unique_ptr<RamCondition> CreateIndicesTransformer::constructPattern(
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
std::unique_ptr<RamOperation> CreateIndicesTransformer::rewriteAggregate(const RamAggregate* agg) {
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
std::unique_ptr<RamOperation> CreateIndicesTransformer::rewriteScan(const RamScan* scan) {
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

bool CreateIndicesTransformer::createIndices(RamProgram& program) {
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

bool ConvertExistenceChecksTransformer::convertExistenceChecks(RamProgram& program) {
    // TODO: Change these to LambdaRamNodeMapper lambdas
    // Node-mapper that searches for and updates RAM scans nested in RAM inserts

    class RamScanCapturer : public RamNodeMapper {
        mutable bool modified = false;
        ConvertExistenceChecksTransformer* context;

    public:
        RamScanCapturer(ConvertExistenceChecksTransformer* c) : context(c) {}

        bool getModified() const {
            return modified;
        }

        bool dependsOn(const RamExpression* value, const int identifier) const {
            std::vector<const RamExpression*> queue = {value};
            while (!queue.empty()) {
                const RamExpression* val = queue.back();
                queue.pop_back();
                if (const auto* elemAccess = dynamic_cast<const RamElementAccess*>(val)) {
                    if (context->rvla->getLevel(elemAccess) == identifier) {
                        return true;
                    }
                } else if (const auto* intrinsicOp = dynamic_cast<const RamIntrinsicOperator*>(val)) {
                    for (const RamExpression* arg : intrinsicOp->getArguments()) {
                        queue.push_back(arg);
                    }
                } else if (const auto* userDefinedOp = dynamic_cast<const RamUserDefinedOperator*>(val)) {
                    for (const RamExpression* arg : userDefinedOp->getArguments()) {
                        queue.push_back(arg);
                    }
                }
            }
            return false;
        }

        bool dependsOn(const RamCondition* condition, const int identifier) const {
            if (const auto* binRel = dynamic_cast<const RamConstraint*>(condition)) {
                return dependsOn(binRel->getLHS(), identifier) || dependsOn(binRel->getRHS(), identifier);
            }
            return false;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            if (auto* scan = dynamic_cast<RamRelationSearch*>(node.get())) {
                const int identifier = scan->getIdentifier();
                bool isExistCheck = true;
                visitDepthFirst(scan->getOperation(), [&](const RamProject& project) {
                    if (isExistCheck) {
                        std::vector<const RamExpression*> values;
                        // TODO: function to extend vectors
                        const std::vector<RamExpression*> initialVals = project.getValues();
                        values.insert(values.end(), initialVals.begin(), initialVals.end());

                        while (!values.empty()) {
                            const RamExpression* value = values.back();
                            values.pop_back();

                            if (const auto* pack = dynamic_cast<const RamPackRecord*>(value)) {
                                const std::vector<RamExpression*> args = pack->getArguments();
                                values.insert(values.end(), args.begin(), args.end());
                            } else if (const auto* intrinsicOp =
                                               dynamic_cast<const RamIntrinsicOperator*>(value)) {
                                for (auto* arg : intrinsicOp->getArguments()) {
                                    values.push_back(arg);
                                }
                            } else if (value != nullptr && !context->rcva->isConstant(value) &&
                                       context->rvla->getLevel(value) == identifier) {
                                isExistCheck = false;
                                break;
                            }
                        }
                    }
                });
                if (isExistCheck) {
                    visitDepthFirst(scan->getOperation(), [&](const RamUnpackRecord& lookup) {
                        if (isExistCheck) {
                            if (lookup.getReferenceLevel() == identifier) {
                                isExistCheck = false;
                            }
                        }
                    });
                }
                if (isExistCheck) {
                    visitDepthFirst(scan->getOperation(), [&](const RamExpression& expression) {
                        if (isExistCheck) {
                            if (dependsOn(&expression, identifier)) {
                                isExistCheck = false;
                            }
                        }
                    });
                }
                if (isExistCheck) {
                    // create constraint
                    std::unique_ptr<RamCondition> constraint;

                    if (nullptr != dynamic_cast<RamScan*>(scan)) {
                        constraint = std::make_unique<RamNegation>(std::make_unique<RamEmptinessCheck>(
                                std::make_unique<RamRelationReference>(&scan->getRelation())));
                    } else if (auto* indexScan = dynamic_cast<RamIndexScan*>(scan)) {
                        std::vector<std::unique_ptr<RamExpression>> values;
                        for (RamExpression* value : indexScan->getRangePattern()) {
                            if (value != nullptr) {
                                values.emplace_back(value->clone());
                            } else {
                                values.push_back(nullptr);
                            }
                        }
                        constraint = std::make_unique<RamExistenceCheck>(
                                std::make_unique<RamRelationReference>(&scan->getRelation()),
                                std::move(values));
                    }

                    node = std::make_unique<RamFilter>(std::move(constraint),
                            std::unique_ptr<RamOperation>(scan->getOperation().clone()),
                            scan->getProfileText());
                    modified = true;
                }
            }
            node->apply(*this);
            return node;
        }
    };

    // Node-mapper that searches for and updates RAM inserts
    class RamQueryCapturer : public RamNodeMapper {
        mutable bool modified;
        ConvertExistenceChecksTransformer* context;

    public:
        RamQueryCapturer(ConvertExistenceChecksTransformer* c) : modified(false), context(c) {}

        bool getModified() const {
            return modified;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            // get all RAM inserts
            if (auto* insert = dynamic_cast<RamQuery*>(node.get())) {
                RamScanCapturer scanUpdate(context);
                insert->apply(scanUpdate);

                if (scanUpdate.getModified()) {
                    modified = true;
                }
            } else {
                // no need to search for nested RAM inserts
                node->apply(*this);
            }
            return node;
        }
    };

    // level all RAM inserts
    RamQueryCapturer insertUpdate(this);
    program.getMain()->apply(insertUpdate);

    return insertUpdate.getModified();
}

}  // end of namespace souffle
