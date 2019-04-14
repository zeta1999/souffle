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

bool LevelConditionsTransformer::levelConditions(RamProgram& program) {
    bool changed = false;
    visitDepthFirst(program, [&](const RamScan& scan) {
        std::unique_ptr<RamCondition> levelledCondition;
        std::function<std::unique_ptr<RamNode>(std::unique_ptr<RamNode>)> filterRewriter =
                [&](std::unique_ptr<RamNode> node) -> std::unique_ptr<RamNode> {
            // find filters that can be levelled
            if (auto* filter = dynamic_cast<RamFilter*>(node.get())) {
                const RamCondition& condition = filter->getCondition();
                if (rcla->getLevel(&condition) == scan.getIdentifier()) {
                    //std::cout << "Filter: " << *filter << std::endl;
                    //std::cout << "Scan: " << scan << std::endl;
                    std::unique_ptr<RamCondition> clonedCondition(condition.clone());
                    if (levelledCondition == nullptr) {
                        levelledCondition = std::move(clonedCondition);
                    } else {
                        levelledCondition = std::make_unique<RamConjunction>(
                                std::move(levelledCondition), std::move(clonedCondition));
                    }
                    node->apply(makeLambdaRamMapper(filterRewriter));
                    return std::unique_ptr<RamOperation>(filter->getOperation().clone());
                }
            }
            node->apply(makeLambdaRamMapper(filterRewriter));
            return node;
        };
        scan.getOperation().apply(makeLambdaRamMapper(filterRewriter));

        // If a condition applies to this scan level, filter the scan based on the condition
        if (levelledCondition != nullptr) {
            scan.getOperation().apply(makeLambdaRamMapper(
               [&](std::unique_ptr<RamNode> node) -> std::unique_ptr<RamNode> {
                    if (nullptr != dynamic_cast<RamOperation*>(node.get())) {
                         return std::make_unique<RamFilter>(std::move(levelledCondition),
                             std::unique_ptr<RamOperation>(dynamic_cast<RamOperation*>(node.release())));
                    }
                    return node;
               } 
            )); 
            changed = true;
        }
    });
    return changed;
}

/** Get indexable element */
std::unique_ptr<RamExpression> CreateIndicesTransformer::getIndexElement(
        RamCondition* c, size_t& element, size_t identifier) {
    if (auto* binRelOp = dynamic_cast<RamConstraint*>(c)) {
        if (binRelOp->getOperator() == BinaryConstraintOp::EQ) {
            if (auto* lhs = dynamic_cast<RamElementAccess*>(binRelOp->getLHS())) {
                RamExpression* rhs = binRelOp->getRHS();
                if (rvla->getLevel(lhs) == identifier &&
                        (rcva->isConstant(rhs) || rvla->getLevel(rhs) < identifier)) {
                    element = lhs->getElement();
                    return std::unique_ptr<RamExpression>(rhs->clone());
                }
            }
            if (auto* rhs = dynamic_cast<RamElementAccess*>(binRelOp->getRHS())) {
                RamExpression* lhs = binRelOp->getLHS();
                if (rvla->getLevel(rhs) == identifier &&
                        (rcva->isConstant(lhs) || rvla->getLevel(lhs) < identifier)) {
                    element = rhs->getElement();
                    return std::unique_ptr<RamExpression>(lhs->clone());
                }
            }
        }
    }
    return nullptr;
}

std::unique_ptr<RamOperation> CreateIndicesTransformer::rewriteScan(const RamScan* scan) {
    if (const auto* filter = dynamic_cast<const RamFilter*>(&scan->getOperation())) {
        const RamRelation& rel = scan->getRelation();
        const size_t identifier = scan->getIdentifier();

        // Values of index per column of table (if indexable)
        std::vector<std::unique_ptr<RamExpression>> queryPattern(rel.getArity());

        // Remaining conditions which weren't handled by an index
        std::unique_ptr<RamCondition> condition;

        auto addCondition = [&](std::unique_ptr<RamCondition> c) {
            if (condition != nullptr) {
                condition = std::make_unique<RamConjunction>(std::move(condition), std::move(c));
            } else {
                condition = std::move(c);
            }
        };

        bool indexable = false;

        for (auto& cond : getConditions(&filter->getCondition())) {
            size_t element = 0;
            if (std::unique_ptr<RamExpression> value = getIndexElement(cond.get(), element, identifier)) {
                indexable = true;
                if (queryPattern[element] == nullptr) {
                    queryPattern[element] = std::move(value);
                } else {
                    addCondition(std::move(cond));
                }
            } else {
                addCondition(std::move(cond));
            }
        }

        if (indexable) {
            // replace scan by index scan
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
    // TODO: Change these to LambdaRamNodeMapper lambdas
    // Node-mapper that searches for and updates RAM scans nested in RAM inserts
    class RamScanCapturer : public RamNodeMapper {
        mutable bool modified = false;
        CreateIndicesTransformer* context;

    public:
        RamScanCapturer(CreateIndicesTransformer* c) : context(c) {}

        bool getModified() const {
            return modified;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            if (auto* scan = dynamic_cast<RamScan*>(node.get())) {
                if (std::unique_ptr<RamOperation> op = context->rewriteScan(scan)) {
                    modified = true;
                    node = std::move(op);
                }
            }
            node->apply(*this);
            return node;
        }
    };

    // Node-mapper that searches for and updates RAM inserts
    class RamQueryCapturer : public RamNodeMapper {
        mutable bool modified;
        CreateIndicesTransformer* context;

    public:
        RamQueryCapturer(CreateIndicesTransformer* c) : modified(false), context(c) {}

        bool getModified() const {
            return modified;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            // get all RAM inserts
            if (auto* insert = dynamic_cast<RamQuery*>(node.get())) {
                RamScanCapturer scanUpdate(context);
                insert->apply(scanUpdate);
                if (!modified && scanUpdate.getModified()) {
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

        bool dependsOn(const RamExpression* value, const size_t identifier) const {
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

        bool dependsOn(const RamCondition* condition, const size_t identifier) const {
            if (const auto* binRel = dynamic_cast<const RamConstraint*>(condition)) {
                return dependsOn(binRel->getLHS(), identifier) || dependsOn(binRel->getRHS(), identifier);
            }
            return false;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            if (auto* scan = dynamic_cast<RamRelationSearch*>(node.get())) {
                const size_t identifier = scan->getIdentifier();
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
