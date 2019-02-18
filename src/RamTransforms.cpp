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
#include "RamVisitor.h"

namespace souffle {

namespace {

std::vector<RamCondition*> getConditions(const RamCondition* condition) {
    std::vector<RamCondition*> conditions;
    while (condition != nullptr) {
        if (const auto* ramAnd = dynamic_cast<const RamAnd*>(condition)) {
            conditions.push_back(ramAnd->getRHS().clone());
            condition = &ramAnd->getLHS();
        } else {
            conditions.push_back(condition->clone());
            break;
        }
    }
    return conditions;
}

}  // namespace

bool LevelConditionsTransformer::levelConditions(RamProgram& program) {
    // Node-mapper that collects nested conditions which apply to a given scan level
    // TODO: Change these to LambdaRamNodeMapper lambdas
    class RamFilterCapturer : public RamNodeMapper {
        LevelConditionsTransformer* context;

        /** identifier for the tuple */
        const size_t identifier;

        mutable std::unique_ptr<RamCondition> condition;

    public:
        RamFilterCapturer(LevelConditionsTransformer* l, const size_t ident)
                : context(l), identifier(ident) {}

        std::unique_ptr<RamCondition> getCondition() const {
            return std::move(condition);
        }

        void addCondition(std::unique_ptr<RamCondition> c) const {
            if (condition != nullptr) {
                condition = std::make_unique<RamAnd>(std::move(condition), std::move(c));
            } else {
                condition = std::move(c);
            }
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            if (auto* filter = dynamic_cast<RamFilter*>(node.get())) {
                const RamCondition& condition = filter->getCondition();

                if (context->rcla->getLevel(&condition) == identifier) {
                    addCondition(std::unique_ptr<RamCondition>(condition.clone()));

                    // skip this filter
                    node->apply(*this);
                    return std::unique_ptr<RamOperation>(filter->getOperation().clone());
                }
            }

            node->apply(*this);
            return node;
        }
    };

    class RamFilterInsert : public RamNodeMapper {
        std::unique_ptr<RamCondition> condition;

    public:
        RamFilterInsert(std::unique_ptr<RamCondition> c) : condition(std::move(c)) {}

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            if (nullptr != dynamic_cast<RamOperation*>(node.get())) {
                return std::make_unique<RamFilter>(std::unique_ptr<RamCondition>(condition->clone()),
                        std::unique_ptr<RamOperation>(dynamic_cast<RamOperation*>(node.release())));
            }
            return node;
        }
    };

    // Node-mapper that searches for and updates RAM scans nested in RAM inserts
    class RamScanCapturer : public RamNodeMapper {
        mutable bool modified = false;
        LevelConditionsTransformer* context;

    public:
        RamScanCapturer(LevelConditionsTransformer* l) : context(l) {}

        bool getModified() const {
            return modified;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            if (auto* scan = dynamic_cast<RamScan*>(node.get())) {
                RamFilterCapturer filterUpdate(context, scan->getIdentifier());
                node->apply(filterUpdate);

                // If a condition applies to this scan level, filter the scan based on the condition
                if (std::unique_ptr<RamCondition> condition = filterUpdate.getCondition()) {
                    RamFilterInsert filterInsert(std::move(condition));
                    node->apply(filterInsert);
                    modified = true;
                }
            }

            node->apply(*this);
            return node;
        }
    };

    // Node-mapper that searches for and updates RAM inserts
    class RamInsertCapturer : public RamNodeMapper {
        mutable bool modified = false;
        LevelConditionsTransformer* context;

    public:
        RamInsertCapturer(LevelConditionsTransformer* l) : context(l) {}

        bool getModified() const {
            return modified;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            // get all RAM inserts
            if (auto* insert = dynamic_cast<RamInsert*>(node.get())) {
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
    RamInsertCapturer insertUpdate(this);
    program.getMain()->apply(insertUpdate);

    return insertUpdate.getModified();
}

/** Get indexable element */
std::unique_ptr<RamValue> CreateIndicesTransformer::getIndexElement(
        RamCondition* c, size_t& element, size_t identifier) {
    if (auto* binRelOp = dynamic_cast<RamBinaryRelation*>(c)) {
        if (binRelOp->getOperator() == BinaryConstraintOp::EQ) {
            if (auto* lhs = dynamic_cast<RamElementAccess*>(binRelOp->getLHS())) {
                RamValue* rhs = binRelOp->getRHS();
                if (rvla->getLevel(lhs) == identifier &&
                        (rcva->isConstant(rhs) || rvla->getLevel(rhs) < identifier)) {
                    element = lhs->getElement();
                    return binRelOp->takeRHS();
                }
            }
            if (auto* rhs = dynamic_cast<RamElementAccess*>(binRelOp->getRHS())) {
                RamValue* lhs = binRelOp->getLHS();
                if (rvla->getLevel(rhs) == identifier &&
                        (rcva->isConstant(lhs) || rvla->getLevel(lhs) < identifier)) {
                    element = rhs->getElement();
                    return binRelOp->takeLHS();
                }
            }
        }
    }
    return nullptr;
}

std::unique_ptr<RamOperation> CreateIndicesTransformer::rewriteScan(const RamScan* scan) {
    if (const auto* filter = dynamic_cast<const RamFilter*>(&scan->getOperation())) {
        const RamRelationReference& rel = scan->getRelation();
        const size_t identifier = scan->getIdentifier();

        // Values of index per column of table (if indexable)
        std::vector<std::unique_ptr<RamValue>> queryPattern(rel.getArity());

        // Indexable columns for a range query
        SearchColumns keys = 0;

        // Remaining conditions which weren't handled by an index
        std::unique_ptr<RamCondition> condition;

        auto addCondition = [&](std::unique_ptr<RamCondition> c) {
            if (condition != nullptr) {
                condition = std::make_unique<RamAnd>(std::move(condition), std::move(c));
            } else {
                condition = std::move(c);
            }
        };

        for (RamCondition* cond : getConditions(filter->getCondition().clone())) {
            size_t element = 0;
            if (std::unique_ptr<RamValue> value = getIndexElement(cond, element, identifier)) {
                keys |= (1 << element);
                if (queryPattern[element] == nullptr) {
                    queryPattern[element] = std::move(value);
                } else {
                    addCondition(std::unique_ptr<RamCondition>(cond));
                }
            } else {
                addCondition(std::unique_ptr<RamCondition>(cond));
            }
        }

        if (keys != 0) {
            // replace scan by index scan
            return std::make_unique<RamIndexScan>(std::unique_ptr<RamRelationReference>(rel.clone()),
                    identifier, std::move(queryPattern), keys,
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
    class RamInsertCapturer : public RamNodeMapper {
        mutable bool modified;
        CreateIndicesTransformer* context;

    public:
        RamInsertCapturer(CreateIndicesTransformer* c) : modified(false), context(c) {}

        bool getModified() const {
            return modified;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            // get all RAM inserts
            if (auto* insert = dynamic_cast<RamInsert*>(node.get())) {
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
    RamInsertCapturer insertUpdate(this);
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

        bool dependsOn(const RamValue* value, const size_t identifier) const {
            std::vector<const RamValue*> queue = {value};
            while (!queue.empty()) {
                const RamValue* val = queue.back();
                queue.pop_back();
                if (const auto* elemAccess = dynamic_cast<const RamElementAccess*>(val)) {
                    if (context->rvla->getLevel(elemAccess) == identifier) {
                        return true;
                    }
                } else if (const auto* intrinsicOp = dynamic_cast<const RamIntrinsicOperator*>(val)) {
                    for (const RamValue* arg : intrinsicOp->getArguments()) {
                        queue.push_back(arg);
                    }
                } else if (const auto* userDefinedOp = dynamic_cast<const RamUserDefinedOperator*>(val)) {
                    for (const RamValue* arg : userDefinedOp->getArguments()) {
                        queue.push_back(arg);
                    }
                }
            }
            return false;
        }

        bool dependsOn(const RamCondition* condition, const size_t identifier) const {
            if (const auto* binRel = dynamic_cast<const RamBinaryRelation*>(condition)) {
                return dependsOn(binRel->getLHS(), identifier) || dependsOn(binRel->getRHS(), identifier);
            }
            return false;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            if (auto* scan = dynamic_cast<RamRelationSearch*>(node.get())) {
                const size_t identifier = scan->getIdentifier();
                bool isExistCheck = true;
                visitDepthFirst(scan->getOperation(), [&](const RamFilter& filter) {
                    if (isExistCheck) {
                        for (const RamCondition* c : getConditions(filter.getCondition().clone())) {
                            if (dependsOn(c, identifier)) {
                                isExistCheck = false;
                                break;
                            }
                        }
                    }
                });
                if (isExistCheck) {
                    visitDepthFirst(scan->getOperation(), [&](const RamIndexScan& indexScan) {
                        if (isExistCheck) {
                            for (const RamValue* value : indexScan.getRangePattern()) {
                                if (value != nullptr && !context->rcva->isConstant(value) &&
                                        dependsOn(value, identifier)) {
                                    isExistCheck = false;
                                    break;
                                }
                            }
                        }
                    });
                }
                if (isExistCheck) {
                    visitDepthFirst(scan->getOperation(), [&](const RamProject& project) {
                        if (isExistCheck) {
                            std::vector<const RamValue*> values;
                            // TODO: function to extend vectors
                            const std::vector<RamValue*> initialVals = project.getValues();
                            values.insert(values.end(), initialVals.begin(), initialVals.end());

                            while (!values.empty()) {
                                const RamValue* value = values.back();
                                values.pop_back();

                                if (const auto* pack = dynamic_cast<const RamPack*>(value)) {
                                    const std::vector<RamValue*> args = pack->getArguments();
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
                }
                if (isExistCheck) {
                    visitDepthFirst(scan->getOperation(), [&](const RamLookup& lookup) {
                        if (isExistCheck) {
                            if (lookup.getReferenceLevel() == identifier) {
                                isExistCheck = false;
                            }
                        }
                    });
                }
                if (isExistCheck) {
                    visitDepthFirst(scan->getOperation(), [&](const RamExists& exists) {
                        if (isExistCheck) {
                            for (const RamValue* value : exists.getValues()) {
                                if (value != nullptr && !context->rcva->isConstant(value) &&
                                        dependsOn(value, identifier)) {
                                    isExistCheck = false;
                                    break;
                                }
                            }
                        }
                    });
                }
                scan->setIsPureExistenceCheck(isExistCheck);
            }
            node->apply(*this);
            return node;
        }
    };

    // Node-mapper that searches for and updates RAM inserts
    class RamInsertCapturer : public RamNodeMapper {
        mutable bool modified;
        ConvertExistenceChecksTransformer* context;

    public:
        RamInsertCapturer(ConvertExistenceChecksTransformer* c) : modified(false), context(c) {}

        bool getModified() const {
            return modified;
        }

        std::unique_ptr<RamNode> operator()(std::unique_ptr<RamNode> node) const override {
            // get all RAM inserts
            if (auto* insert = dynamic_cast<RamInsert*>(node.get())) {
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
    RamInsertCapturer insertUpdate(this);
    program.getMain()->apply(insertUpdate);

    return insertUpdate.getModified();
}

}  // end of namespace souffle
