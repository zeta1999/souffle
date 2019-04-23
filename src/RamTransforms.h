/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamTransforms.h
 *
 * Defines RAM transformation passes.
 *
 ***********************************************************************/

#pragma once

#include "RamConditionLevel.h"
#include "RamConstValue.h"
#include "RamExpressionLevel.h"
#include "RamTransformer.h"
#include "RamTranslationUnit.h"
#include <memory>
#include <string>

namespace souffle {

class RamProgram;

/**
 * Hoists the conditions to the earliest point in the loop nest where they
 * can be evaluated.
 *
 * levelConditions assumes that filter operations are stored verbose,
 * i.e. a conjunction is expressed by two consecutive filter operations.
 * For example ..
 *
 *  QUERY
 *   ...
 *    IF C1 /\ C2 then
 *     ...
 *
 * should be rewritten / or produced by the translator as
 *
 *  QUERY
 *   ...
 *    IF C1
 *     IF C2
 *      ...
 *
 * otherwise the levelling becomes imprecise. For both conditions
 * the most outer-level is sought rather than separately.
 *
 * If there are transformers prior to levelCondition that introduce
 * conjunction, another transformer is required that splits the
 * filter operations. However, at the moment this is not necessary
 * because the translator delivers already this format.
 *
 */
class LevelConditionsTransformer : public RamTransformer {
public:
    std::string getName() const override {
        return "LevelConditionsTransformer";
    }

    /**
     * @param program the program to be processed
     * @return whether the program was modified
     */
    bool levelConditions(RamProgram& program);

protected:
    RamConditionLevelAnalysis* rcla{nullptr};

    bool transform(RamTranslationUnit& translationUnit) override {
        rcla = translationUnit.getAnalysis<RamConditionLevelAnalysis>();
        return levelConditions(*translationUnit.getProgram());
    }
};

/**
 * Convert RamScan operations to RamIndexScan operations
 */
class CreateIndicesTransformer : public RamTransformer {
public:
    std::string getName() const override {
        return "CreateIndicesTransformer";
    }

    /** Get expression of an equivalence relation of the format t1.x = <expr> or <expr> = t1.x */
    std::unique_ptr<RamExpression> getExpression(RamCondition* c, size_t& element, int level);

    /** Construct patterns for an indexable operation and the remaining condition that cannot be indexed */
    std::unique_ptr<RamCondition> constructPattern(std::vector<std::unique_ptr<RamExpression>>& queryPattern,
            bool& indexable, std::vector<std::unique_ptr<RamCondition>> conditionList, int identifier);

    /** Rewrite a scan operation to an indexed scan operation */
    std::unique_ptr<RamOperation> rewriteScan(const RamScan* scan);

    /** Rewrite an aggregate operation to an indexed aggregate operation */
    std::unique_ptr<RamOperation> rewriteAggregate(const RamAggregate* agg);

    bool createIndices(RamProgram& program);

protected:
    RamExpressionLevelAnalysis* rvla{nullptr};
    RamConstValueAnalysis* rcva{nullptr};

    bool transform(RamTranslationUnit& translationUnit) override {
        rvla = translationUnit.getAnalysis<RamExpressionLevelAnalysis>();
        rcva = translationUnit.getAnalysis<RamConstValueAnalysis>();
        return createIndices(*translationUnit.getProgram());
    }
};

class ConvertExistenceChecksTransformer : public RamTransformer {
public:
    std::string getName() const override {
        return "ConvertExistenceChecksTransformer";
    }

    /**
     * @param program the program to be processed
     * @return whether the program was modified
     */
    bool convertExistenceChecks(RamProgram& program);

protected:
    RamConstValueAnalysis* rcva{nullptr};
    RamConditionLevelAnalysis* rcla{nullptr};
    RamExpressionLevelAnalysis* rvla{nullptr};

    bool transform(RamTranslationUnit& translationUnit) override {
        rcva = translationUnit.getAnalysis<RamConstValueAnalysis>();
        rcla = translationUnit.getAnalysis<RamConditionLevelAnalysis>();
        rvla = translationUnit.getAnalysis<RamExpressionLevelAnalysis>();
        return convertExistenceChecks(*translationUnit.getProgram());
    }
};

}  // end of namespace souffle
