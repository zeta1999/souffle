/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamConditionLevel.cpp
 *
 * Implementation of RAM Condition Level Analysis
 *
 ***********************************************************************/

#include "RamConditionLevel.h"
#include "RamCondition.h"
#include "RamExpressionLevel.h"
#include "RamTranslationUnit.h"
#include "RamVisitor.h"
#include <algorithm>
#include <vector>

namespace souffle {

/** run level analysis for a RAM translation unit */
void RamConditionLevelAnalysis::run(const RamTranslationUnit& translationUnit) {
    rvla = translationUnit.getAnalysis<RamExpressionLevelAnalysis>();
}

/** Get level of condition (which for-loop of a query) */
int RamConditionLevelAnalysis::getLevel(const RamCondition* condition) const {
    // visitor
    class ConditionLevelVisitor : public RamVisitor<int> {
        RamExpressionLevelAnalysis* rvla;

    public:
        ConditionLevelVisitor(RamExpressionLevelAnalysis* rvla) : rvla(rvla) {}

        // conjunction
        int visitConjunction(const RamConjunction& conj) override {
            return std::max(visit(conj.getLHS()), visit(conj.getRHS()));
        }

        // negation
        int visitNegation(const RamNegation& neg) override {
            return visit(neg.getOperand());
        }

        // constraint
        int visitConstraint(const RamConstraint& binRel) override {
            return std::max(rvla->getLevel(binRel.getLHS()), rvla->getLevel(binRel.getRHS()));
        }

        // existence check
        int visitExistenceCheck(const RamExistenceCheck& exists) override {
            int level = -1;
            for (const auto& cur : exists.getValues()) {
                if (cur != nullptr) {
                    level = std::max(level, rvla->getLevel(cur));
                }
            }
            return level;
        }

        // provenance existence check
        int visitProvenanceExistenceCheck(const RamProvenanceExistenceCheck& provExists) override {
            int level = -1;
            for (const auto& cur : provExists.getValues()) {
                if (cur != nullptr) {
                    level = std::max(level, rvla->getLevel(cur));
                }
            }
            return level;
        }

        // emptiness check
        int visitEmptinessCheck(const RamEmptinessCheck& emptiness) override {
            return -1;  // can be in the top level
        }
    };
    return ConditionLevelVisitor(rvla).visit(condition);
}

}  // end of namespace souffle
