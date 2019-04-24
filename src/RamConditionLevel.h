/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamConditionLevel.h
 *
 * Get level of condition (which for-loop of a query)
 *
 ***********************************************************************/

#pragma once

#include "RamAnalysis.h"

namespace souffle {

class RamCondition;
class RamTranslationUnit;
class RamExpressionLevelAnalysis;

/*
 * Class for a level analysis
 */
class RamConditionLevelAnalysis : public RamAnalysis {
public:
    /** name of analysis */
    static constexpr const char* name = "condition-level-analysis";

    /** run level analysis for a RAM translation unit */
    void run(const RamTranslationUnit& translationUnit) override;

    /** Get level */
    int getLevel(const RamCondition* condition) const;

protected:
    RamExpressionLevelAnalysis* rvla;
};

}  // end of namespace souffle
