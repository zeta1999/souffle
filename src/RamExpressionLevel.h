/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamExpressionLevel.h
 *
 * Get level of an expression to determine the placement in the loop-nest
 *
 ***********************************************************************/

#pragma once

#include "RamAnalysis.h"
#include "RamExpression.h"

namespace souffle {

/*
 * Class for a level analysis
 */
class RamExpressionLevelAnalysis : public RamAnalysis {
public:
    /** name of analysis */
    static constexpr const char* name = "value-level-analysis";

    /** run level analysis for a RAM translation unit */
    void run(const RamTranslationUnit& translationUnit) override {}

    /** Get level */
    size_t getLevel(const RamExpression* value) const;
};

}  // end of namespace souffle
