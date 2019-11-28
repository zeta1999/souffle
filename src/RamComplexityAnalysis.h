/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamComplexityAnalysis.h
 *
 * Get the complexity of an expression/condition in terms of 
 * database operations. The number of emptiness checks / existence 
 * checks are added and summed up. An emptiness check is given the
 * weight of one and existence checks is given the weight two.  
 *
 ***********************************************************************/

#pragma once

#include "RamAnalysis.h"
#include "RamNode.h"

namespace souffle {

/**
 * @class RamComplexityAnalysis
 * @brief A Ram Analysis for determining the number of relational 
 *        operations in a condition / expression. 
 *
 *
 */
class RamComplexityAnalysis : public RamAnalysis {
public:
    RamComplexityAnalysis(const char* id) : RamAnalysis(id) {}

    static constexpr const char* name = "complexity-analysis";

    void run(const RamTranslationUnit& translationUnit) override {}

    /**
     * @brief Get complexity of a RAM expression/condition
     */
    int getComplexity(const RamNode* value) const;
};

}  // end of namespace souffle
