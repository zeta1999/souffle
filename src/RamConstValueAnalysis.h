/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamConstAnalysis.h
 *
 * Check whether a RAM value is constant
 *
 ***********************************************************************/

#pragma once

#include <iostream>

#include "RamAnalysis.h"

namespace souffle {

/*
 * Class for a constant check
 */
class RamConstAnalysis : RamAnalysis {
    /** set of constant ram values */ 
    std::set<RamNode *> constRamValues; 

public:

    /** run const value analysis for a RAM translation unit */
    void run(const RamTranslationUnit& translationUnit); 

    /** constant value */
    bool isConstant(RamValue *v) const {
       return constRamValues.find(v) != constRamValues.end(); 
    } 
};

}  // end of namespace souffle
