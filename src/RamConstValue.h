/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamConstValue.h
 *
 * Check whether a RAM value is constant
 *
 ***********************************************************************/

#pragma once

#include <set>

#include "RamAnalysis.h"
#include "RamValue.h"

namespace souffle {

/*
 * Class for a constant check
 */
class RamConstValueAnalysis : public RamAnalysis {
    /** set of constant ram values */
    std::set<const RamValue*> constRamValues;

public:
    RamConstValueAnalysis() = default;

    /** name of analysis */
    static constexpr const char* name = "const-value-analysis";

    /** run const value analysis for a RAM translation unit */
    void run(const RamTranslationUnit& translationUnit) override;

    /** print the analysis result in HTML format */
    void print(std::ostream& os) const override;

    /** constant value */
    bool isConstant(const RamValue* v) const {
        return constRamValues.find(v) != constRamValues.end();
    }
};

}  // end of namespace souffle
