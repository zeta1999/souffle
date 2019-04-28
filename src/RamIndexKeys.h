/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamIndexKeys.h
 *
 * Get the indexable columns for a range query
 *
 ***********************************************************************/

#pragma once

#include "RamAnalysis.h"
#include "RamOperation.h"

namespace souffle {

/*
 * Class to compute the search columns of an index scan
 */
class RamIndexKeysAnalysis : public RamAnalysis {
public:
    /** Name of analysis */
    static constexpr const char* name = "index-scan-keys-analysis";

    /** Run keys analysis for a RAM translation unit */
    void run(const RamTranslationUnit& translationUnit) override {}

    /** Get indexable columns of index scan */
    SearchColumns getRangeQueryColumns(const RamIndexRelationSearch* search) const;

    /** Get indexable columns of index choice */
    SearchColumns getRangeQueryColumns(const RamIndexChoice* search) const;
};

}  // end of namespace souffle
