/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstProfileUse.h
 *
 * Defines a simple class to query profile data from a profile
 * for profile-guided optimisation.
 *
 ***********************************************************************/

#pragma once

#include "AstAnalysis.h"
#include "AstRelation.h"
#include "profile/ProgramRun.h"
#include "profile/Reader.h"
#include <cstddef>
#include <iostream>
#include <map>
#include <set>
#include <stack>
#include <utility>
#include <vector>

namespace souffle {

/**
 * Analysis pass computing the precedence graph of the relations of the datalog progam.
 */
class AstProfileUse : public AstAnalysis {
private:
    /** performance model of profile run */
    std::shared_ptr<profile::ProgramRun> programRun;

public:
    static constexpr const char* name = "profile-use";

    AstProfileUse();

    /* Run analysis */
    void run(const AstTranslationUnit& translationUnit) override;

    /** Output some profile information */
    void print(std::ostream& os) const override;

    /** Return size of relation */
    size_t getRelationSize(const AstRelationIdentifier* rel);
};

}  // end of namespace souffle
