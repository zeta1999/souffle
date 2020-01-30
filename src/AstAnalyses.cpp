/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstAnalyses.cpp
 *
 * Implementation of AST analyses classes
 *
 ***********************************************************************/

#include <cassert>
#include <utility>

#include "AstProgram.h"
#include "AstUtils.h"
#include "AstAnalyses.h"

namespace souffle {

    const size_t AuxiliaryArity::getAuxiliaryArity(const AstAtom* atom, const AstProgram* program) {
          return program->getRelation(atom->getName())->getAuxiliaryArity();
    }

}   // end of namespace souffle
