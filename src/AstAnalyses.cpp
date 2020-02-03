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

#include "AstAnalyses.h"
#include "AstProgram.h"
#include "AstUtils.h"

namespace souffle {

const size_t AuxiliaryArity::getEvaluationArity(const AstAtom* atom) const {
    if (atom->getName().getName().rfind("@delta_") == 0) {
        const AstRelationIdentifier& originalRel = AstRelationIdentifier(atom->getName().getName().substr(7));
        return getArity(program->getRelation(originalRel));
    } else if (atom->getName().getName().rfind("@new_") == 0) {
        const AstRelationIdentifier& originalRel = AstRelationIdentifier(atom->getName().getName().substr(5));
        return getArity(program->getRelation(originalRel));
    } else if (atom->getName().getName().rfind("@info_") == 0) {
        return 0;
    } else {
        return getArity(atom);
    }
}

}  // end of namespace souffle
