/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstAnalyses.h
 *
 * Define of AST analyses classes
 *
 ***********************************************************************/

#pragma once

#include "AstAnalysis.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "GraphUtils.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <iostream>
#include <iterator>
#include <map>
#include <set>
#include <stack>
#include <utility>
#include <vector>

#include "AstTranslationUnit.h"

namespace souffle {

class AstClause;

/**
 * Determine the auxiliary arity for relations
 */
class AuxiliaryArity : public AstAnalysis {
public:
    static constexpr const char* name = "auxiliary-arity";

    void run(const AstTranslationUnit& translationUnit) override {
        program = translationUnit.getProgram();
    }

    /**
     * Returns the number of auxiliary parameters of an atom
     * @param atom the atom (const AstAtom*)
     * @return number of auxiliary attributes
     */
    const size_t getArity(const AstAtom* atom) const {
        return computeArity(program->getRelation(atom->getName()));
    }

    /**
     * Returns the number of auxiliary parameters of a relation
     * @param atom the atom (const AstRelation*)
     * @return number of auxiliary attributes
     */
    const size_t getArity(const AstRelation* relation) const {
        return computeArity(relation);
    }

    /**
     * Returns the number of auxiliary parameters of relations
     * taken delta/info/new into account.
     * @param atom the atom (const AstRelation*)
     * @return number of auxiliary attributes
     */
    const size_t getEvaluationArity(const AstAtom* atom) const;

    /**
     * Returns the number of auxiliary parameters of a relation
     * @param atom the atom (const AstRelation*)
     * @return number of auxiliary attributes
     */
    const size_t computeArity(const AstRelation* relation) const;

private:
    const AstProgram* program;
};

}  // end of namespace souffle
