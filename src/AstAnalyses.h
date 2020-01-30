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

#include "AstProgram.h"
#include "AstAnalysis.h"
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

namespace souffle {

class AstClause;
class AstTranslationUnit;

/**
 * Determine the auxiliary arity for relations
 */
class AuxiliaryArity : public AstAnalysis {
public:
    static constexpr const char* name = "auxiliary-arity";

    void run(const AstTranslationUnit& translationUnit) override {}

    /**
     * Returns the number of auxiliary parameters of an atom
     * @param atom the atom
     * @param program the program containing the relations
     * @return number of auxiliary attributes in the atom
     */
    const size_t getAuxiliaryArity(const AstAtom* atom, const AstProgram* program);

    /**
     * Returns the number of auxiliary parameters of an atom
     * @param atom the atom
     * @param program the program containing the relations
     * @return number of auxiliary attributes in the atom
     */
    const size_t getArity(const AstProgram* program, const AstAtom* atom) const {
          return program->getRelation(atom->getName())->getAuxiliaryArity();
    } 

    const size_t getArity(const AstRelation* relation) const {
        return relation->getAuxiliaryArity(); 
    } 
};

}  // end of namespace souffle
