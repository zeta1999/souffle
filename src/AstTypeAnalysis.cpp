/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstTypeAnalysis.cpp
 *
 * Implements a collection of type analyses operating on AST constructs.
 *
 ***********************************************************************/

// TODO clean up includes
#include "AstTypeAnalysis.h"
#include "AstArgument.h"
// #include "AstAttribute.h"
#include "AstClause.h"
// #include "AstConstraintAnalysis.h"
#include "AstFunctorDeclaration.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "AstTranslationUnit.h"
#include "AstType.h"
#include "Util.h"
// #include "AstTypeEnvironmentAnalysis.h"
// #include "AstUtils.h"
// #include "AstVisitor.h"
// #include "Constraints.h"
// #include "Global.h"
#include "TypeLattice.h"
// #include "TypeSystem.h"
#include <cassert>
#include <map>
// #include <memory>
#include <ostream>
// #include <set>
// #include <string>
// #include <utility>

namespace souffle {

void TypeAnalysis::run(const AstTranslationUnit& translationUnit) {
    auto* typeEnvAnalysis = translationUnit.getAnalysis<TypeEnvironmentAnalysis>();
    TypeLattice lattice = TypeLattice(typeEnvAnalysis->getTypeEnvironment());
    for (const AstRelation* rel : translationUnit.getProgram()->getRelations()) {
        for (const AstClause* clause : rel->getClauses()) {
            // Perform the type analysis
            std::map<const AstArgument*, AnalysisType> clauseArgumentTypes = analyseTypes(lattice, *clause);
            argumentTypes.insert(clauseArgumentTypes.begin(), clauseArgumentTypes.end());

            if (debugStream != nullptr) {
                // Store an annotated clause for printing purposes
                AstClause* annotatedClause = createAnnotatedClause(clause, clauseArgumentTypes);
                annotatedClauses.emplace_back(annotatedClause);
            }
        }
    }
}

void TypeAnalysis::print(std::ostream& os) const {
    // TODO
}

std::map<const AstArgument*, TypeSet> TypeAnalysis::analyseTypes(
        TypeLattice lattice, const AstClause& clause) {
    // TODO
}

}  // end of namespace souffle
