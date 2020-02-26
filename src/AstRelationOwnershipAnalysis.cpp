/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstRelationOwnershipAnalysis.cpp
 *
 * Defines an analysis to associate clauses with declared relations.
 *
 ***********************************************************************/

#include "AstClause.h"
#include "AstRelation.h"
#include "AstRelationOwnershipAnalysis.h"
#include "AstTranslationUnit.h"
#include <map>
#include <vector>

namespace souffle {

void AstRelationOwnershipAnalysis::run(const AstTranslationUnit& translationUnit) {
    const AstProgram& program = *translationUnit.getProgram();

    /* Set up the name->relation map */
    for (AstRelation* relation : program.getRelations()) {
        nameToRelation[relation->getQualifiedName()] = relation;
        nameToClauses[relation->getQualifiedName()] = std::vector<AstClause*>();
    }

    /* Set up the relation->clauses map and orphan clauses */
    for (AstClause* clause : program.getClauses()) {
        auto relationName = clause->getHead()->getQualifiedName();
        if (nameToRelation.find(relationName) != nameToRelation.end()) {
            nameToClauses[relationName].push_back(clause);
        } else {
            orphanClauses.push_back(clause);
        }
    }
}

void AstRelationOwnershipAnalysis::print(std::ostream& os) const {
    os << "--- Relations ---" << std::endl;
    for (const auto& cur : nameToClauses) {
        os << "Relation: " << cur.first << std::endl;
        for (AstClause* clause : cur.second) {
            os << *clause << std::endl;
        }
    }
    os << std::endl;
    os << "--- Orphan Clauses ---" << std::endl;
    for (const auto* orphan : orphanClauses) {
        os << *orphan << std::endl;
    }
}

}
