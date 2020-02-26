/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstRelationOwnershipAnalysis.h
 *
 * Defines an analysis to associate clauses with declared relations.
 *
 ***********************************************************************/

#pragma once

#include "AstAnalysis.h"
#include <map>
#include <vector>

namespace souffle {

class AstClause;
class AstQualifiedName;
class AstTranslationUnit;

/**
 * Analysis pass associating clauses with their relation.
 */
class AstRelationOwnershipAnalysis : public AstAnalysis {
public:
    static constexpr const char* name = "ast-clause-ownership";

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& os) const override;

    /** Get the clauses with the given relation name */
    std::vector<AstClause*> getRelationClauses(const AstQualifiedName& name) const {
        return nameToClauses.at(name);
    }

    /** Get orphan clauses in the program */
    std::vector<AstClause*> getOrphanClauses() const {
        return orphanClauses;
    }

    /** Get relation with the given name in the program */
    AstRelation* getRelation(const AstQualifiedName& name) const {
        auto pos = nameToRelation.find(name);
        return (pos == nameToRelation.end()) ? nullptr : pos->second;
    }

private:
    std::map<AstQualifiedName, std::vector<AstClause*>> nameToClauses;
    std::map<AstQualifiedName, AstRelation*> nameToRelation;
    std::vector<AstClause*> orphanClauses;
};

}
