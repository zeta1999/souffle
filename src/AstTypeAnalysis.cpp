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

#include "AstTypeAnalysis.h"
#include "AstArgument.h"
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
#include "Util.h"
#include <cassert>
#include <map>
#include <ostream>
#include <vector>

namespace souffle {

using typeSol = std::map<const AstArgument*, AnalysisType>;

class TypeConstraint {
public:
    virtual typeSol resolve(const typeSol& existing, const TypeLattice& lattice);
    virtual bool isSatisfied(const typeSol& solution, const TypeLattice& lattice);
    virtual void print(std::ostream& os) const {}
    friend std::ostream& operator<<(std::ostream& out, const AstAnalysis& other) {
        other.print(out);
        return out;
    }
};

class FixedConstraint : TypeConstraint {
private:
    AstArgument* variable;
    AnalysisType bound;

public:
    FixedConstraint(const AstArgument& variable, const AnalysisType bound)
            : variable(variable), bound(bound){};
    typeSol resolve(const typeSol& existing, const TypeLattice& lattice) {
        assert(existing->find(variable) != existing->end() && "Variable already has a type");
        typeSol ret(existing);
        ret[variable] = lattice->meet(existing[variable], bound);
        return *ret;
    }
    bool isSatisfied(const typeSol& solution, const TypeLattice& lattice) {
        assert(solution->find(variable) != solution->end() && "Variable has a type");
        return lattice->isSubtype(solution[variable], bound);
    }
    void print(std::ostream& os) const {
        os << variable << "<:" << bound;
    }
};

class VarConstraint : TypeConstraint {
private:
    AstArgument* variable;
    AstArgument* bound;

public:
    VarConstraint(const AstArgument& variable, const AstArgument& bound) : variable(variable), bound(bound){};
    typeSol resolve(const typeSol& existing, const TypeLattice& lattice) {
        assert(existing->find(variable) != existing->end() && "Variable already has a type");
        assert(existing->find(bound) != existing->end() && "Bound already has a type");
        typeSol ret(existing);
        ret[variable] = lattice->meet(existing[variable], existing[bound);
        return *ret;
    }
    bool isSatisfied(const typeSol& solution, const TypeLattice& lattice) {
        assert(solution->find(variable) != solution->end() && "Variable has a type");
        assert(solution->find(bound) != solution->end() && "Bound has a type");
        return lattice->isSubtype(solution[variable], solution[bound]);
    }
    void print(std::ostream& os) const {
        os << variable << "<:" << bound;
    }
};

class UnionConstraint : TypeConstraint {
private:
    AstArgument* variable;
    AstArgument* firstBound;
    AstArgument* secondBound;

public:
    UnionConstraint(
            const AstArgument& variable, const AstArgument& firstBound, const AstArgument& secondBound)
            : variable(variable), firstBound(firstBound), secondBound(secondBound){};
    typeSol resolve(const typeSol& existing, const TypeLattice& lattice) {
        assert(existing->find(variable) != existing->end() && "Variable already has a type");
        assert(existing->find(firstBound) != existing->end() && "First bound already has a type");
        assert(existing->find(secondBound) != existing->end() && "Second bound already has a type");
        typeSol ret(existing);
        ret[variable] =
                lattice->meet(existing[variable], lattice->join(existing[firstBound], existing[secondBound]));
        return ret;
    }
    bool isSatisfied(const typeSol& solution, const TypeLattice& lattice) {
        assert(solution->find(variable) != solution->end() && "Variable has a type");
        assert(solution->find(firstBound) != solution->end() && "First bound has a type");
        assert(solution->find(secondBound) != solution->end() && "Second bound has a type");
        return lattice->isSubtype(
                solution[variable], lattice->join(solution[firstBound], solution[secondBound]));
    }
    void print(std::ostream& os) const {
        os << variable << "<:(" << firstBound << "∪" << secondBound << ")";
    }
};

class ImplicationConstraint : TypeConstraint {
private:
    std::vector<FixedConstraint> requirements;
    FixedConstraint result;

public:
    ImplicationConstraint(const AstArgument& variable, const AnalysisType bound)
            : result(variable, bound), requirements(){};
    void addRequirement(const AstArgument& variable, const AnalysisType bound) {
        requirements.push_back(FixedConstraint(variable, bound));
    }
    typeSol resolve(const typeSol& existing, const TypeLattice& lattice) {
        for (req : requirements) {
            if (!req.isSatisfied(existing, lattice)) {
                return existing;
            }
        }
        return result.resolve(existing, lattice);
    }
    bool isSatisfied(const typeSol& solution, const TypeLattice& lattice) {
        for (req : requirements) {
            if (!req.isSatisfied(solution, lattice)) {
                return true;
            }
        }
        return result.isSatisfied(solution, lattice);
    }
    void print(std::ostream& os) const {
        os << "(" << join(requirements) << ") => (" << result << ")";
    }
};

std::vector<TypeConstraint> getConstraints(const AstClause& clause) {
    struct constraintFinder : public AstVisitor<std::vector<TypeConstraint>> {
        visitNode(const AstNode& node) {
            std::vector<TypeConstraint> ret;
            for (const AstNode* cur : root.getChildNodes()) {
                std::vector<TypeConstraint> curCons = visitNode(cur);
                ret.insert(ret.end(), curCons.begin(), curCons.end());
            }
            return ret;
        }
        // TODO add other visitors
    };
}

std::map<const AstArgument*, AnalysisType> analyseTypes(const TypeLattice& lattice, const AstClause& clause) {
    // TODO
}

void TypeAnalysis::run(const AstTranslationUnit& translationUnit) {
    auto* typeEnvAnalysis = translationUnit.getAnalysis<TypeEnvironmentAnalysis>();
    TypeLattice lattice = TypeLattice(typeEnvAnalysis->getTypeEnvironment());
    for (const AstRelation* rel : translationUnit.getProgram()->getRelations()) {
        for (const AstClause* clause : rel->getClauses()) {
            // Perform the type analysis
            std::map<const AstArgument*, AnalysisType> clauseArgumentTypes = analyseTypes(*lattice, *clause);
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

}  // end of namespace souffle
