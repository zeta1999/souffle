/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstTypeAnalysis.h
 *
 * A collection of type analyses operating on AST constructs.
 *
 ***********************************************************************/

#pragma once

#include "AstAnalysis.h"
#include "TypeSystem.h"
#include "AstTypeEnvironmentAnalysis.h"
#include "TypeLattice.h"
#include "TypeSystem.h"
#include <cassert>
#include <map>
#include <memory>
#include <sstream>
#include <utility>
#include <vector>
#include <ostream>

namespace souffle {

class AstArgument;
class AstClause;
class AstProgram;
class AstTranslationUnit;
class AstVariable;

class TypeAnalysis : public AstAnalysis {
public:
    TypeAnalysis() = default;

    static constexpr const char* name = "type-analysis";

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& os) const override;

    /**
     * Get the computed type for the given argument.
     */
    const AnalysisType* getType(const AstArgument* argument) const {
        assert(lattice.isValid() && "Cannot determine type without a valid lattice");
        auto found = argumentTypes.find(argument);
        assert(found != argumentTypes.end() && "Argument must have a type");
        return found->second;
    }

    TypeLattice& getLattice() {
        return lattice;
    }

    const TypeLattice& getLattice() const {
        return lattice;
    }

    // Check if any clauses could not be typechecked
    static bool anyInvalidClauses(const AstProgram& program);

    // Get clauses that can be typechecked
    static std::vector<const AstClause*> getValidClauses(const AstProgram& program);

private:
    std::map<const AstArgument*, const AnalysisType*> argumentTypes{};
    std::stringstream analysisLogs{};
    TypeLattice lattice{};

    static std::set<const AstArgument*> getArguments(
            std::map<std::string, const AstVariable*>* variables, const AstClause& clause);

    /**
     * Analyse the given clause and computes for each contained argument a potential type. If the type is a
     * bottom or top type, no consistent typing can be found and the rule can not be properly typed.
     *
     * @param lat a lattice containing available types
     * @param clause the clause to be typed
     * @param program the program the clause is contained in
     * @param debugStream a pointer to the stream where debugging information should be printed
     * @return a map mapping each contained argument to a type
     */
    static std::map<const AstArgument*, const AnalysisType*> analyseTypes(TypeLattice& lat,
            const AstClause& clause, const AstProgram& program, std::ostream* debugStream = nullptr);
};

}  // end of namespace souffle
