/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstTypeEnvironmentAnalysis.h
 *
 * A wrapper for TypeEnvironment to be used for AST Analysis
 *
 ***********************************************************************/

#pragma once

#include "AstAnalysis.h"
#include "GraphUtils.h"
#include "RamTypes.h"
#include "TypeSystem.h"
#include "Util.h"
#include <map>
#include <ostream>
#include <set>

namespace souffle {

class AstProgram;
class AstTranslationUnit;

class TypeEnvironmentAnalysis : public AstAnalysis {
public:
    static constexpr const char* name = "type-environment";

    TypeEnvironmentAnalysis() : AstAnalysis(name) {}

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& os) const override;

    const TypeEnvironment& getTypeEnvironment() const {
        return env;
    }

    const std::set<TypeAttribute>& getPrimitiveTypesInUnion(const AstQualifiedName& identifier) const {
        return primitiveTypesInUnions.at(identifier);
    }

    bool isCyclic(const AstQualifiedName& identifier) const {
        return contains(cyclicTypes, identifier);
    }

private:
    TypeEnvironment env;
    std::map<AstQualifiedName, std::set<TypeAttribute>> primitiveTypesInUnions;
    std::set<AstQualifiedName> cyclicTypes;

    /**
     * Populate Type Environment with Ast Types.
     */
    void createTypes(const std::vector<AstType*>& program);

    /**
     * Link unions and records to their types.
     */
    void linkTypes(const std::vector<AstType*>& program);

    /**
     * Create a type dependency graph.
     */
    Graph<AstQualifiedName> createTypeDependencyGraph(const std::vector<AstType*>& programTypes);

    /**
     * Check intersections of unions with primitive types.
     */
    void analysePrimitiveTypesInUnion(
            const Graph<AstQualifiedName>& dependencyGraph, const std::vector<AstType*>& programTypes);

    /**
     * Find cyclic types.
     */
    void analyseCyclicTypes(
            const Graph<AstQualifiedName>& dependencyGraph, const std::vector<AstType*>& programTypes);
};

}  // end of namespace souffle
