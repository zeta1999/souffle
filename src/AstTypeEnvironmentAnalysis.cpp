/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstTypeEnvironmentAnalysis.cpp
 *
 * Implements AST Analysis methods for a Type Environment
 *
 ***********************************************************************/

#include "AstTypeEnvironmentAnalysis.h"
#include "AstProgram.h"
#include "AstTranslationUnit.h"
#include "AstType.h"
#include "GraphUtils.h"
#include "TypeSystem.h"
#include <cassert>
#include <iostream>

namespace souffle {

void TypeEnvironmentAnalysis::run(const AstTranslationUnit& translationUnit) {
    const AstProgram& program = *translationUnit.getProgram();

    std::vector<AstType*> programTypes = program.getTypes();
    createTypes(programTypes);
    linkTypes(programTypes);

    Graph<AstQualifiedName> typeDependencyGraph{createTypeDependencyGraph(programTypes)};

    analyseCyclicUnions(typeDependencyGraph, programTypes);
    analysePrimitiveTypesInUnion(typeDependencyGraph, programTypes);
}

void TypeEnvironmentAnalysis::createTypes(const std::vector<AstType*>& programTypes) {
    for (const auto* astType : programTypes) {
        // support invalid input with multiple definitions
        if (env.isType(astType->getQualifiedName())) {
            continue;
        }

        if (auto* subType = dynamic_cast<const AstSubsetType*>(astType)) {
            env.createSubsetType(astType->getQualifiedName(), subType->getTypeAttribute());
        } else if (isA<AstUnionType>(*astType)) {
            env.createType<UnionType>(astType->getQualifiedName());
        } else if (isA<AstRecordType>(*astType)) {
            env.createType<RecordType>(astType->getQualifiedName());
        } else {
            fatal("unsupported type construct: %s", typeid(astType).name());
        }
    }
}

void TypeEnvironmentAnalysis::linkTypes(const std::vector<AstType*>& programTypes) {
    for (const auto* astType : programTypes) {
        Type& type = env.getType(astType->getQualifiedName());

        if (isA<AstSubsetType>(*astType)) {
            // nothing to do here
        } else if (auto* astUnion = dynamic_cast<const AstUnionType*>(astType)) {
            auto& unionType = dynamic_cast<UnionType&>(type);

            // add element types
            for (const auto& astType : astUnion->getTypes()) {
                if (env.isType(astType)) {
                    unionType.add(env.getType(astType));
                }
            }
        } else if (auto* astRecord = dynamic_cast<const AstRecordType*>(astType)) {
            auto& recordType = dynamic_cast<RecordType&>(type);

            // add fields
            for (auto&& f : astRecord->getFields()) {
                if (env.isType(f->getTypeName())) {
                    recordType.add(f->getName(), env.getType(f->getTypeName()));
                }
            }
        } else {
            fatal("unsupported type construct: %s", typeid(astType).name());
        }
    }
}

Graph<AstQualifiedName> TypeEnvironmentAnalysis::createTypeDependencyGraph(
        const std::vector<AstType*>& programTypes) {
    Graph<AstQualifiedName> typeDependencyGraph;
    for (const auto* astType : programTypes) {
        if (auto type = dynamic_cast<const AstSubsetType*>(astType)) {
            switch (type->getTypeAttribute()) {
                case TypeAttribute::Signed:
                    typeDependencyGraph.insert(type->getQualifiedName(), "number");
                    break;
                case TypeAttribute::Unsigned:
                    typeDependencyGraph.insert(type->getQualifiedName(), "unsigned");
                    break;
                case TypeAttribute::Float:
                    typeDependencyGraph.insert(type->getQualifiedName(), "float");
                    break;
                case TypeAttribute::Symbol:
                    typeDependencyGraph.insert(type->getQualifiedName(), "symbol");
                    break;
                case TypeAttribute::Record:
                    fatal("invalid type");
            }
        } else if (dynamic_cast<const AstRecordType*>(astType) != nullptr) {
            // do nothing
        } else if (auto type = dynamic_cast<const AstUnionType*>(astType)) {
            for (const auto& subtype : type->getTypes()) {
                typeDependencyGraph.insert(type->getQualifiedName(), subtype);
            }
        } else {
            fatal("unsupported type construct: %s", typeid(astType).name());
        }
    }
    return typeDependencyGraph;
}

void TypeEnvironmentAnalysis::analyseCyclicUnions(
        const Graph<AstQualifiedName>& dependencyGraph, const std::vector<AstType*>& programTypes) {
    for (const auto& astType : programTypes) {
        auto unionType = dynamic_cast<const AstUnionType*>(astType);
        if (unionType == nullptr) {
            continue;
        }

        AstQualifiedName unionName = unionType->getQualifiedName();
        if (dependencyGraph.reaches(unionName, unionName)) {
            auto& unionType = dynamic_cast<UnionType&>(env.getType(unionName));
            unionType.clear();
            cyclicTypes.insert(std::move(unionName));
        }
    }
}

void TypeEnvironmentAnalysis::analysePrimitiveTypesInUnion(
        const Graph<AstQualifiedName>& dependencyGraph, const std::vector<AstType*>& programTypes) {
    for (const auto& astType : programTypes) {
        auto unionType = dynamic_cast<const AstUnionType*>(astType);
        if (unionType == nullptr) {
            continue;
        }
        AstQualifiedName unionName = unionType->getQualifiedName();

        auto iteratorToUnion = primitiveTypesInUnions.find(unionName);

        // Initialize with the empty set
        if (iteratorToUnion == primitiveTypesInUnions.end()) {
            iteratorToUnion = primitiveTypesInUnions.insert({unionName, {}}).first;
        }

        auto& associatedTypes = iteratorToUnion->second;

        // Insert any reachable primitive type
        if (dependencyGraph.reaches(unionName, "number")) {
            associatedTypes.insert(TypeAttribute::Signed);
        }

        if (dependencyGraph.reaches(unionName, "symbol")) {
            associatedTypes.insert(TypeAttribute::Symbol);
        }

        if (dependencyGraph.reaches(unionName, "unsigned")) {
            associatedTypes.insert(TypeAttribute::Unsigned);
        }

        if (dependencyGraph.reaches(unionName, "float")) {
            associatedTypes.insert(TypeAttribute::Float);
        }
    }
}

void TypeEnvironmentAnalysis::print(std::ostream& os) const {
    env.print(os);
}

}  // namespace souffle
