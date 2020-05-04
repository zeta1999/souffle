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

    // Filter redefinitions of predefined types.
    std::vector<AstType*> programTypes;
    for (auto* type : program.getTypes()) {
        if (!env.isType(type->getQualifiedName())) {
            programTypes.push_back(type);
        }
    }
    createTypes(programTypes);
    linkTypes(programTypes);

    Graph<AstQualifiedName> typeDependencyGraph{createTypeDependencyGraph(programTypes)};

    analyseCyclicTypes(typeDependencyGraph, programTypes);
    analysePrimitiveTypesInUnion(typeDependencyGraph, programTypes);
}

void TypeEnvironmentAnalysis::createTypes(const std::vector<AstType*>& programTypes) {
    for (const auto* astType : programTypes) {
        // support invalid input with multiple definitions
        if (env.isType(astType->getQualifiedName())) {
            continue;
        }

        if (isA<AstSubsetType>(*astType)) {
            env.createType<SubsetType>(astType->getQualifiedName());
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
        if (auto* astSubsetType = dynamic_cast<const AstSubsetType*>(astType)) {
            auto& subsetType = dynamic_cast<SubsetType&>(type);

            subsetType.setBaseType(env.getType(astSubsetType->getBaseType()));

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
            for (const auto* field : astRecord->getFields()) {
                if (env.isType(field->getTypeName())) {
                    recordType.add(env.getType(field->getTypeName()));
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
            typeDependencyGraph.insert(type->getQualifiedName(), type->getBaseType());
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

void TypeEnvironmentAnalysis::analyseCyclicTypes(
        const Graph<AstQualifiedName>& dependencyGraph, const std::vector<AstType*>& programTypes) {
    for (const auto& astType : programTypes) {
        AstQualifiedName typeName = astType->getQualifiedName();
        auto& type = env.getType(typeName);

        if (dependencyGraph.reaches(typeName, typeName)) {
            if (auto* unionType = dynamic_cast<UnionType*>(&type)) {
                unionType->clear();
            } else if (auto* subsetType = dynamic_cast<SubsetType*>(&type)) {
                subsetType->setBaseType(env.getType("number"));
            }
            cyclicTypes.insert(std::move(typeName));
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
        for (auto& type : env.getPrimitiveTypes()) {
            if (dependencyGraph.reaches(unionName, type.getName())) {
                associatedTypes.insert(type.getName());
            }
        }
    }
}

void TypeEnvironmentAnalysis::print(std::ostream& os) const {
    env.print(os);
}

}  // namespace souffle
