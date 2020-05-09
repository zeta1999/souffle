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
#include "AstAttribute.h"
#include "AstProgram.h"
#include "AstTranslationUnit.h"
#include "AstType.h"
#include "GraphUtils.h"
#include "TypeSystem.h"
#include "utility/MiscUtil.h"
#include <functional>
#include <ostream>
#include <typeinfo>
#include <utility>
#include <vector>

namespace souffle {

namespace {

Graph<AstQualifiedName> createTypeDependencyGraph(const std::vector<AstType*>& programTypes) {
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

}  // namespace

void TypeEnvironmentAnalysis::run(const AstTranslationUnit& translationUnit) {
    const AstProgram& program = *translationUnit.getProgram();

    auto rawProgramTypes = program.getTypes();
    Graph<AstQualifiedName> typeDependencyGraph{createTypeDependencyGraph(rawProgramTypes)};

    analyseCyclicTypes(typeDependencyGraph, rawProgramTypes);
    analysePrimitiveTypesInUnion(typeDependencyGraph, rawProgramTypes);

    std::map<AstQualifiedName, const AstType*> nameToAstType;

    // Filter redefined primitive types and cyclic types.
    std::vector<AstType*> programTypes;
    for (auto* type : program.getTypes()) {
        if (env.isType(type->getQualifiedName()) || isCyclic(type->getQualifiedName())) {
            continue;
        }
        programTypes.push_back(type);
        nameToAstType.insert({type->getQualifiedName(), type});
    }

    for (const auto* astType : programTypes) {
        createType(astType->getQualifiedName(), typeDependencyGraph, nameToAstType);
    }
}

const Type* TypeEnvironmentAnalysis::createType(const AstQualifiedName& typeName,
        const Graph<AstQualifiedName>& typeDependencyGraph,
        const std::map<AstQualifiedName, const AstType*>& nameToAstType) {
    // base case
    if (env.isType(typeName)) {
        return &env.getType(typeName);
    }

    auto iterToType = nameToAstType.find(typeName);
    if (iterToType == nameToAstType.end()) {
        return nullptr;
    }
    const AstType& astType = *iterToType->second;

    if (isA<AstSubsetType>(astType)) {
        auto* baseType =
                createType(as<AstSubsetType>(astType)->getBaseType(), typeDependencyGraph, nameToAstType);

        if (baseType == nullptr) {
            return nullptr;
        }

        // Subset of a record is a special case.
        if (isA<RecordType>(baseType)) {
            return &env.createType<SubsetRecordType>(typeName, *as<RecordType>(baseType));
        }

        return &env.createType<SubsetType>(typeName, *baseType);

    } else if (isA<AstUnionType>(astType)) {
        std::vector<const Type*> elements;
        for (const auto& element : as<AstUnionType>(astType)->getTypes()) {
            auto* elementType = createType(element, typeDependencyGraph, nameToAstType);
            if (elementType == nullptr) {
                return nullptr;
            }
            elements.push_back(elementType);
        }
        return &env.createType<UnionType>(typeName, std::move(elements));

    } else if (isA<AstRecordType>(astType)) {
        // Record type must be created upfront as it may be its-own member.
        auto& recordType = env.createType<RecordType>(typeName);
        std::vector<const Type*> elements;
        for (const auto* field : as<AstRecordType>(astType)->getFields()) {
            auto* elementType = createType(field->getTypeName(), typeDependencyGraph, nameToAstType);
            if (elementType == nullptr) {
                return nullptr;
            }
            elements.push_back(elementType);
        }
        recordType.setFields(std::move(elements));
        return &recordType;
    } else {
        fatal("unsupported type construct: %s", typeid(astType).name());
    }
}

void TypeEnvironmentAnalysis::analyseCyclicTypes(
        const Graph<AstQualifiedName>& dependencyGraph, const std::vector<AstType*>& programTypes) {
    for (const auto& astType : programTypes) {
        AstQualifiedName typeName = astType->getQualifiedName();

        if (dependencyGraph.reaches(typeName, typeName)) {
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
