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
    updateTypeEnvironment(*translationUnit.getProgram());
}

void TypeEnvironmentAnalysis::print(std::ostream& os) const {
    env.print(os);
}

/**
 * A utility function utilized by the finishParsing member function to update a type environment
 * out of a given list of types in the AST
 *
 * @param types the types specified in the input file, contained in the AST
 * @param env the type environment to be updated
 */
void TypeEnvironmentAnalysis::updateTypeEnvironment(const AstProgram& program) {
    // build up new type system based on defined types

    // create all type symbols in a first step
    for (const auto& cur : program.getTypes()) {
        // support faulty codes with multiple definitions
        if (env.isType(cur->getQualifiedName())) {
            continue;
        }

        // create type within type environment
        if (auto* t = dynamic_cast<const AstSubsetType*>(cur)) {
            if (t->getTypeAttribute() == TypeAttribute::Signed) {
                env.createNumericType(cur->getQualifiedName());
            } else {
                env.createSymbolType(cur->getQualifiedName());
            }
        } else if (dynamic_cast<const AstUnionType*>(cur) != nullptr) {
            // initialize the union
            env.createUnionType(cur->getQualifiedName());
        } else if (dynamic_cast<const AstRecordType*>(cur) != nullptr) {
            // initialize the record
            env.createRecordType(cur->getQualifiedName());
        } else {
            std::cout << "Unsupported type construct: " << typeid(cur).name() << "\n";
            assert(false && "Unsupported Type Construct!");
        }
    }

    // link symbols in a second step
    for (const auto& cur : program.getTypes()) {
        Type* type = env.getModifiableType(cur->getQualifiedName());
        assert(type && "It should be there!");

        if (dynamic_cast<const AstSubsetType*>(cur) != nullptr) {
            // nothing to do here
        } else if (auto* t = dynamic_cast<const AstUnionType*>(cur)) {
            // get type as union type
            auto* ut = dynamic_cast<UnionType*>(type);
            if (ut == nullptr) {
                continue;  // support faulty input
            }

            // add element types
            for (const auto& cur : t->getTypes()) {
                if (env.isType(cur)) {
                    ut->add(env.getType(cur));
                }
            }
        } else if (auto* t = dynamic_cast<const AstRecordType*>(cur)) {
            // get type as record type
            auto* rt = dynamic_cast<RecordType*>(type);
            if (rt == nullptr) {
                continue;  // support faulty input
            }

            // add fields
            for (const auto& f : t->getFields()) {
                if (env.isType(f.type)) {
                    rt->add(f.name, env.getType(f.type));
                }
            }
        } else {
            std::cout << "Unsupported type construct: " << typeid(cur).name() << "\n";
            assert(false && "Unsupported Type Construct!");
        }
    }

    // partition unions into numeric or symbolic types
    Graph<AstQualifiedName> typeDependencyGraph;
    for (const auto& cur : program.getTypes()) {
        if (auto type = dynamic_cast<const AstSubsetType*>(cur)) {
            if (type->getTypeAttribute() == TypeAttribute::Signed) {
                typeDependencyGraph.insert(type->getQualifiedName(), "number");
            } else {
                typeDependencyGraph.insert(type->getQualifiedName(), "symbol");
            }
        } else if (dynamic_cast<const AstRecordType*>(cur) != nullptr) {
            // do nothing
        } else if (auto type = dynamic_cast<const AstUnionType*>(cur)) {
            for (const auto& subtype : type->getTypes()) {
                typeDependencyGraph.insert(type->getQualifiedName(), subtype);
            }
        } else {
            std::cout << "Unsupported type construct: " << typeid(cur).name() << "\n";
            assert(false && "Unsupported Type Construct!");
        }
    }
    for (const auto& cur : program.getTypes()) {
        if (auto unionType = dynamic_cast<const AstUnionType*>(cur)) {
            AstQualifiedName unionName = unionType->getQualifiedName();
            if (typeDependencyGraph.reaches(unionName, "number")) {
                numericUnions.insert(unionName);
            }
            if (typeDependencyGraph.reaches(unionName, "symbol")) {
                symbolicUnions.insert(unionName);
            }
        }
    }
}

}  // end of namespace souffle
