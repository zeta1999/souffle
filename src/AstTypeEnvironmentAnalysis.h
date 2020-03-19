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
#include "RamTypes.h"
#include "TypeSystem.h"
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

    const std::set<TypeAttribute>& getUnionType(const AstQualifiedName& identifier) const {
        return unionTypes.at(identifier);
    }

private:
    TypeEnvironment env;
    std::map<AstQualifiedName, std::set<TypeAttribute>> unionTypes;

    void updateTypeEnvironment(const AstProgram& program);
};

}  // end of namespace souffle
