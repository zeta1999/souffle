/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018 The Souffle Developers. All Rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstFunctorDeclaration.h
 *
 * Defines external functors.
 *
 ***********************************************************************/

#pragma once

#include "AstNode.h"
#include "RamTypes.h"
#include "Util.h"

#include <algorithm>
#include <string>
#include <vector>

namespace souffle {

/**
 * AstFunctorDeclaration
 */

class AstFunctorDeclaration : public AstNode {
public:
    AstFunctorDeclaration(const std::string& name, const std::vector<RamTypeAttribute>& argsTypes,
            RamTypeAttribute returnType)
            : name(name), argsTypes(argsTypes), returnType(returnType) {
        assert(name.length() > 0 && "functor name is empty");
    }

    void print(std::ostream& out) const override {
        auto convert = [&](RamTypeAttribute type) {
            switch (type) {
                case RamTypeAttribute::Signed:
                    return "number";
                case RamTypeAttribute::Symbol:
                    return "symbol";
                case RamTypeAttribute::Float:
                    return "float";
                case RamTypeAttribute::Unsigned:
                    return "unsigned";
                default:
                    abort();
            }
        };
        out << ".declfun " << name << "(";
        std::vector<std::string> args;
        for (auto argType : argsTypes) {
            args.push_back(convert(argType));
        }
        out << join(args, ",");
        out << "):" << convert(returnType) << std::endl;
    }

    /** get name */
    const std::string& getName() const {
        return name;
    }

    /** get type */
    const std::vector<RamTypeAttribute>& getArgsTypes() const {
        return argsTypes;
    }

    RamTypeAttribute getReturnType() const {
        return returnType;
    }

    /** get number of arguments */
    size_t getArity() const {
        return argsTypes.size();
    }

    // /** is return type a symbolic value */
    // bool isSymbolic() const {
    //     return (type[type.length() - 1] == 'S');
    // }

    // /** is return type a number value */
    // bool isNumerical() const {
    //     return (type[type.length() - 1] == 'N');
    // }

    // /** accepts the i-th argument as a symbolic value */
    // bool acceptsSymbols(size_t idx) const {
    //     assert(idx <= getArity() && "argument index out of bound");
    //     return (type[idx] == 'S');
    // }

    // /** accepts the i-th argument as a number value */
    // bool acceptsNumbers(size_t idx) const {
    //     assert(idx <= getArity() && "argument index out of bound");
    //     return (type[idx] == 'N');
    // }

    /** clone */
    AstFunctorDeclaration* clone() const override {
        auto* res = new AstFunctorDeclaration(name, argsTypes, returnType);
        res->setSrcLoc(getSrcLoc());
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstFunctorDeclaration*>(&node));
        const auto& other = static_cast<const AstFunctorDeclaration&>(node);
        return name == other.name && argsTypes == other.argsTypes && returnType == other.returnType;
    }

    /** name of functor */
    const std::string name;

    /** Types of arguments */
    const std::vector<RamTypeAttribute> argsTypes;

    /** Type of the return value */
    const RamTypeAttribute returnType;
};

}  // end of namespace souffle
