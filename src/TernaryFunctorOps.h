/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TernaryFunctorOps.h
 *
 * Defines ternary functor operators for AST and RAM
 *
 ***********************************************************************/

#pragma once

#include <cassert>
#include <iostream>

namespace souffle {

/**
 * Ternary Functor Operators
 */
enum class TernaryOp {
    __UNDEFINED__,  // undefined operator
    SUBSTR,         // addition
};

/**
 * Converts operator to its symbolic representation
 */
inline std::string getSymbolForTernaryOp(TernaryOp op) {
    switch (op) {
        case TernaryOp::SUBSTR:
            return "substr";
        default:
            break;
    }
    assert(false && "Unsupported Operator!");
    return "?";
}

/**
 * Converts symbolic representation of an operator to the operator
 */
inline TernaryOp getTernaryOpForSymbol(const std::string& symbol) {
    if (symbol == "substr") return TernaryOp::SUBSTR;
    std::cout << "Unrecognised operator: " << symbol << "\n";
    assert(false && "Unsupported Operator!");
    return TernaryOp::__UNDEFINED__;
}

}  // end of namespace souffle
