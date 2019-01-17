/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file UnaryOperator.h
 *
 * Defines unary operators and relational operators.
 *
 ***********************************************************************/

#pragma once

#include <cassert>
#include <iostream>
#include <string>

namespace souffle {

/**
 * Unary Operators for functors and constraints
 */
enum class UnaryOp {
    __UNDEFINED__,
    ORD,       // ordinal number of a string
    STRLEN,    // length of a string
    NEG,       // numeric negation
    BNOT,      // bitwise negation
    LNOT,      // logical negation
    TONUMBER,  // convert string to number
    TOSTRING   // convert number to string
};

/**
 * Returns the corresponding symbol for the given relational operator.
 */
inline std::string getSymbolForUnaryOp(UnaryOp op) {
    switch (op) {
        case UnaryOp::ORD:
            return "ord";
        case UnaryOp::STRLEN:
            return "strlen";
        case UnaryOp::NEG:
            return "-";
        case UnaryOp::BNOT:
            return "bnot";
        case UnaryOp::LNOT:
            return "lnot";
        case UnaryOp::TONUMBER:
            return "to_number";
        case UnaryOp::TOSTRING:
            return "to_string";
        default:
            break;
    }
    assert(false && "Unsupported Operator!");
    return "?";
}

/**
 * Returns the corresponding operator for the given symbol.
 */
inline UnaryOp getUnaryOpForSymbol(const std::string& symbol) {
    if (symbol == "ord") return UnaryOp::ORD;
    if (symbol == "strlen") return UnaryOp::STRLEN;
    if (symbol == "-") return UnaryOp::NEG;
    if (symbol == "bnot") return UnaryOp::BNOT;
    if (symbol == "lnot") return UnaryOp::LNOT;
    if (symbol == "to_number") return UnaryOp::TONUMBER;
    if (symbol == "to_string") return UnaryOp::TOSTRING;
    std::cout << "Unrecognised operator: " << symbol << "\n";
    assert(false && "Unsupported Operator!");
    return UnaryOp::__UNDEFINED__;
}

}  // end of namespace souffle
