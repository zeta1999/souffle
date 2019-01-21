/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file BinaryFunctorOps.h
 *
 * Defines binary functor operators for AST and RAM
 *
 ***********************************************************************/

#pragma once

#include <cassert>
#include <iostream>

namespace souffle {

enum class FunctorOp {
    // TODO: is this needed?
    __UNDEFINED__,  // undefined operator

    /** Unary Functor Operators */
    ORD,       // ordinal number of a string
    STRLEN,    // length of a string
    NEG,       // numeric negation
    BNOT,      // bitwise negation
    LNOT,      // logical negation
    TONUMBER,  // convert string to number
    TOSTRING,  // convert number to string

    /** Binary Functor Operators */
    ADD,   // addition
    SUB,   // subtraction
    MUL,   // multiplication
    DIV,   // division
    EXP,   // exponent
    MOD,   // modulus
    BAND,  // bitwise and
    BOR,   // bitwise or
    BXOR,  // bitwise exclusive or
    LAND,  // logical and
    LOR,   // logical or
    MAX,   // max of two numbers
    MIN,   // min of two numbers
    CAT,   // string concatenation

    /** Ternary Functor Operators */
    SUBSTR,  // addition
};

/**
 * Gets expected arity of functor
 */
inline size_t getFunctorOpArity(FunctorOp op) {
    switch (op) {
        /** Unary Functor Operators */
        case FunctorOp::ORD:
        case FunctorOp::STRLEN:
        case FunctorOp::NEG:
        case FunctorOp::BNOT:
        case FunctorOp::LNOT:
        case FunctorOp::TONUMBER:
        case FunctorOp::TOSTRING:
            return 1U;

        /** Binary Functor Operators */
        case FunctorOp::ADD:
        case FunctorOp::SUB:
        case FunctorOp::MUL:
        case FunctorOp::DIV:
        case FunctorOp::EXP:
        case FunctorOp::MOD:
        case FunctorOp::BAND:
        case FunctorOp::BOR:
        case FunctorOp::BXOR:
        case FunctorOp::LAND:
        case FunctorOp::LOR:
        case FunctorOp::MAX:
        case FunctorOp::MIN:
        case FunctorOp::CAT:
            return 2U;

        /** Ternary Functor Operators */
        case FunctorOp::SUBSTR:
            return 3U;

        /** Undefined */
        default:
            break;
    }

    assert(false && "unsupported operator");
    return 0U;
}

inline std::string getSymbolForFunctorOp(FunctorOp op) {
    switch (op) {
        /** Unary Functor Operators */
        case FunctorOp::ORD:
            return "ord";
        case FunctorOp::STRLEN:
            return "strlen";
        case FunctorOp::NEG:
            return "-";
        case FunctorOp::BNOT:
            return "bnot";
        case FunctorOp::LNOT:
            return "lnot";
        case FunctorOp::TONUMBER:
            return "to_number";
        case FunctorOp::TOSTRING:
            return "to_string";

        /** Binary Functor Operators */
        case FunctorOp::ADD:
            return "+";
        case FunctorOp::SUB:
            return "-";
        case FunctorOp::MUL:
            return "*";
        case FunctorOp::DIV:
            return "/";
        case FunctorOp::EXP:
            return "^";
        case FunctorOp::MOD:
            return "%";
        case FunctorOp::BAND:
            return "band";
        case FunctorOp::BOR:
            return "bor";
        case FunctorOp::BXOR:
            return "bxor";
        case FunctorOp::LAND:
            return "land";
        case FunctorOp::LOR:
            return "lor";
        case FunctorOp::MAX:
            return "max";
        case FunctorOp::MIN:
            return "min";
        case FunctorOp::CAT:
            return "cat";

        /** Ternary Functor Operators */
        case FunctorOp::SUBSTR:
            return "substr";

        /** Undefined */
        default:
            break;
    }

    assert(false && "unsupported operator");
    return "?";
}

// TODO: NOT ONE TO ONE!!! SO get rid of this maybe? or somehow get around this?
/**
 * Converts symbolic representation of an operator to the operator
 */
inline FunctorOp getFunctorOpForSymbol(const std::string& symbol) {
    /** Unary Functor Operators */
    if (symbol == "ord") return FunctorOp::ORD;
    if (symbol == "strlen") return FunctorOp::STRLEN;
    if (symbol == "-") return FunctorOp::NEG;
    if (symbol == "bnot") return FunctorOp::BNOT;
    if (symbol == "lnot") return FunctorOp::LNOT;
    if (symbol == "to_number") return FunctorOp::TONUMBER;
    if (symbol == "to_string") return FunctorOp::TOSTRING;

    /** Binary Functor Operators */
    if (symbol == "+") return FunctorOp::ADD;
    if (symbol == "-") return FunctorOp::SUB;
    if (symbol == "*") return FunctorOp::MUL;
    if (symbol == "/") return FunctorOp::DIV;
    if (symbol == "^") return FunctorOp::EXP;
    if (symbol == "%") return FunctorOp::MOD;
    if (symbol == "band") return FunctorOp::BAND;
    if (symbol == "bor") return FunctorOp::BOR;
    if (symbol == "bxor") return FunctorOp::BXOR;
    if (symbol == "land") return FunctorOp::LAND;
    if (symbol == "lor") return FunctorOp::LOR;
    if (symbol == "max") return FunctorOp::MAX;
    if (symbol == "min") return FunctorOp::MIN;
    if (symbol == "cat") return FunctorOp::CAT;

    /** Ternary Functor Operators */
    if (symbol == "substr") return FunctorOp::SUBSTR;

    /** Undefined */
    assert(false && "unrecognised operator");
    return FunctorOp::__UNDEFINED__;
}

// TODO: change all the asserts to have consistent errors

/**
 * Determines whether the given operator has a numeric return value
 */
inline bool isNumericFunctorOp(const FunctorOp op) {
    switch (op) {
        /** Unary Functor Operators */
        case FunctorOp::ORD:
        case FunctorOp::STRLEN:
        case FunctorOp::NEG:
        case FunctorOp::BNOT:
        case FunctorOp::LNOT:
        case FunctorOp::TONUMBER:
            return true;
        case FunctorOp::TOSTRING:
            return false;

        /** Binary Functor Operators */
        case FunctorOp::ADD:
        case FunctorOp::SUB:
        case FunctorOp::MUL:
        case FunctorOp::DIV:
        case FunctorOp::EXP:
        case FunctorOp::BAND:
        case FunctorOp::BOR:
        case FunctorOp::BXOR:
        case FunctorOp::LAND:
        case FunctorOp::LOR:
        case FunctorOp::MOD:
        case FunctorOp::MAX:
        case FunctorOp::MIN:
            return true;
        case FunctorOp::CAT:
            return false;

        /** Ternary Functor Operators */
        case FunctorOp::SUBSTR:
            return false;

        /** Undefined */
        default:
            break;
    }

    assert(false && "unsupported operator");
    return false;
}

/*
 * Determines whether the operator has a symbolic return value.
 */
inline bool isSymbolicFunctorOp(const FunctorOp op) {
    // TODO: maybe write it out explicitly in case more types are added later on
    return !isNumericFunctorOp(op);
}

/**
 * Determines whether an argument has a number value
 */
inline bool functorOpAcceptsNumbers(size_t arg, const FunctorOp op) {
    size_t expectedArity = getFunctorOpArity(op);
    assert(arg >= 0 && arg < expectedArity && "argument out of range");

    switch (op) {
        /** Unary Functor Operators */
        case FunctorOp::NEG:
        case FunctorOp::BNOT:
        case FunctorOp::LNOT:
        case FunctorOp::TOSTRING:
            return true;
        case FunctorOp::ORD:
        case FunctorOp::STRLEN:
        case FunctorOp::TONUMBER:
            return false;

        /** Binary Functor Operators */
        case FunctorOp::ADD:
        case FunctorOp::SUB:
        case FunctorOp::MUL:
        case FunctorOp::DIV:
        case FunctorOp::EXP:
        case FunctorOp::BAND:
        case FunctorOp::BOR:
        case FunctorOp::BXOR:
        case FunctorOp::LAND:
        case FunctorOp::LOR:
        case FunctorOp::MOD:
        case FunctorOp::MAX:
        case FunctorOp::MIN:
            return true;
        case FunctorOp::CAT:
            return false;

        /** Ternary Functor Operators */
        case FunctorOp::SUBSTR:
            return arg == 1 || arg == 2;

        /** Undefined */
        default:
            break;
    }

    assert(false && "unsupported operator");
    return false;
}

/**
 * Determines whether an argument has a symbolic value
 */
inline bool functorOpAcceptsSymbols(size_t arg, const FunctorOp op) {
    return !functorOpAcceptsNumbers(arg, op);
}

/**
 * Determines whether a functor should be written using infix notation (e.g. `a + b + c`)
 * or prefix notation (e.g. `+(a,b,c)`)
 */
inline bool isInfixFunctorOp(const FunctorOp op) {
    switch (op) {
        case FunctorOp::ADD:
        case FunctorOp::SUB:
        case FunctorOp::MUL:
        case FunctorOp::DIV:
        case FunctorOp::EXP:
        case FunctorOp::BAND:
        case FunctorOp::BOR:
        case FunctorOp::BXOR:
        case FunctorOp::LAND:
        case FunctorOp::LOR:
        case FunctorOp::MOD:
            return true;
        default:
            return false;
    }
}

}  // end of namespace souffle
