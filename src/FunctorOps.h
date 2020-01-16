/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file FunctorOps.h
 *
 * Defines intrinsic functor operators for AST and RAM
 *
 ***********************************************************************/

#pragma once

#include <cassert>
#include <iostream>

namespace souffle {

enum class FunctorOp {
    /** Unary Functor Operators */
    ORD,       // ordinal number of a string
    STRLEN,    // length of a string
    NEG,       // Signed numeric negation
    FNEG,      // Float numeric negation
    BNOT,      // Signed bitwise negation
    UBNOT,     // Unsigned bitwise negation
    LNOT,      // Signed logical negation
    ULNOT,     // Unsigned logical negation
    TONUMBER,  // convert string to number
    TOSTRING,  // convert number to string
    ITOU,      // convert signed number to unsigned
    UTOI,      // convert unsigned number to signed
    ITOF,      // convert signed number to float
    FTOI,      // convert float number to signed number.
    UTOF,      // convert unsigned number to a float.
    FTOU,      // convert float to unsigned number.

    /** Binary Functor Operators */
    ADD,    // addition
    SUB,    // subtraction
    MUL,    // multiplication
    DIV,    // division
    EXP,    // exponent
    MAX,    // max of two numbers
    MIN,    // min of two numbers
    MOD,    // modulus
    BAND,   // bitwise and
    BOR,    // bitwise or
    BXOR,   // bitwise exclusive or
    LAND,   // logical and
    LOR,    // logical or
    UADD,   // addition
    USUB,   // subtraction
    UMUL,   // multiplication
    UDIV,   // division
    UEXP,   // exponent
    UMAX,   // max of two numbers
    UMIN,   // min of two numbers
    UMOD,   // modulus
    UBAND,  // bitwise and
    UBOR,   // bitwise or
    UBXOR,  // bitwise exclusive or
    ULAND,  // logical and
    ULOR,   // logical or
    FADD,   // addition
    FSUB,   // subtraction
    FMUL,   // multiplication
    FDIV,   // division
    FEXP,   // exponent
    FMAX,   // max of two floats
    FMIN,   // min of two floats

    CAT,  // string concatenation

    /** Ternary Functor Operators */
    SUBSTR,  // substring

    /** Undefined */
    __UNDEFINED__,  // undefined operator
};

/**
 * Checks whether a functor operation can have a given argument count.
 */
inline bool isValidFunctorOpArity(FunctorOp op, size_t arity) {
    switch (op) {
        /** Unary Functor Operators */
        case FunctorOp::ORD:
        case FunctorOp::STRLEN:
        case FunctorOp::NEG:
        case FunctorOp::FNEG:
        case FunctorOp::BNOT:
        case FunctorOp::UBNOT:
        case FunctorOp::LNOT:
        case FunctorOp::TONUMBER:
        case FunctorOp::TOSTRING:
        case FunctorOp::ITOU:
        case FunctorOp::UTOI:
        case FunctorOp::ITOF:
        case FunctorOp::FTOI:
        case FunctorOp::UTOF:
        case FunctorOp::FTOU:
            return arity == 1;

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
        case FunctorOp::UADD:
        case FunctorOp::USUB:
        case FunctorOp::UMUL:
        case FunctorOp::UDIV:
        case FunctorOp::UEXP:
        case FunctorOp::UMOD:
        case FunctorOp::UBAND:
        case FunctorOp::UBOR:
        case FunctorOp::UBXOR:
        case FunctorOp::ULAND:
        case FunctorOp::ULOR:
        case FunctorOp::FADD:
        case FunctorOp::FSUB:
        case FunctorOp::FMUL:
        case FunctorOp::FDIV:
        case FunctorOp::FEXP:
            return arity == 2;

        /** Ternary Functor Operators */
        case FunctorOp::SUBSTR:
            return arity == 3;

        /** Non-fixed */
        case FunctorOp::MAX:
        case FunctorOp::MIN:
        case FunctorOp::UMAX:
        case FunctorOp::UMIN:
        case FunctorOp::FMAX:
        case FunctorOp::FMIN:
        case FunctorOp::CAT:
            return arity >= 2;

        /** Undefined */
        default:
            break;
    }

    assert(false && "unsupported operator");
    return false;
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
    return !isNumericFunctorOp(op);
}

/**
 * Determines whether an argument has a number value
 */
inline bool functorOpAcceptsNumbers(size_t arg, const FunctorOp op) {
    switch (op) {
        /** Unary Functor Operators */
        case FunctorOp::NEG:
        case FunctorOp::BNOT:
        case FunctorOp::LNOT:
        case FunctorOp::TOSTRING:
            assert(arg < 1 && "unary functor argument out of bounds");
            return true;
        case FunctorOp::ORD:
        case FunctorOp::STRLEN:
        case FunctorOp::TONUMBER:
            assert(arg < 1 && "unary functor argument out of bounds");
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
            assert(arg < 2 && "binary functor argument out of bounds");
            return true;

        /** Ternary Functor Operators */
        case FunctorOp::SUBSTR:
            assert(arg < 3 && "ternary functor argument out of bounds");
            return arg == 1 || arg == 2;

        /** Non-fixed Functor Operators */
        case FunctorOp::MAX:
        case FunctorOp::MIN:
            return true;
        case FunctorOp::CAT:
            return false;

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
