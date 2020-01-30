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

#include "RamPrimitiveTypes.h"

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
inline bool isValidFunctorOpArity(const FunctorOp op, const size_t arity) {
    switch (op) {
        /** Unary Functor Operators */
        case FunctorOp::ORD:
        case FunctorOp::STRLEN:
        case FunctorOp::NEG:
        case FunctorOp::FNEG:
        case FunctorOp::BNOT:
        case FunctorOp::UBNOT:
        case FunctorOp::LNOT:
        case FunctorOp::ULNOT:
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
        case FunctorOp::__UNDEFINED__:
            break;
    }

    assert(false && "unsupported operator");
    return false;
}

inline std::string getSymbolForFunctorOp(const FunctorOp op) {
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
        case FunctorOp::FADD:
        case FunctorOp::UADD:
            return "+";
        case FunctorOp::SUB:
        case FunctorOp::USUB:
        case FunctorOp::FSUB:
            return "-";
        case FunctorOp::MUL:
        case FunctorOp::UMUL:
        case FunctorOp::FMUL:
            return "*";
        case FunctorOp::DIV:
        case FunctorOp::UDIV:
        case FunctorOp::FDIV:
            return "/";
        case FunctorOp::EXP:
        case FunctorOp::FEXP:
        case FunctorOp::UEXP:
            return "^";
        case FunctorOp::MOD:
        case FunctorOp::UMOD:
            return "%";
        case FunctorOp::BAND:
        case FunctorOp::UBAND:
            return "band";
        case FunctorOp::BOR:
        case FunctorOp::UBOR:
            return "bor";
        case FunctorOp::BXOR:
        case FunctorOp::UBXOR:
            return "bxor";
        case FunctorOp::LAND:
        case FunctorOp::ULAND:
            return "land";
        case FunctorOp::LOR:
        case FunctorOp::ULOR:
            return "lor";
        case FunctorOp::MAX:
        case FunctorOp::UMAX:
        case FunctorOp::FMAX:
            return "max";
        case FunctorOp::MIN:
        case FunctorOp::UMIN:
        case FunctorOp::FMIN:
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

inline RamPrimitiveType functorReturnType(const FunctorOp op) {
    switch (op) {
        case FunctorOp::ORD:
        case FunctorOp::STRLEN:
        case FunctorOp::NEG:
        case FunctorOp::BNOT:
        case FunctorOp::LNOT:
        case FunctorOp::TONUMBER:
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
        case FunctorOp::FTOI:
        case FunctorOp::UTOI:
            return RamPrimitiveType::Signed;
        case FunctorOp::UBNOT:
        case FunctorOp::ITOU:
        case FunctorOp::FTOU:
        case FunctorOp::ULNOT:
        case FunctorOp::UADD:
        case FunctorOp::USUB:
        case FunctorOp::UMUL:
        case FunctorOp::UDIV:
        case FunctorOp::UEXP:
        case FunctorOp::UMAX:
        case FunctorOp::UMIN:
        case FunctorOp::UMOD:
        case FunctorOp::UBAND:
        case FunctorOp::UBOR:
        case FunctorOp::UBXOR:
        case FunctorOp::ULAND:
        case FunctorOp::ULOR:
            return RamPrimitiveType::Unsigned;
        case FunctorOp::FMAX:
        case FunctorOp::FMIN:
        case FunctorOp::FNEG:
        case FunctorOp::FADD:
        case FunctorOp::FSUB:
        case FunctorOp::ITOF:
        case FunctorOp::UTOF:
        case FunctorOp::FMUL:
        case FunctorOp::FDIV:
        case FunctorOp::FEXP:
            return RamPrimitiveType::Float;
        case FunctorOp::TOSTRING:
        case FunctorOp::CAT:
        case FunctorOp::SUBSTR:
            return RamPrimitiveType::String;
        case FunctorOp::__UNDEFINED__:
            break;
    }
    assert(false && "Bad functor return type query");
    exit(EXIT_FAILURE);
    return RamPrimitiveType::Record;  // Silence warning.
}

inline RamPrimitiveType functorOpArgType(const size_t arg, const FunctorOp op) {
    switch (op) {
        case FunctorOp::ITOF:
        case FunctorOp::ITOU:
        case FunctorOp::NEG:
        case FunctorOp::BNOT:
        case FunctorOp::LNOT:
        case FunctorOp::TOSTRING:
            assert(arg == 0 && "unary functor out of bound");
            return RamPrimitiveType::Signed;
        case FunctorOp::FNEG:
        case FunctorOp::FTOI:
        case FunctorOp::FTOU:
            assert(arg == 0 && "unary functor out of bound");
            return RamPrimitiveType::Float;
        case FunctorOp::ORD:
        case FunctorOp::STRLEN:
        case FunctorOp::TONUMBER:
            assert(arg == 0 && "unary functor out of bound");
            return RamPrimitiveType::String;
        case FunctorOp::UBNOT:
        case FunctorOp::ULNOT:
        case FunctorOp::UTOI:
        case FunctorOp::UTOF:
            assert(arg == 0 && "unary functor out of bound");
            return RamPrimitiveType::Unsigned;
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
            assert(arg < 2 && "binary functor out of bound");
            return RamPrimitiveType::Signed;
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
            assert(arg < 2 && "binary functor out of bound");
            return RamPrimitiveType::Unsigned;
        case FunctorOp::FADD:
        case FunctorOp::FSUB:
        case FunctorOp::FMUL:
        case FunctorOp::FDIV:
        case FunctorOp::FEXP:
            assert(arg < 2 && "binary functor out of bound");
            return RamPrimitiveType::Float;
        case FunctorOp::SUBSTR:
            assert(arg < 3 && "Ternary Functor Operators");
            if (arg == 0) {
                return RamPrimitiveType::String;
            } else {
                return RamPrimitiveType::Signed;  // In the future: Change to unsigned
            }
        case FunctorOp::MAX:
        case FunctorOp::MIN:
            return RamPrimitiveType::Signed;
        case FunctorOp::UMAX:
        case FunctorOp::UMIN:
            return RamPrimitiveType::Unsigned;
        case FunctorOp::FMAX:
        case FunctorOp::FMIN:
            return RamPrimitiveType::Float;
        case FunctorOp::CAT:
            return RamPrimitiveType::String;

        case FunctorOp::__UNDEFINED__:
            break;
    }
    assert(false && "unsupported operator");
    exit(EXIT_FAILURE);
    return RamPrimitiveType::Record;  // silence warning.
}

inline bool isOverloadedFunctor(const FunctorOp op) {
    switch (op) {
        /* Unary */
        case FunctorOp::NEG:
        case FunctorOp::BNOT:
        case FunctorOp::LNOT:
        /* Binary */
        case FunctorOp::ADD:
        case FunctorOp::SUB:
        case FunctorOp::MUL:
        case FunctorOp::DIV:
        case FunctorOp::EXP:
        case FunctorOp::BAND:
        case FunctorOp::BOR:
        case FunctorOp::LAND:
        case FunctorOp::LOR:
        case FunctorOp::MOD:
            return true;
        default:
            break;
    }

    return false;
}

/**
 * Determines whether a functor should be written using infix notation (e.g. `a + b + c`)
 * or prefix notation (e.g. `+(a,b,c)`)
 */
inline bool isInfixFunctorOp(const FunctorOp op) {
    switch (op) {
        case FunctorOp::ADD:
        case FunctorOp::FADD:
        case FunctorOp::UADD:
        case FunctorOp::SUB:
        case FunctorOp::USUB:
        case FunctorOp::FSUB:
        case FunctorOp::MUL:
        case FunctorOp::FMUL:
        case FunctorOp::UMUL:
        case FunctorOp::DIV:
        case FunctorOp::FDIV:
        case FunctorOp::UDIV:
        case FunctorOp::EXP:
        case FunctorOp::FEXP:
        case FunctorOp::UEXP:
        case FunctorOp::BAND:
        case FunctorOp::UBAND:
        case FunctorOp::BOR:
        case FunctorOp::UBOR:
        case FunctorOp::BXOR:
        case FunctorOp::UBXOR:
        case FunctorOp::LAND:
        case FunctorOp::ULAND:
        case FunctorOp::LOR:
        case FunctorOp::ULOR:
        case FunctorOp::MOD:
        case FunctorOp::UMOD:
            return true;
        default:
            return false;
    }
}

}  // end of namespace souffle
