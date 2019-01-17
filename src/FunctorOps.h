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

// TODO: CONVERT THESE TO PROPER FUNCTIONS FOR THIS NEW ENUM
// TODO: MAKE SURE YOU CATCH everything from each type (should be consistent)
// TODO: add a function to check arity
//
///**
// * Converts symbolic representation of an operator to the operator
// */
// inline BinaryOp getBinaryOpForSymbol(const std::string& symbol) {
//    if (symbol == "+") return BinaryOp::ADD;
//    if (symbol == "-") return BinaryOp::SUB;
//    if (symbol == "*") return BinaryOp::MUL;
//    if (symbol == "/") return BinaryOp::DIV;
//    if (symbol == "^") return BinaryOp::EXP;
//    if (symbol == "%") return BinaryOp::MOD;
//    if (symbol == "band") return BinaryOp::BAND;
//    if (symbol == "bor") return BinaryOp::BOR;
//    if (symbol == "bxor") return BinaryOp::BXOR;
//    if (symbol == "land") return BinaryOp::LAND;
//    if (symbol == "lor") return BinaryOp::LOR;
//    if (symbol == "max") return BinaryOp::MAX;
//    if (symbol == "min") return BinaryOp::MIN;
//    if (symbol == "cat") return BinaryOp::CAT;
//    std::cout << "Unrecognised operator: " << symbol << "\n";
//    assert(false && "Unsupported Operator!");
//    return BinaryOp::__UNDEFINED__;
//}
//
///**
// * Determines whether the given operator has a numeric return value.
// */
// inline bool isNumericBinaryOp(const BinaryOp op) {
//    switch (op) {
//        case BinaryOp::ADD:
//        case BinaryOp::SUB:
//        case BinaryOp::MUL:
//        case BinaryOp::DIV:
//        case BinaryOp::EXP:
//        case BinaryOp::BAND:
//        case BinaryOp::BOR:
//        case BinaryOp::BXOR:
//        case BinaryOp::LAND:
//        case BinaryOp::LOR:
//        case BinaryOp::MOD:
//        case BinaryOp::MAX:
//        case BinaryOp::MIN:
//            return true;
//        case BinaryOp::CAT:
//            return false;
//        default:
//            break;
//    }
//    assert(false && "Uncovered case!");
//    return false;
//}
//
///**
// * Determines whether the operator has a symbolic return value.
// */
// inline bool isSymbolicBinaryOp(const BinaryOp op) {
//    return !isNumericBinaryOp(op);
//}
//
///**
// * Determines whether an argument has a number value.
// */
// inline bool binaryOpAcceptsNumbers(int arg, const BinaryOp op) {
//    assert(arg >= 0 && arg < 2 && "argument out of range");
//    switch (op) {
//        case BinaryOp::ADD:
//        case BinaryOp::SUB:
//        case BinaryOp::MUL:
//        case BinaryOp::DIV:
//        case BinaryOp::EXP:
//        case BinaryOp::BAND:
//        case BinaryOp::BOR:
//        case BinaryOp::BXOR:
//        case BinaryOp::LAND:
//        case BinaryOp::LOR:
//        case BinaryOp::MOD:
//        case BinaryOp::MAX:
//        case BinaryOp::MIN:
//            return true;
//        case BinaryOp::CAT:
//            return false;
//        default:
//            break;
//    }
//    assert(false && "Uncovered case!");
//    return false;
//}
//
///**
// * Determines whether an argument has a symbolic value
// */
// inline bool binaryOpAcceptsSymbols(int arg, const BinaryOp op) {
//    return !binaryOpAcceptsNumbers(arg, op);
//}
//
// UNARYYYYY::::
//
// /**
//  * Returns the corresponding operator for the given symbol.
//  */
// inline UnaryOp getUnaryOpForSymbol(const std::string& symbol) {
//     if (symbol == "ord") return UnaryOp::ORD;
//     if (symbol == "strlen") return UnaryOp::STRLEN;
//     if (symbol == "-") return UnaryOp::NEG;
//     if (symbol == "bnot") return UnaryOp::BNOT;
//     if (symbol == "lnot") return UnaryOp::LNOT;
//     if (symbol == "to_number") return UnaryOp::TONUMBER;
//     if (symbol == "to_string") return UnaryOp::TOSTRING;
//     std::cout << "Unrecognised operator: " << symbol << "\n";
//     assert(false && "Unsupported Operator!");
//     return UnaryOp::__UNDEFINED__;
// }
//
// /**
//  * Returns whether the given operator has a numeric return value.
//  */
// inline bool isNumericUnaryOp(const UnaryOp op) {
//     switch (op) {
//         case UnaryOp::ORD:
//         case UnaryOp::STRLEN:
//         case UnaryOp::NEG:
//         case UnaryOp::BNOT:
//         case UnaryOp::LNOT:
//         case UnaryOp::TONUMBER:
//             return true;
//         case UnaryOp::TOSTRING:
//             return false;
//         default:
//             break;
//     }
//     assert(false && "Uncovered case!");
//     return false;
// }
//
// /**
//  * Returns whether the given operator has a symbolic return value.
//  */
// inline bool isSymbolicUnaryOp(const UnaryOp op) {
//     return !isNumericUnaryOp(op);
// }
//
// /**
//  * Returns whether the given operator takes a numeric argument.
//  */
// inline bool unaryOpAcceptsNumbers(const UnaryOp op) {
//     switch (op) {
//         case UnaryOp::NEG:
//         case UnaryOp::BNOT:
//         case UnaryOp::LNOT:
//         case UnaryOp::TOSTRING:
//             return true;
//         case UnaryOp::ORD:
//         case UnaryOp::STRLEN:
//         case UnaryOp::TONUMBER:
//             return false;
//         default:
//             break;
//     }
//     assert(false && "Unsupported operator encountered!");
//     return false;
// }
//
// /**
//  * Returns whether the given operator takes a symbolic argument.
//  */
// inline bool unaryOpAcceptsSymbols(const UnaryOp op) {
//     return !unaryOpAcceptsNumbers(op);
// }
//
//  TERNARYYYYYYYYYYYYYY
//
// /**
//  * Converts symbolic representation of an operator to the operator
//  */
// inline TernaryOp getTernaryOpForSymbol(const std::string& symbol) {
//     if (symbol == "substr") return TernaryOp::SUBSTR;
//     std::cout << "Unrecognised operator: " << symbol << "\n";
//     assert(false && "Unsupported Operator!");
//     return TernaryOp::__UNDEFINED__;
// }
//
// /**
//  * Determines whether the given operator has a numeric return value.
//  */
// inline bool isNumericTernaryOp(const TernaryOp op) {
//     switch (op) {
//         case TernaryOp::SUBSTR:
//             return false;
//         default:
//             break;
//     }
//     assert(false && "Uncovered case!");
//     return false;
// }
//
// /**
//  * Determines whether the operator has a symbolic return value.
//  */
// inline bool isSymbolicTernaryOp(const TernaryOp op) {
//     return !isNumericTernaryOp(op);
// }
//
// /**
//  * Determines whether an argument has a number value.
//  */
// inline bool ternaryOpAcceptsNumbers(int arg, const TernaryOp op) {
//     assert(arg >= 0 && arg < 3 && "argument out of range");
//     switch (op) {
//         case TernaryOp::SUBSTR:
//             return arg == 1 || arg == 2;
//         default:
//             break;
//     }
//     assert(false && "Uncovered case!");
//     return false;
// }
//
// /**
//  * Determines whether an argument has a symbolic value
//  */
// inline bool ternaryOpAcceptsSymbols(int arg, const TernaryOp op) {
//     return !ternaryOpAcceptsNumbers(arg, op);
// }
//
// }  // end of namespace souffle
