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
    assert(false && "undefined operator");
}

// TODO: CONVERT THESE TO PROPER FUNCTIONS FOR THIS NEW ENUM
// TODO: MAKE SURE YOU CATCH everything from each type (should be consistent)
// TODO: add a function to check arity
///**
// * Converts operator to its symbolic representation
// */
// inline std::string getSymbolForBinaryOp(BinaryOp op) {
//    switch (op) {
//        case BinaryOp::ADD:
//            return "+";
//        case BinaryOp::SUB:
//            return "-";
//        case BinaryOp::MUL:
//            return "*";
//        case BinaryOp::DIV:
//            return "/";
//        case BinaryOp::EXP:
//            return "^";
//        case BinaryOp::MOD:
//            return "%";
//        case BinaryOp::BAND:
//            return "band";
//        case BinaryOp::BOR:
//            return "bor";
//        case BinaryOp::BXOR:
//            return "bxor";
//        case BinaryOp::LAND:
//            return "land";
//        case BinaryOp::LOR:
//            return "lor";
//        case BinaryOp::MAX:
//            return "max";
//        case BinaryOp::MIN:
//            return "min";
//        case BinaryOp::CAT:
//            return "cat";
//        default:
//            break;
//    }
//    assert(false && "Unsupported Operator!");
//    return "?";
//}
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
}  // end of namespace souffle
