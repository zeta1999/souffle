/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamPrimitiveTypes.h
 *
 * Defines a primitive types used in Ram
 *
 ***********************************************************************/

#pragma once

#include <cassert>
#include <iostream>
#include <ostream>

namespace souffle {

enum class RamPrimitiveType {
    String,
    Signed,    // Signed number
    Unsigned,  // Unsigned number
    Float,     // Floating point number.
    Record,
};

inline std::ostream& operator<<(std::ostream& os, RamPrimitiveType T) {
    switch (T) {
        case RamPrimitiveType::String:
            os << "RamPrimitiveType::String";
            break;
        case RamPrimitiveType::Signed:
            os << "RamPrimitiveType::Signed";
            break;
        case RamPrimitiveType::Float:
            os << "RamPrimitiveType::Float";
            break;
        case RamPrimitiveType::Unsigned:
            os << "RamPrimitiveType::Unsigned";
            break;
        case RamPrimitiveType::Record:
            os << "RamPrimitiveType::Record";
    }
    return os;
}

/** Convert a char to RamPrimitiveType */
inline RamPrimitiveType RamPrimitiveFromChar(char c) {
    RamPrimitiveType t;
    switch (c) {
        case 's':
            t = RamPrimitiveType::String;
            break;
        case 'i':
            t = RamPrimitiveType::Signed;
            break;
        case 'f':
            t = RamPrimitiveType::Float;
            break;
        case 'u':
            t = RamPrimitiveType::Unsigned;
            break;
        case 'r':
            t = RamPrimitiveType::Record;
            break;
        default:
            std::cerr << "Invalid RamPrimitiveType Char " << c << std::endl;
            assert(false && "Invalid conversion to ram primitive type");
    }
    return t;
}

}  // namespace souffle
