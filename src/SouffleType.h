/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SouffleType.h
 *
 * Type infrastructure used across the compiler/interpreter.
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"

#include <ostream>

namespace souffle {

// define the domain for type ids
// TODO: clarify actual type we want
// TODO: use max type id to ensure we dont go over it
// TODO: should merge this with arity once polymorphism is implemented
using TypeId = RamDomain;
#define MIN_TYPE_ID 0
#define MAX_TYPE_ID MAX_RAM_DOMAIN

// define Kinds
enum class Kind { SYMBOL, NUMBER, RECORD };

inline std::ostream& operator<<(std::ostream& os, Kind kind) {
    switch (kind) {
        case Kind::SYMBOL:
            os << "symbol";
            break;
        case Kind::NUMBER:
            os << "number";
            break;
        case Kind::RECORD:
            os << "any record";
            break;
    }

    return os;
}

}  // namespace souffle
