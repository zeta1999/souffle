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

namespace souffle {

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

}
