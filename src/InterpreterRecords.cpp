/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterRecords.cpp
 *
 * Utilities for handling records in Interpreter
 *
 ***********************************************************************/

#include "InterpreterRecords.h"
#include "RamTypes.h"
#include <cassert>
#include <limits>
#include <map>
#include <unordered_map>
#include <vector>

namespace souffle {

RecordTable::InterpreterRecordMap& RecordTable::getForArity(int arity) {
    static std::unordered_map<int, InterpreterRecordMap> maps;
    auto pos = maps.find(arity);
    if (pos == maps.end()) {
        maps.emplace(arity, arity);
    }

    return maps.find(arity)->second;
}

}  // end of namespace souffle
