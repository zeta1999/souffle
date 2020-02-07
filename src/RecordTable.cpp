/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RecordTable.cpp
 *
 * Temporary file.
 *
 ***********************************************************************/

#include "RecordTable.h"
#include <unordered_map>

namespace souffle {

namespace {
/**
 * The static access function for record maps of certain arities.
 */
RecordMap& getForArity(size_t arity) {
    // the static container -- filled on demand
    static std::unordered_map<size_t, RecordMap> maps;
    auto pos = maps.find(arity);
    if (pos == maps.end()) {
        maps.emplace(arity, arity);
    }

    return maps.find(arity)->second;
}
}  // namespace

RamDomain RecordTable::pack(RamDomain* tuple, size_t arity) {
    // conduct the packing
    return getForArity(arity).pack(tuple);
}

RamDomain* RecordTable::unpack(RamDomain ref, size_t arity) {
    // conduct the unpacking
    return getForArity(arity).unpack(ref);
}

}  // namespace souffle
