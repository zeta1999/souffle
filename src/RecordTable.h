/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RecordTable.h
 *
 * A unidirectional helper table to store record values for printing.
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"

#include <cassert>
#include <map>
#include <vector>

namespace souffle {

class RecordTable {
public:
    RecordTable() {}

    void addRecord(RamDomain ref, const std::vector<RamDomain>& tuple) {
        int arity = tuple.size();

        std::vector<RamDomain> tupleCopy;
        for (int i = 0; i < arity; i++) {
            tupleCopy.push_back(tuple[i]);
        }

        if (recordMap.find(arity) == recordMap.end()) {
            recordMap[arity] = std::map<RamDomain, std::vector<RamDomain>>();
        }
        recordMap[arity][ref] = tupleCopy;
    }

    const std::vector<RamDomain>& getRecord(int arity, RamDomain ref) const {
        return recordMap.at(arity).at(ref);
    }

private:
    std::map<int, std::map<RamDomain, std::vector<RamDomain>>> recordMap{};
};

}  // namespace souffle
