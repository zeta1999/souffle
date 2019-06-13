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
#include <iostream>
#include <map>
#include <vector>

namespace souffle {

class RecordTable {
public:
    RecordTable() {}

    void addRecord(RamDomain ref, const std::vector<RamDomain>& tuple) {
        std::vector<RamDomain> tupleCopy;
        for (size_t i = 0; i < tuple.size(); i++) {
            tupleCopy.push_back(tuple[i]);
        }
        recordMap[ref] = tupleCopy;
    }

    const std::vector<RamDomain>& getRecord(RamDomain ref) const {
        assert(recordMap.find(ref) != recordMap.end() && "reference not in map");
        return recordMap.at(ref);
    }

private:
    std::map<RamDomain, std::vector<RamDomain>> recordMap{};
};

}  // namespace souffle
