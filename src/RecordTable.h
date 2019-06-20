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
    RecordTable() = default;

    /**
     * Add a record tuple with the given reference number to the table.
     */
    void addRecord(RamDomain ref, const std::vector<RamDomain>& tuple) {
        const size_t arity = tuple.size();
        if (recordMap.find(arity) == recordMap.end()) {
            // create record map for that arity if it does not already exist
            recordMap[arity] = std::map<RamDomain, std::vector<RamDomain>>();
        }
        recordMap[arity][ref] = std::vector<RamDomain>(tuple);
    }

    /**
     * Get the record tuple with given arity and reference number.
     */
    const std::vector<RamDomain>& getRecord(int arity, RamDomain ref) const {
        return recordMap.at(arity).at(ref);
    }

    /**
     * Empty the record table.
     */
    void clear() {
        recordMap.clear();
    }

    /**
     * Check if the record table is empty.
     */
    bool empty() const {
        return recordMap.empty();
    }

private:
    std::map<int, std::map<RamDomain, std::vector<RamDomain>>> recordMap{};
};

}  // namespace souffle
