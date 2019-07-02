
/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
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

#include <cassert>
#include <map>
#include <vector>

namespace souffle {

class AstType;

class TypeTable {
public:
    TypeTable(const std::set<std::string>& types) {
        for (const auto& type : types) {
            std::cout << type << std::endl;
        }
    }

private:
    std::map<int, std::string> idToName;
    std::map<std::string, int> nameToId;
    std::map<int, std::vector<int>> recordToFields;
};

}  // namespace souffle
