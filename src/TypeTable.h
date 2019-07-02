
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
    TypeTable() = default;

    void addPrimitiveType(std::string type) {
        addType(type);
        std::cout << "PRIMITIVE: " << type << std::endl;
    }

    void addRecordType(std::string type, const std::vector<std::string>& fields) {
        addType(type);
        std::cout << "RECORD: " << type << " || " << fields << std::endl;
    }

    void addUnionType(std::string type, const std::vector<std::string>& variants) {
        addType(type);
        std::cout << "UNION: " << type << " || " << variants << std::endl;
    }

private:
    std::map<int, std::string> idToName;
    std::map<std::string, int> nameToId;
    std::map<int, std::vector<int>> recordToFields;

    void addType(std::string type) {
        static int count = 0;
        idToName[count] = type;
        nameToId[type] = count;

        count++;
    }
};

}  // namespace souffle
