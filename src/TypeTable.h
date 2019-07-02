
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

    // TODO: should be enum
    void addPrimitiveType(std::string type, std::string kind) {
        addType(type, kind);
    }

    void addRecordType(std::string type, std::vector<std::string> fields) {
        addType(type, "record");
        recordToFields[type] = fields;
    }

    void addUnionType(std::string type, std::string kind) {
        addType(type, kind);
    }

private:
    std::map<int, std::string> idToName;
    std::map<std::string, int> nameToId;
    std::map<std::string, std::vector<std::string>> recordToFields;
    std::map<std::string, std::string> typeToKind;

    int addType(std::string type, std::string kind) {
        static int count = 0;
        idToName[count] = type;
        nameToId[type] = count;
        typeToKind[type] = kind;

        count++;
        return count;
    }
};

}  // namespace souffle
