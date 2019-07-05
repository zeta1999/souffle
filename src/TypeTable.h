
/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TypeTable.h
 *
 * A unidirectional helper table to store type information.
 *
 ***********************************************************************/

#pragma once

#include "SouffleType.h"
#include <cassert>
#include <map>
#include <vector>

namespace souffle {

class TypeTable {
public:
    TypeTable() {
        addPrimitiveType("number", Kind::NUMBER);
        addPrimitiveType("symbol", Kind::SYMBOL);
        addPrimitiveType("record", Kind::RECORD);
    }

    TypeTable(std::map<std::string, int> nameToId, std::map<int, std::string> idToName,
            std::map<int, std::vector<int>> idToFields, std::map<int, Kind> idToKind)
            : nameToId(nameToId), idToName(idToName), idToFields(idToFields), idToKind(idToKind) {}

    void addPrimitiveType(std::string type, Kind kind) {
        addType(type, kind);
    }

    void addUnionType(std::string type, Kind kind) {
        addType(type, kind);
    }

    void addRecordType(std::string type, std::vector<int> fieldIds) {
        int id = addType(type, Kind::RECORD);
        idToFields[id] = fieldIds;
    }

    int getId(const std::string& type) const {
        return nameToId.at(type);
    }

    Kind getKind(int id) const {
        return idToKind.at(id);
    }

    const std::string& getName(int id) const {
        return idToName.at(id);
    }

    const std::vector<int>& getFieldTypes(int recordId) const {
        return idToFields.at(recordId);
    }

    const std::map<int, std::string>& getIdToNameMap() const {
        return idToName;
    }

    const std::map<int, std::vector<int>> getIdToFieldsMap() const {
        return idToFields;
    }

    const std::map<int, Kind> getIdToKindMap() const {
        return idToKind;
    }

    void print() const {
        std::cout << "--- TYPE TABLE ---" << std::endl;
        for (const auto& pair : idToName) {
            std::cout << pair.first << " <-> " << pair.second << std::endl;
        }
        for (const auto& pair : idToFields) {
            std::cout << pair.first << " -> " << pair.second << std::endl;
        }
        for (const auto& pair : idToKind) {
            std::cout << pair.first << " |-> " << pair.second << std::endl;
        }
        std::cout << " - - - - - - - - -" << std::endl;
    }

private:
    // TODO: maybe change to vectors or some sort of array access
    std::map<std::string, int> nameToId;
    std::map<int, std::string> idToName;
    std::map<int, std::vector<int>> idToFields;
    std::map<int, Kind> idToKind;

    int addType(std::string type, Kind kind) {
        // type must only be added once
        assert(nameToId.find(type) == nameToId.end() && "typename already exists in type table");

        static int count = 0;
        nameToId[type] = count;
        idToName[count] = type;
        idToKind[count] = kind;

        return count++;
    }
};

}  // namespace souffle
