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
#include <set>
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

    void addRecordType(std::string type, std::vector<std::string> fieldNames) {
        int id = addType(type, Kind::RECORD);
        std::vector<int> fieldIds;
        for (auto field : fieldNames) {
            auto fieldPair = nameToId.find(field);
            if (fieldPair != nameToId.end()) {
                fieldIds.push_back(fieldPair->second);
            } else {
                int fieldId = addTentativeType(field);
                fieldIds.push_back(fieldId);
            }
        }
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

    const std::map<std::string, int>& getNameToIdMap() const {
        return nameToId;
    }

    const std::map<int, std::string>& getIdToNameMap() const {
        return idToName;
    }

    const std::map<int, std::vector<int>>& getIdToFieldsMap() const {
        return idToFields;
    }

    const std::map<int, Kind>& getIdToKindMap() const {
        return idToKind;
    }

    bool isComplete() const {
        return tentativeTypes.empty();
    }

private:
    std::map<std::string, int> nameToId;
    std::map<int, std::string> idToName;
    std::map<int, std::vector<int>> idToFields;
    std::map<int, Kind> idToKind;
    std::set<std::string> tentativeTypes{};
    int numTypes{0};

    int addTentativeType(std::string type) {
        assert(nameToId.find(type) == nameToId.end() && "tentative type cannot already exist");
        int id = numTypes++;
        tentativeTypes.insert(type);
        nameToId[type] = id;
        return id;
    }

    int addType(std::string type, Kind kind) {
        // check if tentative type
        int id;
        if (tentativeTypes.find(type) != tentativeTypes.end()) {
            // name to id map must already exist
            assert(nameToId.find(type) != nameToId.end() && "tentative type must already have id");
            id = nameToId.at(type);
            tentativeTypes.erase(type);
        } else {
            // type must only be added once
            assert(nameToId.find(type) == nameToId.end() && "typename already exists in type table");
            id = numTypes++;
            nameToId[type] = id;
        }

        idToName[id] = type;
        idToKind[id] = kind;

        return id;
    }
};

}  // namespace souffle
