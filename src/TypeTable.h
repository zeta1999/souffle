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
#include <string>
#include <vector>

// TODO (tmp): maybe use qualifiers for names instead of just toString?

namespace souffle {

class TypeTable {
public:
    TypeTable() {
        // add the primary types corresponding to each kind
        addPrimitiveType("number", Kind::NUMBER);
        addPrimitiveType("symbol", Kind::SYMBOL);
        addPrimitiveType("record", Kind::RECORD);
    }

    TypeTable(std::map<std::string, TypeId> nameToId, std::map<TypeId, std::string> idToName,
            std::map<TypeId, std::vector<TypeId>> idToFields, std::map<TypeId, Kind> idToKind)
            : nameToId(nameToId), idToName(idToName), idToFields(idToFields), idToKind(idToKind) {}

    /**
     * Add a primitive type to the type table.
     */
    void addPrimitiveType(std::string type, Kind kind) {
        addType(type, kind);
    }

    /**
     * Add a union type to the type table.
     * Note: all variants of a union type have the same kind.
     */
    void addUnionType(std::string type, Kind kind) {
        addType(type, kind);
    }

    /**
     * Add a record type to the type table.
     * If a field type does not exist yet in the type table, it is added tentatively.
     */
    void addRecordType(std::string type, std::vector<std::string> fieldNames) {
        // add the overarching record type
        auto id = addType(type, Kind::RECORD);

        // add the record->fields map
        std::vector<TypeId> fieldIds;
        for (auto field : fieldNames) {
            auto fieldPair = nameToId.find(field);
            if (fieldPair != nameToId.end()) {
                // field already exists in the type table, no need to add it
                fieldIds.push_back(fieldPair->second);
            } else {
                // field does not exist yet
                auto fieldId = addTentativeType(field);
                fieldIds.push_back(fieldId);
            }
        }
        idToFields[id] = fieldIds;
    }

    /** Get the type id of a given type */
    TypeId getId(const std::string& type) const {
        return nameToId.at(type);
    }

    /** Get the kind of a given typeid */
    Kind getKind(TypeId id) const {
        return idToKind.at(id);
    }

    /** Get the name corresponding to a given typeid */
    const std::string& getName(TypeId id) const {
        return idToName.at(id);
    }

    /** Get the fields corresponding to a given record type id */
    const std::vector<TypeId>& getFieldTypes(TypeId recordId) const {
        assert(idToKind.at(recordId) == Kind::RECORD && "only records can have fields");
        return idToFields.at(recordId);
    }

    /** Get the full name-to-id mapping */
    const std::map<std::string, TypeId>& getNameToIdMap() const {
        return nameToId;
    }

    /** Get the full id-to-name mapping */
    const std::map<TypeId, std::string>& getIdToNameMap() const {
        return idToName;
    }

    /** Get the full recordid-to-fields mapping */
    const std::map<TypeId, std::vector<TypeId>>& getIdToFieldsMap() const {
        return idToFields;
    }

    /** Get the full id-to-kind mapping */
    const std::map<TypeId, Kind>& getIdToKindMap() const {
        return idToKind;
    }

    /**
     * Checks if the type table is completely defined.
     * @return true iff no tentative types exist.
     */
    bool isComplete() const {
        return tentativeTypes.empty();
    }

private:
    // type information
    TypeId curId{MIN_TYPE_ID};
    std::map<std::string, TypeId> nameToId;
    std::map<TypeId, std::string> idToName;
    std::map<TypeId, std::vector<TypeId>> idToFields;
    std::map<TypeId, Kind> idToKind;

    // partially defined types
    std::set<std::string> tentativeTypes{};

    /**
     * Defines a type partially, with the expectation that it will be completely defined later.
     * @param type type to partially define
     * @return type-id assigned to the given type
     */
    TypeId addTentativeType(std::string type) {
        if (tentativeTypes.find(type) != tentativeTypes.end()) {
            // tentative type already exists, just return the id
            return nameToId.at(type);
        }

        assert(nameToId.find(type) == nameToId.end() && "tentative type cannot already exist");

        // assign known information
        TypeId id = curId++;
        tentativeTypes.insert(type);
        nameToId[type] = id;
        return id;
    }

    /**
     * Adds a type to the type table.
     *
     * @param type type to add
     * @param kind type kind
     * @return id assigned to the type
     */
    TypeId addType(std::string type, Kind kind) {
        TypeId id;
        if (tentativeTypes.find(type) != tentativeTypes.end()) {
            // type already partially defined
            assert(nameToId.find(type) != nameToId.end() && "tentative type must already have id");
            id = nameToId.at(type);
            tentativeTypes.erase(type);
        } else {
            // type must only be added once
            assert(nameToId.find(type) == nameToId.end() && "typename already exists in type table");
            id = curId++;
            nameToId[type] = id;
        }

        // complete type metadata
        idToName[id] = type;
        idToKind[id] = kind;

        return id;
    }
};

}  // namespace souffle
