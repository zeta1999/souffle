
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

#include <cassert>
#include <map>
#include <vector>

// TODO: fix types used

namespace souffle {

class TypeTable {
public:
    TypeTable() {
        typeToKind["number"] = "number";
        typeToKind["symbol"] = "symbol";
        typeToKind["record"] = "record";
    }

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

    int getTypeId(const std::string& type) const {
        return nameToId.at(type);
    }

    std::string getRecordName(int typeId) const {
        return idToName.at(typeId);
    }

    std::vector<char> getFieldKinds(const std::string& recordName) const {
        const std::vector<std::string>& fields = recordToFields.at(recordName);
        std::vector<char> res;
        for (const auto& _field : fields) {
            std::cout << "getting _field: " << _field << std::endl;
            const std::string& field = typeToKind.at(_field);
            std::cout << "field is: " << field << std::endl;
            if (field == "record") {
                res.push_back('r');
            } else if (field == "number") {
                res.push_back('i');
            } else if (field == "symbol") {
                res.push_back('s');
            } else if (field == "union") {
                assert(false && "union currently not supported");
            } else {
                std::cout << field << std::endl;
                assert(false && "unexpected kind");
            }
        }
        return res;
    }

    const std::vector<std::string>& getFieldTypes(const std::string& recordName) const {
        return recordToFields.at(recordName);
    }

    int getRecordArity(const std::string& recordName) const {
        return recordToFields.at(recordName).size();
    }

    void print() const {
        std::cout << "TYPE TABLE" << std::endl;
        for (const auto& pair : idToName) {
            std::cout << pair.first << " <-> " << pair.second << std::endl;
        }
        for (const auto& pair : recordToFields) {
            std::cout << pair.first << " -> " << pair.second << std::endl;
        }
        for (const auto& pair : typeToKind) {
            std::cout << pair.first << " |-> " << pair.second << std::endl;
        }
    }

private:
    std::map<int, std::string> idToName;
    std::map<std::string, int> nameToId;
    std::map<std::string, std::vector<std::string>> recordToFields;
    std::map<std::string, std::string> typeToKind;

    int addType(std::string type, std::string kind) {
        static int count = 100;
        idToName[count] = type;
        nameToId[type] = count;
        typeToKind[type] = kind;

        count++;
        return count;
    }
};

}  // namespace souffle
