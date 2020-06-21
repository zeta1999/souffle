/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file WriteStreamJSON.h
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"
#include "SymbolTable.h"
#include "WriteStream.h"
#include "json11.h"
#include "utility/ContainerUtil.h"

#include <map>
#include <ostream>
#include <string>
#include <vector>
#include <variant>
#include <stack>

namespace souffle {

class WriteStreamJSON : public WriteStream {
protected:
    WriteStreamJSON(const std::map<std::string, std::string>& rwOperation, const SymbolTable& symbolTable,
                    const RecordTable& recordTable)
        : WriteStream(rwOperation, symbolTable, recordTable),
          beautify(getOr(rwOperation, "beautify", "false") == "ture"){};

    const bool beautify;

    void writeNextTupleJSON(std::ostream& destination, const RamDomain* tuple) {
        std::vector<Json> result;

        bool isFirst = true;
        destination << "[";
        for (size_t col = 0; col < arity; ++col) {
            writeNextTupleElement(destination, typeAttributes.at(col), tuple[col]);
            if (isFirst) {
                isFirst = false;
            } else {
                destination << ",\n";
            }
        }

        // Output a JSON array for all tuples
        destination << "]";
    }

    Json writeNextTupleElement(std::ostream& destination, const std::string& type, RamDomain value) {
        switch (type[0]) {
            case 's': destination << symbolTable.unsafeResolve(value); break;
            case 'i': destination << value; break;
            case 'u': destination << ramBitCast<RamUnsigned>(value); break;
            case 'f': destination << ramBitCast<RamFloat>(value); break;
            case 'r': outputJSONRecord(destination, value, type); break;
            default: fatal("unsupported type attribute: `%c`", type[0]);
        }
    }

    void outputJSONRecord(std::ostream& destination, const RamDomain value, const std::string& name) {
        using ValueTuple = std::pair<const RamDomain, const std::string&>;
        std::stack<std::variant<ValueTuple, std::string>> worklist;
        worklist.push(std::make_pair(value, name));

        while(!worklist.empty()) {
            auto curr = std::move(worklist.top());
            worklist.pop();

            if (std::holds_alternative<std::string>(curr)) {
                destination << std::get<std::string>(curr);
                continue;
            }

            const std::string& type = std::get<ValueTuple>(curr).second;
            RamDomain value = std::get<ValueTuple>(curr).first;
            switch (type[0]) {
                case 's': return Json(symbolTable.unsafeResolve(value)); break;
                case 'i': return Json((int) value); break;
                case 'u': return Json((int) ramBitCast<RamUnsigned>(value)); break;
                case 'f': return Json(ramBitCast<RamFloat>(value)); break;
                case 'r': return outputJSONRecord(value, type); break;
                default: fatal("unsupported type attribute: `%c`", type[0]);
            }
        }
    }

};


class WriteFileJSON : public WriteStreamJSON {
public:
    WriteFileJSON(const std::map<std::string, std::string>& rwOperation, const SymbolTable& symbolTable,
                  const RecordTable& recordTable)
        : WriteStreamJSON(rwOperation, symbolTable, recordTable),
          file(rwOperation.at("filename"), std::ios::out | std::ios::binary) {}

protected:
    std::ofstream file;

    void writeNullary() override {
        file << "{}\n";
    }

    void writeNextTuple(const RamDomain* tuple) override {
        writeNextTupleJSON(file, tuple);
    }
};
} /* ma,espace souffle */
