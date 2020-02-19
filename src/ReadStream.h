/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ReadStream.h
 *
 ***********************************************************************/

#pragma once

#include "IODirectives.h"
#include "RamTypes.h"
#include "RecordTable.h"
#include "SymbolTable.h"
#include "Util.h"
#include "json11.h"
#include <cctype>
#include <memory>
#include <string>
#include <vector>

namespace souffle {

using json11::Json;

class ReadStream {
protected:
    ReadStream(const IODirectives& ioDirectives, SymbolTable& symbolTable, RecordTable& recordTable)
            : symbolTable(symbolTable), recordTable(recordTable) {
        const std::string& relationName{ioDirectives.getRelationName()};

        std::string parseErrors;

        types = Json::parse(ioDirectives.get("types"), parseErrors);

        assert(parseErrors.size() == 0 && "Internal JSON parsing failed.");

        arity = static_cast<size_t>(types[relationName]["arity"].long_value());
        auxiliaryArity = static_cast<size_t>(types[relationName]["auxArity"].long_value());

        for (size_t i = 0; i < arity; ++i) {
            std::string type = types[relationName]["types"][i].string_value();
            typeAttributes.push_back(std::move(type));
        }
    }

public:
    template <typename T>
    void readAll(T& relation) {
        auto lease = symbolTable.acquireLock();
        (void)lease;
        while (const auto next = readNextTuple()) {
            const RamDomain* ramDomain = next.get();
            relation.insert(ramDomain);
        }
    }

    virtual ~ReadStream() = default;

protected:
    /**
     * Read a record from a string.
     *
     * This function assumes that the parenthesis are balanced.
     */
    RamDomain readRecord(const std::string& source, const std::string& name, size_t pos = 0) {
        Json recordInfo = types["records"][name];

        // Check if record type information are present
        if (recordInfo.is_null()) {
            throw std::invalid_argument("Missing record type information: " + name);
        }

        // Handle nil case
        consumeWhiteSpace(source, pos);
        if (source.substr(pos, 3) == "nil") {
            return recordTable.getNil();
        }

        const Json recordTypes = recordInfo["types"];
        const size_t recordArity = recordInfo["arity"].long_value();

        std::vector<RamDomain> recordValues(recordArity);

        consumeChar(source, '[', pos);

        for (size_t i = 0; i < recordArity; ++i) {
            const std::string& recordType = recordTypes[i].string_value();
            size_t consumed = 0;

            if (i > 0) {
                consumeChar(source, ',', pos);
            }
            consumeWhiteSpace(source, pos);
            switch (recordType[0]) {
                case 's':
                    recordValues[i] = readStringInRecord(source, pos);
                    break;
                case 'i':
                    recordValues[i] = RamDomainFromString(source.substr(pos), &consumed);
                    break;
                case 'u':
                    recordValues[i] = ramBitCast(RamUnsignedFromString(source.substr(pos), &consumed));
                    break;
                case 'f':
                    recordValues[i] = ramBitCast(RamFloatFromString(source.substr(pos), &consumed));
                    break;
                case 'r':
                    recordValues[i] = readRecord(source, recordType,
                            consumed);  // Pos must be a reference, otherwise this won't work.
                    break;
                default:
                    assert(false && "Invalid type attribute");
            }
            pos += consumed;
        }
        consumeChar(source, ']', pos);
        return recordTable.pack(recordValues);
    }

    RamDomain readStringInRecord(const std::string& source, size_t& pos) {
        size_t index = pos;

        auto endOfElement = [](char c) {
            return std::isspace(static_cast<unsigned char>(c)) || c == ',' || c == ']';
        };

        while (index < source.length() && !endOfElement(source[index])) {
            ++index;
        }

        std::string str = source.substr(pos, index - pos);
        pos = index;

        return symbolTable.unsafeLookup(std::move(str));
    }

    /**
     * Read past given character, consuming any preceding whitespace.
     */
    void consumeChar(const std::string& str, char c, size_t& pos) {
        consumeWhiteSpace(str, pos);
        if (pos == str.length()) {
            throw std::invalid_argument("Invalid record");
        }
        if (str[pos] != c) {
            std::stringstream error;
            error << "Expected: \'" << c << "\', got: " << str[pos];
            throw std::invalid_argument(error.str());
        }
        ++pos;
    }

    /**
     * Advance position in the string until first non-whitespace character.
     */
    void consumeWhiteSpace(const std::string& str, size_t& pos) {
        while (pos < str.length() && std::isspace(static_cast<unsigned char>(str[pos]))) {
            ++pos;
        }
    }

    Json types;

    virtual std::unique_ptr<RamDomain[]> readNextTuple() = 0;
    std::vector<std::string> typeAttributes;
    SymbolTable& symbolTable;
    RecordTable& recordTable;

    size_t arity;
    size_t auxiliaryArity;
};

class ReadStreamFactory {
public:
    virtual std::unique_ptr<ReadStream> getReader(const IODirectives&, SymbolTable&, RecordTable&) = 0;
    virtual const std::string& getName() const = 0;
    virtual ~ReadStreamFactory() = default;
};

} /* namespace souffle */
