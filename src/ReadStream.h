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
#include "json11.h"
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
            RamTypeAttribute type = RamPrimitiveFromChar(types[relationName]["types"][i].string_value()[0]);
            typeAttributes.push_back(type);
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
    Json types;

    virtual std::unique_ptr<RamDomain[]> readNextTuple() = 0;
    std::vector<RamTypeAttribute> typeAttributes;
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
