/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file WriteStream.h
 *
 ***********************************************************************/

#pragma once

#include "IODirectives.h"
#include "RamTypes.h"
#include "SymbolTable.h"
#include "json11.h"

#include <cassert>
#include <string>
#include <vector>

namespace souffle {

using Json = json11::Json;

// Arguments:
// 1) IOdirectives
// 2) symbolTable
// 3) (future) recordTable

class WriteStream {
public:
    // Todo: drop this and fix printsize related issues.
    WriteStream(const std::vector<RamTypeAttribute>& symbolMask, const SymbolTable& symbolTable,
            const size_t auxiliaryArity, bool summary = false)
            : symbolMask(symbolMask), symbolTable(symbolTable), summary(summary),
              arity(symbolMask.size() - auxiliaryArity){};

    WriteStream(const IODirectives& ioDirectives, const SymbolTable& symbolTable, bool summary = false)
            : symbolTable(symbolTable), summary(summary) {
        const std::string& relationName{ioDirectives.getRelationName()};

        std::string parseErrors;

        types = Json::parse(ioDirectives.get("typesystem"), parseErrors);

        assert(parseErrors.size() == 0 && "Internal JSON parsing failed.");

        arity = static_cast<size_t>(types[relationName]["arity"].long_value());

        for (size_t i = 0; i < arity; ++i) {
            RamTypeAttribute type = RamPrimitiveFromChar(types[relationName]["types"][i].string_value()[0]);
            symbolMask.push_back(type);
        }
    }

    template <typename T>
    void writeAll(const T& relation) {
        if (summary) {
            return writeSize(relation.size());
        }
        auto lease = symbolTable.acquireLock();
        (void)lease;  // silence "unused variable" warning
        if (arity == 0) {
            if (relation.begin() != relation.end()) {
                writeNullary();
            }
            return;
        }
        for (const auto& current : relation) {
            writeNext(current);
        }
    }
    template <typename T>
    void writeSize(const T& relation) {
        writeSize(relation.size());
    }

    virtual ~WriteStream() = default;

protected:
    std::vector<RamTypeAttribute> symbolMask;
    const SymbolTable& symbolTable;
    Json types;

    const bool summary;
    size_t arity;

    virtual void writeNullary() = 0;
    virtual void writeNextTuple(const RamDomain* tuple) = 0;
    virtual void writeSize(std::size_t) {
        assert(false && "attempting to print size of a write operation");
    }
    template <typename Tuple>
    void writeNext(const Tuple tuple) {
        writeNextTuple(tuple.data);
    }
    void writeNextTupleElement(std::ostream& destination, RamTypeAttribute type, RamDomain value) {
        switch (type) {
            case RamTypeAttribute::Symbol:
                destination << symbolTable.unsafeResolve(value);
                break;
            case RamTypeAttribute::Signed:
                destination << value;
                break;
            case RamTypeAttribute::Unsigned:
                destination << ramBitCast<RamUnsigned>(value);
                break;
            case RamTypeAttribute::Float:
                destination << ramBitCast<RamFloat>(value);
                break;
            case RamTypeAttribute::Record:
                assert(false && "Record writing is not supported");
        }
    }
};

class WriteStreamFactory {
public:
    virtual std::unique_ptr<WriteStream> getWriter(
            const IODirectives& ioDirectives, const SymbolTable& symbolTable) = 0;

    virtual const std::string& getName() const = 0;
    virtual ~WriteStreamFactory() = default;
};

template <>
inline void WriteStream::writeNext(const RamDomain* tuple) {
    writeNextTuple(tuple);
}

} /* namespace souffle */
