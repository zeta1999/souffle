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
#include "RecordTable.h"
#include "SouffleType.h"
#include "SymbolTable.h"
#include "TypeTable.h"

#include <cassert>
#include <string>
#include <vector>

namespace souffle {

class WriteStream {
public:
    WriteStream(const std::vector<TypeId>& typeMask, const SymbolTable& symbolTable,
            const RecordTable& recordTable, const TypeTable& typeTable, const bool prov, bool summary = false)
            : typeMask(typeMask), symbolTable(symbolTable), recordTable(recordTable), typeTable(typeTable),
              isProvenance(prov), summary(summary), arity(typeMask.size() - (prov ? 2 : 0)) {}
    template <typename T>
    void writeAll(const T& relation) {
        if (summary) {
            return writeSize(relation.size());
        }
        auto lease = symbolTable.acquireLock();
        (void)lease;
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
    const std::vector<TypeId>& typeMask;
    const SymbolTable& symbolTable;
    const RecordTable& recordTable;
    const TypeTable& typeTable;
    const bool isProvenance;
    const bool summary;
    const size_t arity;

    virtual void writeNullary() = 0;
    virtual void writeNextTuple(const RamDomain* tuple) = 0;

    virtual void writeSize(std::size_t size) {
        assert(false && "attempting to print size of a write operation");
    }

    template <typename Tuple>
    void writeNext(const Tuple tuple) {
        writeNextTuple(tuple.data);
    }

    void writeValue(std::ostream& os, RamDomain repr, int typeId) {
        Kind kind = typeTable.getKind(typeId);
        switch (kind) {
            case Kind::NUMBER:
                os << repr;
                break;

            case Kind::SYMBOL:
                os << symbolTable.unsafeResolve(repr);
                break;

            case Kind::RECORD: {
                // special case: nil
                // TODO (tmp): assuming nil = 0
                if (repr == 0) {
                    os << "nil";
                    break;
                }

                // get record metadata
                const auto& name = typeTable.getName(typeId);
                const auto& fieldTypes = typeTable.getFieldTypes(typeId);
                int arity = fieldTypes.size();

                // get record data
                const auto& record = recordTable.getRecord(arity + 1, repr);

                // print out the record recursively
                os << name << "[";
                for (size_t i = 0; i < arity; i++) {
                    if (typeTable.getKind(fieldTypes[i]) == Kind::SYMBOL) {
                        os << "\"";
                        writeValue(os, record[i + 1], fieldTypes[i]);
                        os << "\"";
                    } else {
                        writeValue(os, record[i + 1], fieldTypes[i]);
                    }

                    if (i != arity - 1) {
                        os << ", ";
                    }
                }
                os << "]";
                break;
            }

            default:
                assert(false && "unsupported kind");
        }
    }
};

class WriteStreamFactory {
public:
    virtual std::unique_ptr<WriteStream> getWriter(const std::vector<TypeId>& typeMask,
            const SymbolTable& symbolTable, const RecordTable& recordTable, const TypeTable& typeTable,
            const IODirectives& ioDirectives, const bool provenance) = 0;
    virtual const std::string& getName() const = 0;
    virtual ~WriteStreamFactory() = default;
};

template <>
inline void WriteStream::writeNext(const RamDomain* tuple) {
    writeNextTuple(tuple);
}

} /* namespace souffle */
