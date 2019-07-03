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
#include "SymbolTable.h"
#include "TypeTable.h"

#include <cassert>
#include <string>
#include <vector>

namespace souffle {

class WriteStream {
public:
    WriteStream(const std::vector<int>& typeMask, const SymbolTable& symbolTable,
            const std::vector<int>& recordArityMask, const RecordTable& recordTable,
            const TypeTable& typeTable, const bool prov, bool summary = false)
            : typeMask(typeMask), symbolTable(symbolTable), recordArityMask(recordArityMask),
              recordTable(recordTable), typeTable(typeTable), isProvenance(prov), summary(summary),
              arity(typeMask.size() - (prov ? 2 : 0)) {}
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
    const std::vector<int>& typeMask;
    const SymbolTable& symbolTable;

    // TODO: may no longer need this arity mask
    const std::vector<int>& recordArityMask;
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

    void writeValue(std::ostream& os, RamDomain repr, char kind, int recordType = -1) {
        switch(kind) {
            case 'i':
                os << repr;
                break;

            case 's':
                os << symbolTable.unsafeResolve(repr);
                break;

            case 'r': {
                // get record metadata
                const auto& name = typeTable.getRecordName(recordType);
                const auto& fieldTypes = typeTable.getFieldTypes(recordType);
                int arity = fieldTypes.size();

                // get record data
                const auto& record = recordTable.getRecord(arity + 1, repr);

                // print out the record recursively
                os << name << "[";
                for (int i = 0; i < arity; i++) {
                    char kind = typeTable.getKind(fieldTypes[i]);

                    if (kind == 'r') {
                        writeValue(os, record[i+1], kind, fieldTypes[i]);
                    } else {
                        writeValue(os, record[i+1], kind);
                    }

                    // print delimiter
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
    virtual std::unique_ptr<WriteStream> getWriter(const std::vector<int>& typeMask,
            const SymbolTable& symbolTable, const std::vector<int>& recordArityMask,
            const RecordTable& recordTable, const TypeTable& typeTable, const IODirectives& ioDirectives,
            const bool provenance) = 0;
    virtual const std::string& getName() const = 0;
    virtual ~WriteStreamFactory() = default;
};

template <>
inline void WriteStream::writeNext(const RamDomain* tuple) {
    writeNextTuple(tuple);
}

} /* namespace souffle */
