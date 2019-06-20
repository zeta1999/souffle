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

#include <cassert>
#include <string>
#include <vector>

namespace souffle {

class WriteStream {
public:
    WriteStream(const std::vector<char>& kindMask, const SymbolTable& symbolTable,
            const std::vector<int>& recordArityMask, const RecordTable* recordTable, const bool prov,
            bool summary = false)
            : kindMask(kindMask), symbolTable(symbolTable), recordArityMask(recordArityMask),
              recordTable(recordTable), isProvenance(prov), summary(summary),
              arity(kindMask.size() - (prov ? 2 : 0)) {}
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
    const std::vector<char>& kindMask;
    const SymbolTable& symbolTable;
    const std::vector<int>& recordArityMask;
    const RecordTable* recordTable;
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

    void writeValue(std::ostream& os, size_t col, RamDomain repr) {
        char kind = kindMask.at(col);

        switch (kind) {
            case 'i': {
                os << repr;
                break;
            }

            case 's': {
                os << symbolTable.unsafeResolve(repr);
                break;
            }

            case 'r': {
                int arity = recordArityMask.at(col);
                const auto& record = recordTable->getRecord(arity, repr);

                os << "UnnamedRecord";
                if (record.size() == 0) {
                    os << "[]";
                } else {
                    os << "[" << record[0];
                    for (size_t i = 1; i < record.size(); i++) {
                        os << ", " << record[i];
                    }
                    os << "]";
                }

                break;
            }

            default:
                assert(false && "cannot print value of unknown kind");
        }
    }
};

class WriteStreamFactory {
public:
    virtual std::unique_ptr<WriteStream> getWriter(const std::vector<char>& kindMask,
            const SymbolTable& symbolTable, const std::vector<int>& recordArityMask,
            const RecordTable* recordTable, const IODirectives& ioDirectives, const bool provenance) = 0;
    virtual const std::string& getName() const = 0;
    virtual ~WriteStreamFactory() = default;
};

template <>
inline void WriteStream::writeNext(const RamDomain* tuple) {
    writeNextTuple(tuple);
}

} /* namespace souffle */
