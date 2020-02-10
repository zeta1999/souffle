/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterRecords.h
 *
 * Utilities for handling records in the Interpreter
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"
#include <map>
#include <unordered_map>
#include <vector>

namespace souffle {

class RecordTable {
private:
    mutable Lock access;

    /**
     * A bidirectional mapping between tuples and reference indices.
     */
    class InterpreterRecordMap {
        /** The arity of the stored tuples */
        int arity;

        /** The mapping from tuples to references/indices */
        std::map<std::vector<RamDomain>, RamDomain> r2i;

        /** The mapping from indices to tuples */
        std::vector<std::vector<RamDomain>> i2r;

    public:
        InterpreterRecordMap(int arity) : arity(arity), i2r(1) {}  // note: index 0 element left free

        /**
         * Packs the given tuple -- and may create a new reference if necessary.
         */
        RamDomain pack(const RamDomain* tuple) {
            std::vector<RamDomain> tmp(arity);
            for (int i = 0; i < arity; i++) {
                tmp[i] = tuple[i];
            }

            RamDomain index;
#pragma omp critical(record_pack)
            {
                auto pos = r2i.find(tmp);
                if (pos != r2i.end()) {
                    index = pos->second;
                } else {
#pragma omp critical(record_unpack)
                    {
                        i2r.push_back(tmp);
                        index = i2r.size() - 1;
                        r2i[tmp] = index;

                        // assert that new index is smaller than the range
                        assert(index != std::numeric_limits<RamDomain>::max());
                    }
                }
            }

            return index;
        }

        /**
         * Obtains a pointer to the tuple addressed by the given index.
         */
        RamDomain* unpack(RamDomain index) {
            RamDomain* res;

#pragma omp critical(record_unpack)
            res = &(i2r[index][0]);

            return res;
        }
    };

    std::unordered_map<int, InterpreterRecordMap> maps;

    InterpreterRecordMap& getForArity(int arity) {
        auto lease = access.acquire();
        (void)lease;  // avoid warning;
        auto pos = maps.find(arity);
        if (pos == maps.end()) {
            maps.emplace(arity, arity);
        }

        return maps.find(arity)->second;
    }

public:
    RecordTable() = default;
    virtual ~RecordTable() = default;

    /**
     * A function packing a tuple of the given arity into a reference.
     */
    RamDomain packInterpreter(RamDomain* tuple, int arity) {
        return getForArity(arity).pack(tuple);
    }

    /**
     * A function obtaining a pointer to the tuple addressed by the given reference.
     */
    RamDomain* unpackInterpreter(RamDomain ref, int arity) {
        return getForArity(arity).unpack(ref);
    }

    /**
     * Determines whether the given reference is the null reference encoding
     * the absence of any nested record.
     */
    bool isNullInterpreter(RamDomain ref) {
        return ref == 0;
    }
};

}  // end of namespace souffle
