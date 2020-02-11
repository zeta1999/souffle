/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RecordTable.h
 *
 * Data container to store Records of the Datalog program.
 *
 ***********************************************************************/

#pragma once

#include "CompiledTuple.h"
#include "ParallelUtils.h"
#include "RamTypes.h"
#include <cassert>
#include <iostream>
#include <limits>
#include <map>
#include <unordered_map>
#include <vector>

namespace souffle {

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

class RecordTable {
public:
    RecordTable() = default;
    virtual ~RecordTable() = default;

    /**
     * A function packing a tuple of the given arity into a reference.
     */
    RamDomain pack(RamDomain* tuple, int arity) {
        return getForArity(arity).pack(tuple);
    }

    /**
     * A function packing a tuple of the given arity into a reference.
     */
    template <typename Domain, std::size_t _arity>
    RamDomain pack(ram::Tuple<Domain, _arity> tuple) {
        return getForArity(_arity).pack(tuple.data);
    }

    /**
     * A function obtaining a pointer to the tuple addressed by the given reference.
     */
    RamDomain* unpack(RamDomain ref, int arity) {
        return getForArity(arity).unpack(ref);
    }

    /**
     * A function obtaining a pointer to the tuple addressed by the given reference.
     */
    template<typename Domain, std::size_t _arity>
    ram::Tuple<Domain, _arity> unpackTuple(RamDomain ref) {
        
        ram::Tuple<RamDomain, _arity> tuple;
        RamDomain* data = getForArity(_arity).unpack(ref);

        for (size_t i = 0; i < _arity; ++i) {
            tuple.data[i] = data[i];
        }
        return tuple;
    }

    /**
     * Determines whether the given reference is the null reference encoding
     * the absence of any nested record.
     */
    bool isNull(RamDomain ref) {
        return ref == 0;
    }

private:
    mutable Lock access;
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
};

}  // namespace souffle
