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

#include "ParallelUtils.h"
#include "RamTypes.h"
#include <cassert>
#include <limits>
#include <map>
#include <unordered_map>
#include <vector>

namespace souffle {

/**
 * A bidirectional mapping between tuples and reference indices.
 */
class RecordMap {
public:
    explicit RecordMap(size_t arity) : arity(arity), i2r(1){};  // note: index 0 element left free

    /**
     * Packs the given tuple -- and may create a new reference if necessary.
     */
    RamDomain pack(const RamDomain* tuple) {
        std::vector<RamDomain> tmp(arity);
        for (size_t i = 0; i < arity; i++) {
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

private:
    /** The arity of the stored tuples */
    size_t arity;

    /** The mapping from tuples to references/indices */
    std::map<std::vector<RamDomain>, RamDomain> r2i;

    /** The mapping from indices to tuples */
    std::vector<std::vector<RamDomain>> i2r;
};

/**
 * @class RecordTable
 *
 * Global pool of re-usable strings
 *
 * RecordTable stores Datalog records and converts them to numbers and vice versa.
 */
class RecordTable {
public:
    RamDomain pack(RamDomain* tuple, size_t arity) {
        return getForArity(arity).pack(tuple);
    }

    RamDomain* unpack(RamDomain ref, size_t arity) {
        return getForArity(arity).unpack(ref);
    }

    RamDomain getNull() {
        return 0;
    }

    bool isNull(RamDomain ref) {
        return ref == getNull();
    }

private:
    RecordMap& getForArity(size_t arity) {
        static std::unordered_map<size_t, RecordMap> maps;
        auto pos = maps.find(arity);
        if (pos == maps.end()) {
            maps.emplace(arity, arity);
        }

        return maps.find(arity)->second;
    }
};

}  // namespace souffle
