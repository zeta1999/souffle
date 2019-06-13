/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterRecords.cpp
 *
 * Utilities for handling records in the interpreter
 *
 ***********************************************************************/

#include "InterpreterRecords.h"
#include <cassert>
#include <limits>
#include <map>
#include <vector>

namespace souffle {

namespace {

using namespace std;

/**
 * A bidirectional mapping between tuples and reference indices.
 */
template <typename Tuple>
class RecordMap {
    /** The mapping from tuples to references/indices */
    map<vector<Tuple>, RamDomain> r2i;

    /** The mapping from indices to tuples */
    vector<vector<Tuple>> i2r;

public:
    RecordMap() = default;

    /**
     * Packs the given tuple -- and may create a new reference if necessary.
     */
    RamDomain pack(const Tuple& tuple) {
        RamDomain index;

        // try lookup
#pragma omp critical(record_pack)
        {
            auto pos = r2i.find(tuple);
            if (pos != r2i.end()) {
                // take the previously assigned value
                index = pos->second;
            } else {
                // add tuple to index
#pragma omp critical(record_unpack)
                {
                    i2r.push_back(tuple);
                    index = i2r.size() - 1;
                    r2i[tuple] = index;

                    // assert that the new index is smaller than the range
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

}  // namespace

/**
 * The static access function for record maps of certain types.
 */
template <typename Tuple>
RecordMap<Tuple>& getRecordMap() {
    static RecordMap<Tuple> map;
    return map;
}

template <typename Tuple>
RamDomain pack(const Tuple& tuple) {
    return getRecordMap<Tuple>().pack(tuple);
}

template <typename Tuple>
RamDomain* unpack(RamDomain ref) {
    return getRecordMap<Tuple>().unpack(ref);
}

template <typename TupleType>
RamDomain getNull() {
    return 0;
}

template <typename TupleType>
bool isNull(RamDomain ref) {
    return ref == 0;
}

}  // end of namespace souffle
