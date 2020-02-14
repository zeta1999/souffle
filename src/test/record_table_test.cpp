/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file record_table_test.cpp
 *
 * Tests the record table.
 *
 ***********************************************************************/

#include "CompiledTuple.h"
#include "RecordTable.h"
#include "test.h"
#include <functional>
#include <random>
#include <vector>

namespace souffle::test {

#define NUMBER_OF_TESTS 100

TEST(Pack, Tuple) {
    RecordTable recordTable;
    const ram::Tuple<RamDomain, 3> tuple = {{1, 2, 3}};

    RamDomain ref = recordTable.pack(tuple);

    RamDomain* ptr = recordTable.unpack(ref, 3);

    for (size_t i = 0; i < 3; ++i) {
        EXPECT_EQ(tuple[i], ptr[i]);
    }
}

// Generate random tuples
// pack them all
// unpack and test for equality
TEST(PackUnpack, Tuple) {
    constexpr size_t tupleSize = 3;
    using tupleType = ram::Tuple<RamDomain, tupleSize>;

    RecordTable recordTable;

    // Setup random number generation
    std::default_random_engine randomGenerator(3);
    std::uniform_int_distribution<RamDomain> distribution(
            std::numeric_limits<RamDomain>::lowest(), std::numeric_limits<RamDomain>::max());

    auto random = std::bind(distribution, randomGenerator);

    // Tuples that will be packed
    std::vector<tupleType> toPack(NUMBER_OF_TESTS);

    // Tuple reference after they are packed.
    std::vector<RamDomain> tupleRef(NUMBER_OF_TESTS);

    // Generate and pack the tuples
    for (size_t i = 0; i < NUMBER_OF_TESTS; ++i) {
        toPack[i] = {{random(), random(), random()}};
        tupleRef[i] = recordTable.pack(toPack[i]);
    }

    // unpack and test
    for (size_t i = 0; i < NUMBER_OF_TESTS; ++i) {
        auto unpacked = recordTable.unpackTuple<RamDomain, tupleSize>(tupleRef[i]);
        EXPECT_EQ(toPack[i], unpacked);
    }
}

}  // namespace souffle::test
