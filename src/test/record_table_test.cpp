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

namespace souffle::test {

TEST(Pack, Tuple) {
    RecordTable recordTable;
    const ram::Tuple<int, 3> tuple = {{1, 2, 3}};

    RamDomain ref = recordTable.pack(tuple);

    RamDomain* ptr = recordTable.unpack(ref, 3);

    for (size_t i = 0; i < 3; ++i) {
        EXPECT_EQ(tuple[i], ptr[i]);
    }
}

TEST(PackUnpack, Tuple) {
    RecordTable recordTable;

    const ram::Tuple<int, 3> toPack = {{1, 2, 3}};

    RamDomain ref = recordTable.pack(toPack);

    const ram::Tuple<int, 3> unPacked = recordTable.unpackTuple<int, 3>(ref);

    EXPECT_EQ(toPack, unPacked);
}

}  // namespace souffle::test
