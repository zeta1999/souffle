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
    ram::Tuple<int, 3> tuple = {{1, 2, 3}};

    RamDomain ref = recordTable.pack(tuple);

    RamDomain* ptr = recordTable.unpack(ref, 3);

    for (size_t i = 0; i < 3; ++i) {
        EXPECT_EQ(tuple[i], ptr[i]);
    }
}

TEST(isNull, Tuple) {
    RecordTable recordTable;

    ram::Tuple<int, 0> null;
    ram::Tuple<int, 2> notNull = {{1, 2}};

    EXPECT_TRUE(recordTable.isNull(null));
    EXPECT_FALSE(recordTable.isNull(notNull));
}

}  // namespace souffle::test
