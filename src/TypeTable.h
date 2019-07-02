
/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RecordTable.h
 *
 * A unidirectional helper table to store record values for printing.
 *
 ***********************************************************************/

#pragma once

#include <cassert>
#include <map>
#include <vector>

namespace souffle {

class TypeTable {
public:
    TypeTable(int test) : test(test) {
        std::cout << "Created! Value: " << test << std::endl;
    }

    int getTest() const {
        return test;
    }

private:
    int test;
};

}  // namespace souffle
