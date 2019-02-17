/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RelationDataStructure.h
 *
 * Identifies the available data structures.
 ***********************************************************************/

#pragma once

#include <ostream>

namespace souffle {

/**
 * Data structures used for a relation.
 */
enum class RelationDataStructure {
    BTREE,  // btree data-structure
    BRIE,   // btree data-structure
    EQREL   // equivalence relation
};

inline std::ostream& operator<<(std::ostream& os, RelationDataStructure structure) {
    switch (structure) {
        case RelationDataStructure::BTREE:
            os << "btree";
            break;
        case RelationDataStructure::BRIE:
            os << "brie";
            break;
        case RelationDataStructure::EQREL:
            os << "eqrel";
            break;
        default:
            break;
    }

    return os;
}

}  // end of namespace souffle
