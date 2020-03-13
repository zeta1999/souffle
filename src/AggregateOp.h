/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AggregateOp.h
 *
 * Defines aggregation operators for AST and RAM
 *
 ***********************************************************************/

#pragma once

namespace souffle {

/** Types of aggregation functions */
enum class AggregateOp {
    // int
    max,
    min,
    sum,

    // uint
    fmax,
    fmin,
    fsum,

    // float
    umax,
    umin,
    usum,

    // Count works on all types.
    count,
};

inline bool canBeUsedOnSymbols(const AggregateOp op) {
    return op == AggregateOp::count;
}

}  // namespace souffle
