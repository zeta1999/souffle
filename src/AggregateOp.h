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

#include "RamTypes.h"

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

/**
 * Get type of aggregate.
 *
 * This procedure shouldn't be called on count, as it has many possible types.
 */
inline TypeAttribute getTypeAttributeAggregate(const AggregateOp op) {
    //
    switch (op) {
        case AggregateOp::max:
        case AggregateOp::min:
        case AggregateOp::sum:
            return TypeAttribute::Signed;

        case AggregateOp::fmax:
        case AggregateOp::fmin:
        case AggregateOp::fsum:
            return TypeAttribute::Float;

        case AggregateOp::umax:
        case AggregateOp::umin:
        case AggregateOp::usum:
            return TypeAttribute::Unsigned;

        default:
            break;
    }

    assert(false && "invalid argument");
}

inline bool isOverloadedAggregator(const AggregateOp op) {
    switch (op) {
        case AggregateOp::max:
        case AggregateOp::min:
        case AggregateOp::sum:
            return true;
        default:
            break;
    }

    return false;
}

inline AggregateOp convertOverloadedAggregator(const AggregateOp op, const TypeAttribute type) {
    switch (op) {
        case AggregateOp::sum:
            if (type == TypeAttribute::Float) {
                return AggregateOp::fsum;
            } else if (type == TypeAttribute::Unsigned) {
                return AggregateOp::usum;
            } else {
                assert(false && "Invalid argument;");
            }
            break;
        case AggregateOp::max:
            if (type == TypeAttribute::Float) {
                return AggregateOp::fmax;
            } else if (type == TypeAttribute::Unsigned) {
                return AggregateOp::umax;
            } else {
                assert(false && "Invalid argument;");
            }
            break;
        case AggregateOp::min:
            if (type == TypeAttribute::Float) {
                return AggregateOp::fmin;
            } else if (type == TypeAttribute::Unsigned) {
                return AggregateOp::umin;
            } else {
                assert(false && "Invalid argument;");
            }
            break;
        default:
            assert(false && "Invalid argument");
    }
}

}  // namespace souffle
