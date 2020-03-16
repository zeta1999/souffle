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
    max,
    min,
    sum,

    fmax,
    fmin,
    fsum,
    mean,

    umax,
    umin,
    usum,

    count,
};

/**
 * Get return type of the aggregate.
 */
inline TypeAttribute getTypeAttributeAggregate(const AggregateOp op) {
    switch (op) {
        case AggregateOp::count:
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
    }

    assert(false && "invalid argument");
    exit(EXIT_FAILURE);
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

/**
 * Convert aggregator to a give type.
 * Eg. sum, float → fsum.
 **/
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
