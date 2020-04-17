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
    MAX,
    MIN,
    SUM,

    FMAX,
    FMIN,
    FSUM,
    MEAN,

    UMAX,
    UMIN,
    USUM,

    COUNT,
};

// `[min, max]` # of arguments for each function
inline std::pair<uint8_t, uint8_t> aggregateArity(AggregateOp op) {
    switch (op) {
        case AggregateOp::COUNT:
            return {0, 0};

        case AggregateOp::FMAX:
        case AggregateOp::FMIN:
        case AggregateOp::FSUM:
        case AggregateOp::MAX:
        case AggregateOp::MEAN:
        case AggregateOp::MIN:
        case AggregateOp::SUM:
        case AggregateOp::UMAX:
        case AggregateOp::UMIN:
        case AggregateOp::USUM:
            return {1, 1};
    }

    abort();
}

/**
 * Get return type of the aggregate.
 */
inline TypeAttribute getTypeAttributeAggregate(const AggregateOp op) {
    switch (op) {
        case AggregateOp::COUNT:
        case AggregateOp::MAX:
        case AggregateOp::MIN:
        case AggregateOp::SUM:
            return TypeAttribute::Signed;

        case AggregateOp::MEAN:
        case AggregateOp::FMAX:
        case AggregateOp::FMIN:
        case AggregateOp::FSUM:
            return TypeAttribute::Float;

        case AggregateOp::UMAX:
        case AggregateOp::UMIN:
        case AggregateOp::USUM:
            return TypeAttribute::Unsigned;
    }

    assert(false && "invalid argument");
    exit(EXIT_FAILURE);
}

inline bool isOverloadedAggregator(const AggregateOp op) {
    switch (op) {
        case AggregateOp::MAX:
        case AggregateOp::MIN:
        case AggregateOp::SUM:
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
        case AggregateOp::SUM:
            if (type == TypeAttribute::Float) {
                return AggregateOp::FSUM;
            } else if (type == TypeAttribute::Unsigned) {
                return AggregateOp::USUM;
            } else {
                assert(false && "Invalid argument;");
            }
            break;
        case AggregateOp::MAX:
            if (type == TypeAttribute::Float) {
                return AggregateOp::FMAX;
            } else if (type == TypeAttribute::Unsigned) {
                return AggregateOp::UMAX;
            } else {
                assert(false && "Invalid argument;");
            }
            break;
        case AggregateOp::MIN:
            if (type == TypeAttribute::Float) {
                return AggregateOp::FMIN;
            } else if (type == TypeAttribute::Unsigned) {
                return AggregateOp::UMIN;
            } else {
                assert(false && "Invalid argument;");
            }
            break;
        default:
            assert(false && "Invalid argument");
    }
}

}  // namespace souffle
