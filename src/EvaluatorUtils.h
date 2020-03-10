/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file EvaluatorUtils.h
 *
 * Defines utility functions used by synthesised and interpreter code.
 *
 ***********************************************************************/

#pragma once

#include "CompiledTuple.h"
#include "RamTypes.h"

namespace souffle::evaluator {

template <typename A, typename F /* Tuple<RamDomain,1> -> void */>
void runRange(A from, A to, A step, F&& go) {
#define GO(x) go(Tuple<RamDomain, 1>{ramBitCast(x)})
    if (0 < step) {
        for (auto x = from; x < to; x += step) {
            GO(x);
        }
    } else if (step < 0) {
        for (auto x = from; to < x; x += step) {
            GO(x);
        }
    } else if (from != to) {
        // `step = 0` edge case, only if non-empty range
        GO(from);
    }
#undef GO
}

template <typename A, typename F /* Tuple<RamDomain,1> -> void */>
void runRange(A from, A to, F&& go) {
    return runRange(from, to, A(from <= to ? 1 : -1), std::forward<F>(go));
}

}  // namespace souffle::evaluator
