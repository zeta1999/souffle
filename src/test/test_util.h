/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file test_util.h
 *
 * Some utilities for writing tests
 *
 ***********************************************************************/

#pragma once

#include <algorithm>
#include <iostream>
#include <limits>
#include <random>
#include <vector>

namespace souffle::test {

template <typename T>
std::vector<T> generateRandomVector(const size_t vectorSize, const int seed = 3) {
    std::vector<T> values(vectorSize);

    std::default_random_engine randomGenerator(seed);

    if constexpr (std::is_floating_point<T>::value) {
        // For distribution bonds, following must hold:
        // a ≤ b and b − a ≤ numeric_limits<RealType>::max()
        // (in particular: if given values bounds, it will crash).
        // Investigate better solution.
        std::uniform_real_distribution<T> distribution(-1000, 1000);
        std::generate(values.begin(), values.end(),
                [&distribution, &randomGenerator]() { return distribution(randomGenerator); });
        return values;
    } else {
        std::uniform_int_distribution<T> distribution(
                std::numeric_limits<T>::lowest(), std::numeric_limits<T>::max());
        std::generate(values.begin(), values.end(),
                [&distribution, &randomGenerator]() { return distribution(randomGenerator); });
        return values;
    }
}

}  // namespace souffle::test
