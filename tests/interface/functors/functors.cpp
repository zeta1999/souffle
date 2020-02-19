/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file functors.cpp
 *
 * Testing the user-defined functor interface
 *
 ***********************************************************************/
#include <cmath>
#include <cstdint>
#include <cstring>

extern "C" {

int32_t foo(int32_t n, const char* s) {
    return n + strlen(s);
}

int32_t goo(const char* s, int32_t n) {
    return strlen(s) + n;
}

const char* hoo() {
    return "Hello world!\n";
}

const char* ioo(int32_t n) {
    if (n < 0) {
        return "NEG";
    } else if (n == 0) {
        return "ZERO";
    } else {
        return "POS";
    }
}

uint32_t factorial(uint32_t x) {
    if (x == 0) {
        return 1;
    }

    uint32_t accum = 1;

    while (x > 1) {
        accum *= x;
        --x;
    }

    return accum;
}

int32_t rnd(float x) {
    return round(x);
}
}
