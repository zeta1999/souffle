/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterRecords.h
 *
 * Utilities for handling records in the interpreter
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"

namespace souffle {

/**
 * A function packing a tuple of the given arity into a reference.
 */
template <typename Tuple>
RamDomain pack(const Tuple& tuple);

/**
 * A function obtaining a pointer to the tuple addressed by the given reference.
 */
template <typename Tuple>
RamDomain* unpack(RamDomain ref);

/**
 * Obtains the null-reference constant.
 */
template <typename TupleType>
RamDomain getNull();

/**
 * Determines whether the given reference is the null reference encoding
 * the absence of any nested record.
 */
template <typename TupleType>
bool isNull(RamDomain ref);

}  // end of namespace souffle
