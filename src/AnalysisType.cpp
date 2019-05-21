/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AnalysisType.cpp
 *
 * Implements a collection of analysis types that may appear in a type lattice.
 *
 ***********************************************************************/

#include "AnalysisType.h"
#include "Util.h"
#include <cassert>
#include <sstream>

namespace souffle {

UnionAnalysisType::UnionAnalysisType(std::set<BaseAnalysisType> baseTypes) : baseTypes(baseTypes) {
    std::stringstream repr;
    repr << join(baseTypes, " | ");
    representation = repr.str();

    assert(!baseTypes.empty() && "empty union is not allowed");
    assert(baseTypes.size() > 1 && "union with one element is a base type");

    kind = (*baseTypes.begin()).getKind();
    assert(kind != Kind::RECORD && "record unions are not supported");
    for (const auto& base : baseTypes) {
        assert(base.getKind() == kind && "all union components must have the same kind");
    }
}

UnionAnalysisType::UnionAnalysisType(std::set<BaseAnalysisType> baseTypes, AstTypeIdentifier name)
        : UnionAnalysisType(baseTypes) {
    setName(name);
}

}  // end of namespace souffle
