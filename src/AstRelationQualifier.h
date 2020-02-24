/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstRelationQualifier.h
 *
 * Identifies the available relation qualifiers.
 ***********************************************************************/

#pragma once

#include "RelationRepresentation.h"

namespace souffle {

enum class AstRelationQualifier {
    INPUT,        // relation read from csv
    OUTPUT,       // relation written to csv
    PRINTSIZE,    // number of tuples written to stdout
    OVERRIDABLE,  // rules defined in component can be overwritten by sub-component
    INLINE,       // inlined
    SUPPRESSED,   // warnings suppressed
    BRIE,         // use brie data-structure
    BTREE,        // use btree data-structure
    EQREL,        // use union data-structure
    INFO,         // info relation for provenance
};

inline bool isRelationRepQualifier(const AstRelationQualifier& q) {
    return q == AstRelationQualifier::BRIE || q == AstRelationQualifier::BTREE ||
           q == AstRelationQualifier::EQREL || q == AstRelationQualifier::INFO;
}

inline RelationRepresentation getRelationRepFromQualifier(const AstRelationQualifier& q) {
    switch (q) {
        case AstRelationQualifier::BRIE:
            return RelationRepresentation::BRIE;
        case AstRelationQualifier::BTREE:
            return RelationRepresentation::BTREE;
        case AstRelationQualifier::EQREL:
            return RelationRepresentation::EQREL;
        case AstRelationQualifier::INFO:
            return RelationRepresentation::INFO;
        default:
            assert(false && "qualifier is not a relation representation");
    }
}

inline std::ostream& operator<<(std::ostream& os, AstRelationQualifier q) {
    switch (q) {
        case AstRelationQualifier::INPUT:
            os << "input";
            break;
        case AstRelationQualifier::OUTPUT:
            os << "output";
            break;
        case AstRelationQualifier::PRINTSIZE:
            os << "printsize";
            break;
        case AstRelationQualifier::OVERRIDABLE:
            os << "overridable";
            break;
        case AstRelationQualifier::INLINE:
            os << "inline";
            break;
        case AstRelationQualifier::SUPPRESSED:
            os << "suppressed";
            break;
        case AstRelationQualifier::BRIE:
            os << "brie";
            break;
        case AstRelationQualifier::BTREE:
            os << "btree";
            break;
        case AstRelationQualifier::EQREL:
            os << "eqrel";
            break;
        case AstRelationQualifier::INFO:
            os << "info";
            break;
        default:
            assert(false && "unhandled relation qualifier");
    }

    return os;
}

}  // namespace souffle
