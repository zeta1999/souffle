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
        case AstRelationQUalifier::EQREL:
            return RelationRepresentation::EQREL;
        case AstRelationQualifier::INFO:
            return RelationRepresentation::INFO;
        default:
            assert(false && "qualifier is not a relation representation");
    }
}

}  // namespace souffle
