#pragma once

namespace souffle {

enum class AstRelationQualifier {
    INPUT,        // relation read from csv
    OUTPUT,       // relation written to csv
    PRINTSIZE,    // number of tuples written to stdout
    OVERRIDABLE,  // rules defined in component can be overwritten by sub-component
    INLINE,       // inlined
    BRIE,         // uses brie data structure
    BTREE,        // uses btree data structure
    EQREL,        // uses union relation
    INFO,         // info relation for provenance
    SUPPRESSED,   // warnings suppressed
};

}
