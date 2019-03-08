#pragma once

#include "AnalysisType.h"

namespace souffle {

class TypeLattice {
public:
    // TODO: note that the referenced analysis type will be in the lattice

    // Find the highest common subtype (intersection)
    const AnalysisType meet(const AnalysisType first, const AnalysisType second);

    // Find the lowest common supertype (union)
    const AnalysisType join(const AnalysisType first, const AnalysisType second);

    // Check if lhs is a subtype of rhs
    bool isSubtype(const AnalysisType first, const AnalysisType second) const;

private:
    // TODO: do this eventually
    // TODO: should thsee be stored as unique pointers?
    // std::set<const BaseAnalysisTypes> storedBaseTypes;
    // std::set<const UnionAnalysisTypes> storedUnionTypes;
};

} // namespace souffle
