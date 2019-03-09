#pragma once

#include "AnalysisType.h"

namespace souffle {

class TypeLattice {
public:
    // TODO: note that the referenced analysis type will be in the lattice

    // Find the highest common subtype (intersection)
    const AnalysisType* meet(const AnalysisType* first, const AnalysisType* second);

    // Find the lowest common supertype (union)
    const AnalysisType* join(const AnalysisType* first, const AnalysisType* second);

    // Check if lhs is a subtype of rhs
    bool isSubtype(const AnalysisType* first, const AnalysisType* second) const;

    // Get the equivalent type stored in the lattice
    // If the type does not yet exist in the lattice, it is created
    const AnalysisType* getStoredType(const AnalysisType& type);

private:
    // TODO: add a comparator here
    // TODO: const?
    std::set<std::unique_ptr<AnalysisType>> storedTypes;
};

} // end of namespace souffle
