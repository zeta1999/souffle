#pragma once

#include "AnalysisType.h"
#include "TypeSystem.h"

namespace souffle {

class TypeLattice {
public:
    // TODO: note that the referenced analysis type will be in the lattice

    /** Finds the highest common subtype (intersection) of two types */
    const AnalysisType* meet(const AnalysisType* first, const AnalysisType* second);

    /** Finds the lowest common supertype (union) of two types */
    const AnalysisType* join(const AnalysisType* first, const AnalysisType* second);

    /** Checks if lhs is a subtype of rhs */
    bool isSubtype(const AnalysisType* lhs, const AnalysisType* rhs) const;

    /**
     * Gets the equivalent type stored in the lattice.
     * If the type does not yet exist in the lattice, it is created.
     */
    const AnalysisType* getStoredType(const AnalysisType& type);

    /**
     * Gets the equivalent analysis type stored in the lattice.
     * If the type does not yet exist in the lattice, it is created.
     */
    // TODO: try getting rid of this
    const AnalysisType* getAnalysisType(const AstTypeIdentifier& type);
    const AnalysisType* getAnalysisType(const Type& type);

private:
    // TODO: add a comparator here
    // TODO: const?
    // TODO: const for all these unique pointers throughout?
    std::set<std::unique_ptr<AnalysisType>> storedTypes;
};

}  // end of namespace souffle
