#include "AstTypeAnalysis.h"
#include <ostream>

namespace souffle {

void FixedConstraint::resolve(TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");

    // construct the new type
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* currType = currentSolution->getType(argument);
    const AnalysisType* newType = lattice->meet(currType, imposedType);

    // update the type
    currentSolution->setType(argument, newType);
    assert(isSatisfied(currentSolution));
}

bool FixedConstraint::isSatisfied(const TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* existingType = currentSolution->getType(argument);
    return lattice->isSubtype(existingType, imposedType);
}

} // end of namespace souffle
