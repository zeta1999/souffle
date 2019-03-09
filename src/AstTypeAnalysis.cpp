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
    assert(isSatisfied(currentSolution) && "constraint resolution failed");
}

bool FixedConstraint::isSatisfied(const TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* existingType = currentSolution->getType(argument);
    return lattice->isSubtype(existingType, imposedType);
}

void VariableConstraint::resolve(TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(lhs) && "lower bound does not have an associated type");
    assert(currentSolution->hasType(rhs) && "upper bound does not have an associated type");

    // construct the new type
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* lhsType = currentSolution->getType(lhs);
    const AnalysisType* rhsType = currentSolution->getType(rhs);
    const AnalysisType* newType = lattice->meet(lhsType, rhsType);

    // update the type
    currentSolution->setType(lhs, newType);
    assert(isSatisfied(currentSolution) && "constraint resolution failed");
}

bool VariableConstraint::isSatisfied(const TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(lhs) && "lower bound does not have an associated type");
    assert(currentSolution->hasType(rhs) && "upper bound does not have an associated type");
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* lhsType = currentSolution->getType(lhs);
    const AnalysisType* rhsType = currentSolution->getType(rhs);
    return lattice->isSubtype(lhsType, rhsType);
}

void UnionConstraint::resolve(TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");
    assert(currentSolution->hasType(firstBound) && "first bound does not have an associated type");
    assert(currentSolution->hasType(secondBound) && "second bound does not have an associated type");

    // get the current types
    const AnalysisType* argType = currentSolution->getType(argument);
    const AnalysisType* firstBoundType = currentSolution->getType(argument);
    const AnalysisType* secondBoundType = currentSolution->getType(argument);

    // create the new type
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* newBound = lattice->join(firstBoundType, secondBoundType);
    const AnalysisType* newType = lattice->meet(argType, newBound);

    // update the type
    currentSolution->setType(argument, newType);
    assert(isSatisfied(currentSolution) && "constraint resolution failed");
}

bool UnionConstraint::isSatisfied(const TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");
    assert(currentSolution->hasType(firstBound) && "first bound does not have an associated type");
    assert(currentSolution->hasType(secondBound) && "second bound does not have an associated type");

    // get the types
    const AnalysisType* argType = currentSolution->getType(argument);
    const AnalysisType* firstBoundType = currentSolution->getType(argument);
    const AnalysisType* secondBoundType = currentSolution->getType(argument);

    // create the expected type
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* newBound = lattice->join(firstBoundType, secondBoundType);
    const AnalysisType* newType = lattice->meet(argType, newBound);

    // check it is satisfied
    return lattice->isSubtype(argType, newType);
}

void ImplicationConstraint::resolve(TypeSolution* currentSolution) const {
    // skip if already satisfied
    if (isSatisfied(currentSolution)) {
        return;
    }

    // not satisfied, so all must hold except the consequent - resolve it
    consequent.resolve(currentSolution);
    assert(isSatisfied(currentSolution) && "constraint resolution failed");
}

bool ImplicationConstraint::isSatisfied(const TypeSolution* currentSolution) const {
    for (const FixedConstraint& req : requirements) {
        if (!req.isSatisfied(currentSolution)) {
            return true;
        }
    }
    return consequent.isSatisfied(currentSolution);
}

} // end of namespace souffle
