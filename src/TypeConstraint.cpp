#include "TypeConstraint.h"
#include "AstTypeAnalysis.h"

namespace souffle {

void FixedConstraint::resolve(TypeSolver* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");

    // construct the new type
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* currType = currentSolution->getType(argument);
    const AnalysisType* newType = lattice->meet(currType, imposedType.get());

    // update the type
    // TODO: has to be in the type lattice, check that
    currentSolution->setType(argument, newType);
    assert(isSatisfied(currentSolution) && "fixed constraint resolution failed");
}

bool FixedConstraint::isSatisfied(const TypeSolver* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* existingType = currentSolution->getType(argument);
    return lattice->isSubtype(existingType, imposedType.get());
}

void VariableConstraint::resolve(TypeSolver* currentSolution) const {
    assert(currentSolution->hasType(lhs) && "lower bound does not have an associated type");
    assert(currentSolution->hasType(rhs) && "upper bound does not have an associated type");

    // construct the new type
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* lhsType = currentSolution->getType(lhs);
    const AnalysisType* rhsType = currentSolution->getType(rhs);
    const AnalysisType* newType = lattice->meet(lhsType, rhsType);

    // update the type
    currentSolution->setType(lhs, newType);
    assert(isSatisfied(currentSolution) && "variable constraint resolution failed");
}

bool VariableConstraint::isSatisfied(const TypeSolver* currentSolution) const {
    assert(currentSolution->hasType(lhs) && "lower bound does not have an associated type");
    assert(currentSolution->hasType(rhs) && "upper bound does not have an associated type");
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* lhsType = currentSolution->getType(lhs);
    const AnalysisType* rhsType = currentSolution->getType(rhs);
    return lattice->isSubtype(lhsType, rhsType);
}

void UnionConstraint::resolve(TypeSolver* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");
    const auto& bounds = getBounds();
    for (const auto& bound : bounds) {
        assert(currentSolution->hasType(bound) &&
                "bound in union constraint does not have an associated type");
    }

    // get the current types
    const AnalysisType* argType = currentSolution->getType(argument);
    std::vector<const AnalysisType*> boundTypes;
    for (const auto& bound : bounds) {
        boundTypes.push_back(currentSolution->getType(bound));
    }

    // -- create the new type --
    TypeLattice* lattice = currentSolution->getLattice();

    // take the union of the bound types
    const auto bottomType = BottomAnalysisType();
    const AnalysisType* upperBound = &bottomType;
    for (const auto& boundType : boundTypes) {
        upperBound = lattice->join(upperBound, boundType);
    }

    // intersect it with the existing type
    const AnalysisType* newType = lattice->meet(argType, upperBound);

    // update the type
    currentSolution->setType(argument, newType);
    assert(isSatisfied(currentSolution) && "union constraint resolution failed");
}

bool UnionConstraint::isSatisfied(const TypeSolver* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");
    const auto& bounds = getBounds();
    for (const auto& bound : bounds) {
        assert(currentSolution->hasType(bound) &&
                "bound in union constraint does not have an associated type");
    }

    // get the types
    const AnalysisType* argType = currentSolution->getType(argument);
    std::vector<const AnalysisType*> boundTypes;
    for (const auto& bound : bounds) {
        boundTypes.push_back(currentSolution->getType(bound));
    }

    // -- create the expected type --
    TypeLattice* lattice = currentSolution->getLattice();

    // take the union of the bound types
    const auto bottomType = BottomAnalysisType();
    const AnalysisType* upperBound = &bottomType;
    for (const auto& boundType : boundTypes) {
        upperBound = lattice->join(upperBound, boundType);
    }

    // intersect it with the existing type
    const AnalysisType* newType = lattice->meet(argType, upperBound);

    // check it is satisfied
    return lattice->isSubtype(argType, newType);
}

void ImplicationConstraint::resolve(TypeSolver* currentSolution) const {
    // skip if already satisfied
    if (isSatisfied(currentSolution)) {
        return;
    }

    // not satisfied, so all must hold except the consequent - resolve it
    consequent->resolve(currentSolution);
    assert(isSatisfied(currentSolution) && "implication constraint resolution failed");
}

bool ImplicationConstraint::isSatisfied(const TypeSolver* currentSolution) const {
    for (const auto* req : getRequirements()) {
        if (!req->isSatisfied(currentSolution)) {
            return true;
        }
    }
    return consequent->isSatisfied(currentSolution);
}

}  // end of namespace souffle
