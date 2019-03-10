#include "AstTypeAnalysis.h"
#include <ostream>

namespace souffle {

void FixedConstraint::resolve(TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");

    // construct the new type
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* currType = currentSolution->getType(argument);
    const AnalysisType* newType = lattice->meet(currType, imposedType.get());

    // update the type
    currentSolution->setType(argument, newType);
    assert(isSatisfied(currentSolution) && "constraint resolution failed");
}

bool FixedConstraint::isSatisfied(const TypeSolution* currentSolution) const {
    assert(currentSolution->hasType(argument) && "argument does not have an associated type");
    TypeLattice* lattice = currentSolution->getLattice();
    const AnalysisType* existingType = currentSolution->getType(argument);
    return lattice->isSubtype(existingType, imposedType.get());
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
    consequent->resolve(currentSolution);
    assert(isSatisfied(currentSolution) && "constraint resolution failed");
}

bool ImplicationConstraint::isSatisfied(const TypeSolution* currentSolution) const {
    for (const FixedConstraint* req : getRequirements()) {
        if (!req->isSatisfied(currentSolution)) {
            return true;
        }
    }
    return consequent->isSatisfied(currentSolution);
}

// TODO: make sure "getStoredType" not needed here (dont think it is) - why do we need it? check it's used
// correctly - put note on when to use it
void TypeSolution::generateConstraints() {
    // TODO: maybe type solution add constraint
    // Helper class to find all constraints imposed by a clause
    class ConstraintFinder : public AstVisitor<void> {
    public:
        // TODO: fix up this constructor once done (some args can be removed - just added for ease in the
        // first run through)
        ConstraintFinder(std::set<std::unique_ptr<TypeConstraint>>& constraints, TypeLattice* lattice,
                TypeSolution* sol, AstProgram* program)
                : constraints(constraints), lattice(lattice), sol(sol), program(program) {}

        void visitNode(const AstNode& node) {
            // by default, extract the constraints generated by all children
            for (const AstNode* child : node.getChildNodes()) {
                visit(*child);
            }
        }

        void visitCounter(const AstCounter& counter) {
            // counters must be numbers
            constraints.insert(
                    std::make_unique<FixedConstraint>(&counter, ConstantAnalysisType(Kind::NUMBER)));
        }

        void visitNumberConstant(const AstNumberConstant& constant) {
            // number constants must actually be numbers
            constraints.insert(
                    std::make_unique<FixedConstraint>(&constant, ConstantAnalysisType(Kind::NUMBER)));
        }

        void visitStringConstant(const AstStringConstant& constant) {
            // string constants must actually be strings
            constraints.insert(
                    std::make_unique<FixedConstraint>(&constant, ConstantAnalysisType(Kind::SYMBOL)));
        }

        void visitNullConstant(const AstNullConstant& constant) {
            // nils must be record types
            constraints.insert(
                    std::make_unique<FixedConstraint>(&constant, ConstantAnalysisType(Kind::RECORD)));
        }

        void visitTypeCast(const AstTypeCast& cast) {
            // extract child constraints
            visitNode(cast);

            // argument must be of the resultant type
            // TODO: getting the kind of an AstTypeIdentifier is the limiting part?
            const AnalysisType* type = lattice->getAnalysisType(cast.getType());
            constraints.insert(
                    std::make_unique<FixedConstraint>(&cast, std::unique_ptr<AnalysisType>(type->clone())));
        }

        void visitIntrinsicFunctor(const AstIntrinsicFunctor& functor) {
            // extract child constraints
            visitNode(functor);

            // get the constraints forced by the functor itself
            if (functor.getFunction() == FunctorOp::MAX || functor.getFunction() == FunctorOp::MIN) {
                // result of max and min must be one of the types
                // TODO: why are they different?
                // TODO: shouldnt there be an implication constraint here too?
                const AstArgument* lhs = sol->getRepresentative(functor.getArg(0));
                const AstArgument* rhs = sol->getRepresentative(functor.getArg(1));
                constraints.insert(std::make_unique<UnionConstraint>(&functor, lhs, rhs));
            } else {
                // grab the kind of the functor
                Kind kind;
                if (functor.isSymbolic()) {
                    kind = Kind::SYMBOL;
                } else if (functor.isNumerical()) {
                    kind = Kind::NUMBER;
                } else {
                    assert(false && "unsupported functor output type");
                }

                // restrict the output type of the functor
                constraints.insert(
                        std::make_unique<FixedConstraint>(&functor, TopPrimitiveAnalysisType(kind)));

                // functor applied to constants must give a constant
                // TODO: change implication constraint structure?
                auto constantConstraint =
                        std::make_unique<ImplicationConstraint>(std::make_unique<FixedConstraint>(
                                &functor, std::make_unique<ConstantAnalysisType>(kind)));
                for (size_t i = 0; i < functor.getArity(); i++) {
                    const AstArgument* arg = sol->getRepresentative(functor.getArg(i));
                    if (functor.acceptsSymbols(i)) {
                        constantConstraint->addRequirement(std::make_unique<FixedConstraint>(
                                arg, std::make_unique<ConstantAnalysisType>(Kind::SYMBOL)));
                    } else if (functor.acceptsNumbers(i)) {
                        constantConstraint->addRequirement(std::make_unique<FixedConstraint>(
                                arg, std::make_unique<ConstantAnalysisType>(Kind::NUMBER)));
                    } else {
                        assert(false && "unsupported functor argument type");
                    }
                }
                constraints.insert(std::move(constantConstraint));
            }
        }

        void visitUserDefinedFunctor(const AstUserDefinedFunctor& functor) {
            // extract child constraints
            visitNode(functor);

            // -- get the constraints forced by the functor itself --

            // grab the kind of the functor
            AstFunctorDeclaration* funDecl = program->getFunctorDeclaration(functor.getName());
            Kind kind;
            if (funDecl->isSymbolic()) {
                kind = Kind::SYMBOL;
            } else if (funDecl->isNumerical()) {
                kind = Kind::NUMBER;
            } else {
                assert(false && "unsupported functor out put type");
            }

            // restrict the output type of the functor
            constraints.insert(std::make_unique<FixedConstraint>(&functor, TopPrimitiveAnalysisType(kind)));

            // functor applied to constants must give a constant
            auto constantConstraint =
                    std::make_unique<ImplicationConstraint>(std::make_unique<FixedConstraint>(
                            &functor, std::make_unique<ConstantAnalysisType>(kind)));
            for (size_t i = 0; i < functor.getArgCount(); i++) {
                const AstArgument* arg = sol->getRepresentative(functor.getArg(i));
                if (funDecl->acceptsSymbols(i)) {
                    constantConstraint->addRequirement(std::make_unique<FixedConstraint>(
                            arg, std::make_unique<ConstantAnalysisType>(Kind::SYMBOL)));
                } else if (funDecl->acceptsNumbers(i)) {
                    constantConstraint->addRequirement(std::make_unique<FixedConstraint>(
                            arg, std::make_unique<ConstantAnalysisType>(Kind::NUMBER)));
                } else {
                    assert(false && "unsupported functor argument type");
                }
            }
            constraints.insert(std::move(constantConstraint));
        }

        void visitRecordInit(const AstRecordInit& record) {
            // extract child constraints
            visitNode(record);

            // TODO: handle these constraints!!!
        }

        void visitAggregator(const AstAggregator& aggregate) {
            // extract child constraints
            visitNode(aggregate);

            // TODO: handle these constraints!!!
        }

        void visitAtom(const AstAtom& atom) {
            // extract child constraints
            visitNode(atom);

            // atom arguments must have the correct type
            AstRelation* rel = program->getRelation(atom.getName());
            assert(rel->getArity() == atom.getArity() && "atom has incorrect number of arguments");
            for (size_t i = 0; i < atom.getArity(); i++) {
                // TODO: abstract away this representative business maybe? with the add constraint thing?
                const AstArgument* arg = sol->getRepresentative(atom.getArgument(i));
                const AnalysisType* expectedType =
                        lattice->getAnalysisType(rel->getAttribute(i)->getTypeName());
                constraints.insert(std::make_unique<FixedConstraint>(
                        arg, std::unique_ptr<AnalysisType>(expectedType->clone())));
            }
        }

        void visitNegation(const AstNegation& negation) {
            // only extract child constraints of the internal atom
            return visitNode(*negation.getAtom());
        }

        void visitBinaryConstraint(const AstBinaryConstraint& binary) {
            // extract child constraints
            visitNode(binary);

            // equality implies equivalent types
            if (binary.getOperator() == BinaryConstraintOp::EQ) {
                const AstArgument* lhs = sol->getRepresentative(binary.getLHS());
                const AstArgument* rhs = sol->getRepresentative(binary.getRHS());
                constraints.insert(std::make_unique<VariableConstraint>(lhs, rhs));
                constraints.insert(std::make_unique<VariableConstraint>(rhs, lhs));
            }
        }

        void visitClause(const AstClause& clause) {
            // get constraints from body literals only
            for (const AstLiteral* literal : clause.getBodyLiterals()) {
                // TODO: do i have to put "visitLiteral"?
                visit(*literal);
            }

            // get constraints generated by the children of the head,
            // but not the head itself
            visitNode(*clause.getHead());
        }

    private:
        // TODO: reorder?
        std::set<std::unique_ptr<TypeConstraint>>& constraints;
        TypeLattice* lattice;
        TypeSolution* sol;
        AstProgram* program;  // TODO: reference if kept
    };

    ConstraintFinder(constraints, lattice, this, program).visit(*clause);
}

}  // end of namespace souffle
