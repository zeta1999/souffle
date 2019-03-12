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
    assert(isSatisfied(currentSolution) && "constraint resolution failed");
}

bool UnionConstraint::isSatisfied(const TypeSolution* currentSolution) const {
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

void TypeSolution::generateConstraints() {
    // Helper class to find all constraints imposed by a clause
    class ConstraintFinder : public AstVisitor<void> {
    public:
        // TODO: fix up this constructor once done (some args can be removed - just added for ease in the
        // first run through)
        ConstraintFinder(TypeLattice* lattice, TypeSolution* solver, TypeEnvironment* typeEnvironment,
                AstProgram* program)
                : lattice(lattice), solver(solver), typeEnvironment(typeEnvironment), program(program) {}

        void visitNode(const AstNode& node) {
            // by default, extract the constraints generated by all children
            for (const AstNode* child : node.getChildNodes()) {
                visit(*child);
            }
        }

        void visitCounter(const AstCounter& counter) {
            // counters must be numbers
            solver->addConstraint(
                    std::make_unique<FixedConstraint>(&counter, ConstantAnalysisType(Kind::NUMBER)));
        }

        void visitNumberConstant(const AstNumberConstant& constant) {
            // number constants must actually be numbers
            solver->addConstraint(
                    std::make_unique<FixedConstraint>(&constant, ConstantAnalysisType(Kind::NUMBER)));
        }

        void visitStringConstant(const AstStringConstant& constant) {
            // string constants must actually be strings
            solver->addConstraint(
                    std::make_unique<FixedConstraint>(&constant, ConstantAnalysisType(Kind::SYMBOL)));
        }

        void visitNullConstant(const AstNullConstant& constant) {
            // nils must be record types
            solver->addConstraint(
                    std::make_unique<FixedConstraint>(&constant, ConstantAnalysisType(Kind::RECORD)));
        }

        void visitTypeCast(const AstTypeCast& cast) {
            // extract child constraints
            visitNode(cast);

            // argument must be of the resultant type
            // TODO: getting the kind of an AstTypeIdentifier is the limiting part?
            const AnalysisType* type = lattice->getAnalysisType(cast.getType());
            solver->addConstraint(
                    std::make_unique<FixedConstraint>(&cast, std::unique_ptr<AnalysisType>(type->clone())));
        }

        void visitIntrinsicFunctor(const AstIntrinsicFunctor& functor) {
            // extract child constraints
            visitNode(functor);

            // get the constraints forced by the functor itself
            if (functor.getFunction() == FunctorOp::MAX || functor.getFunction() == FunctorOp::MIN) {
                // result of max and min must be one of the types
                // TODO: why are they different?
                const AstArgument* lhs = solver->getRepresentative(functor.getArg(0));
                const AstArgument* rhs = solver->getRepresentative(functor.getArg(1));
                solver->addConstraint(std::make_unique<UnionConstraint>(&functor, lhs, rhs));
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
                solver->addConstraint(
                        std::make_unique<FixedConstraint>(&functor, TopPrimitiveAnalysisType(kind)));

                // functor applied to constants must give a constant
                // TODO: change implication constraint structure?
                auto constantConstraint =
                        std::make_unique<ImplicationConstraint>(std::make_unique<FixedConstraint>(
                                &functor, std::make_unique<ConstantAnalysisType>(kind)));
                for (size_t i = 0; i < functor.getArity(); i++) {
                    const AstArgument* arg = solver->getRepresentative(functor.getArg(i));
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
                solver->addConstraint(std::move(constantConstraint));
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
            solver->addConstraint(
                    std::make_unique<FixedConstraint>(&functor, TopPrimitiveAnalysisType(kind)));

            // functor applied to constants must give a constant
            auto constantConstraint =
                    std::make_unique<ImplicationConstraint>(std::make_unique<FixedConstraint>(
                            &functor, std::make_unique<ConstantAnalysisType>(kind)));
            for (size_t i = 0; i < functor.getArgCount(); i++) {
                const AstArgument* arg = solver->getRepresentative(functor.getArg(i));
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
            solver->addConstraint(std::move(constantConstraint));
        }

        void visitRecordInit(const AstRecordInit& record) {
            // extract child constraints
            visitNode(record);

            // two scenarios must be considered:
            // (1) the type of the record has been bound to any record type:
            //      - the record is therefore directly grounded
            //      - all arguments must be bound to their expected type
            // (2) all arguments have been bound to their expected type
            //      - the record is therefore grounded via its arguments
            //      - the record must be bound to its expected type
            // the semantic checker
            const auto* rawType =
                    dynamic_cast<const RecordType*>(&typeEnvironment->getType(record.getType()));
            assert(rawType != nullptr && "type of record must be a record type");
            assert(record.getArguments().size() == rawType->getFields().size() &&
                    "record constructor has incorrect number of arguments");

            const auto* recordType = lattice->getAnalysisType(*rawType);
            const auto& fields = rawType->getFields();
            const auto& args = record.getArguments();

            // cover (1)
            for (size_t i = 0; i < args.size(); i++) {
                const AstArgument* arg = solver->getRepresentative(args[i]);
                const AnalysisType* fieldType = lattice->getAnalysisType(fields[i].type);

                // construct the implication constraint
                auto requirement = std::make_unique<FixedConstraint>(
                        &record, std::make_unique<TopPrimitiveAnalysisType>(Kind::RECORD));
                auto consequent = std::make_unique<FixedConstraint>(
                        arg, std::unique_ptr<AnalysisType>(fieldType->clone()));

                auto newConstraint = std::make_unique<ImplicationConstraint>(std::move(consequent));
                newConstraint->addRequirement(std::move(requirement));

                // add it in
                solver->addConstraint(std::move(newConstraint));
            }

            // cover (2)
            auto consequent = std::make_unique<FixedConstraint>(
                    &record, std::unique_ptr<AnalysisType>(recordType->clone()));
            auto finalConstraint = std::make_unique<ImplicationConstraint>(std::move(consequent));

            for (size_t i = 0; i < args.size(); i++) {
                const AstArgument* arg = solver->getRepresentative(args[i]);
                const AnalysisType* fieldType = lattice->getAnalysisType(fields[i].type);

                // add the new requirement
                auto newRequirement = std::make_unique<FixedConstraint>(
                        arg, std::unique_ptr<AnalysisType>(fieldType->clone()));
                finalConstraint->addRequirement(std::move(newRequirement));
            }

            // add in the constraint
            solver->addConstraint(std::move(finalConstraint));
        }

        void visitAggregator(const AstAggregator& aggregate) {
            // extract child constraints
            visitNode(aggregate);

            auto op = aggregate.getOperator();
            if (op == AstAggregator::count || op == AstAggregator::sum) {
                // aggregator type is just a number
                auto newConstraint = std::make_unique<FixedConstraint>(
                        &aggregate, std::make_unique<TopPrimitiveAnalysisType>(Kind::NUMBER));
                solver->addConstraint(std::move(newConstraint));
            } else if (op == AstAggregator::min || op == AstAggregator::max) {
                // aggregator type must match target expression
                const AstArgument* targetExpression =
                        solver->getRepresentative(aggregate.getTargetExpression());
                auto newConstraint = std::make_unique<VariableConstraint>(&aggregate, targetExpression);
                solver->addConstraint(std::move(newConstraint));
            } else {
                assert(false && "unsupported aggregation operator");
            }
        }

        void visitAtom(const AstAtom& atom) {
            // extract child constraints
            visitNode(atom);

            // atom arguments must have the correct type
            AstRelation* rel = program->getRelation(atom.getName());
            assert(rel->getArity() == atom.getArity() && "atom has incorrect number of arguments");
            for (size_t i = 0; i < atom.getArity(); i++) {
                // TODO: abstract away this representative business maybe? with the add constraint thing?
                const AstArgument* arg = solver->getRepresentative(atom.getArgument(i));
                const AnalysisType* expectedType =
                        lattice->getAnalysisType(rel->getAttribute(i)->getTypeName());
                solver->addConstraint(std::make_unique<FixedConstraint>(
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
                const AstArgument* lhs = solver->getRepresentative(binary.getLHS());
                const AstArgument* rhs = solver->getRepresentative(binary.getRHS());
                solver->addConstraint(std::make_unique<VariableConstraint>(lhs, rhs));
                solver->addConstraint(std::make_unique<VariableConstraint>(rhs, lhs));
            }
        }

        void visitClause(const AstClause& clause) {
            // get constraints from body literals only
            for (const AstLiteral* literal : clause.getBodyLiterals()) {
                visit(*literal);
            }

            // get constraints generated by the children of the head
            // the head itself should be ignored
            visitNode(*clause.getHead());
        }

    private:
        // TODO: reorder?
        TypeLattice* lattice;
        TypeSolution* solver;
        TypeEnvironment* typeEnvironment;
        AstProgram* program;  // TODO: reference if kept
    };

    ConstraintFinder(lattice, this, typeEnvironment, program).visit(*clause);
}

}  // end of namespace souffle
