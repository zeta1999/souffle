/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstTypeAnalysis.cpp
 *
 * Implements a collection of type analyses operating on AST constructs.
 *
 ***********************************************************************/

#include "AstTypeAnalysis.h"
#include "AstArgument.h"
#include "AstAttribute.h"
#include "AstClause.h"
// #include "AstConstraintAnalysis.h"
#include "AstFunctorDeclaration.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "AstTranslationUnit.h"
#include "AstType.h"
// #include "AstTypeEnvironmentAnalysis.h"
// #include "AstUtils.h"
// #include "AstVisitor.h"
// #include "Constraints.h"
// #include "Global.h"
#include "AstVisitor.h"
#include "BinaryConstraintOps.h"
#include "FunctorOps.h"
#include "TypeLattice.h"
#include "TypeSystem.h"
#include "Util.h"
#include <cassert>
#include <map>
#include <ostream>
#include <vector>

namespace souffle {

using typeSol = std::map<const AstArgument*, const AnalysisType*>;

class TypeConstraint {
public:
    virtual typeSol resolve(const typeSol existing, TypeLattice& lattice) = 0;
    virtual bool isSatisfied(const typeSol solution, TypeLattice& lattice) = 0;
    virtual void print(std::ostream& os) const = 0;
    friend std::ostream& operator<<(std::ostream& out, const TypeConstraint& other) {
        other.print(out);
        return out;
    }
};

class FixedConstraint : public TypeConstraint {
private:
    const AstArgument* variable;
    const AnalysisType& bound;

public:
    FixedConstraint(const AstArgument* variable, const AnalysisType& bound)
            : variable(variable), bound(bound){};
    typeSol resolve(const typeSol existing, TypeLattice& lattice) override {
        assert(existing.find(variable) != existing.end() && "Variable already has a type");
        typeSol ret(existing);
        ret[variable] = &lattice.meet(*existing.at(variable), bound);
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) override {
        assert(solution.find(variable) != solution.end() && "Variable has a type");
        return lattice.isSubtype(*solution.at(variable), bound);
    }
    void print(std::ostream& os) const override {
        os << *variable << "<:" << bound;
    }
};

class VarConstraint : public TypeConstraint {
private:
    const AstArgument* variable;
    const AstArgument* bound;

public:
    VarConstraint(const AstArgument* variable, const AstArgument* bound) : variable(variable), bound(bound){};
    typeSol resolve(const typeSol existing, TypeLattice& lattice) override {
        assert(existing.find(variable) != existing.end() && "Variable already has a type");
        assert(existing.find(bound) != existing.end() && "Bound already has a type");
        typeSol ret(existing);
        ret[variable] = &lattice.meet(*existing.at(variable), *existing.at(bound));
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) override {
        assert(solution.find(variable) != solution.end() && "Variable has a type");
        assert(solution.find(bound) != solution.end() && "Bound has a type");
        return lattice.isSubtype(*solution.at(variable), *solution.at(bound));
    }
    void print(std::ostream& os) const override {
        os << *variable << "<:" << bound;
    }
};

class UnionConstraint : public TypeConstraint {
private:
    const AstArgument* variable;
    const AstArgument* firstBound;
    const AstArgument* secondBound;

public:
    UnionConstraint(
            const AstArgument* variable, const AstArgument* firstBound, const AstArgument* secondBound)
            : variable(variable), firstBound(firstBound), secondBound(secondBound){};
    typeSol resolve(const typeSol existing, TypeLattice& lattice) override {
        assert(existing.find(variable) != existing.end() && "Variable already has a type");
        assert(existing.find(firstBound) != existing.end() && "First bound already has a type");
        assert(existing.find(secondBound) != existing.end() && "Second bound already has a type");
        typeSol ret(existing);
        ret[variable] = &lattice.meet(
                *existing.at(variable), lattice.join(*existing.at(firstBound), *existing.at(secondBound)));
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) override {
        assert(solution.find(variable) != solution.end() && "Variable has a type");
        assert(solution.find(firstBound) != solution.end() && "First bound has a type");
        assert(solution.find(secondBound) != solution.end() && "Second bound has a type");
        return lattice.isSubtype(
                *solution.at(variable), lattice.join(*solution.at(firstBound), *solution.at(secondBound)));
    }
    void print(std::ostream& os) const override {
        os << *variable << "<:(" << *firstBound << "∪" << *secondBound << ")";
    }
};

class ImplicationConstraint : public TypeConstraint {
private:
    std::vector<FixedConstraint> requirements;
    FixedConstraint result;

public:
    ImplicationConstraint(const AstArgument* variable, const AnalysisType& bound)
            : requirements(), result(variable, bound){};
    void addRequirement(FixedConstraint req) {
        requirements.push_back(req);
    }
    typeSol resolve(const typeSol existing, TypeLattice& lattice) override {
        for (FixedConstraint req : requirements) {
            if (!req.isSatisfied(existing, lattice)) {
                return typeSol(existing);
            }
        }
        return result.resolve(existing, lattice);
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) override {
        for (FixedConstraint req : requirements) {
            if (!req.isSatisfied(solution, lattice)) {
                return true;
            }
        }
        return result.isSatisfied(solution, lattice);
    }
    void print(std::ostream& os) const override {
        os << "(" << join(requirements) << ") -> (" << result << ")";
    }
};

using constraints = std::vector<TypeConstraint>;

constraints getConstraints(const TypeLattice& lattice, const AstClause& clause, const AstProgram& program) {
    class ConstraintFinder : public AstVisitor<constraints> {
    private:
        const TypeLattice& lattice;
        const AstProgram& program;

    public:
        ConstraintFinder(const TypeLattice& lattice, const AstProgram& program)
                : lattice(lattice), program(program) {}
        // By default, just extract the constraints generated by all children
        constraints visitNode(const AstNode& node) {
            constraints cons;
            for (const AstNode* cur : node.getChildNodes()) {
                constraints curCons = visit(*cur);
                cons.insert(cons.end(), curCons.begin(), curCons.end());
            }
            return cons;
        }
        constraints visitCounter(const AstCounter& counter) {
            return constraints(1, FixedConstraint(&counter, lattice.getConstant(Kind::NUMBER)));
        }
        constraints visitNumberConstant(const AstNumberConstant& constant) {
            return constraints(1, FixedConstraint(&constant, lattice.getConstant(Kind::NUMBER)));
        }
        constraints visitStringConstant(const AstStringConstant& constant) {
            return constraints(1, FixedConstraint(&constant, lattice.getConstant(Kind::SYMBOL)));
        }
        constraints visitNullConstant(const AstNullConstant& constant) {
            return constraints(1, FixedConstraint(&constant, lattice.getConstant(Kind::RECORD)));
        }
        constraints visitIntrinsicFunctor(const AstIntrinsicFunctor& functor) {
            constraints cons = visitNode(functor);
            if (functor.getFunction() == FunctorOp::MAX || functor.getFunction() == FunctorOp::MIN) {
                cons.push_back(UnionConstraint(&functor, functor.getArg(0), functor.getArg(1)));
            } else {
                Kind kind;
                if (functor.isSymbolic()) {
                    kind = Kind::SYMBOL;
                } else if (functor.isNumerical()) {
                    kind = Kind::NUMBER;
                } else {
                    assert(false && "Unsupported functor output type");
                }
                const PrimitiveAType& outType = lattice.getPrimitive(kind);
                cons.push_back(FixedConstraint(&functor, outType));
                ImplicationConstraint constCons(&functor, outType.getConstant());
                for (size_t i = 0; i < functor.getArity(); ++i) {
                    if (functor.acceptsSymbols(i)) {
                        constCons.addRequirement(
                                FixedConstraint(functor.getArg(i), lattice.getPrimitive(Kind::SYMBOL)));
                    } else if (functor.acceptsNumbers(i)) {
                        constCons.addRequirement(
                                FixedConstraint(functor.getArg(i), lattice.getPrimitive(Kind::NUMBER)));
                    } else {
                        assert(false && "Unsupported functor input type");
                    }
                }
                cons.push_back(constCons);
            }
            return cons;
        }
        constraints visitUserDefinedFunctor(const AstUserDefinedFunctor& functor) {
            constraints cons = visitNode(functor);
            AstFunctorDeclaration* funDecl = program.getFunctorDeclaration(functor.getName());
            Kind kind;
            if (funDecl->isSymbolic()) {
                kind = Kind::SYMBOL;
            } else if (funDecl->isNumerical()) {
                kind = Kind::NUMBER;
            } else {
                assert(false && "Unsupported functor output type");
            }
            const PrimitiveAType& outType = lattice.getPrimitive(kind);
            cons.push_back(FixedConstraint(&functor, outType));
            ImplicationConstraint constCons(&functor, outType.getConstant());
            assert(funDecl->getArgCount() == functor.getArgCount() && "Functor has correct arity");
            for (size_t i = 0; i < functor.getArgCount(); ++i) {
                if (funDecl->acceptsSymbols(i)) {
                    constCons.addRequirement(
                            FixedConstraint(functor.getArg(i), lattice.getPrimitive(Kind::SYMBOL)));
                } else if (funDecl->acceptsNumbers(i)) {
                    constCons.addRequirement(
                            FixedConstraint(functor.getArg(i), lattice.getPrimitive(Kind::NUMBER)));
                } else {
                    assert(false && "Unsupported functor input type");
                }
            }
            cons.push_back(constCons);
            return cons;
        }
        constraints visitRecordInit(const AstRecordInit& record) {
            constraints cons = visitNode(record);
            auto* type = dynamic_cast<const RecordType*>(&lattice.getEnvironment().getType(record.getType()));
            assert(type != nullptr && "Type of record is a record type");
            assert(record.getArguments().size() == type->getFields().size() &&
                    "Constructor has correct number of arguments");
            FixedConstraint firstReq(&record, lattice.getPrimitive(Kind::RECORD));
            ImplicationConstraint secondCons(&record, lattice.convert(*type));
            for (size_t i = 0; i < record.getArguments().size(); ++i) {
                const AnalysisType& fieldType = lattice.convert(type->getFields()[i].type);
                ImplicationConstraint curCons(record.getArguments()[i], fieldType);
                curCons.addRequirement(firstReq);
                cons.push_back(curCons);
                secondCons.addRequirement(FixedConstraint(record.getArguments()[i], fieldType));
            }
            cons.push_back(secondCons);
            return cons;
        }
        constraints visitAggregator(const AstAggregator& aggregate) {
            constraints cons = visitNode(aggregate);
            if (aggregate.getOperator() == AstAggregator::count ||
                    aggregate.getOperator() == AstAggregator::sum) {
                cons.push_back(FixedConstraint(&aggregate, lattice.getPrimitive(Kind::NUMBER)));
            } else if (aggregate.getOperator() == AstAggregator::min ||
                       aggregate.getOperator() == AstAggregator::max) {
                cons.push_back(VarConstraint(&aggregate, aggregate.getTargetExpression()));
            } else {
                assert(false && "Unsupported aggregation operation");
            }
            return cons;
        }
        constraints visitAtom(const AstAtom& atom) {
            constraints cons = visitNode(atom);
            AstRelation* relation = program.getRelation(atom.getName());
            assert(relation->getArity() == atom.argSize() && "Atom has correct number of arguments");
            for (size_t i = 0; i < atom.argSize(); i++) {
                const AnalysisType& curType = lattice.getType(relation->getAttribute(i)->getTypeName());
                cons.push_back(FixedConstraint(atom.getArgument(i), curType));
            }
            return cons;
        }
        constraints visitNegation(const AstNegation& negation) {
            // Only return constraints generated by children except the atom being negated
            return visitNode(*negation.getAtom());
        }
        constraints visitBinaryConstraint(const AstBinaryConstraint& binary) {
            constraints cons = visitNode(binary);
            if (binary.getOperator() == BinaryConstraintOp::EQ) {
                cons.push_back(VarConstraint(binary.getLHS(), binary.getRHS()));
                cons.push_back(VarConstraint(binary.getRHS(), binary.getLHS()));
            }
            return cons;
        }
        constraints visitClause(const AstClause& clause) {
            constraints cons;
            // Get constraints from body atoms only
            for (const AstLiteral* literal : clause.getBodyLiterals()) {
                constraints curCons = visit(*literal);
                cons.insert(cons.end(), curCons.begin(), curCons.end());
            }
            // Get constraints generated by children of the head, not the head itself
            constraints headCons = visitNode(*clause.getHead());
            cons.insert(cons.end(), headCons.begin(), headCons.end());
            return cons;
        }
    } finder(lattice, program);
    return finder.visit(clause);
}

typeSol TypeAnalysis::analyseTypes(const TypeLattice& lattice, const AstClause& clause,
        const AstProgram& program, std::ostream* debugStream = nullptr) {
    constraints typeCons = getConstraints(lattice, clause, program);
    // TODO
}

void TypeAnalysis::run(const AstTranslationUnit& translationUnit) {
    auto* typeEnvAnalysis = translationUnit.getAnalysis<TypeEnvironmentAnalysis>();
    TypeLattice lattice = TypeLattice(typeEnvAnalysis->getTypeEnvironment());
    for (const AstRelation* rel : translationUnit.getProgram()->getRelations()) {
        for (const AstClause* clause : rel->getClauses()) {
            // Perform the type analysis
            std::map<const AstArgument*, AnalysisType> clauseArgumentTypes = analyseTypes(*lattice, *clause);
            argumentTypes.insert(clauseArgumentTypes.begin(), clauseArgumentTypes.end());

            if (debugStream != nullptr) {
                // Store an annotated clause for printing purposes
                AstClause* annotatedClause = createAnnotatedClause(clause, clauseArgumentTypes);
                annotatedClauses.emplace_back(annotatedClause);
            }
        }
    }
}

void TypeAnalysis::print(std::ostream& os) const {
    // TODO
}

}  // end of namespace souffle
