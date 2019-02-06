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
    const AnalysisType* bound;

public:
    FixedConstraint(const AstArgument* variable, const AnalysisType* bound)
            : variable(variable), bound(bound){};
    FixedConstraint(const FixedConstraint& other) = default;
    FixedConstraint& operator=(const FixedConstraint& other) = default;
    typeSol resolve(const typeSol existing, TypeLattice& lattice) override {
        assert(existing.find(variable) != existing.end() && "Variable does not have a type");
        typeSol ret(existing);
        ret[variable] = &lattice.meet(*existing.at(variable), *bound);
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) override {
        assert(solution.find(variable) != solution.end() && "Variable does not have a type");
        return lattice.isSubtype(*solution.at(variable), *bound);
    }
    void print(std::ostream& os) const override {
        os << *variable << "<:" << *bound;
    }
};

class VarConstraint : public TypeConstraint {
private:
    const AstArgument* variable;
    const AstArgument* bound;

public:
    VarConstraint(const AstArgument* variable, const AstArgument* bound) : variable(variable), bound(bound){};
    VarConstraint(const VarConstraint& other) = default;
    VarConstraint& operator=(const VarConstraint& other) = default;
    typeSol resolve(const typeSol existing, TypeLattice& lattice) override {
        assert(existing.find(variable) != existing.end() && "Variable does not have a type");
        assert(existing.find(bound) != existing.end() && "Bound does not have a type");
        typeSol ret(existing);
        ret[variable] = &lattice.meet(*existing.at(variable), *existing.at(bound));
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) override {
        assert(solution.find(variable) != solution.end() && "Variable does not have a type");
        assert(solution.find(bound) != solution.end() && "Bound does not have a type");
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
    UnionConstraint(const UnionConstraint& other) = default;
    UnionConstraint& operator=(const UnionConstraint& other) = default;
    typeSol resolve(const typeSol existing, TypeLattice& lattice) override {
        assert(existing.find(variable) != existing.end() && "Variable does not have a type");
        assert(existing.find(firstBound) != existing.end() && "First bound does not have a type");
        assert(existing.find(secondBound) != existing.end() && "Second bound does not have a type");
        typeSol ret(existing);
        ret[variable] = &lattice.meet(
                *existing.at(variable), lattice.join(*existing.at(firstBound), *existing.at(secondBound)));
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) override {
        assert(solution.find(variable) != solution.end() && "Variable does not have a type");
        assert(solution.find(firstBound) != solution.end() && "First bound does not have a type");
        assert(solution.find(secondBound) != solution.end() && "Second bound does not have a type");
        return lattice.isSubtype(
                *solution.at(variable), lattice.join(*solution.at(firstBound), *solution.at(secondBound)));
    }
    void print(std::ostream& os) const override {
        os << *variable << "<:(" << *firstBound << "∪" << *secondBound << ")";
    }
};

class ImplicationConstraint : public TypeConstraint {
private:
    std::vector<FixedConstraint> requirements{};
    FixedConstraint result;

public:
    ImplicationConstraint(const AstArgument* variable, const AnalysisType* bound) : result(variable, bound){};
    ImplicationConstraint(const ImplicationConstraint& other) = default;
    ImplicationConstraint& operator=(const ImplicationConstraint& other) = default;
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

class TypeConstraints {
private:
    std::vector<FixedConstraint> fixedCons{};
    std::vector<VarConstraint> varCons{};
    std::vector<UnionConstraint> unionCons{};
    std::vector<ImplicationConstraint> implCons{};

public:
    TypeConstraints() = default;
    TypeConstraints(FixedConstraint con) : fixedCons(1, con) {}
    TypeConstraints(VarConstraint con) : varCons(1, con) {}
    TypeConstraints(UnionConstraint con) : unionCons(1, con) {}
    TypeConstraints(ImplicationConstraint con) : implCons(1, con) {}
    void addConstraint(FixedConstraint con) {
        fixedCons.push_back(con);
    }
    void addConstraint(VarConstraint con) {
        varCons.push_back(con);
    }
    void addConstraint(UnionConstraint con) {
        unionCons.push_back(con);
    }
    void addConstraint(ImplicationConstraint con) {
        implCons.push_back(con);
    }
    void addAll(const TypeConstraints& other) {
        fixedCons.insert(fixedCons.end(), other.fixedCons.begin(), other.fixedCons.end());
        varCons.insert(varCons.end(), other.varCons.begin(), other.varCons.end());
        unionCons.insert(unionCons.end(), other.unionCons.begin(), other.unionCons.end());
        implCons.insert(implCons.end(), other.implCons.begin(), other.implCons.end());
    }
};

TypeConstraints getConstraints(
        const TypeLattice& lattice, const AstClause& clause, const AstProgram& program) {
    class ConstraintFinder : public AstVisitor<TypeConstraints> {
    private:
        const TypeLattice& lattice;
        const AstProgram& program;

    public:
        ConstraintFinder(const TypeLattice& lattice, const AstProgram& program)
                : lattice(lattice), program(program) {}
        // By default, just extract the constraints generated by all children
        TypeConstraints visitNode(const AstNode& node) {
            TypeConstraints cons;
            for (const AstNode* cur : node.getChildNodes()) {
                cons.addAll(visit(*cur));
            }
            return cons;
        }
        TypeConstraints visitCounter(const AstCounter& counter) {
            return TypeConstraints(FixedConstraint(&counter, &lattice.getConstant(Kind::NUMBER)));
        }
        TypeConstraints visitNumberConstant(const AstNumberConstant& constant) {
            return TypeConstraints(FixedConstraint(&constant, &lattice.getConstant(Kind::NUMBER)));
        }
        TypeConstraints visitStringConstant(const AstStringConstant& constant) {
            return TypeConstraints(FixedConstraint(&constant, &lattice.getConstant(Kind::SYMBOL)));
        }
        TypeConstraints visitNullConstant(const AstNullConstant& constant) {
            return TypeConstraints(FixedConstraint(&constant, &lattice.getConstant(Kind::RECORD)));
        }
        TypeConstraints visitIntrinsicFunctor(const AstIntrinsicFunctor& functor) {
            TypeConstraints cons = visitNode(functor);
            if (functor.getFunction() == FunctorOp::MAX || functor.getFunction() == FunctorOp::MIN) {
                cons.addConstraint(UnionConstraint(&functor, functor.getArg(0), functor.getArg(1)));
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
                cons.addConstraint(FixedConstraint(&functor, &outType));
                ImplicationConstraint constCons(&functor, &outType.getConstant());
                for (size_t i = 0; i < functor.getArity(); ++i) {
                    if (functor.acceptsSymbols(i)) {
                        constCons.addRequirement(
                                FixedConstraint(functor.getArg(i), &lattice.getPrimitive(Kind::SYMBOL)));
                    } else if (functor.acceptsNumbers(i)) {
                        constCons.addRequirement(
                                FixedConstraint(functor.getArg(i), &lattice.getPrimitive(Kind::NUMBER)));
                    } else {
                        assert(false && "Unsupported functor input type");
                    }
                }
                cons.addConstraint(constCons);
            }
            return cons;
        }
        TypeConstraints visitUserDefinedFunctor(const AstUserDefinedFunctor& functor) {
            TypeConstraints cons = visitNode(functor);
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
            cons.addConstraint(FixedConstraint(&functor, &outType));
            ImplicationConstraint constCons(&functor, &outType.getConstant());
            assert(funDecl->getArgCount() == functor.getArgCount() && "Functor has incorrect arity");
            for (size_t i = 0; i < functor.getArgCount(); ++i) {
                if (funDecl->acceptsSymbols(i)) {
                    constCons.addRequirement(
                            FixedConstraint(functor.getArg(i), &lattice.getPrimitive(Kind::SYMBOL)));
                } else if (funDecl->acceptsNumbers(i)) {
                    constCons.addRequirement(
                            FixedConstraint(functor.getArg(i), &lattice.getPrimitive(Kind::NUMBER)));
                } else {
                    assert(false && "Unsupported functor input type");
                }
            }
            cons.addConstraint(constCons);
            return cons;
        }
        TypeConstraints visitRecordInit(const AstRecordInit& record) {
            TypeConstraints cons = visitNode(record);
            auto* type = dynamic_cast<const RecordType*>(&lattice.getEnvironment().getType(record.getType()));
            assert(type != nullptr && "Type of record must be a record type");
            assert(record.getArguments().size() == type->getFields().size() &&
                    "Constructor has incorrect number of arguments");
            FixedConstraint firstReq(&record, &lattice.getPrimitive(Kind::RECORD));
            ImplicationConstraint secondCons(&record, &lattice.getType(*type));
            for (size_t i = 0; i < record.getArguments().size(); ++i) {
                const AnalysisType& fieldType = lattice.getType(type->getFields()[i].type);
                ImplicationConstraint curCons(record.getArguments()[i], &fieldType);
                curCons.addRequirement(firstReq);
                cons.addConstraint(curCons);
                secondCons.addRequirement(FixedConstraint(record.getArguments()[i], &fieldType));
            }
            cons.addConstraint(secondCons);
            return cons;
        }
        TypeConstraints visitAggregator(const AstAggregator& aggregate) {
            TypeConstraints cons = visitNode(aggregate);
            if (aggregate.getOperator() == AstAggregator::count ||
                    aggregate.getOperator() == AstAggregator::sum) {
                cons.addConstraint(FixedConstraint(&aggregate, &lattice.getPrimitive(Kind::NUMBER)));
            } else if (aggregate.getOperator() == AstAggregator::min ||
                       aggregate.getOperator() == AstAggregator::max) {
                cons.addConstraint(VarConstraint(&aggregate, aggregate.getTargetExpression()));
            } else {
                assert(false && "Unsupported aggregation operation");
            }
            return cons;
        }
        TypeConstraints visitAtom(const AstAtom& atom) {
            TypeConstraints cons = visitNode(atom);
            AstRelation* relation = program.getRelation(atom.getName());
            assert(relation->getArity() == atom.argSize() && "Atom has incorrect number of arguments");
            for (size_t i = 0; i < atom.argSize(); i++) {
                const AnalysisType& curType = lattice.getType(relation->getAttribute(i)->getTypeName());
                cons.addConstraint(FixedConstraint(atom.getArgument(i), &curType));
            }
            return cons;
        }
        TypeConstraints visitNegation(const AstNegation& negation) {
            // Only return constraints generated by children except the atom being negated
            return visitNode(*negation.getAtom());
        }
        TypeConstraints visitBinaryConstraint(const AstBinaryConstraint& binary) {
            TypeConstraints cons = visitNode(binary);
            if (binary.getOperator() == BinaryConstraintOp::EQ) {
                cons.addConstraint(VarConstraint(binary.getLHS(), binary.getRHS()));
                cons.addConstraint(VarConstraint(binary.getRHS(), binary.getLHS()));
            }
            return cons;
        }
        TypeConstraints visitClause(const AstClause& clause) {
            TypeConstraints cons;
            // Get constraints from body atoms only
            for (const AstLiteral* literal : clause.getBodyLiterals()) {
                cons.addAll(visit(*literal));
            }
            // Get constraints generated by children of the head, not the head itself
            cons.addAll(visitNode(*clause.getHead()));
            return cons;
        }
    } finder(lattice, program);
    return finder.visit(clause);
}

typeSol TypeAnalysis::analyseTypes(const TypeLattice& lattice, const AstClause& clause,
        const AstProgram& program, std::ostream* debugStream) {
    TypeConstraints typeCons = getConstraints(lattice, clause, program);
    // TODO
}

void TypeAnalysis::run(const AstTranslationUnit& translationUnit) {
    // auto* typeEnvAnalysis = translationUnit.getAnalysis<TypeEnvironmentAnalysis>();
    // TypeLattice lattice = TypeLattice(typeEnvAnalysis->getTypeEnvironment());
    // for (const AstRelation* rel : translationUnit.getProgram()->getRelations()) {
    //     for (const AstClause* clause : rel->getClauses()) {
    //         // Perform the type analysis
    //         std::map<const AstArgument*, AnalysisType> clauseArgumentTypes = analyseTypes(*lattice,
    //         *clause); argumentTypes.insert(clauseArgumentTypes.begin(), clauseArgumentTypes.end());
    //     }
    // }
    // TODO
}

void TypeAnalysis::print(std::ostream& os) const {
    // TODO
}

}  // end of namespace souffle
