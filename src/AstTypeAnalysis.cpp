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
#include "Global.h"
#include "TypeLattice.h"
#include "TypeSystem.h"
#include "Util.h"
#include <cassert>
#include <map>
#include <ostream>
#include <set>
#include <vector>

namespace souffle {

const AstArgument* getVar(std::map<std::string, const AstVariable*>* mapping, const AstArgument* arg) {
    if (dynamic_cast<const AstVariable*>(arg) == nullptr) {
        return arg;
    }
    const auto* var = dynamic_cast<const AstVariable*>(arg);
    if (mapping->find(var->getName()) == mapping->end()) {
        (*mapping)[var->getName()] = var;
    }
    return mapping->at(var->getName());
}

using typeSol = std::map<const AstArgument*, const AnalysisType*>;

class TypeConstraint {
public:
    virtual typeSol resolve(const typeSol existing, TypeLattice& lattice) const = 0;
    virtual bool isSatisfied(const typeSol solution, TypeLattice& lattice) const = 0;
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
    typeSol resolve(const typeSol existing, TypeLattice& lattice) const override {
        assert(existing.find(variable) != existing.end() && "Variable does not have a type");
        typeSol ret(existing);
        ret[variable] = lattice.meet(existing.at(variable), bound);
        assert(isSatisfied(ret, lattice) && "Resolving constraint failed");
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) const override {
        assert(solution.find(variable) != solution.end() && "Variable does not have a type");
        return lattice.isSubtype(solution.at(variable), bound);
    }
    void print(std::ostream& os) const override {
        os << "type(" << *variable << ") <: " << *bound;
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
    typeSol resolve(const typeSol existing, TypeLattice& lattice) const override {
        assert(existing.find(variable) != existing.end() && "Variable does not have a type");
        assert(existing.find(bound) != existing.end() && "Bound does not have a type");
        typeSol ret(existing);
        ret[variable] = lattice.meet(existing.at(variable), existing.at(bound));
        assert(isSatisfied(ret, lattice) && "Resolving constraint failed");
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) const override {
        assert(solution.find(variable) != solution.end() && "Variable does not have a type");
        assert(solution.find(bound) != solution.end() && "Bound does not have a type");
        return lattice.isSubtype(solution.at(variable), solution.at(bound));
    }
    void print(std::ostream& os) const override {
        os << "type(" << *variable << ") <: type(" << *bound << ")";
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
    typeSol resolve(const typeSol existing, TypeLattice& lattice) const override {
        assert(existing.find(variable) != existing.end() && "Variable does not have a type");
        assert(existing.find(firstBound) != existing.end() && "First bound does not have a type");
        assert(existing.find(secondBound) != existing.end() && "Second bound does not have a type");
        typeSol ret(existing);
        ret[variable] = lattice.meet(
                existing.at(variable), lattice.join(existing.at(firstBound), existing.at(secondBound)));
        assert(isSatisfied(ret, lattice) && "Resolving constraint failed");
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) const override {
        assert(solution.find(variable) != solution.end() && "Variable does not have a type");
        assert(solution.find(firstBound) != solution.end() && "First bound does not have a type");
        assert(solution.find(secondBound) != solution.end() && "Second bound does not have a type");
        return lattice.isSubtype(
                solution.at(variable), lattice.join(solution.at(firstBound), solution.at(secondBound)));
    }
    void print(std::ostream& os) const override {
        os << "type(" << *variable << ") <: (type(" << *firstBound << ") ∪ type(" << *secondBound << "))";
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
    typeSol resolve(const typeSol existing, TypeLattice& lattice) const override {
        if (isSatisfied(existing, lattice)) {
            return typeSol(existing);
        }
        typeSol ret = result.resolve(existing, lattice);
        assert(isSatisfied(ret, lattice) && "Resolving constraint failed");
        return ret;
    }
    bool isSatisfied(const typeSol solution, TypeLattice& lattice) const override {
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
    std::vector<const TypeConstraint*> getAll() const {
        std::vector<const TypeConstraint*> all;
        for (const FixedConstraint& con : fixedCons) {
            all.push_back(&con);
        }
        for (const VarConstraint& con : varCons) {
            all.push_back(&con);
        }
        for (const UnionConstraint& con : unionCons) {
            all.push_back(&con);
        }
        for (const ImplicationConstraint& con : implCons) {
            all.push_back(&con);
        }
        return all;
    }
    void print(std::ostream& os) const {
        for (const TypeConstraint* con : getAll()) {
            os << "   " << *con << std::endl;
        }
    }
    friend std::ostream& operator<<(std::ostream& out, const TypeConstraints& other) {
        other.print(out);
        return out;
    }
    typeSol solve(TypeLattice& lattice, std::set<const AstArgument*> arguments) const {
        typeSol currentSol;
        for (const AstArgument* arg : arguments) {
            currentSol[arg] = &lattice.getTop();
        }
        for (FixedConstraint con : fixedCons) {
            currentSol = con.resolve(currentSol, lattice);
        }
        typeSol oldSol;
        do {
            oldSol = currentSol;
            for (VarConstraint cons : varCons) {
                currentSol = cons.resolve(currentSol, lattice);
            }
            for (UnionConstraint cons : unionCons) {
                currentSol = cons.resolve(currentSol, lattice);
            }
            for (ImplicationConstraint cons : implCons) {
                currentSol = cons.resolve(currentSol, lattice);
            }
        } while (oldSol != currentSol);
        for (const TypeConstraint* con : getAll()) {
            assert(con->isSatisfied(oldSol, lattice) && "All constraints are satisified");
        }
        return oldSol;
    }
};

TypeConstraints getConstraints(const TypeLattice& lattice,
        std::map<std::string, const AstVariable*>* variables, const AstClause& clause,
        const AstProgram& program) {
    class ConstraintFinder : public AstVisitor<TypeConstraints> {
    private:
        const TypeLattice& lattice;
        std::map<std::string, const AstVariable*>* variables;
        const AstProgram& program;

    public:
        ConstraintFinder(const TypeLattice& lattice, std::map<std::string, const AstVariable*>* variables,
                const AstProgram& program)
                : lattice(lattice), variables(variables), program(program) {}
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
        TypeConstraints visitTypeCast(const AstTypeCast& cast) {
            TypeConstraints cons = visitNode(cast);
            cons.addConstraint(FixedConstraint(&cast, &lattice.getType(cast.getType())));
            return cons;
        }
        TypeConstraints visitIntrinsicFunctor(const AstIntrinsicFunctor& functor) {
            TypeConstraints cons = visitNode(functor);
            if (functor.getFunction() == FunctorOp::MAX || functor.getFunction() == FunctorOp::MIN) {
                cons.addConstraint(UnionConstraint(&functor, getVar(variables, functor.getArg(0)),
                        getVar(variables, functor.getArg(1))));
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
                        constCons.addRequirement(FixedConstraint(
                                getVar(variables, functor.getArg(i)), &lattice.getConstant(Kind::SYMBOL)));
                    } else if (functor.acceptsNumbers(i)) {
                        constCons.addRequirement(FixedConstraint(
                                getVar(variables, functor.getArg(i)), &lattice.getConstant(Kind::NUMBER)));
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
                    constCons.addRequirement(FixedConstraint(
                            getVar(variables, functor.getArg(i)), &lattice.getConstant(Kind::SYMBOL)));
                } else if (funDecl->acceptsNumbers(i)) {
                    constCons.addRequirement(FixedConstraint(
                            getVar(variables, functor.getArg(i)), &lattice.getConstant(Kind::NUMBER)));
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
                ImplicationConstraint curCons(getVar(variables, record.getArguments()[i]), &fieldType);
                curCons.addRequirement(firstReq);
                cons.addConstraint(curCons);
                secondCons.addRequirement(
                        FixedConstraint(getVar(variables, record.getArguments()[i]), &fieldType));
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
                cons.addConstraint(
                        VarConstraint(&aggregate, getVar(variables, aggregate.getTargetExpression())));
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
                cons.addConstraint(FixedConstraint(getVar(variables, atom.getArgument(i)), &curType));
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
                cons.addConstraint(VarConstraint(
                        getVar(variables, binary.getLHS()), getVar(variables, binary.getRHS())));
                cons.addConstraint(VarConstraint(
                        getVar(variables, binary.getRHS()), getVar(variables, binary.getLHS())));
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
    } finder(lattice, variables, program);
    return finder.visit(clause);
}

std::set<const AstArgument*> TypeAnalysis::getArguments(
        std::map<std::string, const AstVariable*>* variables, const AstClause& clause) {
    std::set<const AstArgument*> args;
    visitDepthFirst(clause, [&](const AstArgument& arg) { args.insert(getVar(variables, &arg)); });
    return args;
}

typeSol TypeAnalysis::analyseTypes(
        TypeLattice& lattice, const AstClause& clause, const AstProgram& program, std::ostream* debugStream) {
    std::map<std::string, const AstVariable*> variables;
    TypeConstraints typeCons = getConstraints(lattice, &variables, clause, program);
    typeSol types = typeCons.solve(lattice, getArguments(&variables, clause));
    if (debugStream != nullptr) {
        *debugStream << "Clause:\n" << clause << std::endl << std::endl;
        *debugStream << "Constraints:\n" << typeCons << std::endl;
        *debugStream << "Types:" << std::endl;
        for (std::pair<const AstArgument*, const AnalysisType*> iter : types) {
            *debugStream << "   type(" << *iter.first << ") = " << *iter.second << std::endl;
        }
        *debugStream << std::endl;
    }
    // Make sure each argument object has a type
    visitDepthFirst(clause, [&](const AstVariable& var) { types[&var] = types[getVar(&variables, &var)]; });
    return types;
}

void TypeAnalysis::run(const AstTranslationUnit& translationUnit) {
    // Check if debugging information is being generated and note where logs should be sent
    std::ostream* debugStream = nullptr;
    if (!Global::config().get("debug-report").empty()) {
        debugStream = &analysisLogs;
    }
    auto* typeEnvAnalysis = translationUnit.getAnalysis<TypeEnvironmentAnalysis>();
    TypeLattice lattice = TypeLattice(typeEnvAnalysis->getTypeEnvironment());
    const AstProgram* program = translationUnit.getProgram();
    for (const AstRelation* rel : program->getRelations()) {
        for (const AstClause* clause : rel->getClauses()) {
            // Perform the type analysis
            typeSol clauseArgumentTypes = analyseTypes(lattice, *clause, *program, debugStream);
            argumentTypes.insert(clauseArgumentTypes.begin(), clauseArgumentTypes.end());
        }
    }
}

void TypeAnalysis::print(std::ostream& os) const {
    os << analysisLogs.str();
}

}  // end of namespace souffle
