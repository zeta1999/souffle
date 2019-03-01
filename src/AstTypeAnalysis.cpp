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
#include "AstFunctorDeclaration.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "AstTranslationUnit.h"
#include "AstType.h"
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

// chekc these ones out
// #include "Constraints.h"
// #include "AstConstraintAnalysis.h"

namespace souffle {

using TypeSolution = std::map<const AstArgument*, const AnalysisType*>;

// TODO: what is getVar actually doign here for non-vars? seems to be called on general arguments a lot -
// maybe rename?
const AstArgument* getVar(std::map<std::string, const AstVariable*>& mapping, const AstArgument* arg) {
    if (dynamic_cast<const AstVariable*>(arg) == nullptr) {
        return arg;
    }

    // TODO: commenting
    const auto* var = dynamic_cast<const AstVariable*>(arg);
    if (mapping.find(var->getName()) == mapping.end()) {
        mapping[var->getName()] = var;
    }
    return mapping.at(var->getName());
}

// TODO: maybe turn TypeSolution into a class that contains these constraints instead?
// TODO: type-solution should probably contain the type lattice?

// TODO: how does the lattice update on resolution? why is it passed in?
// TODO: equal method?
/** A type constraint imposed on an argument in a Datalog program */
class TypeConstraint {
public:
    /** Updates the given type mapping to satisfy the represented type constraint */
    virtual TypeSolution resolve(const TypeSolution, TypeLattice& lattice) const = 0;

    /** Checks if a given type mapping satisfies the represented type constraint */
    virtual bool isSatisfied(const TypeSolution existing, TypeLattice& lattice) const = 0;

    /** Output to a given output stream */
    virtual void print(std::ostream& os) const = 0;

    /** Print the constraint onto an output stream */
    friend std::ostream& operator<<(std::ostream& out, const TypeConstraint& other) {
        other.print(out);
        return out;
    }
};

// TODO: existing->current

/**
 * Subclass of type constraint that represents a fixed constraint.
 * i.e. t <: T, where t is a type variable and T is a type constant.
 */
class FixedConstraint : public TypeConstraint {
private:
    const AstArgument* argument;
    const AnalysisType* imposedType;

public:
    FixedConstraint(const AstArgument* argument, const AnalysisType* imposedType)
            : argument(argument), imposedType(imposedType) {}
    FixedConstraint(const FixedConstraint& other) = default;
    FixedConstraint& operator=(const FixedConstraint& other) = default;

    /** Updates the given type mapping to satisfy the represented type constraint */
    TypeSolution resolve(const TypeSolution existing, TypeLattice& lattice) const override {
        assert(existing.find(argument) != existing.end() && "Argument does not have a type");

        // create a copy of the existing type solution
        // TODO: why a copy and not update hte type solution itself?
        TypeSolution ret(existing);

        // argument is now constrained to be a subtype of the given type
        ret[argument] = lattice.meet(existing.at(argument), imposedType);
        assert(isSatisfied(ret, lattice) && "Constraint resolution failed");
        return ret;
    }

    /** Checks if a given type mapping satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution solution, TypeLattice& lattice) const override {
        assert(solution.find(argument) != solution.end() && "Argument does not have a type");
        // TODO: getType if class converted
        return lattice.isSubtype(solution.at(argument), imposedType);
    }

    void print(std::ostream& os) const override {
        os << "type(" << *argument << ") <: " << *imposedType;
    }
};

/**
 * Subclass of type constraint that represents a variable constraint.
 * i.e. t1 <: t2, where t1 and t2 are type variables.
 */
class VarConstraint : public TypeConstraint {
private:
    const AstArgument* left;
    const AstArgument* right;

public:
    VarConstraint(const AstArgument* left, const AstArgument* right) : left(left), right(right) {}
    VarConstraint(const VarConstraint& other) = default;
    VarConstraint& operator=(const VarConstraint& other) = default;

    /** Updates the given type mapping to satisfy the represented type constraint */
    TypeSolution resolve(const TypeSolution existing, TypeLattice& lattice) const override {
        assert(existing.find(left) != existing.end() && "Lower bound does not have a type");
        assert(existing.find(right) != existing.end() && "Upper bound does not have a type");

        // create a copy of the existing type solution
        TypeSolution ret(existing);

        // left is now constraint to be a subtype of right
        ret[left] = lattice.meet(existing.at(left), existing.at(right));
        assert(isSatisfied(ret, lattice) && "Resolving constraint failed");
        return ret;
    }

    /** Checks if a given type mapping satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution solution, TypeLattice& lattice) const override {
        assert(solution.find(left) != solution.end() && "Lower bound does not have a type");
        assert(solution.find(right) != solution.end() && "Upper bound does not have a type");
        return lattice.isSubtype(solution.at(left), solution.at(right));
    }

    void print(std::ostream& os) const override {
        os << "type(" << *left << ") <: type(" << *right << ")";
    }
};

/**
 * Subclass of type constraint that represents a union constraint.
 * i.e. t <: t1 U t2, where t, t1, and t2 are type variables.
 */
class UnionConstraint : public TypeConstraint {
private:
    // TODO: change to a vector of bounds instead
    const AstArgument* argument;
    const AstArgument* firstBound;
    const AstArgument* secondBound;

public:
    UnionConstraint(
            const AstArgument* argument, const AstArgument* firstBound, const AstArgument* secondBound)
            : argument(argument), firstBound(firstBound), secondBound(secondBound) {}
    UnionConstraint(const UnionConstraint& other) = default;
    UnionConstraint& operator=(const UnionConstraint& other) = default;

    /** Updates the given type mapping to satisfy the represented type constraint */
    TypeSolution resolve(const TypeSolution existing, TypeLattice& lattice) const override {
        assert(existing.find(argument) != existing.end() && "Argument does not have a type");
        assert(existing.find(firstBound) != existing.end() && "First bound does not have a type");
        assert(existing.find(secondBound) != existing.end() && "Second bound does not have a type");

        // create a copy of the existing type solution
        TypeSolution ret(existing);

        // argument is now a subtype of any of the right-hand types
        ret[argument] = lattice.meet(
                existing.at(argument), lattice.join(existing.at(firstBound), existing.at(secondBound)));
        assert(isSatisfied(ret, lattice) && "Constraint resolution failed");
        return ret;
    }

    /** Checks if a given type mapping satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution solution, TypeLattice& lattice) const override {
        assert(solution.find(argument) != solution.end() && "Argument does not have a type");
        assert(solution.find(firstBound) != solution.end() && "First bound does not have a type");
        assert(solution.find(secondBound) != solution.end() && "Second bound does not have a type");
        return lattice.isSubtype(
                solution.at(argument), lattice.join(solution.at(firstBound), solution.at(secondBound)));
    }

    void print(std::ostream& os) const override {
        os << "type(" << *argument << ") <: (type(" << *firstBound << ") ∪ type(" << *secondBound << "))";
    }
};

/**
 * Subclass of type constraint that represents an implication constraint.
 * i.e. (t1 <: T1, t2 <: T2, ..., tN <: TN) => (t0 <: T0).
 */
// TODO: look more at this
class ImplicationConstraint : public TypeConstraint {
private:
    std::vector<FixedConstraint> requirements{};
    FixedConstraint result;

public:
    ImplicationConstraint(const AstArgument* variable, const AnalysisType* bound) : result(variable, bound) {}
    ImplicationConstraint(const ImplicationConstraint& other) = default;
    ImplicationConstraint& operator=(const ImplicationConstraint& other) = default;

    void addRequirement(FixedConstraint req) {
        requirements.push_back(req);
    }

    /** Updates the given type mapping to satisfy the represented type constraint */
    TypeSolution resolve(const TypeSolution existing, TypeLattice& lattice) const override {
        // skip if already satisfied
        if (isSatisfied(existing, lattice)) {
            return TypeSolution(existing);
        }

        // not satisfied, so all hold except the consequent
        TypeSolution ret = result.resolve(existing, lattice);
        assert(isSatisfied(ret, lattice) && "Constraint resolution failed");
        return ret;
    }

    /** Checks if a given type mapping satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution solution, TypeLattice& lattice) const override {
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
    // TODO: mvoe private to bottom
private:
    std::vector<FixedConstraint> fixedCons{};
    std::vector<VarConstraint> varCons{};
    std::vector<UnionConstraint> unionCons{};
    std::vector<ImplicationConstraint> implCons{};

public:
    // TODO: spacing out and commening
    TypeConstraints() = default;
    TypeConstraints(FixedConstraint con) : fixedCons(1, con) {}
    TypeConstraints(VarConstraint con) : varCons(1, con) {}
    TypeConstraints(UnionConstraint con) : unionCons(1, con) {}
    TypeConstraints(ImplicationConstraint con) : implCons(1, con) {}

    // TODO: some way to clean this up (like abstract it away)
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
            os << "      " << *con << std::endl;
        }
    }

    friend std::ostream& operator<<(std::ostream& out, const TypeConstraints& other) {
        other.print(out);
        return out;
    }

    TypeSolution solve(TypeLattice& lattice, std::set<const AstArgument*> arguments) const {
        TypeSolution currentSol;
        for (const AstArgument* arg : arguments) {
            currentSol[arg] = lattice.getTop();
        }
        for (FixedConstraint con : fixedCons) {
            currentSol = con.resolve(currentSol, lattice);
        }
        TypeSolution oldSol;
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

// TODO: comment this what is this
class ConstraintFinder : public AstVisitor<TypeConstraints> {
private:
    const TypeLattice& lattice;
    std::map<std::string, const AstVariable*>& variables;
    const AstProgram& program;

public:
    ConstraintFinder(const TypeLattice& lattice, std::map<std::string, const AstVariable*>& variables,
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
        return TypeConstraints(FixedConstraint(&counter, lattice.getConstant(Kind::NUMBER)));
    }

    TypeConstraints visitNumberConstant(const AstNumberConstant& constant) {
        return TypeConstraints(FixedConstraint(&constant, lattice.getConstant(Kind::NUMBER)));
    }

    TypeConstraints visitStringConstant(const AstStringConstant& constant) {
        return TypeConstraints(FixedConstraint(&constant, lattice.getConstant(Kind::SYMBOL)));
    }

    TypeConstraints visitNullConstant(const AstNullConstant& constant) {
        return TypeConstraints(FixedConstraint(&constant, lattice.getConstant(Kind::RECORD)));
    }

    TypeConstraints visitTypeCast(const AstTypeCast& cast) {
        TypeConstraints cons = visitNode(cast);
        cons.addConstraint(FixedConstraint(&cast, lattice.getType(cast.getType())));
        return cons;
    }

    TypeConstraints visitIntrinsicFunctor(const AstIntrinsicFunctor& functor) {
        TypeConstraints cons = visitNode(functor);
        if (functor.getFunction() == FunctorOp::MAX || functor.getFunction() == FunctorOp::MIN) {
            cons.addConstraint(UnionConstraint(
                    &functor, getVar(variables, functor.getArg(0)), getVar(variables, functor.getArg(1))));
        } else {
            Kind kind;
            if (functor.isSymbolic()) {
                kind = Kind::SYMBOL;
            } else if (functor.isNumerical()) {
                kind = Kind::NUMBER;
            } else {
                assert(false && "Unsupported functor output type");
            }
            const PrimitiveAType* outType = lattice.getPrimitive(kind);
            cons.addConstraint(FixedConstraint(&functor, outType));
            ImplicationConstraint constCons(&functor, outType->getConstant());
            for (size_t i = 0; i < functor.getArity(); ++i) {
                if (functor.acceptsSymbols(i)) {
                    constCons.addRequirement(FixedConstraint(
                            getVar(variables, functor.getArg(i)), lattice.getConstant(Kind::SYMBOL)));
                } else if (functor.acceptsNumbers(i)) {
                    constCons.addRequirement(FixedConstraint(
                            getVar(variables, functor.getArg(i)), lattice.getConstant(Kind::NUMBER)));
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
        const PrimitiveAType* outType = lattice.getPrimitive(kind);
        cons.addConstraint(FixedConstraint(&functor, outType));
        ImplicationConstraint constCons(&functor, outType->getConstant());
        assert(funDecl->getArgCount() == functor.getArgCount() && "Functor has incorrect arity");
        for (size_t i = 0; i < functor.getArgCount(); ++i) {
            if (funDecl->acceptsSymbols(i)) {
                constCons.addRequirement(FixedConstraint(
                        getVar(variables, functor.getArg(i)), lattice.getConstant(Kind::SYMBOL)));
            } else if (funDecl->acceptsNumbers(i)) {
                constCons.addRequirement(FixedConstraint(
                        getVar(variables, functor.getArg(i)), lattice.getConstant(Kind::NUMBER)));
            } else {
                assert(false && "Unsupported functor input type");
            }
        }
        cons.addConstraint(constCons);
        return cons;
    }

    TypeConstraints visitRecordInit(const AstRecordInit& record) {
        TypeConstraints cons = visitNode(record);
        auto* type = dynamic_cast<const RecordType*>(&lattice.getEnvironment()->getType(record.getType()));
        assert(type != nullptr && "Type of record must be a record type");
        assert(record.getArguments().size() == type->getFields().size() &&
                "Constructor has incorrect number of arguments");
        FixedConstraint firstReq(&record, lattice.getPrimitive(Kind::RECORD));
        ImplicationConstraint secondCons(&record, lattice.getType(*type));
        for (size_t i = 0; i < record.getArguments().size(); ++i) {
            const AnalysisType* fieldType = lattice.getType(type->getFields()[i].type);
            ImplicationConstraint curCons(getVar(variables, record.getArguments()[i]), fieldType);
            curCons.addRequirement(firstReq);
            cons.addConstraint(curCons);
            secondCons.addRequirement(
                    FixedConstraint(getVar(variables, record.getArguments()[i]), fieldType));
        }
        cons.addConstraint(secondCons);
        return cons;
    }

    TypeConstraints visitAggregator(const AstAggregator& aggregate) {
        TypeConstraints cons = visitNode(aggregate);
        if (aggregate.getOperator() == AstAggregator::count ||
                aggregate.getOperator() == AstAggregator::sum) {
            cons.addConstraint(FixedConstraint(&aggregate, lattice.getPrimitive(Kind::NUMBER)));
        } else if (aggregate.getOperator() == AstAggregator::min ||
                   aggregate.getOperator() == AstAggregator::max) {
            cons.addConstraint(VarConstraint(&aggregate, getVar(variables, aggregate.getTargetExpression())));
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
            const AnalysisType* curType = lattice.getType(relation->getAttribute(i)->getTypeName());
            cons.addConstraint(FixedConstraint(getVar(variables, atom.getArgument(i)), curType));
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
            cons.addConstraint(
                    VarConstraint(getVar(variables, binary.getLHS()), getVar(variables, binary.getRHS())));
            cons.addConstraint(
                    VarConstraint(getVar(variables, binary.getRHS()), getVar(variables, binary.getLHS())));
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
};

// TODO: move some to be static methods?

TypeConstraints getConstraints(const TypeLattice& lattice,
        std::map<std::string, const AstVariable*>& variables, const AstClause& clause,
        const AstProgram& program) {
    return ConstraintFinder(lattice, variables, program).visit(clause);
}

std::set<const AstArgument*> TypeAnalysis::getArguments(
        std::map<std::string, const AstVariable*>& variables, const AstClause& clause) {
    std::set<const AstArgument*> args;
    visitDepthFirst(clause, [&](const AstArgument& arg) { args.insert(getVar(variables, &arg)); });
    return args;
}

TypeSolution TypeAnalysis::analyseTypes(
        TypeLattice& lattice, const AstClause& clause, const AstProgram& program, std::ostream* debugStream) {
    std::map<std::string, const AstVariable*> variables;
    TypeConstraints typeCons = getConstraints(lattice, variables, clause, program);
    TypeSolution types = typeCons.solve(lattice, getArguments(variables, clause));

    if (debugStream != nullptr) {
        *debugStream << "Clause:\n" << clause << std::endl << std::endl;
        *debugStream << "   Constraints:\n" << typeCons << std::endl;
        *debugStream << "   Types:" << std::endl;
        for (std::pair<const AstArgument*, const AnalysisType*> iter : types) {
            *debugStream << "      type(" << *iter.first << ") = " << *iter.second << std::endl;
        }
        *debugStream << std::endl;
    }

    // Make sure each argument object has a type
    visitDepthFirst(clause, [&](const AstVariable& var) { types[&var] = types[getVar(variables, &var)]; });
    return types;
}

void TypeAnalysis::run(const AstTranslationUnit& translationUnit) {
    // Check if debugging information is being generated and note where logs should be sent
    std::ostream* debugStream = nullptr;
    if (!Global::config().get("debug-report").empty()) {
        debugStream = &analysisLogs;
    }
    auto* typeEnvAnalysis = translationUnit.getAnalysis<TypeEnvironmentAnalysis>();
    // TODO: figure out point of environment here
    lattice.setEnvironment(&typeEnvAnalysis->getTypeEnvironment());

    // TODO: when is a lattice not valid?
    if (lattice.isValid()) {
        const AstProgram& program = *translationUnit.getProgram();
        if (debugStream != nullptr && hasInvalidClauses(program)) {
            *debugStream << "Some clauses were skipped as they cannot be typechecked" << std::endl
                         << std::endl;
        }
        for (const AstClause* clause : getValidClauses(program)) {
            // Perform the type analysis
            TypeSolution clauseArgumentTypes = analyseTypes(lattice, *clause, program, debugStream);
            argumentTypes.insert(clauseArgumentTypes.begin(), clauseArgumentTypes.end());
        }
    }
}

void TypeAnalysis::print(std::ostream& os) const {
    if (lattice.isValid()) {
        os << analysisLogs.str();
    } else {
        os << "Unable to run type analysis as valid type lattice could not be constructed";
    }
}

bool TypeAnalysis::hasInvalidClauses(const AstProgram& program) {
    return !getValidClauses(program).empty();
}

std::vector<const AstClause*> TypeAnalysis::getValidClauses(const AstProgram& program) {
    std::vector<const AstClause*> valid;
    for (const AstRelation* rel : program.getRelations()) {
        for (const AstClause* clause : rel->getClauses()) {
            // TODO (azreika [olligobber]) : fix this up
            bool skipClause = false;
            visitDepthFirst(*clause, [&](const AstAtom& atom) {
                auto* relDecl = program.getRelation(atom.getName());
                if (relDecl == nullptr) {
                    skipClause = true;
                } else if (relDecl->getArity() != atom.getArity()) {
                    skipClause = true;
                } else {
                    for (const AstAttribute* attribute : relDecl->getAttributes()) {
                        if (attribute->getTypeName() == "symbol" || attribute->getTypeName() == "number") {
                            continue;
                        }
                        if (program.getType(attribute->getTypeName()) == nullptr) {
                            skipClause = true;
                            break;
                        }
                    }
                }
            });

            visitDepthFirst(*clause, [&](const AstUserDefinedFunctor& fun) {
                AstFunctorDeclaration* funDecl = program.getFunctorDeclaration(fun.getName());
                if (funDecl == nullptr) {
                    skipClause = true;
                } else if (funDecl->getArgCount() != fun.getArgCount()) {
                    skipClause = true;
                }
            });

            visitDepthFirst(*clause, [&](const AstRecordInit& record) {
                const auto* recordType =
                        dynamic_cast<const AstRecordType*>(program.getType(record.getType()));
                if (recordType == nullptr) {
                    skipClause = true;
                } else if (record.getArguments().size() != recordType->getFields().size()) {
                    skipClause = true;
                }
            });

            visitDepthFirst(*clause, [&](const AstTypeCast& cast) {
                if (cast.getType() == "symbol" || cast.getType() == "number") {
                    return;
                }
                if (program.getType(cast.getType()) == nullptr) {
                    skipClause = true;
                }
            });

            if (!skipClause) {
                valid.push_back(clause);
            }
        }
    }
    return valid;
}

}  // end of namespace souffle
