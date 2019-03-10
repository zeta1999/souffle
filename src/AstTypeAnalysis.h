#pragma once

#include "AnalysisType.h"
#include "AstAnalysis.h"
#include "AstArgument.h"
#include "AstLiteral.h"
#include "AstVisitor.h"
#include "TypeLattice.h"
#include "TypeSystem.h"
#include "Util.h"
#include <ostream>

namespace souffle {

class AstArgument;
class TypeSolution;

/** A type constraint imposed on an argument in a program */
class TypeConstraint {
public:
    /** Updates the given type solution to satisfy the represented type constraint */
    virtual void resolve(TypeSolution*) const = 0;

    /** Checks if the given type solution satisfies the represented type constraint */
    virtual bool isSatisfied(const TypeSolution*) const = 0;

    bool operator==(const TypeConstraint& other) const {
        return this == &other || (typeid(*this) == typeid(other) && equal(other));
    }

    bool operator!=(const TypeConstraint& other) const {
        return !(*this == other);
    }

    /** Clone the type constraint */
    virtual TypeConstraint* clone() const = 0;

    /** Output to a given output stream */
    virtual void print(std::ostream& out) const = 0;

    /** Print the constraint onto an output stream */
    friend std::ostream& operator<<(std::ostream& out, const TypeConstraint& other) {
        other.print(out);
        return out;
    }

protected:
    virtual bool equal(const TypeConstraint& other) const = 0;
};

/**
 * Subclass of type constraint that represents a fixed constraint.
 * i.e. t <: T, where t is a type variable and T is a type constant.
 */
class FixedConstraint : public TypeConstraint {
public:
    FixedConstraint(const AstArgument* argument, std::unique_ptr<AnalysisType> imposedType)
            : argument(argument), imposedType(std::move(imposedType)) {}
    FixedConstraint(const FixedConstraint& other) = default;
    FixedConstraint& operator=(const FixedConstraint& other) = default;

    /** Updates the given type solution to satisfy the represented type constraint */
    void resolve(TypeSolution*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution*) const override;

    /** Clone the type constraint */
    FixedConstraint* clone() const override {
        return new FixedConstraint(argument, std::unique_ptr<AnalysisType>(imposedType->clone()));
    }

    /** Output to a given output stream */
    void print(std::ostream& out) const override {
        out << "type(" << *argument << ") <: " << *imposedType;
    }

protected:
    bool equal(const TypeConstraint& cons) const override {
        assert(dynamic_cast<const FixedConstraint*>(&cons) != nullptr);
        const auto& other = static_cast<const FixedConstraint&>(cons);
        return argument == other.argument && *imposedType == *other.imposedType;
    }

private:
    const AstArgument* argument;
    // TODO: should these be const?
    std::unique_ptr<AnalysisType> imposedType;
};

/**
 * Subclass of type constraint that represents a variable constraint.
 * i.e. t1 <: t2, where t1 and t2 are type variables.
 */
class VariableConstraint : public TypeConstraint {
public:
    VariableConstraint(const AstArgument* lhs, const AstArgument* rhs) : lhs(lhs), rhs(rhs) {}
    VariableConstraint(const VariableConstraint& other) = default;
    VariableConstraint& operator=(const VariableConstraint& other) = default;

    /** Updates the given type solution to satisfy the represented type constraint */
    void resolve(TypeSolution*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution*) const override;

    /** Clone the type constraint */
    VariableConstraint* clone() const override {
        return new VariableConstraint(lhs, rhs);
    }

    /** Output to a given output stream */
    void print(std::ostream& out) const override {
        out << "type(" << *lhs << ") <: type(" << *rhs << ")";
    }

protected:
    bool equal(const TypeConstraint& cons) const override {
        assert(dynamic_cast<const VariableConstraint*>(&cons) != nullptr);
        const auto& other = static_cast<const VariableConstraint&>(cons);
        return lhs == other.lhs && rhs == other.rhs;
    }

private:
    const AstArgument* lhs;
    const AstArgument* rhs;
};

/**
 * Subclass of type constraint that represents a union constraint.
 * i.e. t <: t1 U t2, where t, t1, and t2 are type variables.
 */
class UnionConstraint : public TypeConstraint {
public:
    // TODO: change to a vector of bounds
    UnionConstraint(
            const AstArgument* argument, const AstArgument* firstBound, const AstArgument* secondBound)
            : argument(argument), firstBound(firstBound), secondBound(secondBound) {}
    UnionConstraint(const UnionConstraint& other) = default;
    UnionConstraint& operator=(const UnionConstraint& other) = default;

    /** Updates the given type solution to satisfy the represented type constraint */
    void resolve(TypeSolution*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution*) const override;

    /** Clone the type constraint */
    UnionConstraint* clone() const override {
        return new UnionConstraint(argument, firstBound, secondBound);
    }

    /** Output to a given output stream */
    void print(std::ostream& out) const override {
        out << "type(" << *argument << ") <: (type(" << *firstBound << ") ∪ type(" << *secondBound << "))";
    }

protected:
    bool equal(const TypeConstraint& cons) const override {
        assert(dynamic_cast<const UnionConstraint*>(&cons) != nullptr);
        const auto& other = static_cast<const UnionConstraint&>(cons);
        return argument == other.argument && firstBound == other.firstBound &&
               secondBound == other.secondBound;
    }

private:
    const AstArgument* argument;
    const AstArgument* firstBound;
    const AstArgument* secondBound;
};

/**
 * Subclass of type constraint that represents an implication constraint.
 * i.e. (t1 <: T1, t2 <: T2, ..., tN <: TN) => (t0 <: T0).
 */
class ImplicationConstraint : public TypeConstraint {
public:
    // TODO: sort out the constructors
    // TODO: why not take in any constraint
    ImplicationConstraint(std::unique_ptr<FixedConstraint> consequent) : consequent(std::move(consequent)) {}
    ImplicationConstraint(const ImplicationConstraint& other) = default;
    ImplicationConstraint& operator=(const ImplicationConstraint& other) = default;

    /** Adds a requirement to the lhs of the implication */
    void addRequirement(std::unique_ptr<FixedConstraint> req) {
        requirements.push_back(std::move(req));
    }

    /** Gets requirements on the lhs of the implication */
    std::vector<FixedConstraint*> getRequirements() const {
        return toPtrVector(requirements);
    }

    /** Updates the given type solution to satisfy the represented type constraint */
    void resolve(TypeSolution*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution*) const override;

    /** Clone the type constraint */
    ImplicationConstraint* clone() const override;

    /** Output to a given output stream */
    void print(std::ostream& out) const override {
        out << "(" << join(getRequirements(), ",", print_deref<FixedConstraint*>()) << ") -> (" << *consequent
            << ")";
    }

protected:
    bool equal(const TypeConstraint& cons) const override {
        assert(dynamic_cast<const ImplicationConstraint*>(&cons) != nullptr);
        const auto& other = static_cast<const ImplicationConstraint&>(cons);
        return requirements == other.requirements && consequent == other.consequent;
    }

private:
    // TODO: set instead?
    // TODO: and const?
    std::vector<std::unique_ptr<FixedConstraint>> requirements{};
    std::unique_ptr<FixedConstraint> consequent;
};

/**
 * A container representing the solution of a type analysis for each argument
 * in a given clause.
 **/
// TODO: TypeSolver instead of TypeSolution?
class TypeSolution {
public:
    // TODO: change this to take in a clause, then get cosntraints and resolve them all
    // TODO: fix constraint resolution etc.
    // TODO: lattice here because...?
    // TODO: get rid of things afterwrads
    TypeSolution(TypeLattice* lattice, AstClause* clause, TypeEnvironment* typeEnvironment, AstProgram* program)
            : lattice(lattice), clause(clause), typeEnvironment(typeEnvironment), program(program) {
        generateConstraints();
        resolveConstraints();
    }

    /** Get the type lattice associated with the type solution */
    TypeLattice* getLattice() const {
        return lattice;
    }

    /** Adds a constraint that needs to be satisfied by the type solution. */
    void addConstraint(std::unique_ptr<TypeConstraint> constraint) {
        constraints.insert(std::move(constraint));
    }

    /**
     * Gets the canonical representative of a given argument.
     * - For variables, this is a variable pointer that represents all variables
     * bound to the same name.
     * - For non-variables, this is the argument pointer itself.
     **/
    // TODO: add an instance variable to support this map
    const AstArgument* getRepresentative(const AstArgument* arg);

    // TODO: note that this is a pointer because the type is in the lattice
    /** Get the computed type for the given argument */
    const AnalysisType* getType(const AstArgument* arg) const {
        auto pos = typeMapping.find(arg);
        assert(pos != typeMapping.end() && "argument does not have a type");
        return pos->second;
    }

    /** Set the type of a given argument */
    void setType(const AstArgument* arg, const AnalysisType* type) {
        typeMapping[arg] = type;
    }

    /** Checks if a type has been computed for the given argument */
    bool hasType(const AstArgument* arg) const {
        return typeMapping.find(arg) != typeMapping.end();
    }

    /** Generates the set of type constraints associated with the clause */
    void generateConstraints();

    /** Resolves all stored constraints until they are simultaneously satisfied */
    void resolveConstraints() {
        // restore everything to the top type
        typeMapping.clear();
        visitDepthFirst(*clause, [&](const AstArgument& arg) {
            // TODO: careful of getRepresentative!!!
            typeMapping[&arg] = lattice->getStoredType(TopAnalysisType());
        });

        // TODO: fixed constraints can be done outside the loop
        // TODO: double check this is equivalent to the old one
        // apply each constraint until all are satisfied (fixed point reached)
        bool changed = true;
        while (changed) {
            changed = false;
            for (const auto& cons : constraints) {
                if (!cons->isSatisfied(this)) {
                    changed = true;
                    cons->resolve(this);
                }
            }
        }
    }

    std::set<TypeConstraint*> getConstraints() const {
        return toPtrSet(constraints);
    }

    void print(std::ostream& out) const {
        for (const auto& pair : typeMapping) {
            assert(pair.first != nullptr && "nullptr argument in type solution");
            assert(pair.second != nullptr && "nullptr analysis type in type solution");
            out << "type(" << *pair.first << ") = " << *pair.second << std::endl;
        }
    }

private:
    // TODO: reorder - maybe get rid of some if possible etc.
    TypeLattice* lattice;
    AstClause* clause;
    TypeEnvironment* typeEnvironment;
    AstProgram* program;
    std::set<std::unique_ptr<TypeConstraint>> constraints{};
    std::map<const AstArgument*, const AnalysisType*> typeMapping{};
};

class TypeAnalysis : public AstAnalysis {
public:
    TypeAnalysis() = default;

    static constexpr const char* name = "type-analysis";

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& out) const override;

    /** Get the computed type for the given argument */
    const AnalysisType* getType(const AstArgument* arg) const {
        return typeSolution->getType(arg);
    }

    /** Get the type lattice associated with the analysis */
    TypeLattice* getLattice() const {
        return typeSolution->getLattice();
    }

private:
    // TODO: why is this here
    std::unique_ptr<TypeLattice> typeLattice;
    std::unique_ptr<TypeSolution> typeSolution;
};

}  // end of namespace souffle
