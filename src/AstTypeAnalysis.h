#pragma once

#include "AnalysisType.h"
#include "AstAnalysis.h"
#include "AstArgument.h"
#include "AstLiteral.h"
#include "TypeLattice.h"
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
    FixedConstraint(const AstArgument* argument, const AnalysisType* imposedType)
            : argument(argument), imposedType(imposedType) {}
    FixedConstraint(const FixedConstraint& other) = default;
    FixedConstraint& operator=(const FixedConstraint& other) = default;

    /** Updates the given type solution to satisfy the represented type constraint */
    void resolve(TypeSolution*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution*) const override;

    /** Clone the type constraint */
    FixedConstraint* clone() const override {
        return new FixedConstraint(argument, imposedType);
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
    const AnalysisType* imposedType;
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
    ImplicationConstraint(const AstArgument* variable, const AnalysisType* bound)
            : consequent(variable, bound) {}
    ImplicationConstraint(const ImplicationConstraint& other) = default;
    ImplicationConstraint& operator=(const ImplicationConstraint& other) = default;

    /** Adds a requirement to the lhs of the implication */
    void addRequirement(FixedConstraint req) {
        requirements.push_back(req);
    }

    /** Updates the given type solution to satisfy the represented type constraint */
    void resolve(TypeSolution*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolution*) const override;

    /** Clone the type constraint */
    ImplicationConstraint* clone() const override;

    /** Output to a given output stream */
    void print(std::ostream& out) const override {
        out << "(" << join(requirements) << ") -> (" << consequent << ")";
    }

protected:
    bool equal(const TypeConstraint& cons) const override {
        assert(dynamic_cast<const ImplicationConstraint*>(&cons) != nullptr);
        const auto& other = static_cast<const ImplicationConstraint&>(cons);
        return requirements == other.requirements && consequent == other.consequent;
    }

private:
    std::vector<FixedConstraint> requirements{};
    FixedConstraint consequent;
};

/**
 * A container representing the solution of a type analysis for each argument,
 * given a set of constraints.
 **/
class TypeSolution {
public:
    TypeSolution(
            std::set<const AstArgument*> arguments, std::set<std::unique_ptr<TypeConstraint>> constraints)
            : lattice(std::make_unique<TypeLattice>()), constraints(std::move(constraints)),
              arguments(arguments) {
        resolveAllConstraints();
    }

    /** Get the type lattice associated with the type solution */
    TypeLattice* getLattice() const {
        return lattice.get();
    }

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

    /** Resolves all constraints until they are simultaneously satisfied */
    void resolveAllConstraints() {
        // restore everything to the top type
        typeMapping.clear();
        for (const auto* arg : arguments) {
            typeMapping[arg] = lattice->getStoredType(TopAnalysisType());
        }

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
    std::unique_ptr<TypeLattice> lattice;
    std::set<std::unique_ptr<TypeConstraint>> constraints;
    std::set<const AstArgument*> arguments;
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
    std::unique_ptr<TypeSolution> typeSolution;
};

}  // end of namespace souffle
