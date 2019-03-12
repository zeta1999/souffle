#pragma once

#include "AnalysisType.h"
#include "AstArgument.h"
#include "AstLiteral.h"
#include "Util.h"
#include <ostream>

namespace souffle {

class TypeSolver;

/** A type constraint imposed on an argument in a program */
class TypeConstraint {
public:
    /** Updates the given type solution to satisfy the represented type constraint */
    virtual void resolve(TypeSolver*) const = 0;

    /** Checks if the given type solution satisfies the represented type constraint */
    virtual bool isSatisfied(const TypeSolver*) const = 0;

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
    void resolve(TypeSolver*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolver*) const override;

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
    void resolve(TypeSolver*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolver*) const override;

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
 * i.e. t <: t1 U t2 U ... U tn, where t, t_i are type variables.
 */
class UnionConstraint : public TypeConstraint {
public:
    UnionConstraint(const AstArgument* argument, std::vector<const AstArgument*> bounds) : argument(argument), bounds(bounds) {}
    UnionConstraint(const UnionConstraint& other) = default;
    UnionConstraint& operator=(const UnionConstraint& other) = default;

    /** Updates the given type solution to satisfy the represented type constraint */
    void resolve(TypeSolver*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolver*) const override;

    /** Gets the bounds on the RHS of the constraint */
    const std::vector<const AstArgument*>& getBounds() const;

    /** Clone the type constraint */
    UnionConstraint* clone() const override {
        return new UnionConstraint(argument, bounds);
    }

    /** Output to a given output stream */
    void print(std::ostream& out) const override {
        out << "type(" << *argument << ") <: (";
        out << "type(" << *bounds[0] << ")";
        for (size_t i = 1; i < bounds.size(); i++) {
            out << " ∪ type(" << *bounds[i] << ")";
        }
        out << ")";
    }

protected:
    bool equal(const TypeConstraint& cons) const override {
        assert(dynamic_cast<const UnionConstraint*>(&cons) != nullptr);
        const auto& other = static_cast<const UnionConstraint&>(cons);
        return argument == other.argument && bounds == other.bounds;
    }

private:
    const AstArgument* argument;
    const std::vector<const AstArgument*> bounds;
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
    void resolve(TypeSolver*) const override;

    /** Checks if the given type solution satisfies the represented type constraint */
    bool isSatisfied(const TypeSolver*) const override;

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

} // end of namespace souffle
