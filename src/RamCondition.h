/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamCondition.h
 *
 * Defines a class for evaluating conditions in the Relational Algebra
 * Machine.
 *
 ***********************************************************************/

#pragma once

#include "BinaryConstraintOps.h"
#include "RamExpression.h"
#include "RamNode.h"
#include "RamRelation.h"
#include "SymbolTable.h"

#include <algorithm>
#include <sstream>
#include <stack>
#include <string>

#include <cstdlib>

namespace souffle {

/**
 * Abstract Class for RAM condition
 */
class RamCondition : public RamNode {
public:
    RamCondition() = default;

    /** Create clone */
    RamCondition* clone() const override = 0;
};

/**
 * Conjunction
 */
class RamConjunction : public RamCondition {
public:
    RamConjunction(std::unique_ptr<RamCondition> l, std::unique_ptr<RamCondition> r)
            : RamCondition(), lhs(std::move(l)), rhs(std::move(r)) {}

    /** Get left-hand side of conjunction */
    const RamCondition& getLHS() const {
        assert(lhs);
        return *lhs;
    }

    /** Get right-hand side of conjunction */
    const RamCondition& getRHS() const {
        assert(rhs);
        return *rhs;
    }

    /** Print */
    void print(std::ostream& os) const override {
        os << "(";
        lhs->print(os);
        os << " and ";
        rhs->print(os);
        os << ")";
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {lhs.get(), rhs.get()};
    }

    /** Create clone */
    RamConjunction* clone() const override {
        RamConjunction* res = new RamConjunction(
                std::unique_ptr<RamCondition>(lhs->clone()), std::unique_ptr<RamCondition>(rhs->clone()));
        return res;
    }

    /** Apply */
    void apply(const RamNodeMapper& map) override {
        lhs = map(std::move(lhs));
        rhs = map(std::move(rhs));
    }

protected:
    /** Left-hand side of conjunction */
    std::unique_ptr<RamCondition> lhs;

    /** Right-hand side of conjunction */
    std::unique_ptr<RamCondition> rhs;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamConjunction*>(&node));
        const auto& other = static_cast<const RamConjunction&>(node);
        return getLHS() == other.getLHS() && getRHS() == other.getRHS();
    }
};

/**
 * Negation
 */
class RamNegation : public RamCondition {
public:
    RamNegation(std::unique_ptr<RamCondition> operand) : RamCondition(), operand(std::move(operand)) {}

    /** Get operand of negation */
    const RamCondition& getOperand() const {
        assert(nullptr != operand);
        return *operand;
    }

    /** Print */
    void print(std::ostream& os) const override {
        os << "(not ";
        operand->print(os);
        os << ")";
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {operand.get()};
    }

    /** Create clone */
    RamNegation* clone() const override {
        RamNegation* res = new RamNegation(std::unique_ptr<RamCondition>(operand->clone()));
        return res;
    }

    /** Apply */
    void apply(const RamNodeMapper& map) override {
        operand = map(std::move(operand));
    }

protected:
    /** Operand */
    std::unique_ptr<RamCondition> operand;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamNegation*>(&node));
        const auto& other = static_cast<const RamNegation&>(node);
        return getOperand() == other.getOperand();
    }
};

/**
 * Binary constraint
 */
class RamConstraint : public RamCondition {
public:
    RamConstraint(BinaryConstraintOp op, std::unique_ptr<RamExpression> l, std::unique_ptr<RamExpression> r)
            : RamCondition(), op(op), lhs(std::move(l)), rhs(std::move(r)) {}

    /** Print */
    void print(std::ostream& os) const override {
        os << "(";
        lhs->print(os);
        os << " " << toBinaryConstraintSymbol(op) << " ";
        rhs->print(os);
        os << ")";
    }

    /** Get left-hand side */
    RamExpression* getLHS() const {
        return lhs.get();
    }

    /** Get right-hand side */
    RamExpression* getRHS() const {
        return rhs.get();
    }

    /** Get operator symbol */
    BinaryConstraintOp getOperator() const {
        return op;
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {lhs.get(), rhs.get()};
    }

    /** Create clone */
    RamConstraint* clone() const override {
        RamConstraint* res = new RamConstraint(op, std::unique_ptr<RamExpression>(lhs->clone()),
                std::unique_ptr<RamExpression>(rhs->clone()));
        return res;
    }

    /** Apply */
    void apply(const RamNodeMapper& map) override {
        lhs = map(std::move(lhs));
        rhs = map(std::move(rhs));
    }

protected:
    /** Operator */
    BinaryConstraintOp op;

    /** Left-hand side of constraint*/
    std::unique_ptr<RamExpression> lhs;

    /** Right-hand side of constraint */
    std::unique_ptr<RamExpression> rhs;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamConstraint*>(&node));
        const auto& other = static_cast<const RamConstraint&>(node);
        return getOperator() == other.getOperator() && getLHS() == other.getLHS() &&
               getRHS() == other.getRHS();
    }
};

/**
 * Abstract existence check
 */
class RamAbstractExistenceCheck : public RamCondition {
public:
    RamAbstractExistenceCheck(
            std::unique_ptr<RamRelationReference> relRef, std::vector<std::unique_ptr<RamExpression>> vals)
            : RamCondition(), relationRef(std::move(relRef)), values(std::move(vals)) {}

    /** Get relation */
    const RamRelation& getRelation() const {
        return *relationRef->get();
    }

    /** Get arguments */
    std::vector<RamExpression*> getValues() const {
        return toPtrVector(values);
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        std::vector<const RamNode*> res = {relationRef.get()};
        for (const auto& cur : values) {
            res.push_back(cur.get());
        }
        return res;
    }

    /** Apply */
    void apply(const RamNodeMapper& map) override {
        relationRef = map(std::move(relationRef));
        for (auto& val : values) {
            if (val != nullptr) {
                val = map(std::move(val));
            }
        }
    }

protected:
    /* Relation */
    std::unique_ptr<RamRelationReference> relationRef;

    /** Pattern -- nullptr if undefined */
    std::vector<std::unique_ptr<RamExpression>> values;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamAbstractExistenceCheck*>(&node));
        const auto& other = static_cast<const RamAbstractExistenceCheck&>(node);
        return getRelation() == other.getRelation() && equal_targets(values, other.values);
    }
};

/**
 * Existence check for a tuple(-pattern) in a relation
 */
class RamExistenceCheck : public RamAbstractExistenceCheck {
public:
    RamExistenceCheck(
            std::unique_ptr<RamRelationReference> relRef, std::vector<std::unique_ptr<RamExpression>> vals)
            : RamAbstractExistenceCheck(std::move(relRef), std::move(vals)) {}

    /** Print */
    void print(std::ostream& os) const override {
        os << "("
           << join(values, ",",
                      [](std::ostream& out, const std::unique_ptr<RamExpression>& value) {
                          if (!value) {
                              out << "_";
                          } else {
                              out << *value;
                          }
                      })
           << ") ∈ " << getRelation().getName();
    }

    /** Create clone */
    RamExistenceCheck* clone() const override {
        std::vector<std::unique_ptr<RamExpression>> newValues;
        for (auto& cur : values) {
            RamExpression* val = nullptr;
            if (cur != nullptr) {
                val = cur->clone();
            }
            newValues.emplace_back(val);
        }
        RamExistenceCheck* res = new RamExistenceCheck(
                std::unique_ptr<RamRelationReference>(relationRef->clone()), std::move(newValues));
        return res;
    }

protected:
    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamExistenceCheck*>(&node));
        return RamAbstractExistenceCheck::equal(node);
    }
};

/**
 * Existence check for a relation for provenance existence check
 */
class RamProvenanceExistenceCheck : public RamAbstractExistenceCheck {
public:
    RamProvenanceExistenceCheck(
            std::unique_ptr<RamRelationReference> relRef, std::vector<std::unique_ptr<RamExpression>> vals)
            : RamAbstractExistenceCheck(std::move(relRef), std::move(vals)) {}

    /** Print */
    void print(std::ostream& os) const override {
        os << "("
           << join(values, ",",
                      [](std::ostream& out, const std::unique_ptr<RamExpression>& value) {
                          if (!value) {
                              out << "_";
                          } else {
                              out << *value;
                          }
                      })
           << ") prov∈ " << getRelation().getName();
    }

    /** Create clone */
    RamProvenanceExistenceCheck* clone() const override {
        std::vector<std::unique_ptr<RamExpression>> newValues;
        for (auto& cur : values) {
            RamExpression* val = nullptr;
            if (cur != nullptr) {
                val = cur->clone();
            }
            newValues.emplace_back(val);
        }
        RamProvenanceExistenceCheck* res = new RamProvenanceExistenceCheck(
                std::unique_ptr<RamRelationReference>(relationRef->clone()), std::move(newValues));
        return res;
    }

protected:
    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(dynamic_cast<const RamProvenanceExistenceCheck*>(&node));
        return RamAbstractExistenceCheck::equal(node);
    }
};

/**
 * Emptiness check for a relation
 */
class RamEmptinessCheck : public RamCondition {
public:
    RamEmptinessCheck(std::unique_ptr<RamRelationReference> relRef)
            : RamCondition(), relationRef(std::move(relRef)) {}

    /** Get relation */
    const RamRelation& getRelation() const {
        return *relationRef->get();
    }

    /** Print */
    void print(std::ostream& os) const override {
        os << "(" << getRelation().getName() << " = ∅)";
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {relationRef.get()};
    }

    /** Create clone */
    RamEmptinessCheck* clone() const override {
        RamEmptinessCheck* res =
                new RamEmptinessCheck(std::unique_ptr<RamRelationReference>(relationRef->clone()));
        return res;
    }

    /** Apply */
    void apply(const RamNodeMapper& map) override {
        relationRef = map(std::move(relationRef));
    }

protected:
    /** Relation */
    std::unique_ptr<RamRelationReference> relationRef;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamEmptinessCheck*>(&node));
        const auto& other = static_cast<const RamEmptinessCheck&>(node);
        return getRelation() == other.getRelation();
    }
};

/**
 * @brief Convert terms of a conjunction to a list
 * @param A RAM condition
 * @param A list of RAM conditions
 *
 * Convert a condition of the format C1 /\ C2 /\ ... /\ Cn
 * to a list {C1, C2, ..., Cn}.
 */
inline std::vector<std::unique_ptr<RamCondition>> toConjunctionList(const RamCondition* condition) {
    std::vector<std::unique_ptr<RamCondition>> list;
    std::stack<const RamCondition*> stack;
    if (condition != nullptr) {
        stack.push(condition);
        while (stack.size() > 0) {
            condition = stack.top();
            stack.pop();
            if (const auto* ramConj = dynamic_cast<const RamConjunction*>(condition)) {
                stack.push(&ramConj->getLHS());
                stack.push(&ramConj->getRHS());
            } else {
                list.emplace_back(condition->clone());
            }
        }
    }
    return list;
}

/**
 * @brief Convert list of conditions to a conjunction
 * @param A list of RAM conditions
 * @param A RAM condition
 *
 * Convert a list {C1, C2, ..., Cn} to a condition of
 * the format C1 /\ C2 /\ ... /\ Cn.
 */
inline std::unique_ptr<RamCondition> toCondition(const std::vector<const RamCondition*>& list) {
    std::unique_ptr<RamCondition> result;
    for (const RamCondition* cur : list) {
        if (result == nullptr) {
            result = std::unique_ptr<RamCondition>(cur->clone());
        } else {
            result = std::make_unique<RamConjunction>(
                    std::move(result), std::unique_ptr<RamCondition>(cur->clone()));
        }
    }
    return result;
}

}  // end of namespace souffle
