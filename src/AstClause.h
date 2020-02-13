/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstClause.h
 *
 * Defines class Clause that represents rules including facts, predicates, and
 * queries in a Datalog program.
 *
 ***********************************************************************/

#pragma once

#include "AstAbstract.h"
#include "AstArgument.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "Util.h"
#include <cassert>
#include <cstddef>
#include <map>
#include <memory>
#include <ostream>
#include <utility>
#include <vector>

namespace souffle {

/**
 * An execution order for atoms within a clause.
 */
class AstExecutionOrder : public AstNode {
public:
    void print(std::ostream& out) const override {
        out << "(" << join(order) << ")";
    }

    /** appends index of an atom */
    void appendAtomIndex(int index) {
        order.push_back(index);
    }

    /** get order */
    const std::vector<unsigned int>& getOrder() const {
        return order;
    }

    AstExecutionOrder* clone() const override {
        auto res = new AstExecutionOrder();
        res->setSrcLoc(getSrcLoc());
        res->order = order;
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstExecutionOrder*>(&node));
        const auto& other = static_cast<const AstExecutionOrder&>(node);
        return order == other.order;
    }

private:
    /** literal order of body (starting from 1) */
    std::vector<unsigned int> order;
};

/**
 * The class utilized to model user-defined execution plans for various
 * versions of clauses.
 */
class AstExecutionPlan : public AstNode {
public:
    void print(std::ostream& out) const override {
        if (!plans.empty()) {
            out << " .plan ";
            out << join(plans, ", ",
                    [](std::ostream& os, const auto& arg) { os << arg.first << ":" << *arg.second; });
        }
    }

    /** updates execution order for rule version */
    void setOrderFor(int version, std::unique_ptr<AstExecutionOrder> plan) {
        plans[version] = std::move(plan);
    }

    /** get orders */
    std::map<int, const AstExecutionOrder*> getOrders() const {
        std::map<int, const AstExecutionOrder*> result;
        for (auto& plan : plans) {
            result.insert(std::make_pair(plan.first, plan.second.get()));
        }
        return result;
    }

    AstExecutionPlan* clone() const override {
        auto res = new AstExecutionPlan();
        res->setSrcLoc(getSrcLoc());
        for (auto& plan : plans) {
            res->setOrderFor(plan.first, std::unique_ptr<AstExecutionOrder>(plan.second->clone()));
        }
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& plan : plans) {
            plan.second = map(std::move(plan.second));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> childNodes;
        for (auto& plan : plans) {
            childNodes.push_back(plan.second.get());
        }
        return childNodes;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstExecutionPlan*>(&node));
        const auto& other = static_cast<const AstExecutionPlan&>(node);
        if (plans.size() != other.plans.size()) {
            return false;
        }
        auto iter = plans.begin();
        auto otherIter = other.plans.begin();
        for (; iter != plans.end(); ++iter, ++otherIter) {
            if (iter->first != otherIter->first) {
                return false;
            }
            if (*iter->second != *otherIter->second) {
                return false;
            }
        }
        return true;
    }

private:
    /** mapping versions of clauses to execution plans */
    std::map<int, std::unique_ptr<AstExecutionOrder>> plans;
};

/**
 * Intermediate representation of a datalog clause.
 *
 *  A clause can either be:
 *      - a fact  - a clause with no body (e.g., X(a,b))
 *      - a rule  - a clause with a head and a body (e.g., Y(a,b) -: X(a,b))
 *
 * TODO: Currently Clause object is used to represent 2 different types of datalog
 *       clauses, such as rules, queries and facts. This solution was to quickly
 *       overcome issues related to bottom-up construction of IR. In future,
 *       Clause should be  made abstract and have 2 subclasses: Rule and Fact.
 *       Tidy-up interface/classes: this is a mess...
 */
class AstClause : public AstNode {
public:
    void print(std::ostream& os) const override {
        if (head != nullptr) {
            head->print(os);
        }
        if (getBodySize() > 0) {
            os << " :- \n   ";
            os << join(getBodyLiterals(), ",\n   ", print_deref<AstLiteral*>());
        }
        os << ".";
        if (getExecutionPlan() != nullptr) {
            getExecutionPlan()->print(os);
        }
    }

    /** Add a Literal to the body of the clause */
    void addToBody(std::unique_ptr<AstLiteral> l) {
        if (dynamic_cast<AstAtom*>(l.get()) != nullptr) {
            atoms.emplace_back(static_cast<AstAtom*>(l.release()));
        } else if (dynamic_cast<AstNegation*>(l.get()) != nullptr) {
            negations.emplace_back(static_cast<AstNegation*>(l.release()));
        } else if (dynamic_cast<AstProvenanceNegation*>(l.get()) != nullptr) {
            provNegations.emplace_back(static_cast<AstProvenanceNegation*>(l.release()));
        } else if (dynamic_cast<AstConstraint*>(l.get()) != nullptr) {
            constraints.emplace_back(static_cast<AstConstraint*>(l.release()));
        } else {
            assert(false && "Unsupported literal type!");
        }
    }

    /** Set the head of clause to @p h */
    void setHead(std::unique_ptr<AstAtom> h) {
        assert(!head && "Head is already set");
        head = std::move(h);
    }

    /** Return the atom that represents the head of the clause */
    AstAtom* getHead() const {
        return head.get();
    }

    /** Return the number of elements in the body of the Clause */
    // TODO (b-scholz): remove this method
    size_t getBodySize() const {
        return atoms.size() + negations.size() + provNegations.size() + constraints.size();
    }

    /** Return the i-th Literal in body of the clause */
    // TODO (b-scholz): remove this method
    AstLiteral* getBodyLiteral(size_t idx) const {
        if (idx < atoms.size()) {
            return atoms[idx].get();
        }
        idx -= atoms.size();
        if (idx < negations.size()) {
            return negations[idx].get();
        }
        idx -= negations.size();
        if (idx < provNegations.size()) {
            return provNegations[idx].get();
        }
        idx -= provNegations.size();
        return constraints[idx].get();
    }

    /** Obtains a copy of the internally maintained body literals */
    std::vector<AstLiteral*> getBodyLiterals() const {
        std::vector<AstLiteral*> res;
        for (auto& cur : atoms) {
            res.push_back(cur.get());
        }
        for (auto& cur : negations) {
            res.push_back(cur.get());
        }
        for (auto& cur : provNegations) {
            res.push_back(cur.get());
        }
        for (auto& cur : constraints) {
            res.push_back(cur.get());
        }
        return res;
    }

    /** Re-orders atoms to be in the given order. */
    // TODO (b-scholz): remove this method
    void reorderAtoms(const std::vector<unsigned int>& newOrder) {
        // Validate given order
        assert(newOrder.size() == atoms.size());
        std::vector<unsigned int> nopOrder;
        for (unsigned int i = 0; i < atoms.size(); i++) {
            nopOrder.push_back(i);
        }
        assert(std::is_permutation(nopOrder.begin(), nopOrder.end(), newOrder.begin()));

        // Reorder atoms
        std::vector<std::unique_ptr<AstAtom>> oldAtoms(atoms.size());
        atoms.swap(oldAtoms);
        for (unsigned int i = 0; i < newOrder.size(); i++) {
            atoms[i].swap(oldAtoms[newOrder[i]]);
        }
    }

    /** Obtains a list of contained body-atoms. */
    // TODO (b-scholz): remove this method
    std::vector<AstAtom*> getAtoms() const {
        return toPtrVector(atoms);
    }

    /** Obtains a list of contained negations. */
    // TODO (b-scholz): remove this method
    std::vector<AstNegation*> getNegations() const {
        return toPtrVector(negations);
    }

    /** Obtains a list of constraints */
    // TODO (b-scholz): remove this method
    std::vector<AstConstraint*> getConstraints() const {
        return toPtrVector(constraints);
    }

    /** Obtains a list of binary constraints */
    // TODO (b-scholz): remove this method
    std::vector<AstBinaryConstraint*> getBinaryConstraints() const {
        std::vector<AstBinaryConstraint*> result;
        for (auto& constraint : constraints) {
            if (auto* br = dynamic_cast<AstBinaryConstraint*>(constraint.get())) {
                result.push_back(br);
            }
        }
        return result;
    }

    /** Updates the fixed execution order flag */
    void setFixedExecutionPlan(bool value = true) {
        fixedPlan = value;
    }

    /** Determines whether the execution order plan is fixed */
    bool hasFixedExecutionPlan() const {
        return fixedPlan;
    }

    /** Obtains the execution plan associated to this clause or null if there is none */
    const AstExecutionPlan* getExecutionPlan() const {
        return plan.get();
    }

    /** Updates the execution plan associated to this clause */
    void setExecutionPlan(std::unique_ptr<AstExecutionPlan> plan) {
        this->plan = std::move(plan);
    }

    /** Resets the execution plan */
    void clearExecutionPlan() {
        plan = nullptr;
    }

    /** Determines whether this is a internally generated clause */
    bool isGenerated() const {
        return generated;
    }

    /** Updates the generated flag */
    void setGenerated(bool value = true) {
        generated = value;
    }

    /** Gets the clause number */
    size_t getClauseNum() const {
        return clauseNum;
    }

    /** Sets the clause number */
    void setClauseNum(size_t num) {
        clauseNum = num;
    }

    /** clone head */
    // TODO (b-scholz): remove this method
    AstClause* cloneHead() const {
        auto* clone = new AstClause();
        clone->setSrcLoc(getSrcLoc());
        clone->setHead(std::unique_ptr<AstAtom>(getHead()->clone()));
        if (getExecutionPlan() != nullptr) {
            clone->setExecutionPlan(std::unique_ptr<AstExecutionPlan>(getExecutionPlan()->clone()));
        }
        clone->setFixedExecutionPlan(hasFixedExecutionPlan());
        return clone;
    }

    AstClause* clone() const override {
        auto res = new AstClause();
        res->setSrcLoc(getSrcLoc());
        if (getExecutionPlan() != nullptr) {
            res->setExecutionPlan(std::unique_ptr<AstExecutionPlan>(plan->clone()));
        }
        res->head = (head) ? std::unique_ptr<AstAtom>(head->clone()) : nullptr;
        for (const auto& cur : atoms) {
            res->atoms.emplace_back(cur->clone());
        }
        for (const auto& cur : negations) {
            res->negations.emplace_back(cur->clone());
        }
        for (const auto& cur : provNegations) {
            res->provNegations.emplace_back(cur->clone());
        }
        for (const auto& cur : constraints) {
            res->constraints.emplace_back(cur->clone());
        }
        res->fixedPlan = fixedPlan;
        res->generated = generated;
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        head = map(std::move(head));
        for (auto& lit : atoms) {
            lit = map(std::move(lit));
        }
        for (auto& lit : negations) {
            lit = map(std::move(lit));
        }
        for (auto& lit : provNegations) {
            lit = map(std::move(lit));
        }
        for (auto& lit : constraints) {
            lit = map(std::move(lit));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> res = {head.get()};
        for (auto& cur : atoms) {
            res.push_back(cur.get());
        }
        for (auto& cur : negations) {
            res.push_back(cur.get());
        }
        for (auto& cur : provNegations) {
            res.push_back(cur.get());
        }
        for (auto& cur : constraints) {
            res.push_back(cur.get());
        }
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstClause*>(&node));
        const auto& other = static_cast<const AstClause&>(node);
        return *head == *other.head && equal_targets(atoms, other.atoms) &&
               equal_targets(negations, other.negations) && equal_targets(constraints, other.constraints);
    }

    /** The head of the clause */
    std::unique_ptr<AstAtom> head;

    /** The atoms in the body of this clause */
    // TODO (b-scholz): remove
    std::vector<std::unique_ptr<AstAtom>> atoms;

    /** The negations in the body of this clause */
    // TODO (b-scholz): remove
    std::vector<std::unique_ptr<AstNegation>> negations;

    /** The provenance negations in the body of this clause */
    // TODO (b-scholz): remove
    std::vector<std::unique_ptr<AstProvenanceNegation>> provNegations;

    /** The constraints in the body of this clause */
    // TODO (b-scholz): remove
    std::vector<std::unique_ptr<AstConstraint>> constraints;

    /** Determines whether the given execution order should be enforced */
    // TODO (b-scholz): confused state / double-check
    bool fixedPlan = false;

    /** The user defined execution plan -- if any */
    std::unique_ptr<AstExecutionPlan> plan;

    /** Determines whether this is an internally generated clause resulting from resolving syntactic sugar */
    bool generated = false;

    /** Stores a unique number for each clause in a relation,
        used for provenance */
    // TODO (b-scholz): move to an AST analysis
    size_t clauseNum = 0;
};

}  // end of namespace souffle
