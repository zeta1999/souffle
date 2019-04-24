/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamOperation.h
 *
 * Defines the Operation of a relational algebra query.
 *
 ***********************************************************************/

#pragma once

#include "RamCondition.h"
#include "RamExpression.h"
#include "RamNode.h"
#include "RamRelation.h"
#include "RamTypes.h"
#include "Util.h"
#include <cassert>
#include <cstddef>
#include <iosfwd>
#include <memory>
#include <string>
#include <vector>

namespace souffle {

/**
 * Abstract class for a relational algebra operation
 */
class RamOperation : public RamNode {
public:
    RamOperation(RamNodeType type) : RamNode(type) {}

    virtual void print(std::ostream& os, int tabpos) const = 0;

    void print(std::ostream& os) const override {
        print(os, 0);
    }

    std::vector<const RamNode*> getChildNodes() const override = 0;

    void apply(const RamNodeMapper& map) override = 0;

    RamOperation* clone() const override = 0;

protected:
    bool equal(const RamNode& node) const override = 0;
};

/**
 * Abstract class for a nesting operations in a loop-nest
 */
class RamNestedOperation : public RamOperation {
public:
    RamNestedOperation(RamNodeType type, std::unique_ptr<RamOperation> nested, std::string profileText = "")
            : RamOperation(type), nestedOperation(std::move(nested)), profileText(std::move(profileText)) {}

    /** Get nested operation */
    RamOperation& getOperation() const {
        assert(nullptr != nestedOperation);
        return *nestedOperation;
    }

    /** Get profile text */
    const std::string& getProfileText() const {
        return profileText;
    }

    void print(std::ostream& os, int tabpos) const override {
        nestedOperation->print(os, tabpos + 1);
    }

    std::vector<const RamNode*> getChildNodes() const override {
        return {nestedOperation.get()};
    }

    void apply(const RamNodeMapper& map) override {
        nestedOperation = map(std::move(nestedOperation));
    }

protected:
    /** Nested operation */
    std::unique_ptr<RamOperation> nestedOperation;

    /** Profile text */
    const std::string profileText;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamNestedOperation*>(&node));
        const auto& other = static_cast<const RamNestedOperation&>(node);
        return getOperation() == other.getOperation() && getProfileText() == other.getProfileText();
    }
};

/**
 * Abstract class for relation searches and lookups
 */
class RamSearch : public RamNestedOperation {
public:
    RamSearch(RamNodeType type, int ident, std::unique_ptr<RamOperation> nested, std::string profileText = "")
            : RamNestedOperation(type, std::move(nested), std::move(profileText)), identifier(ident) {}

    /** Get identifier */
    int getIdentifier() const {
        return identifier;
    }

    std::vector<const RamNode*> getChildNodes() const override {
        return RamNestedOperation::getChildNodes();
    }

protected:
    /** Identifier for the tuple */
    const int identifier;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamSearch*>(&node));
        const auto& other = static_cast<const RamSearch&>(node);
        return RamNestedOperation::equal(other) && getIdentifier() == other.getIdentifier();
    }
};

/**
 * Abstract class for relation searches
 */
class RamRelationSearch : public RamSearch {
public:
    RamRelationSearch(RamNodeType type, std::unique_ptr<RamRelationReference> relRef, int ident,
            std::unique_ptr<RamOperation> nested, std::string profileText = "")
            : RamSearch(type, ident, std::move(nested), std::move(profileText)),
              relationRef(std::move(relRef)) {}

    /** Get search relation */
    const RamRelation& getRelation() const {
        return *relationRef->get();
    }

    void apply(const RamNodeMapper& map) override {
        RamSearch::apply(map);
        relationRef = map(std::move(relationRef));
    }

    std::vector<const RamNode*> getChildNodes() const override {
        auto res = RamSearch::getChildNodes();
        res.push_back(relationRef.get());
        return res;
    }

protected:
    /** Search relation */
    std::unique_ptr<RamRelationReference> relationRef;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamRelationSearch*>(&node));
        const auto& other = static_cast<const RamRelationSearch&>(node);
        return RamSearch::equal(other) && getRelation() == other.getRelation();
    }
};

/**
 * Relation Scan
 *
 * Iterate all tuples of a relation
 */
class RamScan : public RamRelationSearch {
public:
    RamScan(std::unique_ptr<RamRelationReference> rel, int ident, std::unique_ptr<RamOperation> nested,
            std::string profileText = "")
            : RamRelationSearch(RN_Scan, std::move(rel), ident, std::move(nested), std::move(profileText)) {}

    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "FOR t" << getIdentifier();
        os << " IN " << getRelation().getName() << std::endl;
        RamRelationSearch::print(os, tabpos + 1);
    }

    RamScan* clone() const override {
        return new RamScan(std::unique_ptr<RamRelationReference>(relationRef->clone()), getIdentifier(),
                std::unique_ptr<RamOperation>(getOperation().clone()), getProfileText());
    }

    std::vector<const RamNode*> getChildNodes() const override {
        return RamRelationSearch::getChildNodes();
    }
};

/**
 * Relation Scan with Index
 *
 * Search for tuples of a relation matching a criteria
 */
class RamIndexRelationSearch : public RamRelationSearch {
public:
    RamIndexRelationSearch(RamNodeType type, std::unique_ptr<RamRelationReference> r, int ident,
            std::vector<std::unique_ptr<RamExpression>> queryPattern, std::unique_ptr<RamOperation> nested,
            std::string profileText = "")
            : RamRelationSearch(type, std::move(r), ident, std::move(nested), std::move(profileText)),
              queryPattern(std::move(queryPattern)) {
        assert(getRangePattern().size() == getRelation().getArity());
    }

    /** Get range pattern */
    std::vector<RamExpression*> getRangePattern() const {
        return toPtrVector(queryPattern);
    }

    std::vector<const RamNode*> getChildNodes() const override {
        auto res = RamRelationSearch::getChildNodes();
        for (auto& cur : queryPattern) {
            if (cur != nullptr) {
                res.push_back(cur.get());
            }
        }
        return res;
    }

    void apply(const RamNodeMapper& map) override {
        RamRelationSearch::apply(map);
        for (auto& cur : queryPattern) {
            if (cur != nullptr) {
                cur = map(std::move(cur));
            }
        }
    }

protected:
    /** Values of index per column of table (if indexable) */
    std::vector<std::unique_ptr<RamExpression>> queryPattern;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamIndexRelationSearch*>(&node));
        const auto& other = static_cast<const RamIndexRelationSearch&>(node);
        return RamRelationSearch::equal(other) && equal_targets(queryPattern, other.queryPattern);
    }
};

/**
 * Relation Scan with Index
 *
 * Search for tuples of a relation matching a criteria
 */
class RamIndexScan : public RamIndexRelationSearch {
public:
    RamIndexScan(std::unique_ptr<RamRelationReference> r, int ident,
            std::vector<std::unique_ptr<RamExpression>> queryPattern, std::unique_ptr<RamOperation> nested,
            std::string profileText = "")
            : RamIndexRelationSearch(RN_IndexScan, std::move(r), ident, std::move(queryPattern),
                      std::move(nested), std::move(profileText)) {}

    void print(std::ostream& os, int tabpos) const override {
        const RamRelation& rel = getRelation();
        os << times(" ", tabpos);
        os << "SEARCH " << rel.getName() << " AS t" << getIdentifier() << " ON INDEX ";
        bool first = true;
        for (unsigned int i = 0; i < rel.getArity(); ++i) {
            if (queryPattern[i] != nullptr) {
                if (first) {
                    first = false;
                } else {
                    os << " and ";
                }
                os << "t" << getIdentifier() << "." << rel.getArg(i) << "=";
                queryPattern[i]->print(os);
            }
        }
        os << std::endl;
        RamIndexRelationSearch::print(os, tabpos + 1);
    }

    RamIndexScan* clone() const override {
        std::vector<std::unique_ptr<RamExpression>> resQueryPattern(queryPattern.size());
        for (unsigned int i = 0; i < queryPattern.size(); ++i) {
            if (nullptr != queryPattern[i]) {
                resQueryPattern[i] = std::unique_ptr<RamExpression>(queryPattern[i]->clone());
            }
        }
        RamIndexScan* res = new RamIndexScan(std::unique_ptr<RamRelationReference>(relationRef->clone()),
                getIdentifier(), std::move(resQueryPattern),
                std::unique_ptr<RamOperation>(getOperation().clone()), getProfileText());
        return res;
    }
};

/**
 * Aggregation
 */
class RamAggregate : public RamIndexRelationSearch {
public:
    /** Types of aggregation functions */
    enum Function { MAX, MIN, COUNT, SUM };

    RamAggregate(std::unique_ptr<RamOperation> nested, Function fun,
            std::unique_ptr<RamRelationReference> relRef, std::unique_ptr<RamExpression> expression,
            std::unique_ptr<RamCondition> condition, std::vector<std::unique_ptr<RamExpression>> queryPattern,
            int ident)
            : RamIndexRelationSearch(
                      RN_Aggregate, std::move(relRef), ident, std::move(queryPattern), std::move(nested)),
              function(fun), expression(std::move(expression)), condition(std::move(condition)) {}

    /** Get condition */
    const RamCondition* getCondition() const {
        return condition.get();
    }

    /** Get aggregation function */
    Function getFunction() const {
        return function;
    }

    /** Get target expression */
    const RamExpression* getExpression() const {
        return expression.get();
    }

    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        switch (function) {
            case MIN:
                os << "MIN ";
                break;
            case MAX:
                os << "MAX ";
                break;
            case COUNT:
                os << "COUNT ";
                break;
            case SUM:
                os << "SUM ";
                break;
        }
        if (function != COUNT) {
            os << *expression << " ";
        }
        os << "AS t" << getIdentifier() << ".0 IN t" << getIdentifier() << " ∈ " << getRelation().getName();
        os << "("
           << join(queryPattern, ",",
                      [&](std::ostream& out, const std::unique_ptr<RamExpression>& expression) {
                          if (expression == nullptr) {
                              out << "_";
                          } else {
                              out << *expression;
                          }
                      })
           << ")";
        if (condition != nullptr) {
            os << " WHERE " << getCondition();
        }
        os << std::endl;
        RamIndexRelationSearch::print(os, tabpos + 1);
    }

    std::vector<const RamNode*> getChildNodes() const override {
        auto res = RamIndexRelationSearch::getChildNodes();
        if (expression != nullptr) {
            res.push_back(expression.get());
        }
        if (condition != nullptr) {
            res.push_back(condition.get());
        }
        return res;
    }

    RamAggregate* clone() const override {
        std::vector<std::unique_ptr<RamExpression>> pattern;
        for (auto const& e : queryPattern) {
            pattern.push_back(std::unique_ptr<RamExpression>((e != nullptr) ? e->clone() : nullptr));
        }
        RamAggregate* res = new RamAggregate(std::unique_ptr<RamOperation>(getOperation().clone()), function,
                std::unique_ptr<RamRelationReference>(relationRef->clone()),
                expression == nullptr ? nullptr : std::unique_ptr<RamExpression>(expression->clone()),
                condition == nullptr ? nullptr : std::unique_ptr<RamCondition>(condition->clone()),
                std::move(pattern), getIdentifier());
        return res;
    }

    void apply(const RamNodeMapper& map) override {
        RamIndexRelationSearch::apply(map);
        if (condition != nullptr) {
            condition = map(std::move(condition));
        }
        if (expression != nullptr) {
            expression = map(std::move(expression));
        }
    }

protected:
    /** Aggregation function */
    Function function;

    /** Aggregation expression */
    std::unique_ptr<RamExpression> expression;

    /** Aggregation tuple condition */
    std::unique_ptr<RamCondition> condition;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamAggregate*>(&node));
        const auto& other = static_cast<const RamAggregate&>(node);
        if (getCondition() != nullptr && other.getCondition() != nullptr &&
                *getCondition() != *other.getCondition()) {
            return false;
        }
        return RamIndexRelationSearch::equal(other) && getCondition() == other.getCondition() &&
               getFunction() == other.getFunction() && getExpression() == other.getExpression();
    }
};

/**
 * Record lookup
 */
class RamUnpackRecord : public RamSearch {
public:
    RamUnpackRecord(
            std::unique_ptr<RamOperation> nested, int ident, int ref_level, size_t ref_pos, size_t arity)
            : RamSearch(RN_UnpackRecord, ident, std::move(nested)), refLevel(ref_level), refPos(ref_pos),
              arity(arity) {}

    /** Get reference level */
    int getReferenceLevel() const {
        return refLevel;
    }

    /** Get reference position */
    std::size_t getReferencePosition() const {
        return refPos;
    }

    /** Get arity */
    std::size_t getArity() const {
        return arity;
    }

    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "UNPACK env(t" << refLevel << ", i" << refPos << ") INTO t"
           << getIdentifier() << std::endl;
        RamSearch::print(os, tabpos + 1);
    }

    RamUnpackRecord* clone() const override {
        RamUnpackRecord* res = new RamUnpackRecord(std::unique_ptr<RamOperation>(getOperation().clone()),
                getIdentifier(), refLevel, refPos, arity);
        return res;
    }

protected:
    /** Level of the tuple containing record reference */
    const int refLevel;

    /** Position of the tuple containing record reference */
    const size_t refPos;

    /** Arity of the unpacked tuple */
    const size_t arity;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamUnpackRecord*>(&node));
        const auto& other = static_cast<const RamUnpackRecord&>(node);
        return RamSearch::equal(other) && getReferencePosition() == other.getReferencePosition() &&
               getReferenceLevel() == other.getReferenceLevel() && getArity() == other.getArity();
    }
};

/**
 * Filter statement
 */
class RamFilter : public RamNestedOperation {
public:
    RamFilter(std::unique_ptr<RamCondition> cond, std::unique_ptr<RamOperation> nested,
            std::string profileText = "")
            : RamNestedOperation(RN_Filter, std::move(nested), std::move(profileText)),
              condition(std::move(cond)) {}

    /** Get condition */
    const RamCondition& getCondition() const {
        return *condition;
    }

    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "IF " << getCondition() << std::endl;
        RamNestedOperation::print(os, tabpos + 1);
    }

    std::vector<const RamNode*> getChildNodes() const override {
        auto res = RamNestedOperation::getChildNodes();
        res.push_back(condition.get());
        return res;
    }

    RamFilter* clone() const override {
        return new RamFilter(std::unique_ptr<RamCondition>(condition->clone()),
                std::unique_ptr<RamOperation>(getOperation().clone()));
    }

    void apply(const RamNodeMapper& map) override {
        RamNestedOperation::apply(map);
        condition = map(std::move(condition));
    }

protected:
    /**
     * Condition that is checked for each obtained tuple
     *
     * If condition is a nullptr, then no condition applies
     */
    std::unique_ptr<RamCondition> condition;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamFilter*>(&node));
        const auto& other = static_cast<const RamFilter&>(node);
        return RamNestedOperation::equal(node) && getCondition() == other.getCondition();
    }
};

/** Projection */
class RamProject : public RamOperation {
public:
    RamProject(std::unique_ptr<RamRelationReference> relRef,
            std::vector<std::unique_ptr<RamExpression>> expressions)
            : RamOperation(RN_Project), relationRef(std::move(relRef)), expressions(std::move(expressions)) {}

    /** Get relation */
    const RamRelation& getRelation() const {
        return *relationRef->get();
    }

    /** Get expressions */
    std::vector<RamExpression*> getValues() const {
        return toPtrVector(expressions);
    }

    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "PROJECT (" << join(expressions, ", ", print_deref<std::unique_ptr<RamExpression>>())
           << ") INTO " << getRelation().getName() << std::endl;
    }

    std::vector<const RamNode*> getChildNodes() const override {
        std::vector<const RamNode*> res;
        res.push_back(relationRef.get());
        for (const auto& cur : expressions) {
            res.push_back(cur.get());
        }
        return res;
    }

    RamProject* clone() const override {
        std::vector<std::unique_ptr<RamExpression>> newValues;
        for (auto& cur : expressions) {
            newValues.emplace_back(cur->clone());
        }
        RamProject* res = new RamProject(
                std::unique_ptr<RamRelationReference>(relationRef->clone()), std::move(newValues));
        return res;
    }

    void apply(const RamNodeMapper& map) override {
        relationRef = map(std::move(relationRef));
        for (auto& cur : expressions) {
            cur = map(std::move(cur));
        }
    }

protected:
    /** Relation */
    std::unique_ptr<RamRelationReference> relationRef;

    /* Values for projection */
    std::vector<std::unique_ptr<RamExpression>> expressions;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamProject*>(&node));
        const auto& other = static_cast<const RamProject&>(node);
        return getRelation() == other.getRelation() && equal_targets(expressions, other.expressions);
    }
};

/** A statement for returning from a ram subroutine */
class RamReturn : public RamOperation {
public:
    RamReturn(std::vector<std::unique_ptr<RamExpression>> vals)
            : RamOperation(RN_Return), expressions(std::move(vals)) {}

    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "RETURN (";
        for (auto val : getValues()) {
            if (val == nullptr) {
                os << "_";
            } else {
                val->print(os);
            }

            if (val != *(getValues().end() - 1)) {
                os << ", ";
            }
        }
        os << ")" << std::endl;
    }

    std::vector<RamExpression*> getValues() const {
        return toPtrVector(expressions);
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        std::vector<const RamNode*> res;
        for (const auto& cur : expressions) {
            if (cur != nullptr) {
                res.push_back(cur.get());
            }
        }
        return res;
    }

    RamReturn* clone() const override {
        std::vector<std::unique_ptr<RamExpression>> newValues;
        for (auto& cur : expressions) {
            if (cur != nullptr) {
                newValues.emplace_back(cur->clone());
            } else {
                newValues.push_back(nullptr);
            }
        }
        return new RamReturn(std::move(newValues));
    }

    void apply(const RamNodeMapper& map) override {
        for (auto& cur : expressions) {
            if (cur != nullptr) {
                cur = map(std::move(cur));
            }
        }
    }

protected:
    std::vector<std::unique_ptr<RamExpression>> expressions;

    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamReturn*>(&node));
        const auto& other = static_cast<const RamReturn&>(node);
        return equal_targets(expressions, other.expressions);
    }
};

}  // namespace souffle
