/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamStatement.h
 *
 * Defines abstract class Statement and sub-classes for implementing the
 * Relational Algebra Machine (RAM), which is an abstract machine.
 *
 ***********************************************************************/

#pragma once

#include "RamExpression.h"
#include "RamNode.h"
#include "RamOperation.h"
#include "RamRelation.h"
#include "Util.h"

#include <algorithm>
#include <memory>
#include <ostream>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

/**
 * Abstract class for RAM statements
 */
class RamStatement : public RamNode {
public:
    RamStatement() = default;

    /** Pretty print with indentation */
    virtual void print(std::ostream& os, int tabpos) const = 0;

    /** Print RAM statement */
    void print(std::ostream& os) const override {
        print(os, 0);
    }

    /** Create clone */
    RamStatement* clone() const override = 0;
};

/**
 * RAM Statements with a single relation
 */
class RamRelationStatement : public RamStatement {
public:
    RamRelationStatement(std::unique_ptr<RamRelationReference> relRef)
            : RamStatement(), relationRef(std::move(relRef)) {}

    /** Get RAM relation */
    const RamRelation& getRelation() const {
        return *relationRef->get();
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {relationRef.get()};
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        relationRef = map(std::move(relationRef));
    }

protected:
    /** Relation */
    std::unique_ptr<RamRelationReference> relationRef;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamRelationStatement*>(&node));
        const auto& other = static_cast<const RamRelationStatement&>(node);
        return getRelation() == other.getRelation();
    }
};

/**
 * Create new RAM relation
 */
class RamCreate : public RamRelationStatement {
public:
    RamCreate(std::unique_ptr<RamRelationReference> relRef) : RamRelationStatement(std::move(relRef)) {}

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        const RamRelation& rel = getRelation();
        os << times(" ", tabpos);
        os << "CREATE " << rel.getName() << " " << rel.getRepresentation() << std::endl;
    };

    /** Create clone */
    RamCreate* clone() const override {
        RamCreate* res = new RamCreate(std::unique_ptr<RamRelationReference>(relationRef->clone()));
        return res;
    }
};

/**
 * Load data into a relation
 */
class RamLoad : public RamRelationStatement {
public:
    RamLoad(std::unique_ptr<RamRelationReference> relRef, std::vector<IODirectives> ioDirectives)
            : RamRelationStatement(std::move(relRef)), ioDirectives(std::move(ioDirectives)) {}

    const std::vector<IODirectives>& getIODirectives() const {
        return ioDirectives;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        const RamRelation& rel = getRelation();
        os << times(" ", tabpos);
        os << "LOAD DATA FOR " << rel.getName() << " FROM {";
        os << join(ioDirectives, "], [",
                [](std::ostream& out, const IODirectives& directives) { out << directives; });
        os << ioDirectives << "}";
        os << std::endl;
    };

    /** Create clone */
    RamLoad* clone() const override {
        RamLoad* res = new RamLoad(std::unique_ptr<RamRelationReference>(relationRef->clone()), ioDirectives);
        return res;
    }

protected:
    const std::vector<IODirectives> ioDirectives;
};

/**
 * Store data of a relation
 */
class RamStore : public RamRelationStatement {
public:
    RamStore(std::unique_ptr<RamRelationReference> relRef, std::vector<IODirectives> ioDirectives)
            : RamRelationStatement(std::move(relRef)), ioDirectives(std::move(ioDirectives)) {}

    const std::vector<IODirectives>& getIODirectives() const {
        return ioDirectives;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        const RamRelation& rel = getRelation();
        os << times(" ", tabpos);
        os << "STORE DATA FOR " << rel.getName() << " TO {";
        os << join(ioDirectives, "], [",
                [](std::ostream& out, const IODirectives& directives) { out << directives; });
        os << "}";
        os << std::endl;
    };

    /** Create clone */
    RamStore* clone() const override {
        RamStore* res =
                new RamStore(std::unique_ptr<RamRelationReference>(relationRef->clone()), ioDirectives);
        return res;
    }

protected:
    const std::vector<IODirectives> ioDirectives;
};

/**
 * Delete tuples of a relation
 */
class RamClear : public RamRelationStatement {
public:
    RamClear(std::unique_ptr<RamRelationReference> relRef) : RamRelationStatement(std::move(relRef)) {}

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        const RamRelation& rel = getRelation();
        os << times(" ", tabpos);
        os << "CLEAR ";
        os << rel.getName();
        os << std::endl;
    }

    /** Create clone */
    RamClear* clone() const override {
        RamClear* res = new RamClear(std::unique_ptr<RamRelationReference>(relationRef->clone()));
        return res;
    }
};

/**
 * Drop relation, i.e., delete it from memory
 */
class RamDrop : public RamRelationStatement {
public:
    RamDrop(std::unique_ptr<RamRelationReference> relRef) : RamRelationStatement(std::move(relRef)) {}

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        const RamRelation& rel = getRelation();
        os << times(" ", tabpos);
        os << "DROP " << rel.getName();
        os << std::endl;
    }
    /** Create clone */
    RamDrop* clone() const override {
        RamDrop* res = new RamDrop(std::unique_ptr<RamRelationReference>(relationRef->clone()));
        return res;
    }
};

/**
 * Merge tuples from a source into target relation.
 * Note that semantically uniqueness of tuples is not checked.
 */
class RamMerge : public RamStatement {
public:
    RamMerge(std::unique_ptr<RamRelationReference> tRef, std::unique_ptr<RamRelationReference> sRef)
            : RamStatement(), targetRef(std::move(tRef)), sourceRef(std::move(sRef)) {
        const RamRelation* source = sourceRef->get();
        const RamRelation* target = targetRef->get();
        assert(source->getArity() == target->getArity() && "mismatching relations");
        for (size_t i = 0; i < source->getArity(); i++) {
            assert(source->getArgTypeQualifier(i) == target->getArgTypeQualifier(i) && "mismatching type");
        }
    }

    /** Get source relation */
    const RamRelation& getSourceRelation() const {
        return *sourceRef->get();
    }

    /** Get target relation */
    const RamRelation& getTargetRelation() const {
        return *targetRef->get();
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "MERGE " << getTargetRelation().getName() << " WITH " << getSourceRelation().getName();
        os << std::endl;
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {sourceRef.get(), targetRef.get()};
    }

    /** Create clone */
    RamMerge* clone() const override {
        RamMerge* res = new RamMerge(std::unique_ptr<RamRelationReference>(targetRef->clone()),
                std::unique_ptr<RamRelationReference>(sourceRef->clone()));
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        sourceRef = map(std::move(sourceRef));
        targetRef = map(std::move(targetRef));
    }

protected:
    std::unique_ptr<RamRelationReference> targetRef;
    std::unique_ptr<RamRelationReference> sourceRef;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamMerge*>(&node));
        const auto& other = static_cast<const RamMerge&>(node);
        return getTargetRelation() == other.getTargetRelation() &&
               getSourceRelation() == other.getSourceRelation();
    }
};

/**
 * Swap operation two relations
 */
class RamSwap : public RamStatement {
public:
    RamSwap(std::unique_ptr<RamRelationReference> f, std::unique_ptr<RamRelationReference> s)
            : RamStatement(), first(std::move(f)), second(std::move(s)) {
        assert(first->get()->getArity() == second->get()->getArity() && "mismatching relations");
        for (size_t i = 0; i < first->get()->getArity(); i++) {
            assert(first->get()->getArgTypeQualifier(i) == second->get()->getArgTypeQualifier(i) &&
                    "mismatching type");
        }
    }

    /** Get first relation */
    const RamRelation& getFirstRelation() const {
        return *first->get();
    }

    /** Get second relation */
    const RamRelation& getSecondRelation() const {
        return *second->get();
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "SWAP (" << getFirstRelation().getName() << ", " << getSecondRelation().getName() << ")";
        os << std::endl;
    };

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {first.get(), second.get()};
    }

    /** Create clone */
    RamSwap* clone() const override {
        RamSwap* res = new RamSwap(std::unique_ptr<RamRelationReference>(first->clone()),
                std::unique_ptr<RamRelationReference>(second->clone()));
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        first = map(std::move(first));
        second = map(std::move(second));
    }

protected:
    /** first argument of swap statement */
    std::unique_ptr<RamRelationReference> first;

    /** second argument of swap statement */
    std::unique_ptr<RamRelationReference> second;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamSwap*>(&node));
        const auto& other = static_cast<const RamSwap&>(node);
        return getFirstRelation() == other.getFirstRelation() &&
               getSecondRelation() == other.getSecondRelation();
    }
};

/**
 * Insert a fact into a relation
 */
class RamFact : public RamRelationStatement {
public:
    RamFact(std::unique_ptr<RamRelationReference> relRef, std::vector<std::unique_ptr<RamExpression>>&& v)
            : RamRelationStatement(std::move(relRef)), values(std::move(v)) {}

    /** Get arguments of fact */
    std::vector<RamExpression*> getValues() const {
        return toPtrVector(values);
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "INSERT (" << join(values, ",", print_deref<std::unique_ptr<RamExpression>>()) << ") INTO "
           << getRelation().getName();
        os << std::endl;
    };

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        std::vector<const RamNode*> res = RamRelationStatement::getChildNodes();
        for (const auto& cur : values) {
            res.push_back(cur.get());
        }
        return res;
    }

    /** Create clone */
    RamFact* clone() const override {
        RamFact* res = new RamFact(std::unique_ptr<RamRelationReference>(relationRef->clone()), {});
        for (auto& cur : values) {
            res->values.emplace_back(cur->clone());
        }
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        RamRelationStatement::apply(map);
        for (auto& val : values) {
            val = map(std::move(val));
        }
    }

protected:
    /** Arguments of fact */
    std::vector<std::unique_ptr<RamExpression>> values;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamFact*>(&node));
        const auto& other = static_cast<const RamFact&>(node);
        return RamRelationStatement::equal(other) && equal_targets(values, other.values);
    }
};

/**
 * A relational algebra query
 */
class RamQuery : public RamStatement {
public:
    RamQuery(std::unique_ptr<RamOperation> o) : RamStatement(), operation(std::move(o)) {}

    /** Get RAM operation */
    RamOperation& getOperation() const {
        assert(operation);
        return *operation;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "QUERY" << std::endl;
        operation->print(os, tabpos + 1);
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {operation.get()};
    }

    /** Create clone */
    RamQuery* clone() const override {
        RamQuery* res;
        res = new RamQuery(std::unique_ptr<RamOperation>(operation->clone()));
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        operation = map(std::move(operation));
    }

protected:
    /** RAM operation */
    std::unique_ptr<RamOperation> operation;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamQuery*>(&node));
        const auto& other = static_cast<const RamQuery&>(node);
        return getOperation() == other.getOperation();
    }
};

/**
 * Sequence of RAM statements
 *
 * Execute statement one by one from an ordered list of statements.
 */
class RamSequence : public RamStatement {
public:
    RamSequence() : RamStatement() {}

    template <typename... Stmts>
    RamSequence(std::unique_ptr<Stmts>&&... stmts) : RamStatement() {
        // move all the given statements into the vector (not so simple)
        std::unique_ptr<RamStatement> tmp[] = {std::move(stmts)...};
        for (auto& cur : tmp) {
            statements.emplace_back(std::move(cur));
        }
        for (const auto& cur : statements) {
            (void)cur;
            assert(cur);
        }
    }

    /** Add new statement to the end of ordered list */
    void add(std::unique_ptr<RamStatement> stmt) {
        if (stmt) {
            statements.push_back(std::move(stmt));
        }
    }

    /** Get RAM statements from ordered list */
    std::vector<RamStatement*> getStatements() const {
        return toPtrVector(statements);
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        for (const auto& stmt : statements) {
            stmt->print(os, tabpos);
        }
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        std::vector<const RamNode*> res;
        for (const auto& cur : statements) {
            res.push_back(cur.get());
        }
        return res;
    }

    /** Create clone */
    RamSequence* clone() const override {
        auto* res = new RamSequence();
        for (auto& cur : statements) {
            res->add(std::unique_ptr<RamStatement>(cur->clone()));
        }
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        for (auto& stmt : statements) {
            stmt = map(std::move(stmt));
        }
    }

protected:
    /** ordered list of RAM statements */
    std::vector<std::unique_ptr<RamStatement>> statements;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamSequence*>(&node));
        const auto& other = static_cast<const RamSequence&>(node);

        return equal_targets(statements, other.statements);
    }
};

/**
 * Parallel block
 *
 * Execute statements in parallel and wait until all statements have
 * completed their execution before completing the execution of the
 * parallel block.
 */
class RamParallel : public RamStatement {
public:
    RamParallel() : RamStatement() {}

    /** Add new statement to parallel block */
    void add(std::unique_ptr<RamStatement> stmt) {
        if (stmt) {
            statements.push_back(std::move(stmt));
        }
    }

    /** Get statements of parallel block */
    std::vector<RamStatement*> getStatements() const {
        return toPtrVector(statements);
    }

    /* Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "PARALLEL" << std::endl;
        for (auto const& stmt : statements) {
            stmt->print(os, tabpos + 1);
        }
        os << times(" ", tabpos) << "END PARALLEL" << std::endl;
    }

    /** Obtains a list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        std::vector<const RamNode*> res;
        for (const auto& cur : statements) {
            res.push_back(cur.get());
        }
        return res;
    }

    /** Create clone */
    RamParallel* clone() const override {
        auto* res = new RamParallel();
        for (auto& cur : statements) {
            res->add(std::unique_ptr<RamStatement>(cur->clone()));
        }
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        for (auto& stmt : statements) {
            stmt = map(std::move(stmt));
        }
    }

protected:
    /** list of statements executed in parallel */
    std::vector<std::unique_ptr<RamStatement>> statements;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamParallel*>(&node));
        const auto& other = static_cast<const RamParallel&>(node);
        return equal_targets(statements, other.statements);
    }
};

/**
 * Statement loop
 *
 * Execute the statement repeatedly until statement terminates loop via an exit statement
 */
class RamLoop : public RamStatement {
public:
    RamLoop(std::unique_ptr<RamStatement> b) : RamStatement(), body(std::move(b)) {}

    template <typename... Stmts>
    RamLoop(std::unique_ptr<RamStatement> f, std::unique_ptr<RamStatement> s, std::unique_ptr<Stmts>... rest)
            : RamStatement(),
              body(std::make_unique<RamSequence>(std::move(f), std::move(s), std::move(rest)...)) {}

    /** Get loop body */
    const RamStatement& getBody() const {
        return *body;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "LOOP" << std::endl;
        body->print(os, tabpos + 1);
        os << times(" ", tabpos) << "END LOOP" << std::endl;
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {body.get()};
    }

    /** Create clone */
    RamLoop* clone() const override {
        RamLoop* res = new RamLoop(std::unique_ptr<RamStatement>(body->clone()));
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        body = map(std::move(body));
    }

protected:
    /** Body of loop */
    std::unique_ptr<RamStatement> body;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamLoop*>(&node));
        const auto& other = static_cast<const RamLoop&>(node);
        return *body == *other.body;
    }
};

/**
 * Exit statement for a loop
 *
 * Exits a loop if exit condition holds.
 */
class RamExit : public RamStatement {
public:
    RamExit(std::unique_ptr<RamCondition> c) : RamStatement(), condition(std::move(c)) {}

    /** Get exit condition */
    const RamCondition& getCondition() const {
        assert(condition);
        return *condition;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "EXIT " << getCondition() << std::endl;
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {condition.get()};
    }

    /** Create clone */
    RamExit* clone() const override {
        RamExit* res = new RamExit(std::unique_ptr<RamCondition>(condition->clone()));
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        condition = map(std::move(condition));
    }

protected:
    /** exit condition */
    std::unique_ptr<RamCondition> condition;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamExit*>(&node));
        const auto& other = static_cast<const RamExit&>(node);
        return *condition == *other.condition;
    }
};

/**
 * Execution time logger for a statement
 *
 * Logs the execution time of a statement. Before and after
 * the execution of the logging statement the wall-clock time
 * is taken to compute the time duration for the statement.
 * Duration and logging message is printed after the execution
 * of the statement.
 */
class RamLogTimer : public RamStatement {
public:
    RamLogTimer(
            std::unique_ptr<RamStatement> stmt, std::string msg, std::unique_ptr<RamRelationReference> relRef)
            : RamStatement(), statement(std::move(stmt)), message(std::move(msg)),
              relationRef(std::move(relRef)) {
        assert(statement);
    }

    /** get logging message */
    const std::string& getMessage() const {
        return message;
    }

    /** get logging statement */
    const RamStatement& getStatement() const {
        assert(statement);
        return *statement;
    }

    /** get logged relation */
    const RamRelation* getRelation() const {
        if (relationRef != nullptr) {
            return relationRef->get();
        } else {
            return nullptr;
        }
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "START_TIMER \"" << stringify(message) << "\"" << std::endl;
        statement->print(os, tabpos + 1);
        os << times(" ", tabpos) << "END_TIMER" << std::endl;
    }

    /** Obtains a list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {statement.get()};
    }

    /** Create clone */
    RamLogTimer* clone() const override {
        RamLogTimer* res = new RamLogTimer(std::unique_ptr<RamStatement>(statement->clone()), message,
                std::unique_ptr<RamRelationReference>(relationRef->clone()));
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        statement = map(std::move(statement));
    }

protected:
    /** logging statement */
    std::unique_ptr<RamStatement> statement;

    /** logging message */
    std::string message;

    /** Relation */
    std::unique_ptr<RamRelationReference> relationRef;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamLogTimer*>(&node));
        const auto& other = static_cast<const RamLogTimer&>(node);
        return *statement == *other.statement && message == other.message;
    }
};

/**
 * Debug statement
 */
class RamDebugInfo : public RamStatement {
public:
    RamDebugInfo(std::unique_ptr<RamStatement> stmt, std::string msg)
            : RamStatement(), statement(std::move(stmt)), message(std::move(msg)) {
        assert(statement);
    }

    /** Get debugging message */
    const std::string& getMessage() const {
        return message;
    }

    /** Get debugging statement */
    const RamStatement& getStatement() const {
        assert(statement);
        return *statement;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "BEGIN_DEBUG \"" << stringify(message) << "\"" << std::endl;
        statement->print(os, tabpos + 1);
        os << times(" ", tabpos) << "END_DEBUG" << std::endl;
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {statement.get()};
    }

    /** Create clone */
    RamDebugInfo* clone() const override {
        RamDebugInfo* res = new RamDebugInfo(std::unique_ptr<RamStatement>(statement->clone()), message);
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        statement = map(std::move(statement));
    }

protected:
    /** debugging statement */
    std::unique_ptr<RamStatement> statement;

    /** debugging message */
    std::string message;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamLogTimer*>(&node));
        const auto& other = static_cast<const RamLogTimer&>(node);
        return getStatement() == other.getStatement() && getMessage() == other.getMessage();
    }
};

/**
 * Stratum statement
 *
 * Wrap strata of program
 */
class RamStratum : public RamStatement {
public:
    RamStratum(std::unique_ptr<RamStatement> b, const int i) : RamStatement(), body(std::move(b)), index(i) {}

    /** Get stratum body */
    const RamStatement& getBody() const {
        return *body;
    }

    /** Get stratum index */
    const int getIndex() const {
        return index;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos);
        os << "BEGIN_STRATUM " << index << std::endl;
        body->print(os, tabpos + 1);
        os << times(" ", tabpos);
        os << "END_STRATUM " << index << std::endl;
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {body.get()};
    }

    /** Create clone */
    RamStratum* clone() const override {
        RamStratum* res = new RamStratum(std::unique_ptr<RamStatement>(body->clone()), index);
        return res;
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {
        body = map(std::move(body));
    }

protected:
    /** Body of stratum */
    std::unique_ptr<RamStatement> body;
    const int index;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamStratum*>(&node));
        const auto& other = static_cast<const RamStratum&>(node);
        return *body == *other.body && index == other.index;
    }
};

/**
 *  Log relation size and a logging message.
 */
class RamLogSize : public RamRelationStatement {
public:
    RamLogSize(std::unique_ptr<RamRelationReference> relRef, std::string message)
            : RamRelationStatement(std::move(relRef)), message(std::move(message)) {}

    /** Get logging message */
    const std::string& getMessage() const {
        return message;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << times(" ", tabpos) << "LOGSIZE " << getRelation().getName();
        os << " TEXT "
           << "\"" << stringify(message) << "\"";
        os << std::endl;
    }

    /** Create clone */
    RamLogSize* clone() const override {
        RamLogSize* res =
                new RamLogSize(std::unique_ptr<RamRelationReference>(relationRef->clone()), message);
        return res;
    }

protected:
    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamLogSize*>(&node));
        const auto& other = static_cast<const RamLogSize&>(node);
        RamRelationStatement::equal(other);
        return getMessage() == other.getMessage();
    }

protected:
    /** logging message */
    std::string message;
};

#ifdef USE_MPI

class RamRecv : public RamRelationStatement {
public:
    RamRecv(std::unique_ptr<RamRelationReference> r, const int s)
            : RamRelationStatement(std::move(r)), sourceStratum(s) {}

    const int getSourceStratum() const {
        return sourceStratum;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << std::string(tabpos, '\t');
        os << "RECV DATA FOR " << getRelation().getName() << " FROM STRATUM {" << sourceStratum << "}";
    };

    /** Create clone */
    RamRecv* clone() const override {
        RamRecv* res =
                new RamRecv(std::unique_ptr<RamRelationReference>(relationRef->clone()), sourceStratum);
        return res;
    }

protected:
    const int sourceStratum;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamRecv*>(&node));
        const auto& other = static_cast<const RamRecv&>(node);
        RamRelationStatement::equal(other);
        return sourceStratum == other.sourceStratum;
    }
};

class RamSend : public RamRelationStatement {
public:
    RamSend(std::unique_ptr<RamRelationReference> r, const std::set<size_t> s)
            : RamRelationStatement(std::move(r)), destinationStrata(s) {}

    const std::set<size_t> getDestinationStrata() const {
        return destinationStrata;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << std::string(tabpos, '\t');
        os << "SEND DATA FOR " << getRelation().getName() << " TO STRATUM {";
        auto it = destinationStrata.begin();
        os << *it;
        ++it;
        while (it != destinationStrata.end()) {
            os << ", " << *it;
            ++it;
        }
        os << "}";
    };

    /** Create clone */
    RamSend* clone() const override {
        RamSend* res =
                new RamSend(std::unique_ptr<RamRelationReference>(relationRef->clone()), destinationStrata);
        return res;
    }

protected:
    const std::set<size_t> destinationStrata;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamSend*>(&node));
        const auto& other = static_cast<const RamSend&>(node);
        return destinationStrata == other.destinationStrata;
    }
};

class RamNotify : public RamStatement {
public:
    RamNotify() : RamStatement() {}

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << std::string(tabpos, '\t');
        os << "NOTIFY";
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {};
    }

    /** Create clone */
    RamNotify* clone() const override {
        return new RamNotify();
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {}

protected:
    /** Check equality */
    bool equal(const RamNode& node) const override {
        return true;
    }
};

class RamWait : public RamStatement {
public:
    RamWait(const size_t c) : RamStatement(), count(c) {}

    /** Get count of termination signals required. */
    const int getCount() const {
        return count;
    }

    /** Pretty print */
    void print(std::ostream& os, int tabpos) const override {
        os << std::string(tabpos, '\t');
        os << "WAIT";
    }

    /** Obtain list of child nodes */
    std::vector<const RamNode*> getChildNodes() const override {
        return {};
    }

    /** Create clone */
    RamWait* clone() const override {
        return new RamWait(count);
    }

    /** Apply mapper */
    void apply(const RamNodeMapper& map) override {}

protected:
    const size_t count;

    /** Check equality */
    bool equal(const RamNode& node) const override {
        assert(nullptr != dynamic_cast<const RamWait*>(&node));
        const auto& other = static_cast<const RamWait&>(node);
        return other.count == count;
    }
};

#endif

}  // end of namespace souffle
