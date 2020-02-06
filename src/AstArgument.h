/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstArgument.h
 *
 * Define the classes Argument, Variable, and Constant to represent
 * variables and constants in literals. Variable and Constant are
 * sub-classes of class argument.
 *
 ***********************************************************************/

#pragma once

#include "AstAbstract.h"
#include "AstNode.h"
#include "AstType.h"
#include "FunctorOps.h"
#include "SymbolTable.h"
#include "Util.h"
#include <cassert>
#include <cstddef>
#include <memory>
#include <ostream>
#include <string>
#include <utility>
#include <vector>

namespace souffle {


/**
 * Subclass of Argument that represents a named variable
 */
class AstVariable : public AstArgument {
public:
    AstVariable(std::string n) : AstArgument(), name(std::move(n)) {}

    void print(std::ostream& os) const override {
        os << name;
    }

    /** Updates this variable name */
    void setName(const std::string& name) {
        this->name = name;
    }

    /** @return Variable name */
    const std::string& getName() const {
        return name;
    }

    AstVariable* clone() const override {
        auto* res = new AstVariable(name);
        res->setSrcLoc(getSrcLoc());
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstVariable*>(&node));
        const auto& other = static_cast<const AstVariable&>(node);
        return name == other.name;
    }

    /** variable name */
    std::string name;
};

/**
 * Subclass of Argument that represents an unnamed variable
 */
class AstUnnamedVariable : public AstArgument {
public:
    AstUnnamedVariable() : AstArgument() {}

    void print(std::ostream& os) const override {
        os << "_";
    }

    AstUnnamedVariable* clone() const override {
        auto* res = new AstUnnamedVariable();
        res->setSrcLoc(getSrcLoc());
        return res;
    }
};

/**
 * Subclass of Argument that represents a counter (for projections only)
 */
class AstCounter : public AstArgument {
public:
    AstCounter() : AstArgument() {}

    void print(std::ostream& os) const override {
        os << "$";
    }

    AstCounter* clone() const override {
        auto* res = new AstCounter();
        res->setSrcLoc(getSrcLoc());
        return res;
    }
};

/**
 * Subclass of Argument that represents a datalog constant value
 */
class AstConstant : public AstArgument {
public:
    AstConstant(RamDomain i) : AstArgument(), idx(i) {}

    /** @return Return the index of this constant in the SymbolTable */
    RamDomain getIndex() const {
        return idx;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstConstant*>(&node));
        const auto& other = static_cast<const AstConstant&>(node);
        return idx == other.idx;
    }

    /** Index of this Constant in the SymbolTable */
    RamDomain idx;
};

/**
 * Subclass of Argument that represents a datalog constant value
 */
class AstStringConstant : public AstConstant {
public:
    AstStringConstant(SymbolTable& symTable, const std::string& c)
            : AstConstant(symTable.lookup(c)), symTable(symTable) {}

    void print(std::ostream& os) const override {
        os << "\"" << getConstant() << "\"";
    }

    /** @return String representation of this Constant */
    const std::string& getConstant() const {
        return symTable.resolve(getIndex());
    }

    AstStringConstant* clone() const override {
        auto* res = new AstStringConstant(symTable, getIndex());
        res->setSrcLoc(getSrcLoc());
        return res;
    }

private:
    // TODO (b-scholz): Remove Symbol Table / store as string / change hierarchy
    // Don't use numbers to store strings in AST
    AstStringConstant(SymbolTable& symTable, size_t index) : AstConstant(index), symTable(symTable) {}

    /** Symbol table */ 
    SymbolTable& symTable;
};

/**
 * Subclass of Argument that represents a datalog constant value
 */
class AstNumberConstant : public AstConstant {
public:
    AstNumberConstant(RamDomain num) : AstConstant(num) {}

    void print(std::ostream& os) const override {
        os << idx;
    }

    AstNumberConstant* clone() const override {
        auto* res = new AstNumberConstant(getIndex());
        res->setSrcLoc(getSrcLoc());
        return res;
    }
};

/**
 * Subclass of AstConstant that represents a null-constant (no record)
 */
class AstNilConstant : public AstConstant {
public:
    AstNilConstant() : AstConstant(0) {}

    void print(std::ostream& os) const override {
        os << "nil";
    }

    AstNilConstant* clone() const override {
        auto* res = new AstNilConstant();
        res->setSrcLoc(getSrcLoc());
        return res;
    }
};

/**
 * A common base class for AST functors
 */
// TODO (azreika): consider pushing some common Intr/Extr functor functionality here
class AstFunctor : public AstArgument {};

/**
 * Subclass of AstFunctor that represents an intrinsic (built-in) functor
 */
class AstIntrinsicFunctor : public AstFunctor {
public:
    template <typename... Operands>
    AstIntrinsicFunctor(FunctorOp function, Operands... operands) : function(function) {
        std::unique_ptr<AstArgument> tmp[] = {std::move(operands)...};
        for (auto& cur : tmp) {
            args.push_back(std::move(cur));
        }

        assert(isValidFunctorOpArity(function, args.size()) && "invalid number of arguments for functor");
    }

    AstIntrinsicFunctor(FunctorOp function, std::vector<std::unique_ptr<AstArgument>> operands)
            : function(function), args(std::move(operands)){};

    void print(std::ostream& os) const override {
        if (isInfixFunctorOp(function)) {
            os << "(";
            os << join(args, getSymbolForFunctorOp(function), print_deref<std::unique_ptr<AstArgument>>());
            os << ")";
        } else {
            os << getSymbolForFunctorOp(function);
            os << "(";
            os << join(args, ",", print_deref<std::unique_ptr<AstArgument>>());
            os << ")";
        }
    }

    /** Get i-th argument */ 
    AstArgument* getArg(size_t idx) const {
        assert(idx >= 0 && idx < args.size() && "wrong argument");
        return args[idx].get();
    }

    /** Get function */ 
    FunctorOp getFunction() const {
        return function;
    }

    /** Get arity */ 
    size_t getArity() const {
        return args.size();
    }

    /** Get arguments */ 
    std::vector<AstArgument*> getArguments() const {
        return toPtrVector(args);
    }

    /** Set arguments */ 
    void setArg(size_t idx, std::unique_ptr<AstArgument> arg) {
        assert(idx >= 0 && idx < args.size() && "wrong argument");
        args[idx] = std::move(arg);
    }

    /** Check if the return value of this functor is a number type. */
    bool isNumerical() const {
        return isNumericFunctorOp(function);
    }

    /** Check if the return value of this functor is a symbol type. */
    bool isSymbolic() const {
        return isSymbolicFunctorOp(function);
    }

    /** Check if the argument of this functor is a number type. */
    bool acceptsNumbers(size_t arg) const {
        return functorOpAcceptsNumbers(arg, function);
    }

    /** Check if the argument of this functor is a symbol type. */
    bool acceptsSymbols(size_t arg) const {
        return functorOpAcceptsSymbols(arg, function);
    }

    AstIntrinsicFunctor* clone() const override {
        std::vector<std::unique_ptr<AstArgument>> argsCopy;
        for (auto& arg : args) {
            argsCopy.emplace_back(arg->clone());
        }
        auto res = new AstIntrinsicFunctor(function, std::move(argsCopy));
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& arg : args) {
            arg = map(std::move(arg));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        auto res = AstArgument::getChildNodes();
        for (auto& arg : args) {
            res.push_back(arg.get());
        }
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstIntrinsicFunctor*>(&node));
        const auto& other = static_cast<const AstIntrinsicFunctor&>(node);
        return function == other.function && equal_targets(args, other.args);
    }

    /** Function */ 
    FunctorOp function;

    /** Arguments */ 
    std::vector<std::unique_ptr<AstArgument>> args;
};

/**
 * Subclass of AstFunctor that represents an extrinsic (user-defined) functor
 */
class AstUserDefinedFunctor : public AstFunctor {
public:
    AstUserDefinedFunctor() = default;

    AstUserDefinedFunctor(std::string name) : name(std::move(name)) {}

    /** get name */
    const std::string& getName() const {
        return name;
    }

    /** set name */
    void setName(const std::string& n) {
        name = n;
    }

    /** get argument */
    const AstArgument* getArg(size_t idx) const {
        assert(idx >= 0 && idx < args.size() && "argument index out of bounds");
        return args[idx].get();
    }

    /** get number of arguments */
    size_t getArgCount() const {
        return args.size();
    }

    /** set argument */
    void setArg(size_t idx, std::unique_ptr<AstArgument> arg) {
        assert(idx >= 0 && idx < args.size() && "argument index out of bounds");
        args[idx] = std::move(arg);
    }

    /** get arguments */
    std::vector<AstArgument*> getArguments() const {
        return toPtrVector(args);
    }

    /** add argument to argument list */
    void add(std::unique_ptr<AstArgument> arg) {
        args.push_back(std::move(arg));
    }

    /** print user-defined functor */
    void print(std::ostream& os) const override {
        os << '@' << name << "(" << join(args, ",", print_deref<std::unique_ptr<AstArgument>>()) << ")";
    }

    AstUserDefinedFunctor* clone() const override {
        auto res = new AstUserDefinedFunctor();
        for (auto& cur : args) {
            res->args.emplace_back(cur->clone());
        }
        res->setSrcLoc(getSrcLoc());
        res->setName(getName());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& arg : args) {
            arg = map(std::move(arg));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        auto res = AstArgument::getChildNodes();
        for (auto& cur : args) {
            res.push_back(cur.get());
        }
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstUserDefinedFunctor*>(&node));
        const auto& other = static_cast<const AstUserDefinedFunctor&>(node);
        return name == other.name && equal_targets(args, other.args);
    }

protected:
    /** name of user-defined functor */
    std::string name;

    /** arguments of user-defined functor */
    std::vector<std::unique_ptr<AstArgument>> args;
};

/**
 * An argument that takes a list of values and combines them into a
 * new record.
 */
class AstRecordInit : public AstArgument {
public:
    AstRecordInit() = default;

    void add(std::unique_ptr<AstArgument> arg) {
        args.push_back(std::move(arg));
    }

    std::vector<AstArgument*> getArguments() const {
        return toPtrVector(args);
    }

    void print(std::ostream& os) const override {
        os << "[" << join(args, ",", print_deref<std::unique_ptr<AstArgument>>()) << "]";
    }

    AstRecordInit* clone() const override {
        auto res = new AstRecordInit();
        for (auto& cur : args) {
            res->args.emplace_back(cur->clone());
        }
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& arg : args) {
            arg = map(std::move(arg));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        auto res = AstArgument::getChildNodes();
        for (auto& cur : args) {
            res.push_back(cur.get());
        }
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstRecordInit*>(&node));
        const auto& other = static_cast<const AstRecordInit&>(node);
        return equal_targets(args, other.args);
    }

    /** The list of components to be aggregated into a record */
    std::vector<std::unique_ptr<AstArgument>> args;
};

/**
 * An argument capable of casting a value of one type into another.
 */
class AstTypeCast : public AstArgument {
public:
    AstTypeCast(std::unique_ptr<AstArgument> value, AstTypeIdentifier type)
            : value(std::move(value)), type(std::move(type)) {}

    void print(std::ostream& os) const override {
        os << "as(" << *value << "," << type << ")";
    }

    /** Get value */ 
    AstArgument* getValue() const {
        return value.get();
    }
  
    /** Get type */ 
    const AstTypeIdentifier& getType() const {
        return type;
    }

    /** Set type */ 
    void setType(const AstTypeIdentifier& type) {
        this->type = type;
    }

    std::vector<const AstNode*> getChildNodes() const override {
        auto res = AstArgument::getChildNodes();
        res.push_back(value.get());
        return res;
    }

    AstTypeCast* clone() const override {
        auto res = new AstTypeCast(std::unique_ptr<AstArgument>(value->clone()), type);
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        value = map(std::move(value));
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstTypeCast*>(&node));
        const auto& other = static_cast<const AstTypeCast&>(node);
        return type == other.type && *value == *other.value;
    }

    /** The value to be casted */
    std::unique_ptr<AstArgument> value;

    /** The target type name */
    AstTypeIdentifier type;
};

/**
 * An argument aggregating a value from a sub-query.
 * TODO (b-scholz): fix body literal interface; 
 * remove getters/setters for individual literals  
 */
class AstAggregator : public AstArgument {
public:
    /**
     * The kind of utilised aggregation operator.
     * Note: lower-case is utilized due to a collision with
     *  constants in the parser.
     */
    enum Op { min, max, count, sum };

    /** Creates a new aggregation node */
    AstAggregator(Op fun) : fun(fun), expression(nullptr) {}

    void print(std::ostream& os) const override {
        switch (fun) {
            case sum:
                os << "sum";
                break;
            case min:
                os << "min";
                break;
            case max:
                os << "max";
                break;
            case count:
                os << "count";
                break;
            default:
                break;
        }
        if (expression) {
            os << " " << *expression;
        }
        os << " : ";
        if (body.size() > 1) {
            os << "{ ";
        }
        os << join(body, ", ", print_deref<std::unique_ptr<AstLiteral>>());
        if (body.size() > 1) {
            os << " }";
        }
    }


    /** Get aggregate operator */ 
    Op getOperator() const {
        return fun;
    }

    /** Set target expression */ 
    void setTargetExpression(std::unique_ptr<AstArgument> arg) {
        expression = std::move(arg);
    }

    /** Get target expression */ 
    const AstArgument* getTargetExpression() const {
        return expression.get();
    }

    /** Get body literals */ 
    std::vector<AstLiteral*> getBodyLiterals() const {
        return toPtrVector(body);
    }

    /** Clear body literals */ 
    void clearBodyLiterals() {
        body.clear();
    }

    /** Add body literal */ 
    void addBodyLiteral(std::unique_ptr<AstLiteral> lit) {
        body.push_back(std::move(lit));
    }

    std::vector<const AstNode*> getChildNodes() const override {
        auto res = AstArgument::getChildNodes();
        if (expression) {
            res.push_back(expression.get());
        }
        for (auto& cur : body) {
            res.push_back(cur.get());
        }
        return res;
    }

    AstAggregator* clone() const override {
        auto res = new AstAggregator(fun);
        res->expression = (expression) ? std::unique_ptr<AstArgument>(expression->clone()) : nullptr;
        for (const auto& cur : body) {
            res->body.emplace_back(cur->clone());
        }
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        if (expression) {
            expression = map(std::move(expression));
        }
        for (auto& cur : body) {
            cur = map(std::move(cur));
        }
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstAggregator*>(&node));
        const auto& other = static_cast<const AstAggregator&>(node);
        return fun == other.fun && equal_ptr(expression, other.expression) && equal_targets(body, other.body);
    }

private:
    /** The aggregation operator of this aggregation step */
    Op fun;

    /** The expression to be aggregated */
    std::unique_ptr<AstArgument> expression;

    /** A list of body-literals forming a sub-query which's result is projected and aggregated */
    std::vector<std::unique_ptr<AstLiteral>> body;
};

/**
 * An argument taking its value from an argument of a RAM subroutine
 */
class AstSubroutineArgument : public AstArgument {
public:
    AstSubroutineArgument(size_t n) : AstArgument(), number(n) {}

    void print(std::ostream& os) const override {
        os << "arg_" << number;
    }

    /** Return argument number */
    size_t getNumber() const {
        return number;
    }

    AstSubroutineArgument* clone() const override {
        auto* res = new AstSubroutineArgument(number);
        res->setSrcLoc(getSrcLoc());
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstSubroutineArgument*>(&node));
        const auto& other = static_cast<const AstSubroutineArgument&>(node);
        return number == other.number;
    }

private:
    /** Index of argument in argument list*/
    size_t number;
};

}  // end of namespace souffle
