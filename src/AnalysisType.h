/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AnalysisType.h
 *
 * A collection of analysis types that may appear in a type lattice.
 *
 ***********************************************************************/

#pragma once

#include "AstType.h"
#include "Util.h"
#include <cassert>
#include <sstream>

namespace souffle {

enum class Kind { SYMBOL, NUMBER, RECORD };

inline std::ostream& operator<<(std::ostream& os, Kind kind) {
    switch (kind) {
        case Kind::SYMBOL:
            os << "symbol";
            break;
        case Kind::NUMBER:
            os << "number";
            break;
        case Kind::RECORD:
            os << "record";
            break;
    }

    return os;
}

/** Represents a node in the type lattice during type analysis */
class AnalysisType {
public:
    // -- constructors --

    AnalysisType() = default;
    AnalysisType(const AnalysisType&) = default;
    AnalysisType(AnalysisType&&) = default;

    // -- validity --

    // checks that the type is not any form of top or bottom type
    virtual bool isValidType() const = 0;

    // -- operators --

    AnalysisType& operator=(const AnalysisType&) = default;
    AnalysisType& operator=(AnalysisType&&) = default;

    bool operator==(const AnalysisType& other) const {
        return this == &other || (typeid(*this) == typeid(other) && equal(other));
    }

    bool operator!=(const AnalysisType& other) const {
        return !(*this == other);
    }

    virtual AnalysisType* clone() const = 0;

    // -- printing --

    virtual void print(std::ostream& out) const = 0;

    friend std::ostream& operator<<(std::ostream& out, const AnalysisType& type) {
        type.print(out);
        return out;
    }

protected:
    virtual bool equal(const AnalysisType& other) const = 0;
};

/** The top element of the lattice */
class TopAnalysisType : public AnalysisType {
public:
    TopAnalysisType() = default;
    TopAnalysisType(const TopAnalysisType&) = default;
    TopAnalysisType(TopAnalysisType&&) = default;

    bool isValidType() const override {
        return false;
    }

    TopAnalysisType* clone() const override {
        return new TopAnalysisType();
    }

    void print(std::ostream& out) const override {
        out << "top type";
    }

protected:
    bool equal(const AnalysisType& type) const override {
        // all top types are equivalent
        assert(dynamic_cast<const TopAnalysisType*>(&type) != nullptr);
        return true;
    }
};

/** The bottom element of the lattice */
class BottomAnalysisType : public AnalysisType {
public:
    BottomAnalysisType() = default;
    BottomAnalysisType(const BottomAnalysisType&) = default;
    BottomAnalysisType(BottomAnalysisType&&) = default;

    bool isValidType() const override {
        return false;
    }

    BottomAnalysisType* clone() const override {
        return new BottomAnalysisType();
    }

    void print(std::ostream& out) const override {
        out << "bottom type";
    }

protected:
    bool equal(const AnalysisType& type) const override {
        // all bottom types are equivalent
        assert(dynamic_cast<const BottomAnalysisType*>(&type) != nullptr);
        return true;
    }
};

/** A lattice element that is neither the top nor the bottom element */
class InnerAnalysisType : public AnalysisType {
public:
    InnerAnalysisType() = default;
    InnerAnalysisType(const InnerAnalysisType&) = default;
    InnerAnalysisType(InnerAnalysisType&&) = default;

    // each inner type belongs to a separate sublattice, depending on the kind
    virtual Kind getKind() const = 0;

    virtual InnerAnalysisType* clone() const = 0;
};

/** A top primitive in a lattice, just below the top element */
class TopPrimitiveAnalysisType : public InnerAnalysisType {
public:
    TopPrimitiveAnalysisType(Kind kind) : kind(kind) {}
    TopPrimitiveAnalysisType(const TopPrimitiveAnalysisType&) = default;
    TopPrimitiveAnalysisType(TopPrimitiveAnalysisType&&) = default;

    Kind getKind() const override {
        return kind;
    }

    TopPrimitiveAnalysisType* clone() const override {
        return new TopPrimitiveAnalysisType(kind);
    }

    bool isValidType() const override {
        return (kind != Kind::RECORD);
    }

    void print(std::ostream& out) const override {
        switch (kind) {
            case Kind::SYMBOL:
                out << "symbol";
                break;
            case Kind::NUMBER:
                out << "number";
                break;
            case Kind::RECORD:
                out << "any record";
                break;
        }
    }

protected:
    bool equal(const AnalysisType& type) const override {
        assert(dynamic_cast<const TopPrimitiveAnalysisType*>(&type) != nullptr);
        const auto& other = static_cast<const TopPrimitiveAnalysisType&>(type);
        return kind == other.kind;
    }

private:
    Kind kind;
};

/** A constant primitive in the lattice, just above the bottom element */
class ConstantAnalysisType : public InnerAnalysisType {
public:
    ConstantAnalysisType(Kind kind) : kind(kind) {}
    ConstantAnalysisType(const ConstantAnalysisType&) = default;
    ConstantAnalysisType(ConstantAnalysisType&&) = default;

    Kind getKind() const override {
        return kind;
    }

    bool isValidType() const override {
        return true;
    }

    ConstantAnalysisType* clone() const override {
        return new ConstantAnalysisType(kind);
    }

    void print(std::ostream& out) const override {
        switch (kind) {
            case Kind::SYMBOL:
                out << "symbol constant";
                break;
            case Kind::NUMBER:
                out << "number constant";
                break;
            case Kind::RECORD:
                out << "nil record";
                break;
        }
    }

protected:
    bool equal(const AnalysisType& type) const override {
        assert(dynamic_cast<const ConstantAnalysisType*>(&type) != nullptr);
        const auto& other = static_cast<const ConstantAnalysisType&>(type);
        return kind == other.kind;
    }

private:
    Kind kind;
};

/** A bottom primitive in the lattice, just above its respective constant primitive type */
class BottomPrimitiveAnalysisType : public InnerAnalysisType {
public:
    BottomPrimitiveAnalysisType(Kind kind) : kind(kind) {}
    BottomPrimitiveAnalysisType(const BottomPrimitiveAnalysisType&) = default;
    BottomPrimitiveAnalysisType(BottomPrimitiveAnalysisType&&) = default;

    Kind getKind() const override {
        return kind;
    }

    bool isValidType() const override {
        return false;
    }

    BottomPrimitiveAnalysisType* clone() const override {
        return new BottomPrimitiveAnalysisType(kind);
    }

    void print(std::ostream& out) const override {
        switch (kind) {
            case Kind::SYMBOL:
                out << "bottom symbol";
                break;
            case Kind::NUMBER:
                out << "bottom number";
                break;
            case Kind::RECORD:
                out << "bottom record";
                break;
        }
    }

protected:
    bool equal(const AnalysisType& type) const override {
        assert(dynamic_cast<const BottomPrimitiveAnalysisType*>(&type) != nullptr);
        const auto& other = static_cast<const BottomPrimitiveAnalysisType&>(type);
        return kind == other.kind;
    }

private:
    Kind kind;
};

/** A base type in the lattice, just above the bottom primitives */
class BaseAnalysisType : public InnerAnalysisType {
public:
    BaseAnalysisType(Kind kind, AstTypeIdentifier name) : kind(kind), name(name) {}
    BaseAnalysisType(const BaseAnalysisType&) = default;
    BaseAnalysisType(BaseAnalysisType&&) = default;

    Kind getKind() const override {
        return kind;
    }

    BaseAnalysisType* clone() const override {
        return new BaseAnalysisType(kind, name);
    }

    bool isValidType() const override {
        return true;
    }

    bool operator<(const BaseAnalysisType& other) const {
        if (kind == other.kind) {
            return name < other.name;
        }
        return kind < other.kind;
    }

    void print(std::ostream& out) const override {
        out << name;
    }

protected:
    Kind kind;
    AstTypeIdentifier name;

    bool equal(const AnalysisType& type) const override {
        assert(dynamic_cast<const BaseAnalysisType*>(&type) != nullptr);
        const auto& other = static_cast<const BaseAnalysisType&>(type);
        return kind == other.kind && name == other.name;
    }
};

/** A record base type, just above the record bottom primitive */
class RecordAnalysisType : public BaseAnalysisType {
public:
    RecordAnalysisType(AstTypeIdentifier name) : BaseAnalysisType(Kind::RECORD, name) {}
    RecordAnalysisType(const RecordAnalysisType&) = default;
    RecordAnalysisType(RecordAnalysisType&&) = default;

    void addField(std::unique_ptr<InnerAnalysisType> field) {
        assert(field->isValidType() && "field must be valid type");
        fields.push_back(std::move(field));
    }

    RecordAnalysisType* clone() const override {
        auto* clone = new RecordAnalysisType(name);
        for (const auto& field : fields) {
            clone->addField(std::unique_ptr<InnerAnalysisType>(field->clone()));
        }
        return clone;
    }

    std::vector<InnerAnalysisType*> getFields() const {
        return toPtrVector(fields);
    }

    void print(std::ostream& out) const override {
        out << name;
    }

protected:
    bool equal(const AnalysisType& type) const override {
        assert(dynamic_cast<const RecordAnalysisType*>(&type) != nullptr);
        const auto& other = static_cast<const RecordAnalysisType&>(type);
        return name == other.name && equal_targets(fields, other.fields);
    }

private:
    std::vector<std::unique_ptr<InnerAnalysisType>> fields{};
};

/** A union type, sitting between base types and the top primitive types */
class UnionAnalysisType : public InnerAnalysisType {
public:
    UnionAnalysisType(std::set<BaseAnalysisType> baseTypes);
    UnionAnalysisType(std::set<BaseAnalysisType> baseTypes, AstTypeIdentifier name);
    UnionAnalysisType(const UnionAnalysisType&) = default;
    UnionAnalysisType(UnionAnalysisType&&) = default;

    const std::set<BaseAnalysisType>& getBaseTypes() const {
        return baseTypes;
    }

    void setName(AstTypeIdentifier name) {
        representation = toString(name);
    }

    void setName(std::string name) {
        reprsentation = repr;
    }

    Kind getKind() const override {
        return kind;
    }

    UnionAnalysisType* clone() const override {
        auto* clone = new UnionAnalysisType(baseTypes);
        clone->setName(representation);
        return clone;
    }

    bool isValidType() const override {
        return true;
    }

    void print(std::ostream& out) const override {
        out << representation;
    }

protected:
    bool equal(const AnalysisType& type) const override {
        assert(dynamic_cast<const UnionAnalysisType*>(&type) != nullptr);
        const auto& other = static_cast<const UnionAnalysisType&>(type);
        return kind == other.kind && baseTypes == other.baseTypes;
    }

private:
    Kind kind;
    std::set<BaseAnalysisType> baseTypes;
    std::string representation;
};

}  // end of namespace souffle
