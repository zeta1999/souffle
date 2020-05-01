/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstType.h
 *
 * Defines a type, i.e., disjoint supersets of the universe
 *
 ***********************************************************************/

#pragma once

#include "AstAttribute.h"
#include "AstNode.h"
#include "AstQualifiedName.h"
#include "RamTypes.h"
#include "Util.h"

#include <iostream>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

/**
 *  @class Type
 *  @brief An abstract base class for types within the AST.
 */
class AstType : public AstNode {
public:
    AstType(AstQualifiedName name = {}, SrcLocation loc = {})
            : AstNode(std::move(loc)), name(std::move(name)) {}

    /** get type name */
    const AstQualifiedName& getQualifiedName() const {
        return name;
    }

    /** set type name */
    void setQualifiedName(AstQualifiedName name) {
        this->name = std::move(name);
    }

    AstType* clone() const override = 0;

private:
    /** type name */
    AstQualifiedName name;
};

/**
 * A subset type is named type that can either be a sub-type of
 * the predefined types (float/unsigned/number/symbol).
 */
class AstSubsetType : public AstType {
public:
    AstSubsetType(AstQualifiedName name, TypeAttribute type, SrcLocation loc = {})
            : AstType(std::move(name), std::move(loc)), type(type) {}

    AstSubsetType* clone() const override {
        return new AstSubsetType(getQualifiedName(), type, getSrcLoc());
    }

    TypeAttribute getTypeAttribute() const {
        return type;
    }

protected:
    void print(std::ostream& os) const override {
        os << ".type " << getQualifiedName() << " <: ";
        switch (type) {
            case TypeAttribute::Signed: os << "number"; break;
            case TypeAttribute::Unsigned: os << "unsigned"; break;
            case TypeAttribute::Float: os << "float"; break;
            case TypeAttribute::Symbol: os << "symbol"; break;
            case TypeAttribute::Record: fatal("Invalid type");
        }
    }

    bool equal(const AstNode& node) const override {
        const auto& other = static_cast<const AstSubsetType&>(node);
        return getQualifiedName() == other.getQualifiedName() && type == other.type;
    }

private:
    /** type attribute */
    TypeAttribute type;
};

/**
 * A union type combines multiple types into a new super type.
 * Each of the enumerated types become a sub-type of the new
 * union type.
 */
class AstUnionType : public AstType {
public:
    AstUnionType(AstQualifiedName name, std::vector<AstQualifiedName> types, SrcLocation loc = {})
            : AstType(std::move(name), std::move(loc)), types(std::move(types)) {}

    /** Obtains a reference to the list element types */
    const std::vector<AstQualifiedName>& getTypes() const {
        return types;
    }

    /** Adds another element type */
    void add(AstQualifiedName type) {
        types.push_back(std::move(type));
    }

    /** Set variant type */
    void setVariantType(size_t idx, AstQualifiedName type) {
        types.at(idx) = std::move(type);
    }

    AstUnionType* clone() const override {
        return new AstUnionType(getQualifiedName(), types, getSrcLoc());
    }

protected:
    void print(std::ostream& os) const override {
        os << ".type " << getQualifiedName() << " = " << join(types, " | ");
    }

    bool equal(const AstNode& node) const override {
        const auto& other = static_cast<const AstUnionType&>(node);
        return getQualifiedName() == other.getQualifiedName() && types == other.types;
    }

private:
    /** The list of types aggregated by this union type */
    std::vector<AstQualifiedName> types;
};

/**
 * A record type aggregates a list of fields into a new type.
 * Each record type has a name making it unique. Two record
 * types are unrelated to all other types (they do not have
 * any super or sub types).
 */
class AstRecordType : public AstType {
public:
    AstRecordType(AstQualifiedName name, VecOwn<AstAttribute> fields, SrcLocation loc = {})
            : AstType(std::move(name), std::move(loc)), fields(std::move(fields)) {}

    /** add field to record type */
    void add(std::string name, AstQualifiedName type) {
        fields.push_back(mk<AstAttribute>(std::move(name), std::move(type)));
    }

    /** get fields of record */
    std::vector<AstAttribute*> getFields() const {
        return toPtrVector(fields);
    }

    /** set field type */
    void setFieldType(size_t idx, AstQualifiedName type) {
        fields.at(idx)->setTypeName(std::move(type));
    }

    AstRecordType* clone() const override {
        return new AstRecordType(getQualifiedName(), souffle::clone(fields), getSrcLoc());
    }

protected:
    void print(std::ostream& os) const override {
        os << ".type " << getQualifiedName() << "[" << join(fields, ", ") << "]";
    }

    bool equal(const AstNode& node) const override {
        const auto& other = dynamic_cast<const AstRecordType&>(node);
        return getQualifiedName() == other.getQualifiedName() && equal_targets(fields, other.fields);
    }

private:
    /** record fields */
    VecOwn<AstAttribute> fields;
};

}  // end of namespace souffle
