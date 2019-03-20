/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TypeLattice.cpp
 *
 * Implements the TypeLattice class, which is used to perform a type analysis.
 *
 ***********************************************************************/

#include "TypeLattice.h"
#include "AnalysisType.h"
#include <cassert>

namespace souffle {

const AnalysisType* TypeLattice::meet(const AnalysisType* lhs, const AnalysisType* rhs) {
    // A ^ B = A if A <: B
    if (isSubtype(lhs, rhs)) {
        return getStoredType(*lhs);
    }
    if (isSubtype(rhs, lhs)) {
        return getStoredType(*rhs);
    }

    // neither are top or bottom, so they are inner types
    const auto* lhsInner = dynamic_cast<const InnerAnalysisType*>(lhs);
    const auto* rhsInner = dynamic_cast<const InnerAnalysisType*>(rhs);
    assert(lhsInner != nullptr && "unsupported analysis type");
    assert(rhsInner != nullptr && "unsupported analysis type");

    // each kind has a separate sublattice
    if (lhsInner->getKind() != rhsInner->getKind()) {
        // lhs and rhs are in different sublattices
        return getStoredType(BottomAnalysisType());
    }

    // so both have the same kind
    Kind kind = lhsInner->getKind();

    // records do not allow subtyping so they must be different records
    if (kind == Kind::RECORD) {
        return getStoredType(BottomPrimitiveAnalysisType(kind));
    }

    if (dynamic_cast<const BaseAnalysisType*>(lhs) != nullptr ||
            dynamic_cast<const BaseAnalysisType*>(rhs) != nullptr) {
        // one is a base type, but they are not subtypes
        // therefore, disjoint but same kind
        return getStoredType(BottomPrimitiveAnalysisType(kind));
    }

    // neither is a subtype of the other, and they both have the same kind
    // therefore, they are both union types
    const auto* lhsUnion = dynamic_cast<const UnionAnalysisType*>(lhs);
    const auto* rhsUnion = dynamic_cast<const UnionAnalysisType*>(rhs);
    assert(lhsUnion != nullptr && "unsupported analysis type");
    assert(rhsUnion != nullptr && "unsupported analysis type");

    // A ^ B = the intersection of the base types
    std::set<BaseAnalysisType> intersection;
    const std::set<BaseAnalysisType>& rhsBaseTypes = rhsUnion->getBaseTypes();
    for (const BaseAnalysisType& base : lhsUnion->getBaseTypes()) {
        if (rhsBaseTypes.find(base) != rhsBaseTypes.end()) {
            intersection.insert(base);
        }
    }

    if (intersection.size() == 0) {
        // types are disjoint
        return getStoredType(BottomPrimitiveAnalysisType(kind));
    }

    if (intersection.size() == 1) {
        // intersection is a single base type
        return getStoredType(*intersection.begin());
    }

    // result is a union type
    return getStoredType(UnionAnalysisType(intersection));
}

const AnalysisType* TypeLattice::join(const AnalysisType* lhs, const AnalysisType* rhs) {
    // A v B = B if A <: B
    if (isSubtype(lhs, rhs)) {
        return rhs;
    }
    if (isSubtype(rhs, lhs)) {
        return lhs;
    }

    // neither are top or bottom, so they are inner types
    const auto* lhsInner = dynamic_cast<const InnerAnalysisType*>(lhs);
    const auto* rhsInner = dynamic_cast<const InnerAnalysisType*>(rhs);
    assert(lhsInner != nullptr && "unsupported analysis type");
    assert(rhsInner != nullptr && "unsupported analysis type");

    // each kind has a separate lattice
    if (lhsInner->getKind() != rhsInner->getKind()) {
        // lhs and rhs are in different sublattices
        return getStoredType(TopAnalysisType());
    }

    // so both have the same kind
    Kind kind = lhsInner->getKind();

    // records do not allow subtyping so they must be different records
    if (kind == Kind::RECORD) {
        return getStoredType(TopPrimitiveAnalysisType(kind));
    }

    // therefore must be distinct union or base types
    std::set<BaseAnalysisType> contents;

    // take care of lhs
    if (auto* bat = dynamic_cast<const BaseAnalysisType*>(lhs)) {
        contents.insert(*bat);
    } else if (auto* uat = dynamic_cast<const UnionAnalysisType*>(lhs)) {
        for (const BaseAnalysisType base : uat->getBaseTypes()) {
            // TODO: make sure this is a copy (and throughout)
            contents.insert(base);
        }
    } else {
        assert(false && "unsupported analysis type");
    }

    // take care of rhs
    if (auto* bat = dynamic_cast<const BaseAnalysisType*>(rhs)) {
        contents.insert(*bat);
    } else if (auto* uat = dynamic_cast<const UnionAnalysisType*>(rhs)) {
        for (const BaseAnalysisType base : uat->getBaseTypes()) {
            contents.insert(base);
        }
    } else {
        assert(false && "unsupported analysis type");
    }

    // check that the universe isn't broken
    assert(contents.size() > 1 && "union of distinct non-empty sets somehow has less than two elements");

    return getStoredType(UnionAnalysisType(contents));
}

bool TypeLattice::isSubtype(const AnalysisType* lhs, const AnalysisType* rhs) const {
    // t <: t
    if (*lhs == *rhs) {
        return true;
    }

    // all types are a subtype of top
    if (dynamic_cast<const TopAnalysisType*>(rhs) != nullptr) {
        return true;
    }

    // top is not a subtype of any t != top
    if (dynamic_cast<const TopAnalysisType*>(lhs) != nullptr) {
        return false;
    }

    // bottom is a subtype of all types
    if (dynamic_cast<const BottomAnalysisType*>(lhs) != nullptr) {
        return true;
    }

    // bottom is not a subtype of any t != bottom
    if (dynamic_cast<const BottomAnalysisType*>(rhs) != nullptr) {
        return false;
    }

    // not top or bottom, so both types are different inner types
    const auto* lhsInner = dynamic_cast<const InnerAnalysisType*>(lhs);
    const auto* rhsInner = dynamic_cast<const InnerAnalysisType*>(rhs);
    assert(lhsInner != nullptr && "unsupported analysis type");
    assert(rhsInner != nullptr && "unsupported analysis type");

    // must have the same kind
    if (lhsInner->getKind() != rhsInner->getKind()) {
        return false;
    }

    // so lhs and rhs are different inner types in the same sublattice!

    // primitive types are the top of every sublattice
    if (dynamic_cast<const TopPrimitiveAnalysisType*>(rhs) != nullptr) {
        return true;
    }
    if (dynamic_cast<const TopPrimitiveAnalysisType*>(lhs) != nullptr) {
        return false;
    }

    // constant types are the bottom of every sublattice
    if (dynamic_cast<const ConstantAnalysisType*>(lhs) != nullptr) {
        return true;
    }
    if (dynamic_cast<const ConstantAnalysisType*>(rhs) != nullptr) {
        return false;
    }

    // every non-constant is a supertype of the bottom primitive type
    if (dynamic_cast<const BottomPrimitiveAnalysisType*>(lhs) != nullptr) {
        return true;
    }
    if (dynamic_cast<const BottomPrimitiveAnalysisType*>(rhs) != nullptr) {
        return false;
    }

    // therefore must be distinct union or base types!
    if (lhsInner->getKind() == Kind::RECORD) {
        // records cannot have unions, so must be distinct base record types
        return false;
    }

    // the only remaining subtype of a base type is the base type itself
    // but the two types are distinct
    if (dynamic_cast<const BaseAnalysisType*>(rhs) != nullptr) {
        return false;
    }

    // rhs must now be a union type
    const auto* rhsUnion = dynamic_cast<const UnionAnalysisType*>(rhs);
    assert(rhsUnion != nullptr && "unsupported analysis type");

    // unions are a collection of base types
    const std::set<BaseAnalysisType>& rhsBaseTypes = rhsUnion->getBaseTypes();

    if (const auto* lhsBaseType = dynamic_cast<const BaseAnalysisType*>(lhs)) {
        // check if the base type is in the union
        return rhsBaseTypes.find(*lhsBaseType) != rhsBaseTypes.end();
    }

    // lhs must be a union as well
    const auto* lhsUnion = dynamic_cast<const UnionAnalysisType*>(lhs);
    assert(lhsUnion != nullptr && "unsupported analysis type");

    // lhs base types must all be in the rhs
    for (const BaseAnalysisType& base : lhsUnion->getBaseTypes()) {
        if (rhsBaseTypes.find(base) == rhsBaseTypes.end()) {
            return false;
        }
    }

    // therefore, lhs = rhs
    return true;
}

// TODO: removable?
bool TypeLattice::isSubtype(const AnalysisType& lhs, const AnalysisType& rhs) const {
    return isSubtype(&lhs, &rhs);
}

const InnerAnalysisType* TypeLattice::getAnalysisType(const AstTypeIdentifier& type) const {
    auto pos = aliases.find(type);
    assert(pos != aliases.end() && "type does not exist in the lattice");
    return pos->second;
}

const InnerAnalysisType* TypeLattice::getAnalysisType(const Type& type) const {
    assert(typeEnvironment->isType(type) && "type does not exist in the type environment");
    return getAnalysisType(type.getName());
}

// TODO: maybe get rid of a return value m8
const InnerAnalysisType* TypeLattice::addType(const Type& type) {
    assert(typeEnvironment->isType(type) && "type does not exist in the type environment");

    const auto& typeName = type.getName();

    if (aliases.find(typeName) != aliases.end()) {
        // type has already been added
        return aliases[type.getName()];
    }

    if (auto* baseType = dynamic_cast<const PrimitiveType*>(&type)) {
        // -- base types --

        // get the kind
        Kind kind;
        if (baseType->getBaseType().getName() == "symbol") {
            kind = Kind::SYMBOL;
        } else if (baseType->getBaseType().getName() == "number") {
            kind = Kind::NUMBER;
        } else {
            assert(false && "unsupported primitive type");
        }

        // get the corresponding base analysis type
        aliases[typeName] = getStoredType(BaseAnalysisType(kind, typeName));
        return aliases[typeName];
    } else if (auto* recordType = dynamic_cast<const RecordType*>(&type)) {
        // -- record types --

        // add the record now to avoid potential infinite recursion
        auto* recordAnalysisType = getStoredType(RecordAnalysisType(typeName));
        aliases[typeName] = recordAnalysisType;

        // first time we see this record, so can add fields recursively
        assert(recordAnalysisType->getFields().size() == 0 && "record analysis type already has fields");
        for (const auto& field : recordType->getFields()) {
            const auto* fieldAnalysisType = addType(field.type);
            recordAnalysisType->addField(std::unique_ptr<InnerAnalysisType>(fieldAnalysisType->clone()));
        }

        return aliases[typeName];
    } else if (auto* unionType = dynamic_cast<const UnionType*>(&type)) {
        // -- union types --

        const auto& elementTypes = unionType->getElementTypes();
        if (elementTypes.empty()) {
            // invalid: empty union types
            this->valid = false;
            return nullptr;
        }
        assert(!elementTypes.empty() && "union type cannot be empty");

        // TODO: what if recursive union types?
        // TODO: reducing union types?
        // create the set of member types
        bool isPrimitive = false;
        Kind kind;
        std::set<BaseAnalysisType> memberTypes;
        for (const Type* memberType : elementTypes) {
            const InnerAnalysisType* memberAnalysisType = addType(*memberType);
            if (dynamic_cast<const TopPrimitiveAnalysisType*>(memberAnalysisType)) {
                if (isPrimitive && kind != memberAnalysisType->getKind()) {
                    // invalid: comprised of two differently kinded primitives
                    this->valid = false;
                    return nullptr;
                }

                // contains a primitive, so the entire union is that primitive
                if (!isPrimitive) {
                    isPrimitive = true;
                    kind = memberAnalysisType->getKind();
                    aliases[typeName] = getStoredType(TopPrimitiveAnalysisType(kind));
                }
            } else if (auto* uat = dynamic_cast<const UnionAnalysisType*>(memberAnalysisType)) {
                for (const BaseAnalysisType memberBaseType : uat->getBaseTypes()) {
                    memberTypes.insert(memberBaseType);
                }
            } else if (auto* bat = dynamic_cast<const BaseAnalysisType*>(memberAnalysisType)) {
                memberTypes.insert(*bat);
            } else {
                assert(memberAnalysisType == nullptr && "unsupported member type");
                assert(!this->isValid() && "lattice should be invalid");
                return nullptr;
            }
        }

        // check what the kind is
        if (!isPrimitive) {
            kind = (*memberTypes.begin()).getKind();
        }

        // make sure the rest are the same kind
        for (const BaseAnalysisType& base : memberTypes) {
            if (base.getKind() != kind) {
                this->valid = false;
                return nullptr;
            }
        }

        // handle primitive case
        if (isPrimitive) {
            aliases[typeName] = getStoredType(TopPrimitiveAnalysisType(kind));
            return aliases[typeName];
        }

        // handle trivial case
        if (memberTypes.size() == 1) {
            aliases[typeName] = getStoredType(*(memberTypes.begin()));
            return aliases[typeName];
        }

        // otherwise, just a regular union type
        assert(memberTypes.size() > 1 && "unexpected member type size");
        aliases[typeName] = getStoredType(UnionAnalysisType(memberTypes, typeName));
        return aliases[typeName];
    } else {
        assert(false && "unsupported type");
    }
}

}  // end of namespace souffle
