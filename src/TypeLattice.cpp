#include "TypeLattice.h"
#include "AnalysisType.h"
#include <cassert>

namespace souffle {

const AnalysisType* TypeLattice::meet(const AnalysisType* lhs, const AnalysisType* rhs) {
    // A ^ B = A if A <: B
    if (isSubtype(lhs, rhs)) {
        return lhs;
    }
    if (isSubtype(rhs, lhs)) {
        return rhs;
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
    if (lhs == rhs) {
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

const AnalysisType* TypeLattice::getStoredType(const AnalysisType& type) {
    for (const auto& other : storedTypes) {
        if (*other == type) {
            return other.get();
        }
    }

    const auto& newType = std::unique_ptr<AnalysisType>(type.clone());
    storedTypes.insert(std::move(newType));
    return newType.get();
}

}  // namespace souffle
