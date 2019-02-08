#include "TypeLattice.h"
#include "AstType.h"
#include "TypeSystem.h"
#include "Util.h"
#include <deque>
#include <map>
#include <ostream>
#include <set>
#include <vector>

namespace souffle {

const PrimitiveAType& InnerAType::getPrimitive() const {
    return lattice->getPrimitive(this->getKind());
}

const ConstantAType& InnerAType::getConstant() const {
    return lattice->getConstant(this->getKind());
}

UnionAType::UnionAType(const TypeLattice* lattice, std::set<const BaseAType*> bases)
        : InnerAType(lattice), representation(toString(join(bases, " | "))), bases(bases) {
    assert(!bases.empty() && "Empty union is not allowed");
    assert(bases.size() > 1 && "Union with one element is just a base type");
    Kind kind = (*bases.begin())->getKind();
    for (const BaseAType* b : bases) {
        assert(b->getKind() == kind && "All components of union must have the same type");
    }
}

UnionAType::UnionAType(const TypeLattice* lattice, std::set<const BaseAType*> bases, AstTypeIdentifier name)
        : InnerAType(lattice), representation(toString(name)), bases(bases) {
    assert(!bases.empty() && "Empty union is not allowed");
    assert(bases.size() > 1 && "Union with one element is just a base type");
    Kind kind = (*bases.begin())->getKind();
    for (const BaseAType* b : bases) {
        assert(b->getKind() == kind && "All components of union must have the same type");
    }
}

void UnionAType::setName(AstTypeIdentifier name) {
    representation = toString(name);
}

const AnalysisType* TypeLattice::meet(const AnalysisType* first, const AnalysisType* second) {
    assert(first->lattice == this && "Type must be in this lattice");
    assert(second->lattice == this && "Type must be in this lattice");
    if (isSubtype(first, second)) {
        return first;
    }
    if (isSubtype(second, first)) {
        return second;
    }
    // Neither can be top or bottom, so they are inner types
    const auto* firstInner = dynamic_cast<const InnerAType*>(first);
    const auto* secondInner = dynamic_cast<const InnerAType*>(second);
    assert(firstInner != nullptr && "Unsupported type");
    assert(secondInner != nullptr && "Unsupported type");
    if (firstInner->getKind() != secondInner->getKind()) {
        // The types are in different sub-lattices
        return &bot;
    }
    Kind kind = firstInner->getKind();
    if (kind == Kind::RECORD) {
        // They aren't subtypes, so they must be different records
        return &getBotPrim(kind);
    }
    if (dynamic_cast<const BaseAType*>(first) != nullptr ||
            dynamic_cast<const BaseAType*>(second) != nullptr) {
        // One is a base type, but they are not subtypes, and hence must be disjoint
        return &getBotPrim(kind);
    }
    // The only option is for both to be union types
    const auto* firstUnion = dynamic_cast<const UnionAType*>(first);
    const auto* secondUnion = dynamic_cast<const UnionAType*>(second);
    assert(firstUnion != nullptr && "Unsupported type");
    assert(secondUnion != nullptr && "Unsupported type");
    std::set<const BaseAType*> intersection;
    for (const BaseAType* base : firstUnion->getBases()) {
        if (secondUnion->getBases().count(base) > 0) {
            intersection.insert(base);
        }
    }
    if (intersection.size() == 0) {
        // Types are disjoint
        return &getBotPrim(kind);
    }
    if (intersection.size() == 1) {
        return *intersection.begin();
    }
    for (UnionAType& other : unions) {
        if (other.getBases() == intersection) {
            return &other;
        }
    }
    unions.push_back(UnionAType(this, intersection));
    return &unions.back();
}

const AnalysisType* TypeLattice::join(const AnalysisType* first, const AnalysisType* second) {
    assert(first->lattice == this && "Type must be in this lattice");
    assert(second->lattice == this && "Type must be in this lattice");
    if (isSubtype(first, second)) {
        return second;
    }
    if (isSubtype(second, first)) {
        return first;
    }
    // Neither can be top or bottom, so they are inner types
    const auto* firstInner = dynamic_cast<const InnerAType*>(first);
    const auto* secondInner = dynamic_cast<const InnerAType*>(second);
    assert(firstInner != nullptr && "Unsupported type");
    assert(secondInner != nullptr && "Unsupported type");
    if (firstInner->getKind() != secondInner->getKind()) {
        // The types are in different sub-lattices
        return &top;
    }
    Kind kind = firstInner->getKind();
    if (kind == Kind::RECORD) {
        // They aren't subtypes, so they must be different records
        return &getPrimitive(kind);
    }
    // The output must be some union type containing both inputs
    std::set<const BaseAType*> contents;
    if (dynamic_cast<const BaseAType*>(first) != nullptr) {
        contents.insert(dynamic_cast<const BaseAType*>(first));
    } else if (dynamic_cast<const UnionAType*>(first) != nullptr) {
        contents = dynamic_cast<const UnionAType*>(first)->getBases();
    } else {
        assert(false && "Unsupported type");
    }
    if (dynamic_cast<const BaseAType*>(second) != nullptr) {
        contents.insert(dynamic_cast<const BaseAType*>(second));
    } else if (dynamic_cast<const UnionAType*>(second) != nullptr) {
        for (const BaseAType* base : dynamic_cast<const UnionAType*>(second)->getBases()) {
            contents.insert(base);
        }
    } else {
        assert(false && "Unsupported type");
    }
    assert(contents.size() > 1 && "Union of non-equal non-empty sets somehow has less than 2 elements");
    for (UnionAType& other : unions) {
        if (other.getBases() == contents) {
            return &other;
        }
    }
    unions.push_back(UnionAType(this, contents));
    return &unions.back();
}

bool TypeLattice::isSubtype(const AnalysisType* first, const AnalysisType* second) const {
    assert(first->lattice == this && "Type must be in this lattice");
    assert(second->lattice == this && "Type must be in this lattice");
    if (first == second) {
        // Most subtype checks fall into this case
        return true;
    }
    if (dynamic_cast<const TopAType*>(second) != nullptr) {
        // Everything is a subtype of top
        return true;
    }
    if (dynamic_cast<const TopAType*>(first) != nullptr) {
        // Everything else is not a supertype of top
        return false;
    }
    if (dynamic_cast<const BotAType*>(first) != nullptr) {
        // Everything is a supertype of bottom
        return true;
    }
    if (dynamic_cast<const BotAType*>(second) != nullptr) {
        // Everything else is not a subtype of bottom
        return false;
    }
    // Neither are top/bottom, so they are both inner types
    const auto* firstInner = dynamic_cast<const InnerAType*>(first);
    const auto* secondInner = dynamic_cast<const InnerAType*>(second);
    assert(firstInner != nullptr && "Unsupported type");
    assert(secondInner != nullptr && "Unsupported type");
    if (firstInner->getKind() != secondInner->getKind()) {
        // The types are unrelated
        return false;
    }
    // Now we know both types are in the same sub-lattice
    if (dynamic_cast<const PrimitiveAType*>(second) != nullptr) {
        // Everything is a subtype of top
        return true;
    }
    if (dynamic_cast<const PrimitiveAType*>(first) != nullptr) {
        // Everything else is not a supertype of top
        return false;
    }
    if (dynamic_cast<const ConstantAType*>(first) != nullptr) {
        // Everything is a supertype of bottom
        return true;
    }
    if (dynamic_cast<const ConstantAType*>(second) != nullptr) {
        // Everything else is not a subtype of bottom
        return false;
    }
    if (dynamic_cast<const BotPrimAType*>(first) != nullptr) {
        // Everything else is a supertype of the second bottom
        return true;
    }
    if (dynamic_cast<const BotPrimAType*>(second) != nullptr) {
        // Everything else is not a subtype of the second bottom
        return false;
    }
    if (firstInner->getKind() == Kind::RECORD) {
        // Record subtyping is not supported for types other than top/bottom/constant
        return false;
    }
    if (dynamic_cast<const BaseAType*>(second) != nullptr) {
        // The other type cannot be this base type or lower
        return false;
    }
    // The only remaining possibility for the second type is to be a union type
    const auto* secondUnion = dynamic_cast<const UnionAType*>(second);
    assert(secondUnion != nullptr && "Unsupported type");
    const std::set<const BaseAType*>& secondBases = secondUnion->getBases();
    if (dynamic_cast<const BaseAType*>(first) != nullptr) {
        // Check if the base type is in the union
        return secondBases.count(dynamic_cast<const BaseAType*>(first)) > 0;
    }
    // The only remaining possibility for the first type is to be a union type
    const auto* firstUnion = dynamic_cast<const UnionAType*>(first);
    assert(firstUnion != nullptr && "Unsupported type");
    // Check if the sets of base types are subsets
    for (const BaseAType* base : firstUnion->getBases()) {
        if (secondBases.count(base) == 0) {
            return false;
        }
    }
    return true;
}

const InnerAType* TypeLattice::addType(const Type* type) {
    assert(env.isType(*type) && "Type must be in environment");
    if (aliases.count(type->getName()) == 0) {
        if (dynamic_cast<const PrimitiveType*>(type) != nullptr) {
            auto* baseType = dynamic_cast<const PrimitiveType*>(type);
            Kind kind;
            if (baseType->getBaseType().getName() == "symbol") {
                kind = Kind::SYMBOL;
            } else if (baseType->getBaseType().getName() == "number") {
                kind = Kind::NUMBER;
            } else {
                assert(false && "Unsupported primitive type");
            }
            bases.push_back(BaseAType(this, kind, AstTypeIdentifier(type->getName())));
            aliases[type->getName()] = &bases.back();
        } else if (dynamic_cast<const RecordType*>(type) != nullptr) {
            // Add the record now to avoid infinite recursion
            records.push_back(RecordAType(this, type->getName()));
            RecordAType& thisRecord = records.back();
            aliases[type->getName()] = &thisRecord;
            // Now add fields recursively
            auto* recordType = dynamic_cast<const RecordType*>(type);
            for (RecordType::Field field : recordType->getFields()) {
                thisRecord.addField(addType(&field.type));
            }
        } else if (dynamic_cast<const UnionType*>(type) != nullptr) {
            auto* unionType = dynamic_cast<const UnionType*>(type);
            std::set<const BaseAType*> memberTypes;
            bool isPrimitive = false;
            for (const Type* memberType : unionType->getElementTypes()) {
                const InnerAType* memberAType = addType(memberType);
                if (dynamic_cast<const PrimitiveAType*>(memberAType) != nullptr) {
                    isPrimitive = true;
                    aliases[type->getName()] = &primitives.find(memberAType->getKind())->second;
                    break;
                } else if (dynamic_cast<const UnionAType*>(memberAType) != nullptr) {
                    for (const BaseAType* memberBaseType :
                            dynamic_cast<const UnionAType*>(memberAType)->getBases()) {
                        memberTypes.insert(memberBaseType);
                    }
                } else if (dynamic_cast<const BaseAType*>(memberAType) != nullptr) {
                    memberTypes.insert(dynamic_cast<const BaseAType*>(memberAType));
                } else {
                    assert(false && "Unsupported member type");
                }
            }
            if (!isPrimitive) {
                if (memberTypes.size() == 1) {
                    aliases[type->getName()] = *memberTypes.begin();
                } else if (memberTypes.size() > 1) {
                    bool inLattice = false;
                    for (UnionAType other : unions) {
                        if (memberTypes == other.getBases()) {
                            inLattice = true;
                            aliases[type->getName()] = &other;
                        }
                    }
                    if (!inLattice) {
                        unions.push_back(UnionAType(this, memberTypes, type->getName()));
                        aliases[type->getName()] = &unions.back();
                    }
                } else {
                    assert(false && "Invalid union");
                }
            }
        } else {
            assert(false && "Unsupported type");
        }
    }
    return aliases[type->getName()];
}

TypeLattice::TypeLattice(const TypeEnvironment& env) : env(env), top(this), bot(this) {
    for (Kind kind : {Kind::NUMBER, Kind::SYMBOL, Kind::RECORD}) {
        primitives.insert(std::pair<Kind, PrimitiveAType>(kind, PrimitiveAType(this, kind)));
        constants.insert(std::pair<Kind, ConstantAType>(kind, ConstantAType(this, kind)));
        botprims.insert(std::pair<Kind, BotPrimAType>(kind, BotPrimAType(this, kind)));
    }
    aliases["number"] = &primitives.find(Kind::NUMBER)->second;
    aliases["symbol"] = &primitives.find(Kind::SYMBOL)->second;
    for (const Type& type : env.getAllTypes()) {
        addType(&type);
    }
}

const InnerAType& TypeLattice::getType(const Type& type) const {
    assert(env.isType(type) && "Type must be in environment");
    return getType(type.getName());
}

}  // namespace souffle
