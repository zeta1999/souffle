#include "TypeLattice.h"
#include "AstType.h"
#include "TypeSystem.h"
#include "Util.h"
#include <map>
#include <set>
#include <sstream>
#include <vector>

namespace souffle {

const PrimitiveAType& InnerAType::getPrimitive() const {
    return lattice->getPrimitive(this->getKind());
}

const ConstantAType& InnerAType::getConstant() const {
    return lattice->getConstant(this->getKind());
}

UnionAType::UnionAType(const TypeLattice* lattice, std::set<const BaseAType*> bases)
        : InnerAType(lattice), bases(bases) {
    std::stringstream repr;
    repr << join(bases, "|");
    representation = repr.str();
    assert(!bases.empty() && "Empty union is not allowed");
    assert(bases.size() > 1 && "Union with one element is just a base type");
    Kind kind = (*bases.begin())->getKind();
    for (const BaseAType* b : bases) {
        assert(b->getKind() == kind && "All components of union must have the same type");
    }
}

UnionAType::UnionAType(const TypeLattice* lattice, std::set<const BaseAType*> bases, AstTypeIdentifier name)
        : InnerAType(lattice), bases(bases) {
    std::stringstream repr;
    repr << name;
    representation = repr.str();
    assert(!bases.empty() && "Empty union is not allowed");
    assert(bases.size() > 1 && "Union with one element is just a base type");
    Kind kind = (*bases.begin())->getKind();
    for (const BaseAType* b : bases) {
        assert(b->getKind() == kind && "All components of union must have the same type");
    }
}

void UnionAType::setName(AstTypeIdentifier name) {
    std::stringstream repr;
    repr << name;
    representation = repr.str();
}

const InnerAType* TypeLattice::addType(const Type* type) {
    assert(env.isType(*type) && "Type must be in environment");
    if (aliases.find(type->getName()) == aliases.end()) {
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
            // Simplified implementation that does not use meet/join
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

            // Better implementation using meet/join
            // TODO: Add faster operation using meet/join
        } else {
            assert(false && "Unsupported type");
        }
    }
    return aliases[type->getName()];
}

TypeLattice::TypeLattice(const TypeEnvironment& env)
        : env(env), top(this), primitives(), constants(), botprims(), bot(this), bases(), records(), unions(),
          aliases() {
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
    // TODO
}

}  // namespace souffle
