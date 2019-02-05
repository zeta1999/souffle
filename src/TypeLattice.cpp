#include "TypeLattice.h"
#include "AstType.h"
#include "TypeSystem.h"
#include "Util.h"
#include <sstream>

namespace souffle {

const PrimitiveAType& InnerAType::getPrimitive() {
    return lattice.getPrimitive(this->getKind());
}

const ConstantAType& InnerAType::getConstant() {
    return lattice.getConstant(this->getKind());
}

UnionAType::UnionAType(const TypeLattice& lattice, std::vector<BaseAType*> bases) : InnerAType(lattice), bases(bases) {
    std::stringstream repr;
    repr << join(bases, "|");
    representation = repr.str();
    assert(!bases.empty() && "Empty union is not allowed");
    assert(bases.size() > 1 && "Union with one element is just a base type");
    Kind kind = bases.front()->getKind();
    for (BaseAType* b : bases) {
        assert(b->getKind() == kind && "All components of union have the same type");
    }
}
UnionAType::UnionAType(const TypeLattice& lattice, std::vector<BaseAType*> bases, AstTypeIdentifier name) : InnerAType(lattice), bases(bases) {
    std::stringstream repr;
    repr << name;
    representation = repr.str();
    assert(!bases.empty() && "Empty union is not allowed");
    assert(bases.size() > 1 && "Union with one element is just a base type");
    Kind kind = bases.front()->getKind();
    for (BaseAType* b : bases) {
        assert(b->getKind() == kind && "All components of union have the same type");
    }
}

}  // namespace souffle
