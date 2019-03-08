#include "AnalysisType.h"

namespace souffle {

UnionAnalysisType::UnionAnalysisType(std::set<const BaseAnalysisType> baseTypes) : baseTypes(baseTypes) {
    std::stringstream repr;
    repr << join(baseTypes, " | ");
    representation = repr.str();

    assert(!baseTypes.empty() && "empty union is not allowed");
    assert(baseTypes.size() > 1 && "union with one element is a base type");

    kind = (*baseTypes.begin()).getKind();
    assert(kind != Kind::RECORD && "record unions are not supported");
    for (const BaseAnalysisType base : baseTypes) {
        assert(base.getKind() == kind && "all union components must have the same kind");
    }
}

UnionAnalysisType::UnionAnalysisType(std::set<const BaseAnalysisType> baseTypes, AstTypeIdentifier& name)
        : UnionAnalysisType(baseTypes), representation(toString(name)) {}

void UnionAnalysisType::setName(AstTypeIdentifier& name) {
    representation = toString(name);
}

}  // namespace souffle
