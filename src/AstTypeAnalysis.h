#pragma once

#include "AnalysisType.h"
#include "AstAnalysis.h"
#include "TypeLattice.h"
#include <ostream>

namespace souffle {

class AstArgument;

class TypeSolution {
public:
    TypeSolution() = default;

    /** Get the computed type for a given argument */
    const AnalysisType* getType(const AstArgument* arg) const {
        auto pos = typeMapping.find(arg);
        assert(pos != typeMapping.end() && "argument does not have a type");
        return pos->second;
    }

    void print(std::ostream& out) const;

private:
    std::map<const AstArgument*, const AnalysisType*> typeMapping;
};

class TypeAnalysis : public AstAnalysis {
public:
    TypeAnalysis() : lattice(std::make_unique<TypeLattice>()), typeSolution(std::make_unique<TypeSolution>()) {}

    static constexpr const char* name = "type-analysis";

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& out) const override;

    /** Get the computed type for the given argument */
    const AnalysisType* getType(const AstArgument* arg) const {
        return typeSolution->getType(arg);
    }

    /** Get the type lattice associated with the analysis */
    TypeLattice* getLattice() const {
        return lattice.get();
    }

private:
    std::unique_ptr<TypeLattice> lattice;
    std::unique_ptr<TypeSolution> typeSolution;
};

} // end of namespace souffle
