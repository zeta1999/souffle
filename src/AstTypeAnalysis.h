#pragma once

#include "AnalysisType.h"
#include "AstAnalysis.h"
#include "AstVisitor.h"
#include "TypeConstraint.h"
#include "TypeLattice.h"
#include "TypeSystem.h"
#include <ostream>

namespace souffle {

class AstArgument;

/**
 * A type solver that computes the type for each argument in a given clause.
 **/
class TypeSolver {
public:
    // TODO: change this to take in a clause, then get cosntraints and resolve them all
    // TODO: fix constraint resolution etc.
    // TODO: lattice here because...?
    // TODO: get rid of things afterwrads
    TypeSolver(TypeLattice* lattice, AstClause* clause, TypeEnvironment* typeEnvironment, AstProgram* program)
            : lattice(lattice), clause(clause), typeEnvironment(typeEnvironment), program(program) {
        generateConstraints();
        resolveConstraints();
    }

    /** Get the type lattice associated with the type solution */
    TypeLattice* getLattice() const {
        return lattice;
    }

    /** Adds a constraint that needs to be satisfied by the type solution. */
    void addConstraint(std::unique_ptr<TypeConstraint> constraint) {
        constraints.insert(std::move(constraint));
    }

    /**
     * Gets the canonical representative of a given argument.
     * - For variables, this is a variable pointer that represents all variables
     * bound to the same name.
     * - For non-variables, this is the argument pointer itself.
     **/
    // TODO: add an instance variable to support this map
    const AstArgument* getRepresentative(const AstArgument* arg);

    // TODO: note that this is a pointer because the type is in the lattice
    /** Get the computed type for the given argument */
    const AnalysisType* getType(const AstArgument* arg) const {
        auto pos = typeMapping.find(arg);
        assert(pos != typeMapping.end() && "argument does not have a type");
        return pos->second;
    }

    /** Set the type of a given argument */
    void setType(const AstArgument* arg, const AnalysisType* type) {
        typeMapping[arg] = type;
    }

    /** Checks if a type has been computed for the given argument */
    bool hasType(const AstArgument* arg) const {
        return typeMapping.find(arg) != typeMapping.end();
    }

    /** Generates the set of type constraints associated with the clause */
    void generateConstraints();

    /** Resolves all stored constraints until they are simultaneously satisfied */
    void resolveConstraints() {
        // restore everything to the top type
        typeMapping.clear();
        visitDepthFirst(*clause, [&](const AstArgument& arg) {
            // TODO: careful of getRepresentative!!!
            typeMapping[&arg] = lattice->getStoredType(TopAnalysisType());
        });

        // TODO: fixed constraints can be done outside the loop
        // TODO: double check this is equivalent to the old one
        // apply each constraint until all are satisfied (fixed point reached)
        bool changed = true;
        while (changed) {
            changed = false;
            for (const auto& cons : constraints) {
                if (!cons->isSatisfied(this)) {
                    changed = true;
                    cons->resolve(this);
                }
            }
        }
    }

    std::set<TypeConstraint*> getConstraints() const {
        return toPtrSet(constraints);
    }

    void print(std::ostream& out) const {
        for (const auto& pair : typeMapping) {
            assert(pair.first != nullptr && "nullptr argument in type solution");
            assert(pair.second != nullptr && "nullptr analysis type in type solution");
            out << "type(" << *pair.first << ") = " << *pair.second << std::endl;
        }
    }

private:
    // TODO: reorder - maybe get rid of some if possible etc.
    TypeLattice* lattice;
    AstClause* clause;
    TypeEnvironment* typeEnvironment;
    AstProgram* program;
    std::set<std::unique_ptr<TypeConstraint>> constraints{};
    std::map<const AstArgument*, const AnalysisType*> typeMapping{};
};

/** Type analysis entrypoint */
class TypeAnalysis : public AstAnalysis {
public:
    TypeAnalysis() = default;

    static constexpr const char* name = "type-analysis";

    void run(const AstTranslationUnit& translationUnit) override;

    void print(std::ostream& out) const override;

    /** Get the computed type for the given argument */
    const AnalysisType* getType(const AstArgument* arg) const {
        return typeSolver->getType(arg);
    }

    /** Get the type lattice associated with the analysis */
    TypeLattice* getLattice() const {
        return typeSolver->getLattice();
    }

private:
    // TODO: why is this here
    std::unique_ptr<TypeLattice> typeLattice;
    std::unique_ptr<TypeSolver> typeSolver;
};

}  // end of namespace souffle
