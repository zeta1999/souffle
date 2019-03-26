/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file TypeLattice.h
 *
 * Defines the TypeLattice class, which is used to perform a type analysis.
 *
 ***********************************************************************/

#pragma once

#include "AnalysisType.h"
#include "TypeSystem.h"

namespace souffle {

class TypeLattice {
public:
    TypeLattice() = delete;

    TypeLattice(const TypeEnvironment* typeEnvironment) : typeEnvironment(typeEnvironment) {
        aliases["number"] = getStoredType(TopPrimitiveAnalysisType(Kind::NUMBER));
        aliases["symbol"] = getStoredType(TopPrimitiveAnalysisType(Kind::SYMBOL));
        for (const Type& type : typeEnvironment->getAllTypes()) {
            addType(type);
        }
    }

    /** Checks whether the constructed type lattice is valid */
    const bool isValid() const {
        return valid;
    }

    /** Finds the highest common subtype (intersection) of two types */
    const AnalysisType* meet(const AnalysisType* first, const AnalysisType* second) const;

    /** Finds the lowest common supertype (union) of two types */
    const AnalysisType* join(const AnalysisType* first, const AnalysisType* second) const;

    /** Checks if lhs is a subtype of rhs */
    bool isSubtype(const AnalysisType* lhs, const AnalysisType* rhs) const;
    bool isSubtype(const AnalysisType& lhs, const AnalysisType& rhs) const;

    /** Gets the type environment associated with the type lattice */
    const TypeEnvironment* getTypeEnvironment() const {
        return typeEnvironment;
    }

    /**
     * Gets the equivalent type stored in the lattice.
     * If the type does not yet exist in the lattice, it is created.
     */
    // TODO: make sure this is used everywhere it should be used
    template <typename T>
    T* getStoredType(const T& type) const {
        const AnalysisType& at = static_cast<const AnalysisType&>(type);
        for (const auto& other : storedTypes) {
            if (*other == at) {
                assert(dynamic_cast<T*>(other.get()) && "equivalent types should have equal types");
                return dynamic_cast<T*>(other.get());
            }
        }

        T* newType = type.clone();
        storedTypes.insert(std::unique_ptr<AnalysisType>(newType));
        return newType;
    }

    /**
     * Gets the equivalent analysis type stored in the lattice.
     * If the type does not yet exist in the lattice, it is created.
     */
    const InnerAnalysisType* getAnalysisType(const AstTypeIdentifier& type) const;
    const InnerAnalysisType* getAnalysisType(const Type& type) const;

private:
    mutable std::set<std::unique_ptr<AnalysisType>> storedTypes{};
    const TypeEnvironment* typeEnvironment;
    bool valid{true};
    std::map<AstTypeIdentifier, const InnerAnalysisType*> aliases{};

    /** Adds an analysis type to the lattice corresponding to the given type */
    const InnerAnalysisType* addType(const Type& type);
};

}  // end of namespace souffle
