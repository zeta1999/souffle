#pragma once

#include "RamRelation.h"
#include "IndexSetAnalysis.h"

#include <numeric>

namespace souffle {

class SynthesiserRelation {
private:
    /** Ram relation referred to by this */
    const RamRelation& relation;

    /** Indices used for this relation */
    const IndexSet& indices;

    /** Is this relation used with provenance */
    const bool isProvenance;

public:
    SynthesiserRelation(const RamRelation& rel, const IndexSet& inds, const bool isProvenance = false) : relation(rel), indices(inds), isProvenance(isProvenance) {}

    /** Get arity of relation */
    size_t getArity() const {
        return relation.getArity();
    }

    /** Get data structure of relation */
    std::string getDataStructure() const {
        if (relation.isBTree()) {
            return "btree";
        } else if (relation.isRbtset()) {
            return "rbtset";
        } else if (relation.isHashset()) {
            return "hashset";
        } else if (relation.isBrie()) {
            return "brie";
        } else if (relation.isEqRel()) {
            return "eqrel";
        } else {
            return "auto";
        }
    }

    /** Get list of indices used for relation */
    std::vector<std::vector<int>> getIndices() const {
        std::vector<std::vector<int>> inds = indices.getAllOrders();

        // Add a full index if it does not exist
        bool fullExists = false;
        // check for full ind
        for (auto& ind : inds) {
            if (ind.size() == getArity()) {
                fullExists = true;
            }
        }
        // generate full ind if it does not exist
        if (!fullExists) {
            std::vector<int> fullInd(getArity());
            std::iota(fullInd.begin(), fullInd.end(), 0);
        }

        // If this relation is used with provenance,
        // we must expand all search orders to be full indices,
        // since weak/strong comparators and updaters need this,
        // and also add provenance annotations to the indices
        if (isProvenance) {
            for (auto& ind : inds) {
                if (ind.size() < getArity() - 2) {
                    // use a set as a cache for fast lookup
                    std::set<int> curIndexElems(ind.begin(), ind.end());

                    // expand index to be full
                    for (int i = 0; i < getArity() - 2; i++) {
                        if (curIndexElems.find(i) == curIndexElems.end()) {
                            ind.push_back(i);
                        }
                    }
                }
                
                // add provenance annotations to the index
                ind.push_back(getArity() - 1);
                ind.push_back(getArity() - 2);
            }
        }
    }

    /** Print type name */
    void printTypeName(std::ostream& out) const {
        out << "t";
        out << "_" << getDataStructure();
        out << "_" << getArity();

        for (auto& ind : getIndices()) {
            out << "__" << join(ind, "_");
        }
    }
};

} // end of namespace souffle
