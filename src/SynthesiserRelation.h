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

    /** The data structure used for the relation */
    std::string dataStructure;

    /** The final list of indices used */
    std::vector<std::vector<int>> computedIndices;

    /** Is this relation used with provenance */
    const bool isProvenance;

public:
    SynthesiserRelation(const RamRelation& rel, const IndexSet& indices, const bool isProvenance = false) : relation(rel), indices(indices), isProvenance(isProvenance) {
        // Set data structure
        if (relation.isBTree()) {
            dataStructure = "btree";
        } else if (relation.isRbtset()) {
            dataStructure = "rbtset";
        } else if (relation.isHashset()) {
            dataStructure = "hashset";
        } else if (relation.isBrie()) {
            dataStructure = "brie";
        } else if (relation.isEqRel()) {
            dataStructure = "eqrel";
        } else {
            dataStructure = "auto";
        }

        // Generate and set indices
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
        if (!fullExists && !isProvenance) {
            std::vector<int> fullInd(getArity());
            std::iota(fullInd.begin(), fullInd.end(), 0);
            inds.push_back(fullInd);
        }

        // If this relation is used with provenance,
        // we must expand all search orders to be full indices,
        // since weak/strong comparators and updaters need this,
        // and also add provenance annotations to the indices
        if (isProvenance) {
            // If there is no index, add one
            if (inds.empty()) {
                std::vector<int> fullInd(getArity() - 2);
                std::iota(fullInd.begin(), fullInd.end(), 0);
                inds.push_back(fullInd);
            }
            for (auto& ind : inds) {
                if (ind.size() != getArity() - 2) {
                    // use a set as a cache for fast lookup
                    std::set<int> curIndexElems(ind.begin(), ind.end());

                    // expand index to be full
                    for (int i = 0; i < getArity() - 2; i++) {
                        if (curIndexElems.find(i) == curIndexElems.end()) {
                            ind.push_back(i);
                        }
                    }
                } /* else {
                    while (ind.size() > getArity() - 2) {
                        ind.pop_back();
                    }
                }*/
                
                // add provenance annotations to the index
                ind.push_back(getArity() - 1);
                ind.push_back(getArity() - 2);

                assert(ind.size() == getArity());
            }
        }

        computedIndices = inds;
    }

    /** Get arity of relation */
    size_t getArity() const {
        return relation.getArity();
    }

    /** Get data structure of relation */
    std::string getDataStructure() const {
        return dataStructure;
    }

    /** Get list of indices used for relation,
     * guaranteed that original indices in IndexSet
     * come before any generated indices */
    std::vector<std::vector<int>> getIndices() const {
        return computedIndices;
    }

    /** Get stored IndexSet */
    const IndexSet& getIndexSet() const {
        return indices;
    }

    /** Print type name */
    std::string getTypeName() const {
        if (getArity() == 0) {
            return "t_nullaries";
        }

        std::stringstream out;

        out << "t";
        out << "_" << getDataStructure();
        out << "_" << getArity();

        for (auto& ind : getIndices()) {
            out << "__" << join(ind, "_");
        }

        for (auto& search : getIndexSet().getSearches()) {
            out << "__" << search;
        }

        return out.str();
    }
};

} // end of namespace souffle
