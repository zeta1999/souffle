#pragma once

#include "Global.h"
#include "IndexSetAnalysis.h"
#include "RamRelation.h"

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
    SynthesiserRelation(const RamRelation& rel, const IndexSet& indices, const bool isProvenance = false)
            : relation(rel), indices(indices), isProvenance(isProvenance) {
        // Set data structure

        // Generate and set indices
        std::vector<std::vector<int>> inds = indices.getAllOrders();

        // generate a full index if it doesn't exist
        if (inds.empty()) {
            std::vector<int> fullInd(getArity());
            std::iota(fullInd.begin(), fullInd.end(), 0);
            inds.push_back(fullInd);
        }

        // Non-btree data structures should have all indices expanded
        if (dataStructure == "btree") {
            // Add a full index if it does not exist
            bool fullExists = false;
            // check for full ind
            for (auto& ind : inds) {
                if (ind.size() == getArity()) {
                    fullExists = true;
                }
            }
            // expand the first ind to be full, it is guaranteed that at least one index exists
            if (!fullExists && !isProvenance) {
                std::set<int> curIndexElems(inds[0].begin(), inds[0].end());

                // expand index to be full
                for (int i = 0; i < getArity(); i++) {
                    if (curIndexElems.find(i) == curIndexElems.end()) {
                        inds[0].push_back(i);
                    }
                }
            }
        } else {
            // expand all indexes to be full
            for (auto& ind : inds) {
                if (ind.size() != getArity()) {
                    // use a set as a cache for fast lookup
                    std::set<int> curIndexElems(ind.begin(), ind.end());

                    // expand index to be full
                    for (int i = 0; i < getArity(); i++) {
                        if (curIndexElems.find(i) == curIndexElems.end()) {
                            ind.push_back(i);
                        }
                    }
                }

                assert(ind.size() == getArity());
            }
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
                } else {
                    while (ind.size() > getArity() - 2) {
                        ind.pop_back();
                    }
                }

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

    /** Get stored RamRelation */
    const RamRelation& getRamRelation() const {
        return relation;
    }

    /** Print type name */
    std::string getTypeName() const {
        if (getArity() == 0) {
            return "t_nullaries";
        }

        if (getDataStructure() == "eqrel") {
            return "t_eqrel";
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

    static std::unique_ptr<SynthesiserRelation> getSynthesiserRelation(const RamRelation& ramRel, const IndexSet& indexSet) {
        SynthesiserRelation* rel;
        if (arity == 0) {
            rel = new SynthesiserNullaryRelation(ramRel, indexSet);
        } else if (ramRel.isBTree() || isProvenance) {
            if (ramRel.getArity() > 6) {
                rel = new SynthesiserIndirectRelation(ramRel, indexSet);
            } else {
                rel = new SynthesiserDirectRelation(ramRel, indexSet);
            }
        } else if (ramRel.isBrie()) {
            rel = new SynthesiserBrieRelation(ramRel, indexSet);
        } else if (ramRel.isEqRel()) {
            rel = new SynthesiserEqrelRelation(ramRel, indexSet);
        } else {
            if (Global::config().has("data-structure")) {
                if (Global::config().get("data-structure") == "btree") {
                    if (ramRel.getArity() > 6) {
                        rel = new SynthesiserIndirectRelation(ramRel, indexSet);
                    } else {
                        rel = new SynthesiserDirectRelation(ramRel, indexSet);
                    }
                } else if (Global::config().get("data-structure") == "brie") {
                    rel = new SynthesiserBrieRelation(ramRel, indexSet);
                } else if (Global::config().get("data-structure") == "eqrel") {
            rel = new SynthesiserEqrelRelation(ramRel, indexSet);
                }
            } else {
                if (getArity() > 6) {
                    rel = new SynthesiserIndirectRelation(ramRel, indexSet);
                } else if (ramRel.getArity() > 2) {
                    rel = new SynthesiserDirectRelation(ramRel, indexSet);
                } else {
                    rel = new SynthesiserBrieRelation(ramRel, indexSet);
                }
            }
        }

        // generate index set
        rel->generateIndexSet();

        // set data structure
        rel->generateDataStructure();

        return std::unique_ptr<SynthesiserRelation>(rel);
    }
};

class SynthesiserNullaryRelation : public SynthesiserRelation {
};

class SynthesiserDirectRelation : public SynthesiserRelation {
};

class SynthesiserIndirectRelation : public SynthesiserRelation {
};

class SynthesiserBrieRelation : public SynthesiserRelation {
};

class SynthesiserEqrelRelation : public SynthesiserRelation {
};

}  // end of namespace souffle
