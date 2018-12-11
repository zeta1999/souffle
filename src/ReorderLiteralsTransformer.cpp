/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file MinimiseProgramTransformer.cpp
 *
 * Define classes and functionality related to program minimisation.
 *
 ***********************************************************************/

#include "AstProfileUse.h"
#include "AstTransforms.h"
#include "AstVisitor.h"
#include "Global.h"

namespace souffle {

/**
 * Counts the number of bound arguments in a given atom.
 */
unsigned int numBoundArguments(const AstAtom* atom, const std::set<std::string>& boundVariables) {
    int count = 0;

    for (const AstArgument* arg : atom->getArguments()) {
        // Argument is bound iff all contained variables are bound
        bool isBound = true;
        visitDepthFirst(*arg, [&](const AstVariable& var) {
            if (boundVariables.find(var.getName()) == boundVariables.end()) {
                isBound = false;
            }
        });
        if (isBound) {
            count++;
        }
    }

    return count;
}

/**
 * Checks that the given literal is a proposition - that is,
 * an atom with no arguments, which is hence independent of the
 * rest of the clause.
 */
bool isProposition(const AstLiteral* literal) {
    const AstAtom* correspondingAtom = dynamic_cast<const AstAtom*>(literal);
    if (correspondingAtom == nullptr) {
        // Just a constraint with no associated atom
        return false;
    }

    // Check that it has no arguments
    return correspondingAtom->getArguments().empty();
}

/**
 * Returns a SIPS function based on the SIPS option provided.
 * The SIPS function will return the index of the appropriate atom in a clause
 * given a goal.
 *
 * For example, the 'max-bound' SIPS function will return the
 * atom in the clause with the maximum number of bound arguments.
 */
std::function<unsigned int(std::vector<AstAtom*>, const std::set<std::string>&)> getSIPSfunction(
        const std::string& SIPSchosen) {
    // --- Create the appropriate SIPS function. ---

    // Each SIPS function has a priority metric (e.g. max-bound atoms). The function will typically
    // take in the atom, and a set of variables bound so far, and return the index of the atom that
    // maximises the priority metric.

    // If an atom in the vector should be ignored, set it to be the nullpointer.
    std::function<unsigned int(std::vector<AstAtom*>, const std::set<std::string>&)> getNextAtomSIPS;

    if (SIPSchosen == "naive") {
        // Choose the first predicate with at least one bound argument
        getNextAtomSIPS = [&](std::vector<AstAtom*> atoms, const std::set<std::string>& boundVariables) {
            for (unsigned int i = 0; i < atoms.size(); i++) {
                if (atoms[i] == nullptr) {
                    // Already processed, move on
                    continue;
                }

                if (isProposition(atoms[i]) || (numBoundArguments(atoms[i], boundVariables) >= 1)) {
                    return i;
                }
            }

            // None found, so just return the first non-null
            for (unsigned int i = 0; i < atoms.size(); i++) {
                if (atoms[i] != nullptr) {
                    return i;
                }
            }

            // Fall back to the first
            return 0U;
        };
    } else if (SIPSchosen == "max-bound") {
        // Order based on maximum number of bound variables
        getNextAtomSIPS = [&](std::vector<AstAtom*> atoms, const std::set<std::string>& boundVariables) {
            int currMaxBound = -1;
            unsigned int currMaxIdx = 0;

            for (unsigned int i = 0; i < atoms.size(); i++) {
                if (atoms[i] == nullptr) {
                    // Already processed, move on
                    continue;
                }

                if (isProposition(atoms[i])) {
                    return i;
                }

                int numBound = numBoundArguments(atoms[i], boundVariables);
                if (numBound > currMaxBound) {
                    currMaxBound = numBound;
                    currMaxIdx = i;
                }
            }

            return currMaxIdx;
        };
    } else if (SIPSchosen == "max-ratio") {
        // Order based on maximum ratio of bound to unbound
        getNextAtomSIPS = [&](std::vector<AstAtom*> atoms, const std::set<std::string>& boundVariables) {
            auto isLargerRatio = [&](std::pair<int, int> lhs, std::pair<int, int> rhs) {
                return (lhs.first * rhs.second > lhs.second * rhs.first);
            };

            std::pair<int, int> currMaxRatio = std::pair<int, int>(-1, 1);
            unsigned int currMaxIdx = 0;

            for (unsigned int i = 0; i < atoms.size(); i++) {
                if (atoms[i] == nullptr) {
                    // Already processed, move on
                    continue;
                }

                if (isProposition(atoms[i])) {
                    return i;
                }

                int numBound = numBoundArguments(atoms[i], boundVariables);
                int numArgs = atoms[i]->getArity();
                if (isLargerRatio(std::make_pair(numBound, numArgs), currMaxRatio)) {
                    currMaxRatio = std::make_pair(numBound, numArgs);
                    currMaxIdx = i;
                }
            }

            return currMaxIdx;
        };
    } else if (SIPSchosen == "all-bound") {
        // Prioritise those with all arguments bound; otherwise, left to right
        getNextAtomSIPS = [&](std::vector<AstAtom*> atoms, const std::set<std::string>& boundVariables) {
            unsigned int currFirst = 0;
            bool seen = false;

            for (unsigned int i = 0; i < atoms.size(); i++) {
                const AstAtom* currAtom = atoms[i];

                if (currAtom == nullptr) {
                    // Already processed, move on
                    continue;
                }

                if (isProposition(currAtom)) {
                    // Propositions are the best
                    return i;
                }

                if (numBoundArguments(currAtom, boundVariables) == currAtom->getArity()) {
                    // All arguments are bound!
                    return i;
                }

                if (!seen) {
                    // First valid atom, set as default priority
                    seen = true;
                    currFirst = i;
                }
            }

            return currFirst;
        };
    } else if (SIPSchosen == "least-free") {
        // Order based on the least amount of non-bound arguments in the atom
        getNextAtomSIPS = [&](std::vector<AstAtom*> atoms, const std::set<std::string>& boundVariables) {
            int currLeastFree = -1;
            unsigned int currLeastIdx = 0;

            for (unsigned int i = 0; i < atoms.size(); i++) {
                if (atoms[i] == nullptr) {
                    // Already processed, move on
                    continue;
                }

                if (isProposition(atoms[i])) {
                    return i;
                }

                int numBound = numBoundArguments(atoms[i], boundVariables);
                int numFree = atoms[i]->getArity() - numBound;
                if (currLeastFree == -1 || numFree < currLeastFree) {
                    currLeastFree = numFree;
                    currLeastIdx = i;
                }
            }

            return currLeastIdx;
        };
    } else if (SIPSchosen == "least-free-vars") {
        // Order based on the least amount of free variables in the atom
        getNextAtomSIPS = [&](std::vector<AstAtom*> atoms, const std::set<std::string>& boundVariables) {
            int currLeastFree = -1;
            unsigned int currLeastIdx = 0;

            for (unsigned int i = 0; i < atoms.size(); i++) {
                if (atoms[i] == nullptr) {
                    // Already processed, move on
                    continue;
                }

                if (isProposition(atoms[i])) {
                    return i;
                }

                std::set<std::string> freeVars;
                visitDepthFirst(*atoms[i], [&](const AstVariable& var) {
                    if (boundVariables.find(var.getName()) == boundVariables.end()) {
                        freeVars.insert(var.getName());
                    }
                });

                int numFreeVars = freeVars.size();
                if (currLeastFree == -1 || numFreeVars < currLeastFree) {
                    currLeastFree = numFreeVars;
                    currLeastIdx = i;
                }
            }

            return currLeastIdx;
        };
    } else {
        // Keep the same order - leftmost takes precedence
        getNextAtomSIPS = [&](std::vector<AstAtom*> atoms, const std::set<std::string>& boundVariables) {
            for (unsigned int i = 0; i < atoms.size(); i++) {
                if (atoms[i] == nullptr) {
                    // Already processed, move on
                    continue;
                }

                return i;
            }

            return 0U;
        };
    }

    return getNextAtomSIPS;
}

/**
 * Finds the new ordering of a given vector of atoms after the given SIPS is applied.
 */
std::vector<unsigned int> applySIPS(
        std::function<unsigned int(std::vector<AstAtom*>, const std::set<std::string>&)> getNextAtomSIPS,
        std::vector<AstAtom*> atoms) {
    std::set<std::string> boundVariables;
    std::vector<unsigned int> newOrder(atoms.size());

    unsigned int numAdded = 0;
    while (numAdded < atoms.size()) {
        // Grab the next atom, based on the SIPS priority
        unsigned int nextIdx = getNextAtomSIPS(atoms, boundVariables);
        AstAtom* nextAtom = atoms[nextIdx];

        // Set all arguments that are variables as bound
        // Arguments that are functors, etc. should not bind anything
        for (AstArgument* arg : nextAtom->getArguments()) {
            if (AstVariable* var = dynamic_cast<AstVariable*>(arg)) {
                boundVariables.insert(var->getName());
            }
        }

        newOrder[numAdded] = nextIdx;  // add to the ordering
        atoms[nextIdx] = nullptr;      // mark as done
        numAdded++;                    // move on
    }

    return newOrder;
}

bool ReorderLiteralsTransformer::transform(AstTranslationUnit& translationUnit) {
    bool changed = false;
    AstProgram& program = *translationUnit.getProgram();

    // --- Reordering --- : Prepend Propositions
    auto prependPropositions = [&](AstClause* clause) {
        const std::vector<AstAtom*>& atoms = clause->getAtoms();

        // Calculate the new ordering
        std::vector<unsigned int> nonPropositionIndices;
        std::vector<unsigned int> newOrder;

        bool seenNonProp = false;
        for (unsigned int i = 0; i < atoms.size(); i++) {
            if (isProposition(atoms[i])) {
                newOrder.push_back(i);
                if (seenNonProp) {
                    changed = true;
                }
            } else {
                nonPropositionIndices.push_back(i);
                seenNonProp = true;
            }
        }
        for (unsigned int idx : nonPropositionIndices) {
            newOrder.push_back(idx);
        }

        // Reorder the clause accordingly
        clause->reorderAtoms(newOrder);
    };

    // Literal reordering is a rule-local transformation
    for (const AstRelation* rel : program.getRelations()) {
        for (AstClause* clause : rel->getClauses()) {
            // Ignore clauses with fixed execution plans
            if (clause->hasFixedExecutionPlan()) {
                continue;
            }

            // Prepend propositions
            prependPropositions(clause);

            if (Global::config().has("SIPS")) {
                // Grab the atoms in the clause
                std::vector<AstAtom*> atoms = clause->getAtoms();

                // Decide which SIPS to use
                std::function<unsigned int(std::vector<AstAtom*>, const std::set<std::string>&)>
                        getNextAtomSIPS = getSIPSfunction(Global::config().get("SIPS"));

                // Apply the SIPS to get a new ordering
                std::vector<unsigned int> newOrdering = applySIPS(getNextAtomSIPS, atoms);

                // Check if we have a change
                for (unsigned int i = 0; !changed && i < newOrdering.size(); i++) {
                    if (newOrdering[i] != i) {
                        changed = true;
                    }
                }

                // Reorder the clause accordingly
                clause->reorderAtoms(newOrdering);
            }
        }
    }

    // Profile-Guided Reordering
    if (Global::config().has("profile-use")) {
        auto* profileUse = translationUnit.getAnalysis<AstProfileUse>();

        auto profilerSIPS = [&](std::vector<AstAtom*> atoms, const std::set<std::string>& boundVariables) {
            double currOptimalVal = -1;
            unsigned int currOptimalIdx = 0;
            bool set = false;

            for (unsigned int i = 0; i < atoms.size(); i++) {
                if (atoms[i] == nullptr) {
                    // Already processed, move on
                    continue;
                }

                AstAtom* atom = atoms[i];

                if (isProposition(atom)) {
                    return i;
                }

                int numBound = numBoundArguments(atoms[i], boundVariables);
                int numArgs = atoms[i]->getArity();
                int numFree = numArgs - numBound;

                double value = log(profileUse->getRelationSize(atom->getName()));
                value *= (numFree * 1.0) / numArgs;
                if (!set || value < currOptimalVal) {
                    set = true;
                    currOptimalVal = value;
                    currOptimalIdx = i;
                }
            }

            return currOptimalIdx;
        };

        // TODO (azreika): pull out to a function
        for (AstRelation* rel : program.getRelations()) {
            for (AstClause* clause : rel->getClauses()) {
                if (clause->hasFixedExecutionPlan()) {
                    continue;
                }

                std::vector<AstAtom*> atoms = clause->getAtoms();
                std::vector<unsigned int> newOrdering = applySIPS(profilerSIPS, atoms);

                // Check if we have a change
                for (unsigned int i = 0; !changed && i < newOrdering.size(); i++) {
                    if (newOrdering[i] != i) {
                        changed = true;
                    }
                }

                // Reorder the clause accordingly
                clause->reorderAtoms(newOrdering);
            }
        }
    }

    return changed;
}

}  // namespace souffle
