#include "AstProgram.h"
#include "AstTransforms.h"
#include "AstVisitor.h"
#include <stack>

namespace souffle {

// TODO ABDUL: something weird going on with extractdisconecneojtoejododj transformer so fix that at some point maybe

/**
 * Extract valid permutations from a given permutation matrix of valid moves.
 */
std::vector<std::vector<unsigned int>> extractPermutations(std::vector<std::vector<unsigned int>> permutationMatrix) {
    size_t clauseSize = permutationMatrix.size();
    // keep track of the possible end-positions of each atom in the first clause
    std::vector<std::vector<unsigned int>> validMoves;
    for (size_t i = 0; i < clauseSize; i++) {
        std::vector<unsigned int> currentRow;
        for (size_t j = 0; j < clauseSize; j++) {
            if (permutationMatrix[i][j] == 1) {
                currentRow.push_back(j);
            }
        }
        validMoves.push_back(currentRow);
    }

    // extract the possible permutations, DFS style
    std::vector<std::vector<unsigned int>> permutations;
    std::vector<unsigned int> seen(clauseSize);
    std::vector<unsigned int> currentPermutation;
    std::stack<std::vector<unsigned int>> todoStack;

    todoStack.push(std::vector<unsigned int>(validMoves[0])); // TODO: is this necessary?
    size_t currentIdx = 0;
    while (!todoStack.empty()) {
        if (currentIdx == clauseSize) {
            // permutation is complete
            permutations.push_back(currentPermutation);

            // undo the last number added to the permutation
            currentIdx--;
            if (currentIdx >= 0) {
                seen[currentPermutation[currentIdx]] = 0;
            }
            currentPermutation.pop_back();

            // see if we can pick up other permutations
            continue;
        }

        // pull out the possibilities for the current point of the permutation
        std::vector<unsigned int> possibilities = todoStack.top();
        todoStack.pop();
        if (possibilities.empty()) {
            // no more possibilities at this point, so undo our last move
            currentIdx--;
            if (currentIdx >= 0) {
                seen[currentPermutation[currentIdx]] = 0;
            }
            currentPermutation.pop_back();

            // continue looking for permutations
            continue;
        }

        // try the next possibility
        unsigned int nextNum = possibilities[0];

        // update the possibility vector for the current position
        possibilities.erase(possibilities.begin());
        todoStack.push(possibilities);

        if (seen[nextNum]) {
            // number already seen in this permutation
            continue;
        } else {
            // number can be used
            seen[nextNum] = 1;
            currentPermutation.push_back(nextNum);
            currentIdx++;

            // if we havent reached the end of the permutation,
            // push up the valid moves for the next position
            if (currentIdx < clauseSize) {
                todoStack.push(std::vector<unsigned int>(validMoves[currentIdx]));
            }
        }
    }

    // found all permutations
    return permutations;
}

bool areBijectivelyEquivalent(AstClause* left, AstClause* right) {
    // only check bijective equivalence for a subset of the possible clauses
    auto isValidClause = [&](const AstClause* clause) {
        bool valid = true;

        // check that all body literals are atoms
        // i.e. avoid clauses with constraints or negations
        for (AstLiteral* lit : clause->getBodyLiterals()) {
            if (!dynamic_cast<AstAtom*>(lit)) {
                valid = false;
            }
        }

        // check that all arguments are either constants or variables
        // i.e. only allow primitive arguments
        visitDepthFirst(*clause, [&](const AstArgument& arg) {
            if (!dynamic_cast<const AstVariable*>(&arg) && !dynamic_cast<const AstConstant*>(&arg)) {
                valid = false;
            }
        });

        return valid;
    };

    if (!isValidClause(left) || !isValidClause(right)) {
        return false;
    }

    // rules must be the same length to be equal
    if (left->getBodyLiterals().size() != right->getBodyLiterals().size()) {
        return false;
    }

    // set up the n x n permutation matrix, where n is the number of
    // atoms in the clause, including the head atom
    size_t size = left->getBodyLiterals().size() + 1;
    std::vector<std::vector<unsigned int>> adj = std::vector<std::vector<unsigned int>>(size);
    for (size_t i = 0; i < adj.size(); i++) {
        adj[i] = std::vector<unsigned int>(size);
    }

    // checks if the atom at leftIdx in the left clause can potentially be
    // matched up with the atom at rightIdx in the right clause
    // NOTE: index 0 refers to the head atom, index 1 to the first body atom, etc.
    auto possibleMove = [&](AstClause* left, int leftIdx, AstClause* right, int rightIdx) {
        // invalid indices
        if (leftIdx < 0 || rightIdx < 0) {
            return false;
        }

        // handle the case where one of the indices refers to the head
        if (leftIdx == 0 && rightIdx == 0) {
            const AstAtom* leftHead = left->getHead()->getAtom();
            const AstAtom* rightHead = right->getHead()->getAtom();
            return leftHead->getName() == rightHead->getName();
        } else if (leftIdx == 0 || rightIdx == 0) {
            return false;
        }

        // both must hence be body atoms
        int leftBodyAtomIdx = leftIdx - 1;
        const AstAtom* leftAtom = left->getBodyLiteral(leftBodyAtomIdx)->getAtom();

        int rightBodyAtomIdx = rightIdx - 1;
        const AstAtom* rightAtom = right->getBodyLiteral(rightBodyAtomIdx)->getAtom();

        return leftAtom->getName() == rightAtom->getName();
    };

    // create permutation matrix
    for (size_t i = 0; i < size; i++) {
        for (size_t j = 0; j < size; j++) {
            if (possibleMove(left, i, right, j)) {
                adj[i][j] = 1;
            }
        }
    }


    auto validPermutation = [&](AstClause* left, AstClause* right, std::vector<unsigned int> permutation) {
        AstClause* clone = left->clone();
        std::vector<unsigned int> unsignedVersion(permutation.begin()+1, permutation.end());
        for (size_t i = 0; i < unsignedVersion.size(); i++) {
            unsignedVersion[i] -= 1;
        }
        std::vector<unsigned int> newOrdering(unsignedVersion.size());
        for (size_t i = 0; i < unsignedVersion.size(); i++) {
            newOrdering[unsignedVersion[i]] = i;
        }
        std::cout << "we here " << std::endl;
        std::cout << newOrdering << std::endl;
        std::cout << "before: " << *clone << std::endl;
        clone->reorderAtoms(newOrdering);
        left = clone;
        std::cout << "after: " << *clone << std::endl;
        std::map<std::string, std::string> variableMap;
        visitDepthFirst(*left, [&](const AstVariable& var) {
            variableMap[var.getName()] = "";
        });

        // TODO ABDUL
        std::cout << "checking: " << *clone << " " << *right << " " << std::endl;

        // match the head
        AstAtom* leftHead = left->getHead();
        AstAtom* rightHead = right->getHead();

        // match the body literals
        std::vector<AstLiteral*> leftBodyLiterals = left->getBodyLiterals();
        std::vector<AstLiteral*> rightBodyLiterals = right->getBodyLiterals();
        leftBodyLiterals.push_back(leftHead);
        rightBodyLiterals.push_back(rightHead);

        auto singletonThing = [&](const AstArgument& arg) { return (dynamic_cast<const AstVariable*>(&arg) || dynamic_cast<const AstConstant*>(&arg)); };
        auto isVariable = [&](const AstArgument* arg) { return (dynamic_cast<const AstVariable*>(arg) || 0); };
        auto isConstant = [&](const AstArgument* arg) { return (dynamic_cast<const AstConstant*>(arg) || 0); };


        bool bad = false;
        visitDepthFirst(*left, [&](const AstArgument& arg) {
            if (!singletonThing(arg)) { bad = true; }
        });
        visitDepthFirst(*right, [&](const AstArgument& arg) {
            if (!singletonThing(arg)) { bad = true; }
        });
        if (bad) { std::cout << "THESE WERE BAD: " << *left << " " << *right << std::endl; return false; }

        bool equiv = true;
        for (size_t i = 0; i < leftBodyLiterals.size() && equiv; i++) {
            std::cout << "getting: " << std::endl;
            std::vector<AstArgument*> leftArgs = leftBodyLiterals[i]->getAtom()->getArguments();
            std::vector<AstArgument*> rightArgs = rightBodyLiterals[i]->getAtom()->getArguments();
            for (size_t j = 0; j < leftArgs.size(); j++) {
                if (isVariable(leftArgs[j]) && isVariable(rightArgs[j])) {
                    auto leftVar = dynamic_cast<AstVariable*>(leftArgs[j])->getName();
                    auto rightVar = dynamic_cast<AstVariable*>(rightArgs[j])->getName();
                    std::cout << leftVar << " VS " << rightVar << std::endl;
                    std::cout << "in " << *leftBodyLiterals[i] << " vs " << *rightBodyLiterals[i] << "...." << std::endl;
                    std::string currSymbol = variableMap[leftVar];
                    if(currSymbol == "") {
                        variableMap[leftVar] = rightVar;
                    } else if (currSymbol != rightVar) {
                        std::cout << "bad: " << leftVar << " " << currSymbol << " " << rightVar << std::endl;
                        equiv = false;
                        break;
                    }
                } else if (isConstant(leftArgs[j]) && isConstant(rightArgs[j])) {
                    auto leftVar = dynamic_cast<AstConstant*>(leftArgs[j])->getIndex();
                    auto rightVar = dynamic_cast<AstConstant*>(rightArgs[j])->getIndex();
                    if (leftVar != rightVar) {
                        equiv = false;
                    }
                } else {
                    equiv = false;
                    break;
                }
            }
        }
        std::cout << variableMap << std::endl;
        std::cout << "equiv? " << equiv << std::endl;

        return equiv;
    };

    std::vector<std::vector<unsigned int>> permutations = extractPermutations(adj);
    for (auto permutation : permutations) {
        std::cout << "testing " << permutation << " ... " << std::endl;
        if (validPermutation(left, right, permutation)) {
            std::cout << "THEYRE EQUIVALENT!!!" << std::endl;
            return true;
        }
        std::cout << permutation << " failed " << std::endl;
    }

    return false;
}

bool MinimiseProgramTransformer::transform(AstTranslationUnit& translationUnit) {
    AstProgram& program = *translationUnit.getProgram();

    std::vector<AstClause*> clausesToDelete;
    for (AstRelation* rel : program.getRelations()) {
        std::vector<std::vector<AstClause*>> equivalenceClasses;
        for (AstClause* clause : rel->getClauses()) {
            bool added = false;

            for (std::vector<AstClause*>& eqClass : equivalenceClasses) {
                AstClause* representative = eqClass[0];
                if (areBijectivelyEquivalent(representative, clause)) {
                    eqClass.push_back(clause);
                    clausesToDelete.push_back(clause);
                    std::cout << "found equivalent rules :))))" << std::endl;
                    added = true;
                    break;
                }
            }

            if (!added) {
                std::vector<AstClause*> clauseToAdd;
                clauseToAdd.push_back(clause);
                equivalenceClasses.push_back(clauseToAdd);
            }
        }

        std::cout << rel << std::endl << equivalenceClasses << std::endl;
        for (auto eqclass : equivalenceClasses) {
            std::cout << "EQUIVALENCE CLASS:" << std::endl;
            for (auto clause : eqclass) {
                std::cout << "?:::"  << *clause << std::endl;
            }
            std::cout << "                        " << std::endl;
        }
    }

    for (auto clause : clausesToDelete) {
        program.removeClause(clause);
    }

    return clausesToDelete.size() > 0;
}

}  // namespace souffle
