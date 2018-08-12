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

/**
 * Check if the atom at leftIdx in the left clause can potentially be matched up
 * with the atom at rightIdx in the right clause.
 * NOTE: index 0 refers to the head atom, index 1 to the first body atom, and so on.
 */
bool isValidMove(AstClause* left, size_t leftIdx, AstClause* right, size_t rightIdx) {
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
}

/**
 * Check whether a valid variable mapping exists for the given permutation.
 */
bool isValidPermutation(AstClause* left, AstClause* right, std::vector<unsigned int> permutation) {
    // --- perform the permutation ---

    auto reorderedLeft = std::unique_ptr<AstClause>(left->clone());

    // deduce the body atom permutation from the full clause permutation
    std::vector<unsigned int> bodyPermutation(permutation.begin()+1, permutation.end());
    for (size_t i = 0; i < bodyPermutation.size(); i++) {
        bodyPermutation[i] -= 1;
    }

    // currently, <permutation[i] == j> indicates that atom i should map to position j
    // internally, for the clause class' reordering function, <permutation[i] == j> indicates
    // that position i should contain atom j
    // rearrange the permutation to match the internals
    // TODO: perhaps change the internals because this came up in magic set too
    std::vector<unsigned int> reorderedPermutation(bodyPermutation.size());
    for (size_t i = 0; i < bodyPermutation.size(); i++) {
        reorderedPermutation[bodyPermutation[i]] = i;
    }

    // perform the permutation
    reorderedLeft->reorderAtoms(reorderedPermutation);

    // --- check if a valid variable exists corresponding to this permutation ---

    std::map<std::string, std::string> variableMap;
    visitDepthFirst(*reorderedLeft, [&](const AstVariable& var) {
        variableMap[var.getName()] = "";
    });

    // need to match the variables in the body
    std::vector<AstLiteral*> leftAtoms = reorderedLeft->getBodyLiterals();
    std::vector<AstLiteral*> rightAtoms = right->getBodyLiterals();

    // need to match the variables in the head
    leftAtoms.push_back(reorderedLeft->getHead());
    rightAtoms.push_back(right->getHead());

    // check if a valid variable mapping exists
    auto isVariable = [&](const AstArgument* arg) {
        return dynamic_cast<const AstVariable*>(arg);
    };

    auto isConstant = [&](const AstArgument* arg) {
        return dynamic_cast<const AstConstant*>(arg);
    };

    bool validMapping = true;
    for (size_t i = 0; i < leftAtoms.size() && validMapping; i++) {
        // match arguments
        std::vector<AstArgument*> leftArgs = leftAtoms[i]->getAtom()->getArguments();
        std::vector<AstArgument*> rightArgs = rightAtoms[i]->getAtom()->getArguments();

        for (size_t j = 0; j < leftArgs.size(); j++) {
            AstArgument* leftArg = leftArgs[j];
            AstArgument* rightArg = rightArgs[j];
            if (isVariable(leftArg) && isVariable(rightArg)) {
                // both variables, their names should match to each other through the clause
                std::string leftVarName = dynamic_cast<AstVariable*>(leftArg)->getName();
                std::string rightVarName = dynamic_cast<AstVariable*>(rightArg)->getName();

                std::string currentMap = variableMap[leftVarName];
                if (currentMap == "") {
                    // unassigned yet, so assign it appropriately
                    variableMap[leftVarName] = rightVarName;
                } else if (currentMap != rightVarName) {
                    // mapping is inconsistent!
                    // clauses cannot be equivalent under this permutation
                    validMapping = false;
                    break;
                }
            } else if (isConstant(leftArg) && isConstant(rightArg)) {
                // check if its the same constant
                auto leftCst = dynamic_cast<AstConstant*>(leftArg)->getIndex();
                auto rightCst = dynamic_cast<AstConstant*>(rightArg)->getIndex();

                if (leftCst != rightCst) {
                    // constants don't match, failed!
                    validMapping = false;
                    break;
                }
            } else {
                // not the same type, failed!
                validMapping = false;
                break;
            }
        }
    }

    // return whether a valid variable mapping exists for this permutation
    return validMapping;
}

/**
 * Check whether two clauses are bijectively equivalent.
 */
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
    std::vector<std::vector<unsigned int>> permutationMatrix = std::vector<std::vector<unsigned int>>(size);
    for (size_t i = 0; i < permutationMatrix.size(); i++) {
        permutationMatrix[i] = std::vector<unsigned int>(size);
    }

    // create permutation matrix
    for (size_t i = 0; i < size; i++) {
        for (size_t j = 0; j < size; j++) {
            if (isValidMove(left, i, right, j)) {
                permutationMatrix[i][j] = 1;
            }
        }
    }

    // check if any of these permutations have valid variable mappings associated with them
    std::vector<std::vector<unsigned int>> permutations = extractPermutations(permutationMatrix);
    for (auto permutation : permutations) {
        if (isValidPermutation(left, right, permutation)) {
            // valid permutation with valid corresponding variable mapping exists
            // therefore, the two clauses are equivalent!
            return true;
        }
    }

    return false;
}

bool MinimiseProgramTransformer::transform(AstTranslationUnit& translationUnit) {
    AstProgram& program = *translationUnit.getProgram();

    std::vector<AstClause*> clausesToDelete;

    // split up each relation's rules into equivalene classes
    // TODO: perhaps move into an analysis
    for (AstRelation* rel : program.getRelations()) {
        std::vector<std::vector<AstClause*>> equivalenceClasses;

        for (AstClause* clause : rel->getClauses()) {
            bool added = false;

            for (std::vector<AstClause*>& eqClass : equivalenceClasses) {
                AstClause* rep = eqClass[0];

                if (areBijectivelyEquivalent(rep, clause)) {
                    // clause belongs to an existing equivalence class, so delete it
                    eqClass.push_back(clause);
                    clausesToDelete.push_back(clause);
                    added = true;
                    break;
                }
            }

            if (!added) {
                // clause does not belong to any existing equivalence class, so keep it
                // TODO: better syntax?
                std::vector<AstClause*> clauseToAdd;
                clauseToAdd.push_back(clause);
                equivalenceClasses.push_back(clauseToAdd);
            }
        }
    }

    // remove non-representative clauses
    for (auto clause : clausesToDelete) {
        program.removeClause(clause);
    }

    // changed iff any clauses were deleted
    return !clausesToDelete.empty();
}

}  // namespace souffle
