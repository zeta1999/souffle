#include "AstProgram.h"
#include "AstTransforms.h"
#include "AstVisitor.h"
#include <stack>

namespace souffle {

// TODO ABDUL: something weird going on with extractdisconecneojtoejododj transformer so fix that at some point maybe

bool areBijectivelyEquivalent(AstClause* left, AstClause* right) {
    auto isValidClause = [&](const AstClause* clause) {
        bool valid = true;

        // check that all body literals are atoms
        for (AstLiteral* lit : clause->getBodyLiterals()) {
            if (!dynamic_cast<AstAtom*>(lit)) {
                valid = false;
            }
        }

        // check that all arguments are either constants or variables
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

    int size = left->getBodyLiterals().size() + 1;
    std::vector<std::vector<int>> adj = std::vector<std::vector<int>>(size);
    for (size_t i = 0; i < adj.size(); i++) {
        adj[i] = std::vector<int>(size);
    }

    // TODO ABDUL fix up indices maybe idk
    auto possibleMove = [&](AstClause* left, AstClause* right, int start, int end) {
        if (start * end == 0 && start + end != 0) {
            return false;
        } else if (start == 0) {
            return true;
        }
        start -=1;
        end -=1;

        if (left->getBodyLiteral(start)->getAtom()->getName() == right->getBodyLiteral(end)->getAtom()->getName()) {
            return true;
        }
        return false;
    };

    // create matrix of permutations
    for (int i = 0; i < size; i++) {
        for (int j = 0; j < size; j++) {
            if (possibleMove(left, right, i, j)) {
                adj[i][j] = 1;
            }
        }
    }

    auto getValidPermutations = [&](std::vector<std::vector<int>> adj) {
        std::vector<std::vector<int>> sparseAdj;
        for (size_t i = 0; i < adj.size(); i++) {
            std::vector<int> currentRow;
            for (size_t j = 0; j < adj.size(); j++) {
                if (adj[i][j] == 1) {
                    currentRow.push_back(j);
                }
            }
            sparseAdj.push_back(currentRow);
        }

        std::vector<std::vector<int>> permutations;
        // TODO ABDUL: this part
        std::vector<int> seen(adj.size());
        std::vector<int> currentPermutation;
        std::stack<std::vector<int>> stack;

        stack.push(std::vector<int>(sparseAdj[0])); // TODO necessary?
        size_t currPos = 0;
        while (!stack.empty()) {
            std::cout << "JUMPING IN?!?!" << std::endl;
            std::cout << currentPermutation << std::endl;
            if (currPos == adj.size()) {
                std::cout << "we here" << std::endl;
                permutations.push_back(currentPermutation);
                currPos -= 1;
                seen[currentPermutation[currPos]] = 0;
                currentPermutation.pop_back();
                std::cout << "continuing..." << std::endl;
                continue;
            }

            std::vector<int> possibilities = stack.top();
            std::cout << "position: " << currPos << " ; values: " << possibilities << " with seen result: " << seen <<  std::endl;
            stack.pop();
            if (possibilities.size() == 0) {
                std::cout << "we there" << std::endl;
                std::cout << "absolutely broken? btw the size is " << currentPermutation.size() << "adn curr pos is " << currPos << std::endl;
                if (currPos >= 1) {
                    seen[currentPermutation[currPos-1]] = 0;
                }
                currPos -= 1;
                std::cout << "not absolutely broken?" << std::endl;
                currentPermutation.pop_back();
                std::cout << "continuing..." << std::endl;
                continue;
            }
            int currNum = possibilities[0];
            possibilities.erase(possibilities.begin());
            stack.push(possibilities);
            std::cout << "we finalising stuff?" << std::endl;
            if (seen[currNum]) {
                std::cout << "continuing..." << std::endl;
                continue;
            } else {
                seen[currNum] = 1;
                currentPermutation.push_back(currNum);
                currPos += 1;
                if (currPos < adj.size()) {
                    stack.push(std::vector<int>(sparseAdj[currPos]));
                }
            }
        }

        std::cout << "absolute beauty" << std::endl;

        std::cout << permutations << std::endl;

        return permutations;
    };

    auto validPermutation = [&](AstClause* left, AstClause* right, std::vector<int> permutation) {
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

    std::vector<std::vector<int>> permutations = getValidPermutations(adj);

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
