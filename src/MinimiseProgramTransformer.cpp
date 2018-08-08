#include "AstProgram.h"
#include "AstTransforms.h"
#include "AstVisitor.h"
#include <stack>

namespace souffle {

class VariableDecomposition {
private:
    bool valid;
    std::vector<std::vector<std::pair<int,int>>> decomposition;

public:
    const std::vector<std::vector<std::pair<int,int>>>& getDecomposition() const {
        return this->decomposition;
    }

    bool isValid() {
        return valid;
    }

    VariableDecomposition(const AstClause& clause) {
        // decomposition = std::vector<std::vector<std::pair<int,int>>>();

        // check validity
        valid = true;

        // TODO ABDUL get rid of later
        if (clause.isFact()) {
            valid = false;
        }

        // TODO ABDUL add constraints and negtations later
        for (AstLiteral* lit : clause.getBodyLiterals()) {
            if (!dynamic_cast<AstAtom*>(lit)) {
                valid = false;
                break;
            }
        }

        visitDepthFirst(clause, [&](const AstArgument& arg) {
            if (!dynamic_cast<const AstVariable*>(&arg) && !dynamic_cast<const AstConstant*>(&arg)) {
                valid = false;
            }
        });

        if (valid) {
            std::map<std::string, std::vector<std::pair<int,int>>> positions;

            visitDepthFirst(clause, [&](const AstVariable& var) {
                positions[var.getName()] = std::vector<std::pair<int,int>>();
            });

            AstAtom* head = clause.getHead();
            std::vector<AstArgument*> headargs = head->getArguments();
            for (int i = 0; i < headargs.size(); i++) {
                AstArgument* curr = headargs[i];
                if (const AstVariable* var = dynamic_cast<const AstVariable*>(curr)) {
                    positions[var->getName()].push_back(std::make_pair(0,i));
                } else {
                    std::cout << "not a var? " << *curr << std::endl;
                }
            }

            std::vector<AstLiteral*> bodylits = clause.getBodyLiterals();
            for (int i = 0; i < bodylits.size(); i++) {
                const AstAtom* curr = dynamic_cast<const AstAtom*>(bodylits[i]);
                std::vector<AstArgument*> args = curr->getArguments();
                for (int j = 0; j < args.size(); j++) {
                    AstArgument* carg = args[j];
                    if (const AstVariable* var = dynamic_cast<const AstVariable*>(carg)) {
                        positions[var->getName()].push_back(std::make_pair(i+1,j));
                    } else {
                        std::cout << "not a var? " << *carg << std::endl;
                    }
                }
            }

            for (auto el = positions.begin(); el != positions.end(); el++) {
                decomposition.push_back(el->second);
            }

            // std::cout << decomposition << std::endl;
        }

        // std::cout << clause << " valid? " << valid << std::endl;
    }
};

bool areBijectivelyEquivalent(AstClause* left, AstClause* right) {
    VariableDecomposition leftDec = VariableDecomposition(*left);
    VariableDecomposition rightDec = VariableDecomposition(*right);

    if (!leftDec.isValid() || !rightDec.isValid()) {
        return false;
    }

    if (left->getBodyLiterals().size() != right->getBodyLiterals().size()) {
        return false;
    }

    auto ld = leftDec.getDecomposition();
    auto rd = rightDec.getDecomposition();
    std::cout << "checking " << ld << " == " << rd << std::endl;
    std::cout << *left << std::endl << *right << std::endl;

    int size = left->getBodyLiterals().size() + 1;
    std::vector<std::vector<int>> adj = std::vector<std::vector<int>>(size);
    for (int i = 0; i < adj.size(); i++) {
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
        for (int i = 0; i < adj.size(); i++) {
            std::vector<int> currentRow;
            for (int j = 0; j < adj.size(); j++) {
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
        int currPos = 0;
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
        for (int i = 0; i < unsignedVersion.size(); i++) {
            unsignedVersion[i] -= 1;
        }
        std::vector<unsigned int> newOrdering(unsignedVersion.size());
        for (int i = 0; i < unsignedVersion.size(); i++) {
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

        bool bad = false;
        visitDepthFirst(*left, [&](const AstArgument& arg) {
            if (!dynamic_cast<const AstVariable*>(&arg)) {
                bad = true;
            }
        });
        visitDepthFirst(*right, [&](const AstArgument& arg) {
            if (!dynamic_cast<const AstVariable*>(&arg)) {
                bad = true;
            }
        });
        if (bad) { std::cout << "THESE WERE BAD: " << *left << " " << *right << std::endl; return false; }

        bool equiv = true;
        for (int i = 0; i < leftBodyLiterals.size() && equiv; i++) {
            std::cout << "getting: " << std::endl;
            std::vector<AstArgument*> leftArgs = leftBodyLiterals[i]->getAtom()->getArguments();
            std::vector<AstArgument*> rightArgs = rightBodyLiterals[i]->getAtom()->getArguments();
            for (int j = 0; j < leftArgs.size(); j++) {
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

    for (AstRelation* rel : program.getRelations()) {
        std::vector<std::vector<AstClause*>> equivalenceClasses;
        for (AstClause* clause : rel->getClauses()) {
            bool added = false;

            for (std::vector<AstClause*> eqClass : equivalenceClasses) {
                AstClause* representative = eqClass[0];
                if (areBijectivelyEquivalent(representative, clause)) {
                    eqClass.push_back(representative);
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
                std::cout << *clause << std::endl;
            }
            std::cout << "                        " << std::endl;
        }
    }

    return true;
}

}  // namespace souffle
