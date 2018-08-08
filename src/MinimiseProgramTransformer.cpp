#include "AstProgram.h"
#include "AstTransforms.h"
#include "AstVisitor.h"

namespace souffle {

class VariableDecomposition {
private:
    bool valid;
    std::vector<std::vector<std::pair<int,int>>> decomposition;

public:
    VariableDecomposition(const AstClause& clause) {
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

            std::cout << positions << std::endl;
        }

        std::cout << clause << " valid? " << valid << std::endl;
    }

    bool operator==(const VariableDecomposition o) {
        return false;
    }
};

bool areBijectivelyEquivalent(AstClause* left, AstClause* right) {
    VariableDecomposition leftDec = VariableDecomposition(*left);
    VariableDecomposition rightDec = VariableDecomposition(*right);
    return leftDec == rightDec;
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
