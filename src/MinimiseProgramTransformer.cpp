#include "AstProgram.h"
#include "AstTransforms.h"

namespace souffle {
bool areBijectivelyEquivalent(AstClause* left, AstClause* right) {
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
