/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstIOTypeAnalysis.h
 *
 * Implements methods to identify a relation as input, output, or printsize.
 *
 ***********************************************************************/

#include "AstIOTypeAnalysis.h"
#include "AstIODirective.h"
#include "AstRelation.h"
#include "AstTranslationUnit.h"
#include "AstVisitor.h"

namespace souffle {

void IOType::run(const AstTranslationUnit& translationUnit) {
    visitDepthFirst(*translationUnit.getProgram(), [&](const AstIODirective& directive) {
        if (directive.getNames().empty()) {
            return;
        }

        auto* relation = translationUnit.getProgram()->getRelation(directive.getName());
        if (relation == nullptr) {
            return;
        }

        if (directive.isInput()) {
            inputRelations.insert(relation);
        }
        if (directive.isOutput()) {
            outputRelations.insert(relation);
        }
        if (directive.isPrintSize()) {
            printSizeRelations.insert(relation);
        }
    });
}

void IOType::print(std::ostream& os) const {
    os << "input relations: " << inputRelations << std::endl;
    os << "output relations: " << outputRelations << std::endl;
    os << "printSize relations: " << printSizeRelations << std::endl;
}

}  // end of namespace souffle
