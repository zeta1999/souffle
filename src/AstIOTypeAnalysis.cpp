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
#include "AstIO.h"
#include "AstRelation.h"
#include "AstTranslationUnit.h"
#include "AstVisitor.h"

namespace souffle {

void IOType::run(const AstTranslationUnit& translationUnit) {
    const AstProgram& program = *translationUnit.getProgram();
    visitDepthFirst(program, [&](const AstIO& directive) {
        auto* relation = getRelation(program, directive.getQualifiedName());
        if (relation == nullptr) return;
        const std::string& op = directive.getKVP("operation");
        if (op == "input") {
            inputRelations.insert(relation);
        } else if (op == "output") {
            outputRelations.insert(relation);
        } else if (op == "printsize") {
            printSizeRelations.insert(relation);
            outputRelations.insert(relation);
        } else {
            assert("Unrecognized I/O operation");
        }
    });
}

void IOType::print(std::ostream& os) const {
    os << "input relations: " << inputRelations << std::endl;
    os << "output relations: " << outputRelations << std::endl;
    os << "printSize relations: " << printSizeRelations << std::endl;
}

}  // end of namespace souffle
