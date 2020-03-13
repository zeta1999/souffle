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
    visitDepthFirst(program, [&](const AstIO& io) {
        auto* relation = getRelation(program, io.getQualifiedName());
        if (relation == nullptr) {
            return;
        }
        switch (io.getType()) {
            case AstIO::InputIO:
                inputRelations.insert(relation);
                break;
            case AstIO::OutputIO:
                outputRelations.insert(relation);
                break;
            case AstIO::PrintsizeIO:
                printSizeRelations.insert(relation);
                outputRelations.insert(relation);
                break;
            default:
                assert("Unrecognized I/O operation");
        }
    });
}

void IOType::print(std::ostream& os) const {
    os << "input relations: {";
    os << join(inputRelations, ", ",
            [](std::ostream& out, const AstRelation* r) { out << r->getQualifiedName(); });
    os << "}\n";
    os << "output relations: {";
    os << join(outputRelations, ", ",
            [](std::ostream& out, const AstRelation* r) { out << r->getQualifiedName(); });
    os << "}\n";
    os << "printsize relations: {";
    os << join(printSizeRelations, ", ",
            [](std::ostream& out, const AstRelation* r) { out << r->getQualifiedName(); });
    os << "}\n";
}

}  // end of namespace souffle
