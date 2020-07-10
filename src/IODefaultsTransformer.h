/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file IODefaultsTransformer.h
 *
 * Defines AST transformation passes.
 *
 ***********************************************************************/

#pragma once

#include "AstTransformer.h"

namespace souffle {

/**
 * Transformation pass to remove relations which are redundant (do not contribute to output).
 */
class IODefaultsTransformer : public AstTransformer {
public:
    std::string getName() const override {
        return "IODefaultsTransformer";
    }

private:
    bool transform(AstTranslationUnit& translationUnit) override {
        bool changed = false;
        AstProgram* program = translationUnit.getProgram();
        for (AstIO* io : program->getIOs()) {
            // Set a default IO of file
            if (!io->hasDirective("IO")) {
                io->addDirective("IO", "file");
                changed = true;
            }

            // Set the relation name
            if (!io->hasDirective("name")) {
                std::string&& name = toString(join(io->getQualifiedName().getQualifiers(), "."));
                io->addDirective("name", name);
                changed = true;
            }

            // Set the operation type (input/output/printsize)
            if (!io->hasDirective("operation")) {
                if (io->getType() == AstIoType::input) {
                    io->addDirective("operation", "input");
                    changed = true;
                    // Configure input directory
                    if (Global::config().has("fact-dir")) {
                        io->addDirective("fact-dir", Global::config().get("fact-dir"));
                    }
                } else if (io->getType() == AstIoType::output) {
                    io->addDirective("operation", "output");
                    changed = true;
                    // Configure output directory
                    if (Global::config().has("output-dir")) {
                        io->addDirective("output-dir", Global::config().get("output-dir"));
                    }
                } else if (io->getType() == AstIoType::printsize) {
                    io->addDirective("operation", "printsize");
                    io->addDirective("IO", "printsize");
                    changed = true;
                }
            }
        }
        // If output-dir == '-' then convert all output to stdout
        // If output-dir == '-' remove duplicate output
        // add attribute nanes (find relation, get attributes, etc
        return changed;
    }
};

}  // namespace souffle
