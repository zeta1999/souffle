/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamTransformer.cpp
 *
 * Defines the interface for RAM transformation passes.
 *
 ***********************************************************************/

#include "RamTransformer.h"
#include "DebugReport.h"
#include "RamTranslationUnit.h"
#include "Util.h"

#include <algorithm>

namespace souffle {

bool RamTransformer::apply(RamTranslationUnit& translationUnit) {
    // take snapshot of alive analyses before invocation
    std::set<const RamAnalysis*> beforeInvocation = translationUnit.getAliveAnalyses();

    std::stringstream ramProgStrOld;
    ramProgStrOld << translationUnit.getProgram();

    // invoke the transformation
    bool changed = transform(translationUnit);

    // take snapshot of alive analyses after invocation
    std::set<const RamAnalysis*> afterInvocation = translationUnit.getAliveAnalyses();

    // print newly invoked analyses (not for meta transformers)
    if (nullptr == dynamic_cast<RamMetaTransformer*>(this)) {
        for (const RamAnalysis* analysis : afterInvocation) {
            if (0 == beforeInvocation.count(analysis)) {
                std::stringstream ramAnalysisStr;
                analysis->print(ramAnalysisStr);
                if (!ramAnalysisStr.str().empty()) {
                    translationUnit.getDebugReport().addSection(
                            getName(), "RAM Analysis " + analysis->getName(), ramAnalysisStr.str());
                }
            }
        }
    }

    if (changed) {
        translationUnit.invalidateAnalyses();

        TempFileStream in_old, in_new;
        in_old << ramProgStrOld.str();
        in_new << translationUnit.getProgram();
        in_old.flush();
        in_new.flush();
        std::string diff_cmd =
                "diff --new-line-format='+%L' "
                "     --old-line-format='-%L' "
                "     --unchanged-line-format=' %L' ";
        auto ramProgStr = exec_stdout(diff_cmd + in_old.getFileName() + " " + in_new.getFileName()).str();
        translationUnit.getDebugReport().addSection(
                getName(), "RAM Program after " + getName(), ramProgStr);

    } else {
        translationUnit.getDebugReport().addSection(
                getName(), "After " + getName() + " " + " (unchanged)", "");
    }

    /* Abort evaluation of the program if errors were encountered */
    if (translationUnit.getErrorReport().getNumErrors() != 0) {
        std::cerr << translationUnit.getErrorReport();
        std::cerr << std::to_string(translationUnit.getErrorReport().getNumErrors()) +
                             " errors generated, evaluation aborted"
                  << std::endl;
        exit(1);
    }
    return changed;
}

}  // end of namespace souffle
