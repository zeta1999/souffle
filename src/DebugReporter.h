/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file DebugReporter.h
 *
 * Defines an adaptor transformer to capture debug output from other transformers
 *
 ***********************************************************************/
#pragma once

#include "AstTransformer.h"
#include "DebugReport.h"
#include "Util.h"

#include <memory>
#include <set>
#include <string>
#include <utility>

namespace souffle {

class AstTranslationUnit;

/**
 * Transformation pass which wraps another transformation pass and generates
 * a debug report section for the stage after applying the wrapped transformer,
 * and adds it to the translation unit's debug report.
 */
class DebugReporter : public MetaTransformer {
public:
    DebugReporter(std::unique_ptr<AstTransformer> wrappedTransformer)
            : wrappedTransformer(std::move(wrappedTransformer)) {}

    void setDebugReport() override {}

    void setVerbosity(bool verbose) override {
        this->verbose = verbose;
        if (auto* mt = dynamic_cast<MetaTransformer*>(wrappedTransformer.get())) {
            mt->setVerbosity(verbose);
        }
    }

    void disableTransformers(const std::set<std::string>& transforms) override {
        if (auto* mt = dynamic_cast<MetaTransformer*>(wrappedTransformer.get())) {
            mt->disableTransformers(transforms);
        } else if (transforms.find(wrappedTransformer->getName()) != transforms.end()) {
            wrappedTransformer = std::unique_ptr<AstTransformer>(new NullTransformer());
        }
    }

    std::string getName() const override {
        return "DebugReporter";
    }

    /**
     * Generate a debug report section for the current state of the given translation unit
     * with the given id and title, and add the section to the translation unit's debug report.
     * @param translationUnit translation unit to generate and add debug report section
     * @param id the unique id of the generated section
     * @param title the text to display as the heading of the section
     */
    static void generateDebugReport(AstTranslationUnit& translationUnit,
            const std::string& preTransformDatalog, const std::string& id, std::string title);

    /**
     * Generate a full-content diff between two sources.
     * Both arguments are passed into a `std::ostream` so you may exploit stream implementations.
     */
    template <typename A, typename B>
    static std::string generateDiff(const A& prev, const B& curr) {
        TempFileStream in_prev, in_curr;
        in_prev << prev;
        in_curr << curr;
        in_prev.flush();
        in_curr.flush();
        std::string diff_cmd =
                "diff --new-line-format='+%L' "
                "     --old-line-format='-%L' "
                "     --unchanged-line-format=' %L' ";
        return execStdOut(diff_cmd + in_prev.getFileName() + " " + in_curr.getFileName()).str();
    }

    /**
     * Generated a debug report section for a dot graph specification, with the given id and title.
     */
    static DebugReportSection getDotGraphSection(
            std::string id, std::string title, const std::string& dotSpec);

private:
    std::unique_ptr<AstTransformer> wrappedTransformer;

    bool transform(AstTranslationUnit& translationUnit) override;
};

}  // end of namespace souffle
