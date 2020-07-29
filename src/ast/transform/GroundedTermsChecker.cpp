/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file SemanticChecker.cpp
 *
 * Implementation of the grounded terms checker pass.
 *
 ***********************************************************************/

#include "ast/transform/GroundedTermsChecker.h"
#include "AggregateOp.h"
#include "BinaryConstraintOps.h"
#include "ErrorReport.h"
#include "FunctorOps.h"
#include "Global.h"
#include "GraphUtils.h"
#include "RamTypes.h"
#include "RelationTag.h"
#include "SrcLocation.h"
#include "ast/Abstract.h"
#include "ast/Argument.h"
#include "ast/Attribute.h"
#include "ast/Clause.h"
#include "ast/IO.h"
#include "ast/Literal.h"
#include "ast/Node.h"
#include "ast/Program.h"
#include "ast/QualifiedName.h"
#include "ast/Relation.h"
#include "ast/TranslationUnit.h"
#include "ast/Type.h"
#include "ast/TypeSystem.h"
#include "ast/Utils.h"
#include "ast/Visitor.h"
#include "ast/analysis/Ground.h"
#include "ast/analysis/IOType.h"
#include "ast/analysis/PrecedenceGraph.h"
#include "ast/analysis/RecursiveClauses.h"
#include "ast/analysis/RelationSchedule.h"
#include "ast/analysis/SCCGraph.h"
#include "ast/analysis/Type.h"
#include "ast/analysis/TypeEnvironment.h"
#include "utility/ContainerUtil.h"
#include "utility/FunctionalUtil.h"
#include "utility/MiscUtil.h"
#include "utility/StreamUtil.h"
#include "utility/StringUtil.h"
#include "utility/tinyformat.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <functional>
#include <iostream>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <typeinfo>
#include <utility>
#include <vector>

namespace souffle {

void GroundedTermsChecker::verify(AstTranslationUnit& translationUnit) {
    auto&& program = *translationUnit.getProgram();
    auto&& report = translationUnit.getErrorReport();

    // -- check grounded variables and records --
    visitDepthFirst(program.getClauses(), [&](const AstClause& clause) {
        if (isFact(clause)) return;  // only interested in rules

        auto isGrounded = getGroundedTerms(translationUnit, clause);

        std::set<std::string> reportedVars;
        // all terms in head need to be grounded
        for (auto&& cur : getVariables(clause)) {
            if (!isGrounded[cur] && reportedVars.insert(cur->getName()).second) {
                report.addError("Ungrounded variable " + cur->getName(), cur->getSrcLoc());
            }
        }

        // all records need to be grounded
        for (auto&& cur : getRecords(clause)) {
            if (!isGrounded[cur]) {
                report.addError("Ungrounded record", cur->getSrcLoc());
            }
        }
    });
}

}  // end of namespace souffle
