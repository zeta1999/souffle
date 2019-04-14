/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamExpressionLevel.cpp
 *
 * Implementation of RAM Expression Level Analysis
 *
 ***********************************************************************/

#include "RamExpressionLevel.h"
#include "RamVisitor.h"
#include <algorithm>

namespace souffle {

/** Get level of value (which for-loop of a query) */
int RamExpressionLevelAnalysis::getLevel(const RamExpression* value) const {
    // visitor
    class ValueLevelVisitor : public RamVisitor<int> {
    public:
        // number
        int visitNumber(const RamNumber& num) override {
            return -1;
        }

        // tuple element access
        int visitElementAccess(const RamElementAccess& elem) override {
            return elem.getIdentifier();
        }

        // auto increment
        int visitAutoIncrement(const RamAutoIncrement& increment) override {
            return -1;
        }

        // intrinsic functors
        int visitIntrinsicOperator(const RamIntrinsicOperator& op) override {
            int level = -1;
            for (const auto& arg : op.getArguments()) {
                if (arg != nullptr) {
                    level = std::max(level, visit(arg));
                }
            }
            return level;
        }

        // pack operator
        int visitPackRecord(const RamPackRecord& pack) override {
            int level = -1;
            for (const auto& arg : pack.getArguments()) {
                if (arg != nullptr) {
                    level = std::max(level, visit(arg));
                }
            }
            return level;
        }

        // argument
        int visitArgument(const RamArgument& arg) override {
            return -1;
        }

        // user defined operator
        int visitUserDefinedOperator(const RamUserDefinedOperator& op) override {
            int level = -1;
            for (const auto& arg : op.getArguments()) {
                if (arg != nullptr) {
                    level = std::max(level, visit(arg));
                }
            }
            return level;
        }
    };
    return ValueLevelVisitor().visit(value);
}

}  // end of namespace souffle
