/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamConstValue.cpp
 *
 * Implementation of RAM Constant Value Analysis
 *
 ***********************************************************************/

#include "RamConstValue.h"
#include "RamTranslationUnit.h"
#include "RamVisitor.h"

namespace souffle {

/** run constant value analysis */
void RamConstValueAnalysis::run(const RamTranslationUnit& translationUnit) {
    class ConstValueVisitor : public RamVisitor<bool> {
        std::set<const RamValue*>& constRamValues;

    public:
        ConstValueVisitor(std::set<const RamValue*>& cv) : constRamValues(cv) {}

        // number
        bool visitNumber(const RamNumber& num) override {
            constRamValues.insert(&num);
            return true;
        }

        // tuple element access
        bool visitElementAccess(const RamElementAccess& elem) override {
            return false;
        }

        // auto increment
        bool visitAutoIncrement(const RamAutoIncrement& increment) override {
            return false;
        }

        // intrinsic functors
        bool visitIntrinsicOperator(const RamIntrinsicOperator& op) override {
            const auto& args = op.getArguments();
            bool isConst = true;
            for (const auto arg : args) {
                isConst = isConst && visit(arg);
            }
            // add intrinsic to constant value set if all arguments are constant
            if (isConst) {
                constRamValues.insert(&op);
            }
            return isConst;
        }

        // pack operator
        bool visitPack(const RamPack& pack) override {
            // perhaps consider constant if all arguments are constant
            return false;
        }

        // argument
        bool visitArgument(const RamArgument& arg) override {
            return false;
        }

        // user defined operator
        bool visitUserDefinedOperator(const RamUserDefinedOperator& op) override {
            const auto& args = op.getArguments();
            bool isConst = true;
            for (const auto arg : args) {
                isConst = isConst && visit(arg);
            }
            if (isConst) {
                constRamValues.insert(&op);
            }
            return isConst;
        }
    };
    ConstValueVisitor cvv(constRamValues);
    cvv.visit(translationUnit.getProgram());
}

/** print the analysis result in HTML format */
void RamConstValueAnalysis::print(std::ostream& os) const {
    os << "Constant RAM values:" << std::endl;
    for (auto value : constRamValues) {
        os << *value << std::endl;
    }
}

}  // end of namespace souffle
