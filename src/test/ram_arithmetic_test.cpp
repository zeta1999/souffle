/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2020, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ram_arithmetic_test.cpp
 *
 * Tests arithmetic evaluation by the Interpreter.
 *
 ***********************************************************************/

#include "DebugReport.h"
#include "ErrorReport.h"
#include "InterpreterEngine.h"
#include "RamExpression.h"
#include "RamOperation.h"
#include "RamProgram.h"
#include "RamStatement.h"
#include "RamTranslationUnit.h"
#include "SymbolTable.h"

#include "test.h"

#include <map>
#include <random>
#include <string>
#include <vector>

namespace souffle::test {

#define TESTS_PER_OPERATION 20
#define MAGIC_GENERATOR_SEED 3  // seed to random number generator

/** Function to evaluate a single RamExpression. */
RamDomain evalExpression(std::unique_ptr<RamExpression> expression) {
    // Set up RamProgram and translation unit
    std::vector<std::unique_ptr<RamExpression>> returnValues;
    returnValues.emplace_back(std::move(expression));

    Global::config().set("jobs", "1");
    std::unique_ptr<RamStatement> query =
            std::make_unique<RamQuery>(std::make_unique<RamSubroutineReturnValue>(std::move(returnValues)));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    subs.insert(std::make_pair("test", std::move(query)));
    std::vector<std::unique_ptr<RamRelation>> rels;

    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::make_unique<RamSequence>(), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::string name("test");
    std::vector<RamDomain> ret;
    std::vector<bool> errs;

    interpreter->executeSubroutine(name, {}, ret, errs);

    return ret.at(0);
}

RamDomain evalUnary(FunctorOp functor, RamDomain arg) {
    std::vector<std::unique_ptr<RamExpression>> Args;
    Args.push_back(std::make_unique<RamNumber>(arg));

    std::unique_ptr<RamExpression> expression =
            std::make_unique<RamIntrinsicOperator>(functor, std::move(Args));

    return evalExpression(std::move(expression));
}

RamDomain evalBinary(FunctorOp functor, RamDomain arg1, RamDomain arg2) {
    std::vector<std::unique_ptr<RamExpression>> Args;
    Args.push_back(std::make_unique<RamNumber>(arg1));
    Args.push_back(std::make_unique<RamNumber>(arg2));

    std::unique_ptr<RamExpression> expression =
            std::make_unique<RamIntrinsicOperator>(functor, std::move(Args));

    return evalExpression(std::move(expression));
}

TEST(RamNumber, ArithmeticEvaluation) {
    RamDomain num = 42;
    std::unique_ptr<RamExpression> expression = std::make_unique<RamNumber>(num);
    RamDomain result = evalExpression(std::move(expression));
    EXPECT_EQ(result, num);
}

// TEST(RamNumber, SimpleAdd) {
//     std::vector<std::unique_ptr<RamExpression>> Args;
//     Args.push_back(std::make_unique<RamNumber>(1));
//     Args.push_back(std::make_unique<RamNumber>(1));

//     std::unique_ptr<RamExpression> expression =
//             std::make_unique<RamIntrinsicOperator>(FunctorOp::ADD, std::move(Args));

//     RamDomain result = evalExpression(std::move(expression));

//     EXPECT_EQ(result, 2);
// }

TEST(Unary, Neg) {
    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain randomNumber = dist(randomGenerator);
        EXPECT_EQ(evalUnary(FunctorOp::NEG, randomNumber), -randomNumber);
    }
}

TEST(Unary, FloatNeg) {
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_real_distribution<RamFloat> dist(-100.0, 100.0);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamFloat randomNumber = dist(randomGenerator);
        result = evalUnary(FunctorOp::FNEG, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamFloat>(result), -randomNumber);
    }
}

TEST(Unary, BinaryNot) {
    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain randomNumber = dist(randomGenerator);
        EXPECT_EQ(evalUnary(FunctorOp::BNOT, randomNumber), ~randomNumber);
    }
}

TEST(Unary, UnsignedBinaryNot) {
    FunctorOp func = FunctorOp::UBNOT;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned randomNumber = dist(randomGenerator);
        result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), ~randomNumber);
    }
}

TEST(Unary, LogicalNeg) {
    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain randomNumber = dist(randomGenerator);
        EXPECT_EQ(evalUnary(FunctorOp::LNOT, randomNumber), !randomNumber);
    }
}

TEST(Unary, UnsignedLogicalNeg) {
    FunctorOp func = FunctorOp::ULNOT;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned randomNumber = dist(randomGenerator);
        result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), !randomNumber);
    }
}

// TEST(Unary, SingedTpUnsigned) {
//     FunctorOp func = FunctorOp::ULNOT;
//     RamDomain result;

//     std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
//     std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

//     for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
//         RamUnsigned randomNumber = dist(randomGenerator);
//         result = evalUnary(func, ramBitCast(randomNumber));
//         EXPECT_EQ(ramBitCast<RamUnsigned>(result), !randomNumber);
//     }
// }

TEST(Binary, ADD) {
    FunctorOp func = FunctorOp::ADD;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 + arg2);
    }
}

}  // namespace souffle::test
