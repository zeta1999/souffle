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
#include "test_util.h"

#include <cmath>
#include <map>
#include <random>
#include <string>
#include <vector>

namespace souffle::test {

#define TESTS_PER_OPERATION 20

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

RamDomain evalMultiArg(FunctorOp functor, std::vector<std::unique_ptr<RamExpression>> args) {
    std::unique_ptr<RamExpression> expression =
            std::make_unique<RamIntrinsicOperator>(functor, std::move(args));
    return evalExpression(std::move(expression));
}

/** Evaluate a single argument expression */
RamDomain evalUnary(FunctorOp functor, RamDomain arg1) {
    std::vector<std::unique_ptr<RamExpression>> args;
    args.push_back(std::make_unique<RamNumber>(arg1));

    return evalMultiArg(functor, std::move(args));
}

/** Evaluate a binary operator */
RamDomain evalBinary(FunctorOp functor, RamDomain arg1, RamDomain arg2) {
    std::vector<std::unique_ptr<RamExpression>> args;
    args.push_back(std::make_unique<RamNumber>(arg1));
    args.push_back(std::make_unique<RamNumber>(arg2));

    return evalMultiArg(functor, std::move(args));
}

TEST(RamNumber, ArithmeticEvaluation) {
    RamDomain num = 42;
    std::unique_ptr<RamExpression> expression = std::make_unique<RamNumber>(num);
    RamDomain result = evalExpression(std::move(expression));
    EXPECT_EQ(result, num);
}

TEST(Unary, Neg) {
    for (RamDomain randomNumber : generateRandomVector<RamDomain>(TESTS_PER_OPERATION)) {
        EXPECT_EQ(evalUnary(FunctorOp::NEG, randomNumber), -randomNumber);
    }
}

TEST(Unary, FloatNeg) {
    FunctorOp functor = FunctorOp::FNEG;

    for (auto randomNumber : generateRandomVector<RamFloat>(TESTS_PER_OPERATION)) {
        auto result = evalUnary(functor, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamFloat>(result), -randomNumber);
    }
}

TEST(Unary, BinaryNot) {
    FunctorOp functor = FunctorOp::BNOT;

    for (auto randomNumber : generateRandomVector<RamDomain>(TESTS_PER_OPERATION)) {
        EXPECT_EQ(evalUnary(functor, randomNumber), ~randomNumber);
    }
}

TEST(Unary, UnsignedBinaryNot) {
    FunctorOp func = FunctorOp::UBNOT;

    for (auto randomNumber : generateRandomVector<RamUnsigned>(TESTS_PER_OPERATION)) {
        RamDomain result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), ~randomNumber);
    }
}

TEST(Unary, LogicalNeg) {
    FunctorOp functor = FunctorOp::LNOT;

    for (auto randomNumber : generateRandomVector<RamDomain>(TESTS_PER_OPERATION)) {
        EXPECT_EQ(evalUnary(functor, randomNumber), !randomNumber);
    }
}

TEST(Unary, UnsignedLogicalNeg) {
    FunctorOp func = FunctorOp::ULNOT;

    for (auto randomNumber : generateRandomVector<RamUnsigned>(TESTS_PER_OPERATION)) {
        RamDomain result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), static_cast<RamUnsigned>(!randomNumber));
    }
}

TEST(Unary, SingedTpUnsigned) {
    FunctorOp func = FunctorOp::ITOU;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain randomNumber = dist(randomGenerator);
        result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), static_cast<RamUnsigned>(randomNumber));
    }
}

TEST(Unary, UnsignedToSigned) {
    FunctorOp func = FunctorOp::UTOI;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned randomNumber = dist(randomGenerator);
        result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(result, static_cast<RamDomain>(randomNumber));
    }
}

TEST(Unary, SignedToFloat) {
    FunctorOp func = FunctorOp::ITOF;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain randomNumber = dist(randomGenerator);
        result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamFloat>(result), static_cast<RamFloat>(randomNumber));
    }
}

TEST(Unary, FloatToSigned) {
    FunctorOp func = FunctorOp::FTOI;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_real_distribution<RamFloat> dist(-100.0, 100.0);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamFloat randomNumber = dist(randomGenerator);
        result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(result, static_cast<RamDomain>(randomNumber));
    }
}

TEST(Unary, UnsignedTFloat) {
    FunctorOp func = FunctorOp::UTOF;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned randomNumber = dist(randomGenerator);
        result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamFloat>(result), static_cast<RamFloat>(randomNumber));
    }
}

TEST(Unary, FloatToUnsigned) {
    FunctorOp func = FunctorOp::FTOU;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_real_distribution<RamFloat> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamFloat randomNumber = dist(randomGenerator);
        result = evalUnary(func, ramBitCast(randomNumber));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), static_cast<RamUnsigned>(randomNumber));
    }
}

TEST(Binary, SignedAdd) {
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

TEST(Binary, UnsignedAdd) {
    FunctorOp func = FunctorOp::UADD;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 + arg2);
    }
}

TEST(Binary, FloatAdd) {
    FunctorOp func = FunctorOp::FADD;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_real_distribution<RamFloat> dist(-100.0, 100.0);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamFloat arg1 = dist(randomGenerator);
        RamFloat arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamFloat>(result), arg1 + arg2);
    }
}

TEST(Binary, SignedSub) {
    FunctorOp func = FunctorOp::SUB;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 - arg2);
    }
}

TEST(Binary, UnsignedSub) {
    FunctorOp func = FunctorOp::USUB;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 - arg2);
    }
}

TEST(Binary, FloatSub) {
    FunctorOp func = FunctorOp::FSUB;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_real_distribution<RamFloat> dist(-100.0, 100.0);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamFloat arg1 = dist(randomGenerator);
        RamFloat arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamFloat>(result), arg1 - arg2);
    }
}

TEST(Binary, SignedMul) {
    FunctorOp func = FunctorOp::MUL;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 * arg2);
    }
}

TEST(Binary, UnsignedMul) {
    FunctorOp func = FunctorOp::UMUL;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 * arg2);
    }
}

TEST(Binary, FloatMul) {
    FunctorOp func = FunctorOp::FMUL;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_real_distribution<RamFloat> dist(-100.0, 100.0);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamFloat arg1 = dist(randomGenerator);
        RamFloat arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamFloat>(result), arg1 * arg2);
    }
}

TEST(Binary, SignedDiv) {
    FunctorOp func = FunctorOp::DIV;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        if (arg2 != 0) {
            result = evalBinary(func, arg1, arg2);
            EXPECT_EQ(result, arg1 / arg2);
        }
    }
}

TEST(Binary, UnsignedDiv) {
    FunctorOp func = FunctorOp::UDIV;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        if (arg2 != 0) {
            result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
            EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 / arg2);
        }
    }
}

TEST(Binary, FloatDiv) {
    FunctorOp func = FunctorOp::FDIV;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_real_distribution<RamFloat> dist(-100.0, 100.0);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamFloat arg1 = dist(randomGenerator);
        RamFloat arg2 = dist(randomGenerator);
        if (arg2 != 0) {
            result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
            EXPECT_EQ(ramBitCast<RamFloat>(result), arg1 / arg2);
        }
    }
}

TEST(Binary, SignedExp) {
    FunctorOp func = FunctorOp::EXP;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, static_cast<RamDomain>(std::pow(arg1, arg2)));
    }
}

TEST(Binary, UnsignedExp) {
    FunctorOp func = FunctorOp::UEXP;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), static_cast<RamUnsigned>(std::pow(arg1, arg2)));
    }
}

// This can produce NaN, which can't be compared - investigate for a better way.
// TEST(Binary, FloatExp) {
//     FunctorOp func = FunctorOp::FEXP;
//     RamDomain result;

//     std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
//     std::uniform_real_distribution<RamFloat> dist(-100.0, 100.0);

//     for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
//         RamFloat arg1 = dist(randomGenerator);
//         RamFloat arg2 = dist(randomGenerator);
//         std::feclearexcept(FE_ALL_EXCEPT);
//         errno = 0;
//         result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
//         EXPECT_EQ(ramBitCast<RamFloat>(result), static_cast<RamFloat>(std::pow(arg1, arg2)));
//     }
// }

TEST(Binary, SignedMod) {
    FunctorOp func = FunctorOp::MOD;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 % arg2);
    }
}

TEST(Binary, UnsignedMod) {
    FunctorOp func = FunctorOp::UMOD;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 % arg2);
    }
}

TEST(Binary, SignedBinaryAnd) {
    FunctorOp func = FunctorOp::BAND;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 & arg2);
    }
}

TEST(Binary, UnsignedBinaryAnd) {
    FunctorOp func = FunctorOp::UBAND;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 & arg2);
    }
}

TEST(Binary, SignedBinaryOr) {
    FunctorOp func = FunctorOp::BOR;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 | arg2);
    }
}

TEST(Binary, UnsignedBinaryOr) {
    FunctorOp func = FunctorOp::UBOR;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 | arg2);
    }
}

TEST(Binary, SignedBinaryXor) {
    FunctorOp func = FunctorOp::BXOR;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 ^ arg2);
    }
}

TEST(Binary, UnsignedBinaryXor) {
    FunctorOp func = FunctorOp::UBXOR;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 ^ arg2);
    }
}

TEST(Binary, SignedLogicalAnd) {
    FunctorOp func = FunctorOp::LAND;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 || arg2);
    }
}

TEST(Binary, UnsignedLogicalAnd) {
    FunctorOp func = FunctorOp::ULAND;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 || arg2);
    }
}

TEST(Binary, SignedLogicalOr) {
    FunctorOp func = FunctorOp::LOR;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamDomain> dist(-100, 100);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamDomain arg1 = dist(randomGenerator);
        RamDomain arg2 = dist(randomGenerator);
        result = evalBinary(func, arg1, arg2);
        EXPECT_EQ(result, arg1 || arg2);
    }
}

TEST(Binary, UnsignedLogicalOr) {
    FunctorOp func = FunctorOp::ULOR;
    RamDomain result;

    std::mt19937 randomGenerator(MAGIC_GENERATOR_SEED);
    std::uniform_int_distribution<RamUnsigned> dist(0, 1000);

    for (int i = 0; i < TESTS_PER_OPERATION; ++i) {
        RamUnsigned arg1 = dist(randomGenerator);
        RamUnsigned arg2 = dist(randomGenerator);
        result = evalBinary(func, ramBitCast(arg1), ramBitCast(arg2));
        EXPECT_EQ(ramBitCast<RamUnsigned>(result), arg1 || arg2);
    }
}

TEST(MultiArg, Max) {
    FunctorOp functor = FunctorOp::MAX;
    std::vector<std::unique_ptr<souffle::RamExpression>> args;
    RamDomain result;

    for (RamDomain i = 0; i <= 50; ++i) {
        args.push_back(std::make_unique<RamNumber>(i));
    }

    result = evalMultiArg(functor, std::move(args));

    EXPECT_EQ(result, 50);
}

TEST(MultiArg, UnsignedMax) {
    FunctorOp functor = FunctorOp::UMAX;
    std::vector<std::unique_ptr<souffle::RamExpression>> args;
    RamDomain result;

    for (RamUnsigned i = 0; i <= 100; ++i) {
        args.push_back(std::make_unique<RamNumber>(ramBitCast(i)));
    }

    result = evalMultiArg(functor, std::move(args));

    EXPECT_EQ(ramBitCast<RamUnsigned>(result), 100);
}

TEST(MultiArg, FloatMax) {
    FunctorOp functor = FunctorOp::FMAX;
    std::vector<std::unique_ptr<souffle::RamExpression>> args;
    RamDomain result;

    for (RamDomain i = -100; i <= 100; ++i) {
        args.push_back(std::make_unique<RamNumber>(ramBitCast(static_cast<RamFloat>(i))));
    }

    result = evalMultiArg(functor, std::move(args));

    EXPECT_EQ(ramBitCast<RamFloat>(result), static_cast<RamFloat>(100));
}

TEST(MultiArg, Min) {
    FunctorOp functor = FunctorOp::MIN;
    std::vector<std::unique_ptr<souffle::RamExpression>> args;
    RamDomain result;

    for (RamDomain i = 0; i <= 50; ++i) {
        args.push_back(std::make_unique<RamNumber>(i));
    }

    result = evalMultiArg(functor, std::move(args));

    EXPECT_EQ(result, 0);
}

TEST(MultiArg, UnsignedMin) {
    FunctorOp functor = FunctorOp::UMIN;
    std::vector<std::unique_ptr<souffle::RamExpression>> args;
    RamDomain result;

    for (RamUnsigned i = 0; i <= 100; ++i) {
        args.push_back(std::make_unique<RamNumber>(ramBitCast(i)));
    }

    result = evalMultiArg(functor, std::move(args));

    EXPECT_EQ(ramBitCast<RamUnsigned>(result), 0);
}

TEST(MultiArg, FloatMin) {
    FunctorOp functor = FunctorOp::FMIN;
    std::vector<std::unique_ptr<souffle::RamExpression>> args;
    RamDomain result;

    for (RamDomain i = -100; i <= 100; ++i) {
        args.push_back(std::make_unique<RamNumber>(ramBitCast(static_cast<RamFloat>(i))));
    }

    result = evalMultiArg(functor, std::move(args));

    EXPECT_EQ(ramBitCast<RamFloat>(result), static_cast<RamFloat>(-100));
}

}  // namespace souffle::test
