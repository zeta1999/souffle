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
#include "RamRelation.h"
#include "RamStatement.h"
#include "RamTranslationUnit.h"
#include "SymbolTable.h"

#include "test.h"
#include "test_util.h"

#include <fstream>
#include <map>
#include <random>
#include <sstream>
#include <string>
#include <vector>

namespace souffle::test {
#define RANDOM_TESTS 12

// void fillTestFile(const std::string& filename, const std::string& content) {
//     std::ofstream file;
//     file.open(filename);
//     file << content;
//     file.close();
// }

TEST(IO_store, IntepreterStoreFloatSimple) {
    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> types = {"f", "f"};
    std::unique_ptr<RamRelation> myrel =
            std::make_unique<RamRelation>("test", 2, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> dirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}};
    std::vector<IODirectives> ioDirs;
    ioDirs.push_back(IODirectives(dirs));

    std::vector<std::unique_ptr<RamExpression>> exprs;
    exprs.push_back(std::make_unique<RamNumber>(ramBitCast(static_cast<RamFloat>(0.5))));
    exprs.push_back(std::make_unique<RamNumber>(ramBitCast(static_cast<RamFloat>(0.5))));
    std::unique_ptr<RamStatement> main = std::make_unique<RamSequence>(
            std::make_unique<RamQuery>(std::make_unique<RamProject>(std::move(ref1), std::move(exprs))),
            std::make_unique<RamStore>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
0.5	0.5
===============
)";

    EXPECT_EQ(expected, sout.str());
}

TEST(IO_store, InterpretorStoreSigned) {
    std::vector<RamDomain> randomNumbers = generateRandomVector<RamDomain>(RANDOM_TESTS);

    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    // a0 a1 a2...
    std::vector<std::string> attribs(RANDOM_TESTS, "a");
    for (size_t i = 0; i < RANDOM_TESTS; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> types(RANDOM_TESTS, "i");

    std::unique_ptr<RamRelation> myrel = std::make_unique<RamRelation>(
            "test", RANDOM_TESTS, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> dirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}};
    std::vector<IODirectives> ioDirs;
    ioDirs.push_back(IODirectives(dirs));

    std::vector<std::unique_ptr<RamExpression>> exprs;
    for (RamDomain i : randomNumbers) {
        exprs.push_back(std::make_unique<RamNumber>(i));
    }

    std::unique_ptr<RamStatement> main = std::make_unique<RamSequence>(
            std::make_unique<RamQuery>(std::make_unique<RamProject>(std::move(ref1), std::move(exprs))),
            std::make_unique<RamStore>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_store, InterpretorStoreFloat) {
    std::vector<RamFloat> randomNumbers = generateRandomVector<RamFloat>(RANDOM_TESTS);

    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    // a0 a1 a2...
    std::vector<std::string> attribs(RANDOM_TESTS, "a");
    for (size_t i = 0; i < RANDOM_TESTS; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> types(RANDOM_TESTS, "f");

    std::unique_ptr<RamRelation> myrel = std::make_unique<RamRelation>(
            "test", RANDOM_TESTS, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> dirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}};
    std::vector<IODirectives> ioDirs;
    ioDirs.push_back(IODirectives(dirs));

    std::vector<std::unique_ptr<RamExpression>> exprs;
    for (RamFloat f : randomNumbers) {
        exprs.push_back(std::make_unique<RamNumber>(ramBitCast(f)));
    }

    std::unique_ptr<RamStatement> main = std::make_unique<RamSequence>(
            std::make_unique<RamQuery>(std::make_unique<RamProject>(std::move(ref1), std::move(exprs))),
            std::make_unique<RamStore>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_store, InterpretorStoreUnsigned) {
    std::vector<RamUnsigned> randomNumbers = generateRandomVector<RamUnsigned>(RANDOM_TESTS);

    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    // a0 a1 a2...
    std::vector<std::string> attribs(RANDOM_TESTS, "a");
    for (size_t i = 0; i < RANDOM_TESTS; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> types(RANDOM_TESTS, "u");

    std::unique_ptr<RamRelation> myrel = std::make_unique<RamRelation>(
            "test", RANDOM_TESTS, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> dirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}};
    std::vector<IODirectives> ioDirs;
    ioDirs.push_back(IODirectives(dirs));

    std::vector<std::unique_ptr<RamExpression>> exprs;
    for (RamUnsigned u : randomNumbers) {
        exprs.push_back(std::make_unique<RamNumber>(ramBitCast(u)));
    }

    std::unique_ptr<RamStatement> main = std::make_unique<RamSequence>(
            std::make_unique<RamQuery>(std::make_unique<RamProject>(std::move(ref1), std::move(exprs))),
            std::make_unique<RamStore>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << "\t" << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_store, IntepreterStoreSignedChangedDelimeter) {
    std::vector<RamDomain> randomNumbers = generateRandomVector<RamDomain>(RANDOM_TESTS);
    const std::string delimiter{", "};

    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    // a0 a1 a2...
    std::vector<std::string> attribs(RANDOM_TESTS, "a");
    for (size_t i = 0; i < RANDOM_TESTS; ++i) {
        attribs[i].append(std::to_string(i));
    }

    std::vector<std::string> types(RANDOM_TESTS, "i");

    std::unique_ptr<RamRelation> myrel = std::make_unique<RamRelation>(
            "test", RANDOM_TESTS, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> dirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"delimiter", delimiter}};
    std::vector<IODirectives> ioDirs;
    ioDirs.push_back(IODirectives(dirs));

    std::vector<std::unique_ptr<RamExpression>> exprs;
    for (RamDomain i : randomNumbers) {
        exprs.push_back(std::make_unique<RamNumber>(i));
    }

    std::unique_ptr<RamStatement> main = std::make_unique<RamSequence>(
            std::make_unique<RamQuery>(std::make_unique<RamProject>(std::move(ref1), std::move(exprs))),
            std::make_unique<RamStore>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << randomNumbers[0];

    for (size_t i = 1; i < randomNumbers.size(); ++i) {
        expected << delimiter << randomNumbers[i];
    }
    expected << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_store, MixTypesRelation) {
    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    std::vector<std::string> attribs{"t", "o", "s", "i", "a"};

    std::vector<std::string> types{"i", "u", "f", "f", "s"};

    std::unique_ptr<RamRelation> myrel =
            std::make_unique<RamRelation>("test", 5, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> dirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}};  // stdin
    std::vector<IODirectives> ioDirs;
    ioDirs.push_back(IODirectives(dirs));

    SymbolTable symbolTable;
    ErrorReport errReport;
    DebugReport debugReport;

    std::vector<std::unique_ptr<RamExpression>> exprs;
    exprs.push_back(std::make_unique<RamNumber>(3));
    exprs.push_back(std::make_unique<RamNumber>(ramBitCast(static_cast<RamUnsigned>(27))));
    exprs.push_back(std::make_unique<RamNumber>(ramBitCast(static_cast<RamFloat>(27.27))));
    exprs.push_back(std::make_unique<RamNumber>(ramBitCast(static_cast<RamFloat>(27.27))));
    exprs.push_back(std::make_unique<RamNumber>(symbolTable.lookup("meow")));

    std::unique_ptr<RamStatement> main = std::make_unique<RamSequence>(
            std::make_unique<RamQuery>(std::make_unique<RamProject>(std::move(ref1), std::move(exprs))),
            std::make_unique<RamStore>(std::move(ref2), ioDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    RamTranslationUnit translationUnit(std::move(prog), symbolTable, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::stringstream expected;
    expected << "---------------"
             << "\n"
             << "test"
             << "\n"
             << "==============="
             << "\n"
             << 3 << "\t" << 27 << "\t" << 27.27 << "\t" << 27.27 << "\t"
             << "meow"
             << "\n"
             << "==============="
             << "\n";

    EXPECT_EQ(expected.str(), sout.str());
}

TEST(IO_load, IntepreterLoadSigned) {
    const std::string filename{"test/ram_rel.test"};

    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> types = {"i", "i"};
    std::unique_ptr<RamRelation> myrel =
            std::make_unique<RamRelation>("test", 2, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> readDirs = {
            {"IO", "file"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"filename", filename}};
    std::vector<IODirectives> readIoDirs;
    readIoDirs.push_back(IODirectives(readDirs));

    std::map<std::string, std::string> writeDirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"filename", filename}};
    std::vector<IODirectives> writeIoDirs;
    writeIoDirs.push_back(IODirectives(writeDirs));

    std::unique_ptr<RamStatement> main =
            std::make_unique<RamSequence>(std::make_unique<RamLoad>(std::move(ref1), readIoDirs),
                    std::make_unique<RamStore>(std::move(ref2), writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
5	3
===============
)";
    EXPECT_EQ(expected, sout.str());
}

TEST(IO_load, IntepreterLoadFloat) {
    const std::string filename{"test/ram_rel_f.test"};

    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> types = {"f", "f"};
    std::unique_ptr<RamRelation> myrel =
            std::make_unique<RamRelation>("test", 2, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> readDirs = {
            {"IO", "file"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"filename", filename}};
    std::vector<IODirectives> readIoDirs;
    readIoDirs.push_back(IODirectives(readDirs));

    std::map<std::string, std::string> writeDirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}, {"filename", filename}};
    std::vector<IODirectives> writeIoDirs;
    writeIoDirs.push_back(IODirectives(writeDirs));

    std::unique_ptr<RamStatement> main =
            std::make_unique<RamSequence>(std::make_unique<RamLoad>(std::move(ref1), readIoDirs),
                    std::make_unique<RamStore>(std::move(ref2), writeIoDirs));

    rels.push_back(std::move(myrel));
    std::map<std::string, std::unique_ptr<RamStatement>> subs;
    std::unique_ptr<RamProgram> prog =
            std::make_unique<RamProgram>(std::move(rels), std::move(main), std::move(subs));

    SymbolTable symTab;
    ErrorReport errReport;
    DebugReport debugReport;

    RamTranslationUnit translationUnit(std::move(prog), symTab, errReport, debugReport);

    // configure and execute interpreter
    std::unique_ptr<InterpreterEngine> interpreter = std::make_unique<InterpreterEngine>(translationUnit);

    std::streambuf* oldCoutStreambuf = std::cout.rdbuf();
    std::ostringstream sout;
    std::cout.rdbuf(sout.rdbuf());

    interpreter->executeMain();

    std::cout.rdbuf(oldCoutStreambuf);

    std::string expected = R"(---------------
test
===============
0.5	0.5
===============
)";
    EXPECT_EQ(expected, sout.str());
}

}  // end namespace souffle::test
