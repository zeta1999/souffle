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

#include <map>
#include <string>
#include <vector>

namespace souffle {

namespace test {

TEST(IO_stdout, InterpretorStore) {
    Global::config().set("jobs", "1");

    std::vector<std::unique_ptr<RamRelation>> rels;

    std::vector<std::string> attribs = {"a", "b"};
    std::vector<std::string> types = {"i", "i"};
    std::unique_ptr<RamRelation> myrel =
            std::make_unique<RamRelation>("test", 2, 0, attribs, types, RelationRepresentation::BTREE);
    std::unique_ptr<RamRelationReference> ref1 = std::make_unique<RamRelationReference>(myrel.get());
    std::unique_ptr<RamRelationReference> ref2 = std::make_unique<RamRelationReference>(myrel.get());

    std::map<std::string, std::string> dirs = {
            {"IO", "stdout"}, {"attributeNames", "x\ty"}, {"name", "test"}};
    std::vector<IODirectives> ioDirs;
    ioDirs.push_back(IODirectives(dirs));

    std::vector<std::unique_ptr<RamExpression>> exprs;
    exprs.push_back(std::make_unique<RamNumber>(1));
    exprs.push_back(std::make_unique<RamNumber>(2));
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
1	2
===============
)";

    EXPECT_EQ(expected, sout.str());
}

}  // end namespace test
}  // end namespace souffle
