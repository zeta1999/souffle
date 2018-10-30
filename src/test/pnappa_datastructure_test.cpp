/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file pnappa_datastructure_test.cpp
 *
 * A test case testing the miscellaneous auxilliary data structures that pnappa introduced
 *
 ***********************************************************************/


#include "test.h"

#include <algorithm>
#include <iomanip>
#include <iostream>
#include <set>
#include <thread>
#include <utility>
#include <vector>

#ifdef _OPENMP
#include <omp.h>
#endif

#include "PiggyList.h"
#include "BlockList.h"
#include "UnionFind.h"
#include "DisjointSet.h"

namespace souffle {
namespace test {

/** TODO: BlockList - XXX: not sure if I'm deprecating it... I think piggylist is probably superior to it, plus good to remove old code **/
    // TODO: well... all these tests
    // XXX: what about for different CTORS (i.e. different starting container sizes?)

/** Piggy List that allows creation at arbitrary elements **/
TEST(RandomInsertPiggyTest, Scoping) {
    // simply test that namespaces were setup correctly (compile time)
    souffle::RandomInsertPiggyList<size_t> pl;
}

TEST(RandomInsertPiggyTest, Insertion) {
    EXPECT_FALSE(true);

    // insert a bunch of stuff and check the size is valid?
    souffle::RandomInsertPiggyList<size_t> pl;
    EXPECT_EQ(pl.size(), 0);

    pl.insertAt(0, 99);
    EXPECT_EQ(pl.get(0), 99);
    EXPECT_EQ(pl.size(), 1);

    pl.insertAt(1000, 33);
    EXPECT_EQ(pl.get(1000), 33);
    EXPECT_EQ(pl.size(), 2);

    // double insert
    pl.insertAt(1000, 33);
    EXPECT_EQ(pl.get(1000), 33);
    EXPECT_EQ(pl.size(), 2);

}

TEST(RandomInsertPiggyTest, InsertionExpansion) {
    EXPECT_FALSE(true);
    // insert a bunch of stuff and check whether it expanded properly, idk?
}

TEST(RandomInsertPiggyTest, Iteration) {
    EXPECT_FALSE(true);
}

TEST(RandomInsertPiggyTest, CopyCtor) {
    EXPECT_FALSE(true);
}

TEST(RandomInsertPiggyTest, MoveCtor) {
    EXPECT_FALSE(true);
}

TEST(RandomInsertPiggyTest, CopyAssign) {
    EXPECT_FALSE(true);
}

TEST(RandomInsertPiggyTest, MoveAssign) {
    EXPECT_FALSE(true);
}

TEST(RandomInsertPiggyTest, ParallelInsert) {
    EXPECT_FALSE(true);
}




/** Regular Old Piggy List **/
TEST(PiggyTest, Scoping) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, Append) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, ElementCreation) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, Iteration) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, CopyCtor) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, MoveCtor) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, CopyAssign) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, MoveAssign) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, ParallelElementSpawning) {
    EXPECT_FALSE(true);
}

TEST(PiggyTest, ParallelAppend) {
    EXPECT_FALSE(true);
}





/** The underlying Disjoint Set (essentially Anderson '91 Find-Union, but dynamic) **/
TEST(DjTest, Scoping) {
    EXPECT_FALSE(true);
}

TEST(DjTest, MakeNode) {
    EXPECT_FALSE(true);
}

TEST(DjTest, TestUnion) {
    // check whether the unioning works to see if the elements are properly in the same set
    EXPECT_FALSE(true);
}

TEST(DjTest, Iteration) {
    EXPECT_FALSE(true);
}

TEST(DjTest, CopyCtor) {
    EXPECT_FALSE(true);
}

TEST(DjTest, MoveCtor) {
    EXPECT_FALSE(true);
}

TEST(DjTest, CopyAssign) {
    EXPECT_FALSE(true);
}

TEST(DjTest, MoveAssign) {
    EXPECT_FALSE(true);
}

TEST(DjTest, ParallelScaling) {
    // insert, union, and stuff in parallel, then check things are in the valid sets
    EXPECT_FALSE(true);
}



/** The SparseDisjointSet that is used by the BinaryRelation **/
TEST(SparseDjTest, Scoping) {
    EXPECT_FALSE(true);
}

TEST(SparseDjTest, MakeNode) {
    EXPECT_FALSE(true);
}

TEST(SparseDjTest, TestUnion) {
    // check whether the unioning works to see if the elements are properly in the same set
    EXPECT_FALSE(true);
}

TEST(SparseDjTest, Iteration) {
    EXPECT_FALSE(true);
}

TEST(SparseDjTest, CopyCtor) {
    EXPECT_FALSE(true);
}

TEST(SparseDjTest, MoveCtor) {
    EXPECT_FALSE(true);
}

TEST(SparseDjTest, CopyAssign) {
    EXPECT_FALSE(true);
}

TEST(SparseDjTest, MoveAssign) {
    EXPECT_FALSE(true);
}

TEST(SparseDjTest, ParallelScaling) {
    // insert, union, and stuff in parallel, then check things are in the valid sets
    EXPECT_FALSE(true);
}



/** The LambdaBTree - essentially a ripoff to the Btree, but allows a function to be called on successful insert. I just am gonna test a subset of it, because I can argue that BTree already tests the basic stuff **/
TEST(LambdaBTreeTest, Scoping) {
    EXPECT_FALSE(true);
}

TEST(LambdaBTreeTest, Insert) {
    // insert some stuff and make sure the sideeffects are correct (as observed within the lambda), and also that the elements are contained
    EXPECT_FALSE(true);
}

TEST(LambdaBTreeTest, ParallelInsert) {
    // check whether doing the above works, but in parallel
    EXPECT_FALSE(true);
}


}  // namespace test
}  // namespace souffle
