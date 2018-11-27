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
 * PiggyList, RandomInsertPiggyList, DisjointSet, SparseDisjointSet, and LambdaBTree
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
#include "UnionFind.h"

namespace souffle {
namespace test {

/** TODO: BlockList - XXX: not sure if I'm deprecating it... I think piggylist is probably superior to it, plus good to remove old code **/
    /** plus! blocklist's copy and move ctors were the problem in the first place... because StatesList is a blocklist iirc **/
    // TODO: well... all these tests
    // XXX: what about for different CTORS (i.e. different starting container sizes?)

/** Piggy List that allows creation at arbitrary elements **/
TEST(RandomInsertPiggyTest, Scoping) {
    // simply test that namespaces were setup correctly (compile time)
    souffle::RandomInsertPiggyList<size_t> pl;
}

TEST(RandomInsertPiggyTest, Insertion) {
    // insert a bunch of stuff and check the size is valid?
    souffle::RandomInsertPiggyList<size_t> pl;
    EXPECT_EQ(pl.size(), 0);

    pl.insertAt(0, 99);
    EXPECT_EQ(pl.get(0), 99);
    EXPECT_EQ(pl.size(), 1);

    pl.insertAt(1000, 33);
    EXPECT_EQ(pl.get(1000), 33);
    EXPECT_EQ(pl.size(), 2);
}

// XXX: fuck, i've deleted the copy and move ctors lol
TEST(RandomInsertPiggyTest, CopyCtor) {
    souffle::RandomInsertPiggyList<size_t> pl;
    pl.insertAt(1000, 33);
    pl.insertAt(0, 32);
    pl.insertAt(998, 31);
    pl.insertAt(997, 30);
    pl.insertAt(996, 29);

    souffle::RandomInsertPiggyList<size_t> pl2(pl);
    EXPECT_EQ(pl2.size(), pl.size());
    EXPECT_EQ(pl2.get(1000), 33);
    EXPECT_EQ(pl2.get(0), 32);
    EXPECT_EQ(pl2.get(998), 31);
    EXPECT_EQ(pl2.get(997), 30);
    EXPECT_EQ(pl2.get(996), 29);

    // change vice versa
    pl2.get(996) = 1;
    EXPECT_EQ(pl2.get(996), 1);
    // check it hasn't changed in pl1
    EXPECT_EQ(pl.get(996), 29);

    // and the reverse...
    pl.get(998) = 22;
    EXPECT_EQ(pl.get(998), 22);
    EXPECT_EQ(pl2.get(998), 31);
}

TEST(RandomInsertPiggyTest, DoubleClear) {
    // err.. prior versions have had the bug where clear caused double-free errors (as we don't set the container to null)
    // This should *crash* the test-suite if this is the case, but I also try to check the contents here
    souffle::RandomInsertPiggyList<size_t> pl;
    pl.insertAt(0, 3213);
    pl.insertAt(1, 3213);
    pl.insertAt(2, 3213);
    pl.insertAt(3, 3213);
    EXPECT_EQ(pl.size(), 4);
    pl.clear();
    EXPECT_EQ(pl.size(), 0);
    pl.clear();
    EXPECT_EQ(pl.size(), 0);
    pl.insertAt(0, 1);
    EXPECT_EQ(pl.size(), 1);
}

#ifdef _OPENMP
TEST(RandomInsertPiggyTest, ParallelInsert) {
    constexpr size_t limit = 10000;
    souffle::RandomInsertPiggyList<size_t> pl;

    // insert in parallel (no element should be overridden, because this breaks the datastructure)
    #pragma omp parallel for
    for (size_t i = 0; i < limit; ++i) {
        pl.insertAt(i, i);    
    }

    EXPECT_EQ(limit, pl.size());
    for (size_t i = 0; i < limit; ++i) {
        EXPECT_EQ(pl.get(i), i);
    }
}
#endif 



/** Regular Old Piggy List **/
TEST(PiggyTest, Scoping) {
    // simply test that namespaces were setup correctly (compile time)
    souffle::PiggyList<size_t> pl;
}

TEST(PiggyTest, Append) {
    souffle::PiggyList<size_t> pl;
    EXPECT_EQ(pl.size(), 0);
    pl.append(99);
    EXPECT_EQ(pl.size(), 1);
    pl.append(99);
    pl.append(99);
    EXPECT_EQ(pl.size(), 3);
    EXPECT_EQ(pl.get(0), 99);
    EXPECT_EQ(pl.get(1), 99);
    EXPECT_EQ(pl.get(2), 99);

    // larger than BLOCKSIZE
    constexpr size_t N = 10000;

    for (size_t i = 0; i < N; ++i) pl.append(2);
    EXPECT_EQ(pl.size(), N+3);
    // make sure that all of our inserties are exacties
    for (size_t i = 3; i < N+3; ++i) EXPECT_EQ(pl.get(i), 2);
}

TEST(PiggyTest, ElementCreation) {
    // test that we can use makeNode() and set the values properly
    souffle::PiggyList<size_t> pl;
    pl.get(pl.createNode()) = 99;
    EXPECT_EQ(pl.size(), 1);
    EXPECT_EQ(pl.get(0), 99);
}

TEST(PiggyTest, Iteration) {
    souffle::PiggyList<size_t> pl;
    constexpr size_t N = 10000;
    for (size_t i = 0; i < N; ++i) {
        pl.append(i);
    }
    size_t counter = 0;
    for (size_t e : pl) {
        EXPECT_EQ(e, counter++);
    }
}

TEST(PiggyTest, CopyCtor) {
    souffle::PiggyList<size_t> pl;
    constexpr size_t N = 10000;
    for (size_t i = 0; i < N; ++i) {
        pl.append(i);
    }

    souffle::PiggyList<size_t> pl2(pl);
    EXPECT_EQ(pl.size(), pl2.size());

    // check every element is equal and same order
    for (size_t i = 0; i < N; ++i) {
        EXPECT_EQ(pl.get(i), pl2.get(i));
        EXPECT_EQ(pl.get(i), i);
    }

    // TODO: check iterators still work in both
    auto pl1It = pl.begin();
    auto pl2It = pl2.begin();

    while (true) {
        // yep, they both finished at the same time
        if (pl1It == pl.end() && pl2It == pl2.end()) { break; }
        
        // uhoh, they didn't both finish at the same time
        if (pl1It == pl.end() || pl2It == pl2.end()) { EXPECT_FALSE(true && "whoops, looks like the iterators are broken"); }

        EXPECT_EQ(*pl1It, *pl2It);

        ++pl1It;
        ++pl2It;
    }
    
    // change contents of pl1 and makesure they don't change in pl2
    pl.get(2) = 99;
    EXPECT_EQ(pl.get(2), 99);
    EXPECT_EQ(pl2.get(2), 2);

    // check clearing one doesn't invalidate the other
    pl.clear();
    EXPECT_EQ(pl.size(), 0);
    EXPECT_EQ(pl2.size(), N);
    pl2.clear();
    EXPECT_EQ(pl.size(), 0);
    EXPECT_EQ(pl2.size(), 0);
}

TEST(PiggyTest, DoubleClear) {
    // err.. prior versions have had the bug where clear caused double-free errors (as we don't set the container to null)
    // This should *crash* the test-suite if this is the case, but I also try to check the contents here
    souffle::PiggyList<size_t> pl;
    pl.append(3213);
    pl.append(3213);
    pl.append(3213);
    pl.append(3213);
    EXPECT_EQ(pl.size(), 4);
    pl.clear();
    EXPECT_EQ(pl.size(), 0);
    pl.clear();
    EXPECT_EQ(pl.size(), 0);
    pl.append(1);
    EXPECT_EQ(pl.size(), 1);
}

#ifdef _OPENMP
TEST(PiggyTest, ParallelElementSpawning) {
    constexpr size_t N = 10000;
    souffle::PiggyList<size_t> pl;

    #pragma omp parallel for
    for (size_t i = 0; i < N; ++i) {
        size_t pos = pl.createNode();
        pl.get(pos) = pos;
    }
    EXPECT_EQ(N, pl.size());

    for (size_t i = 0; i < N; ++i) {
        EXPECT_EQ(i, pl.get(i));
    }
}

TEST(PiggyTest, ParallelAppend) {
    constexpr size_t N = 10000;
    souffle::PiggyList<size_t> pl;

    #pragma omp parallel for
    for (size_t i = 0; i < N; ++i) {
        pl.append(i);
    }
    EXPECT_EQ(N, pl.size());

    std::set<size_t> verifier;
    for (size_t i = 0; i < N; ++i) {
        verifier.emplace(pl.get(i));
    }
    // check there aren't any duplicate elements
    EXPECT_EQ(verifier.size(), N);
    // check every element within the inserted range exists
    for (size_t e : verifier) {
        EXPECT_TRUE(e >= 0 && e < N);
    }
}

#endif // ifdef _OPENMP




/** The underlying Disjoint Set (essentially Anderson '91 Find-Union, but dynamic) **/
TEST(DjTest, Scoping) {
    souffle::DisjointSet ds;
}

TEST(DjTest, MakeNode) {
    souffle::DisjointSet ds;
    EXPECT_EQ(ds.size(), 0);
    souffle::block_t n = ds.makeNode();
    // parent should be itself
    EXPECT_EQ(DisjointSet::b2p(n), 0);
    // rank should be 0
    EXPECT_EQ(DisjointSet::b2r(n), 0);
    EXPECT_EQ(ds.size(), 1);

    souffle::block_t n2 = ds.makeNode();
    // parent should be itself
    EXPECT_EQ(DisjointSet::b2p(n2), 1);
    // rank should be 0
    EXPECT_EQ(DisjointSet::b2r(n2), 0);
    EXPECT_EQ(ds.size(), 2);
}

TEST(DjTest, TestUnion) {
    // check whether the unioning works to see if the elements are properly in the same set
    souffle::DisjointSet ds;

    parent_t n1 = DisjointSet::b2p(ds.makeNode());
    parent_t n2 = DisjointSet::b2p(ds.makeNode());
    parent_t n3 = DisjointSet::b2p(ds.makeNode());
    parent_t n4 = DisjointSet::b2p(ds.makeNode());
    parent_t n5 = DisjointSet::b2p(ds.makeNode());
    EXPECT_EQ(ds.size(), 5);

    // test running same set doesn't accidentally union them 
    EXPECT_FALSE(ds.sameSet(n1, n2));
    EXPECT_FALSE(ds.sameSet(n1, n2));
    EXPECT_FALSE(ds.sameSet(n1, n2));
    EXPECT_FALSE(ds.sameSet(n1, n2));


    ds.unionNodes(n1, n2);
    EXPECT_TRUE(ds.sameSet(n1, n2));
    EXPECT_FALSE(ds.sameSet(n3, n2));

    ds.unionNodes(n3, n4);
    ds.unionNodes(n3, n5);
    ds.unionNodes(n1, n5);

    // ensure that nodes that weren't explicitly connected are in fact connected
    EXPECT_TRUE(ds.sameSet(n1, n4));
    // and their symmetric
    EXPECT_TRUE(ds.sameSet(n4, n1));

    // size shouldn't change
    EXPECT_EQ(ds.size(), 5);
}

TEST(DjTest, CopyCtor) {
    // insert a bunch into the disjoint set, then make a another one, and ensure that changes don't cross contaminate
    souffle::DisjointSet ds;

    constexpr size_t N = 10000;
    std::vector<block_t> verifier;
    for (size_t i = 0; i < N; ++i) {
        verifier.push_back(ds.makeNode());
    }

    souffle::DisjointSet ds2(ds);
    // all elements copied?
    EXPECT_EQ(ds2.size(), N);
    // original one wasn't impacted
    EXPECT_EQ(ds.size(), N);

    // union and check same set settings
    EXPECT_FALSE(ds.sameSet(0, 1));
    EXPECT_FALSE(ds2.sameSet(0, 1));

    ds.unionNodes(0,1);
    EXPECT_TRUE(ds.sameSet(0, 1));
    EXPECT_FALSE(ds2.sameSet(0, 1));

    ds2.unionNodes(2, 3);
    EXPECT_FALSE(ds.sameSet(2, 3));
    EXPECT_TRUE(ds2.sameSet(2, 3));

    EXPECT_EQ(ds2.size(), ds.size());

    ds.makeNode();
    EXPECT_EQ(ds.size(), N+1);
    EXPECT_EQ(ds2.size(), N);

    ds2.makeNode();
    ds2.makeNode();
    EXPECT_EQ(ds2.size(), N+2);
    EXPECT_EQ(ds.size(), N+1);

    // make a djset that will dtor before the original
    {
        souffle::DisjointSet ds3(ds2);
        ds3.makeNode();
        ds3.unionNodes(5, 6);
        EXPECT_EQ(ds3.size(), N+3);
    }

    EXPECT_EQ(ds2.size(), N+2);
    EXPECT_FALSE(ds2.sameSet(5,6));
}

TEST(DjTest, Clear) {
    souffle::DisjointSet ds;

    ds.clear();
    EXPECT_EQ(ds.size(), 0);
    
    // get ready 2 double free y'all
    for (size_t i = 0; i < 10000; ++i) ds.makeNode();
    ds.unionNodes(1,2);
    ds.clear();
    ds.clear();
}

#ifdef _OPENMP
TEST(DjTest, ParallelScaling) {
    // insert, union, and stuff in parallel, then check things are in the valid sets

    souffle::DisjointSet ds;
    constexpr size_t N = 10000;

    #pragma omp parallel for
    for (size_t i = 0; i < N; ++i) {
        ds.makeNode();        
    }
    // check we didn't miss any
    EXPECT_EQ(ds.size(), N);

    // union everything
    #pragma omp parallel for
    for (size_t i = 0; i < N-1; ++i) {
        ds.unionNodes(i, i+1);
    }
    EXPECT_EQ(ds.size(), N);

    // iterate through and check that the findNode is always the same
    parent_t rep = ds.findNode(0);
    for (size_t i = 0; i < N; ++i) {
        EXPECT_EQ(rep, ds.findNode(i));
    }
}
#endif // ifdef _OPENMP





/** The SparseDisjointSet that is used by the BinaryRelation **/
TEST(SparseDjTest, Scoping) {
    souffle::SparseDisjointSet<size_t> sds;
}

TEST(SparseDjTest, MakeNode) {
    souffle::SparseDisjointSet<size_t> sds;
    EXPECT_EQ(sds.size(), 0);

    sds.makeNode(99);
    EXPECT_EQ(sds.size(), 1);

    // whilst this shouldn't happen in regular use, it *shouldn't* create a new node
    sds.makeNode(99);
    EXPECT_EQ(sds.size(), 1);

    constexpr size_t N = 10000;
    for (size_t i = 0; i < N; ++i) {
        sds.makeNode(i);
    }

    EXPECT_EQ(sds.size(), N);
}

TEST(SparseDjTest, TestUnion) {
    // check whether the unioning works to see if the elements are properly in the same set
    souffle::SparseDisjointSet<size_t> sds;

    sds.makeNode(20);
    sds.makeNode(32);

    EXPECT_FALSE(sds.contains(20, 32));
    sds.unionNodes(20, 32);
    EXPECT_TRUE(sds.contains(20, 32));

    EXPECT_EQ(sds.size(), 2);

    sds.makeNode(33);
    EXPECT_FALSE(sds.contains(20, 33));
    sds.unionNodes(32, 33);
    EXPECT_TRUE(sds.contains(20, 33));
    EXPECT_EQ(sds.size(), 3);
}

TEST(SparseDjTest, SignedData) {
    // test when the sparse dj set stores different signed-ness to the internally stored data
    souffle::SparseDisjointSet<ssize_t> sds;

    EXPECT_EQ(sds.size(), 0);
    sds.makeNode(9999);
    EXPECT_EQ(sds.size(), 1);
}

TEST(SparseDjTest, CopyCtor) {
    // test whether the copy ctor properly isolates them all
    souffle::SparseDisjointSet<size_t> sds;
    sds.makeNode(22);
    sds.makeNode(23);
    sds.makeNode(24);
    sds.makeNode(25);
    sds.unionNodes(23, 24);

    souffle::SparseDisjointSet<size_t> sds2(sds);
    EXPECT_TRUE(sds2.contains(23, 24));
    sds2.unionNodes(23, 25);
    EXPECT_TRUE(sds2.contains(23, 25));
    EXPECT_FALSE(sds.contains(23, 25));

    // check unioning a scoped djsset doesn't mess up others (on dtor)
    {
        souffle::SparseDisjointSet<size_t> sds3(sds);
        // somehow unioning sds2 might mess with sds3?    
        EXPECT_FALSE(sds3.contains(23, 25));
        sds3.unionNodes(25, 22);

        // check that unioning this doesn't affect the others
        EXPECT_TRUE(sds3.contains(22, 25));
        EXPECT_FALSE(sds2.contains(22, 25));
        EXPECT_FALSE(sds.contains(22, 25));
    }
    // check afterward everything still works
    EXPECT_FALSE(sds2.contains(22, 25));
    EXPECT_FALSE(sds.contains(22, 25));

    EXPECT_EQ(sds.size(), 4);
    EXPECT_EQ(sds2.size(), 4);
}

#ifdef _OPENMP
TEST(SparseDjTest, ParallelScaling) {
    // insert, union, and stuff in parallel, then check things are in the valid sets
    souffle::SparseDisjointSet<size_t> sds;
    constexpr size_t N = 10000;

#pragma omp parallel for
    for (size_t i = 0; i < N; ++i) {
        // make things which are relatively sparse (hence why we mul by 50)
        sds.makeNode(i*50);
    }

    EXPECT_EQ(sds.size(), N);
    // union everything
#pragma omp parallel for
    for (size_t i = 0 ; i < N-1; ++i) {
        sds.unionNodes(i*50, (i+1)*50);
    }

    EXPECT_EQ(sds.size(), N);
    for (size_t i = 0; i < N-1; ++i) {
        EXPECT_TRUE(sds.contains(i*50, (i+1)*50));
    }
    EXPECT_EQ(sds.size(), N);
}
#endif // ifdef _OPENMP

typedef std::pair<size_t, size_t> TestPair;
typedef souffle::LambdaBTreeSet<TestPair, std::function<TestPair::second_type(TestPair&)>, souffle::EqrelMapComparator<TestPair>> TestLambdaTree;

/** The LambdaBTree - essentially a ripoff to the Btree, but allows a function to be called on successful insert. I just am gonna test a subset of it, because I can argue that BTree already tests the basic stuff **/
TEST(LambdaBTreeTest, Scoping) {
    // lambda btree set current has some hard coded stuff h1
    TestLambdaTree t;
}

TEST(LambdaBTreeTest, Insert) {
    // insert some stuff and make sure the sideeffects are correct (as observed within the lambda), and also that the elements are contained

    // the ticket machine that sets the second argument (for now our functor will just postincrent to assign)
    std::atomic<size_t> assigner(0); 

    TestLambdaTree t;
    EXPECT_EQ(t.size(), 0);

    std::function<TestPair::second_type(TestPair&)> update_fn = [&](TestPair& tp) { 
        tp.second = assigner.fetch_add(1);
        return tp.second;
    };

    TestPair toInsert = {55, -1};
    // inserting the ticketed value
    EXPECT_EQ(t.insert(toInsert, update_fn), 0);

    EXPECT_EQ(t.size(), 1);

    // the second argument should be ignored because its unknown until you fetch it!
    EXPECT_TRUE(t.contains({55, 1329139123}));

    // post increment of 0 is zero.
    EXPECT_EQ(((*t.find({55, 3232})).second), 0);


    TestPair insert2 = {43, 22};
    // insert should also return the value
    EXPECT_EQ(t.insert(insert2, update_fn), 1);
    EXPECT_TRUE(t.contains({43, 1329139123}));
    // post increment now should equal 1
    EXPECT_EQ(((*t.find({43, 3232})).second), 1);
    EXPECT_EQ(t.size(), 2);

    // now lets insert something that already exists (the anterior is all that matters)
    TestPair alreadyExistsPair = {55, 12313123};
    EXPECT_EQ(t.insert(alreadyExistsPair, [&](TestPair& tp) {
               EXPECT_TRUE(false && "newly inserted function called for an element that already exists!");
               // some dummy value 
               return 3223;
               }), 0);

    EXPECT_EQ(t.size(), 2);
}

#ifdef _OPENMP
TEST(LambdaBTreeTest, ParallelInsert) {
    // check whether doing the above works, but in parallel
    // we must ensure that duplicate elements inserted in parallel stiiilll should only result in singly elements inserted

    constexpr size_t N = 10000;
    {
        // the ticket machine that sets the second argument (for now our functor will just postincrent to assign)
        std::atomic<size_t> assigner(0); 
        std::function<TestPair::second_type(TestPair&)> update_fn = [&](TestPair& tp) { 
            tp.second = assigner.fetch_add(1);
            return tp.second;
        };

        TestLambdaTree t;

#pragma omp parallel for
        for (size_t i = 0; i < N; ++i) {
            TestPair tp = {i, 123132};
            t.insert(tp, update_fn);
        }

        // assert that every number in [0, N) are covered in the posterior of the stored pairs
        std::set<TestPair::second_type> covering;

        // iterating through the tree should be in order
        size_t i = 0;
        for (auto p : t) {
            EXPECT_EQ(p.first, i++);
            covering.emplace(p.second);
        }
        // and have the right number of elements
        EXPECT_EQ(i, N);

        // assert that everything is unique (i.e. a set)
        EXPECT_EQ(covering.size(), N);
        for (size_t i = 0; i < N; ++i) {
            EXPECT_EQ(covering.count(i), 1);
        }
    }

    // now our second test is setting duplicates concurrently (we try and maximise the potentiality of duplication, by trying to make the threads make the same number at the same time)
    size_t num_threads = omp_get_max_threads();
    {
        // shadowing the N to make one that's trimmed by thread number (don't want things non-divisible by the thread count!)
        const size_t N2 = (N/num_threads)*num_threads;

        std::atomic<size_t> assigner(0); 
        std::function<TestPair::second_type(TestPair&)> update_fn = [&](TestPair& tp) { 
            tp.second = assigner.fetch_add(1);
            return tp.second;
        };


        TestLambdaTree t;
        #pragma omp parallel for 
        for (size_t i = 0; i < N2; ++i) {
            TestPair tp = {i/num_threads, 213812309};
            // by doing this, we pretty much make the same insertion num_thread times, for the next num_thread loops
            t.insert(tp, update_fn);
        }
    
        EXPECT_EQ(t.size(), N2/num_threads);
        // iterating through the tree should be in order
        size_t i = 0;
        for (auto p : t) {
            EXPECT_EQ(p.first, i++);
        }

        // we check the assigner atomic, and see if its not larger than the number of times it should have been called (N2/8) times.
        // a violation of this would occur when we try and call the functor twice for the number of threads
        EXPECT_EQ(assigner.load(), N2/num_threads);

        // unfortunately, the above test couuuuld fail if two were called for the same element, then skipped for another... so I go through and check anyway.
        std::vector<bool> verifier(N2/num_threads, false);
        for (auto p : t) {
            if (verifier.at(p.second) == true) {
                EXPECT_TRUE(false && "duplicate posteriors found within the lambdatree");
            }
            // set it to true unconditionally to indicate that we've seen a set value for this posterior.
            verifier.at(p.second) = true;
        }
    }
}
#endif // ifdef _OPENMP


}  // namespace test
}  // namespace souffle
