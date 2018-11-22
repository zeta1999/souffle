/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file binary_relation_test.cpp
 *
 * A test case testing the binary relation member functions
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

#include "BinaryRelation.h"
#include "CompiledRamTuple.h"

// TODO: replace findX tests with getBoundaries tests
// TODO: insert some anteriorIt, and antpostit tests
// TODO: add some extend tests

namespace souffle {
namespace test {

TEST(BinRelTest, Scoping) {
    // simply test that namespaces were setup correctly
    souffle::BinaryRelation<souffle::ram::Tuple<RamDomain, 2>> br;
}

typedef BinaryRelation<ram::Tuple<RamDomain, 2>> BinRel;

TEST(BinRelTest, Basic) {
    BinRel br;
    // empty bin rel should be exactly that
    EXPECT_EQ(br.size(), 0);
    EXPECT_FALSE(br.contains(1, 2));
    EXPECT_FALSE(br.contains(0, 0));

    // test implicit rules
    br.insert(1, 2);
    EXPECT_EQ(br.size(), 4);
    EXPECT_TRUE(br.contains(1, 2));
    EXPECT_TRUE(br.contains(2, 1));
    EXPECT_TRUE(br.contains(1, 1));
    EXPECT_TRUE(br.contains(2, 2));

    // test insert of exactly one pair
    br.insert(3, 3);
    EXPECT_EQ(br.size(), 5);
    EXPECT_TRUE(br.contains(3, 3));
    EXPECT_FALSE(br.contains(1, 3));
    EXPECT_FALSE(br.contains(3, 2));
    size_t count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br.size());
}

TEST(BinRelTest, Clear) {
    BinRel br;
    br.insert(0, 44);
    br.insert(0, 1);

    EXPECT_EQ(9, br.size());
    size_t count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br.size());
    br.clear();
    EXPECT_EQ(0, br.size());
    count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br.size());
}

TEST(BinRelTest, Duplicates) {
    BinRel br;
    // test inserting same pair
    for (int i = 0; i < 10; ++i) br.insert(0, 0);
    EXPECT_EQ(br.size(), 1);

    for (int i = 0; i < 10; ++i) EXPECT_TRUE(br.contains(0, 0));
    EXPECT_EQ(br.size(), 1);
    EXPECT_FALSE(br.contains(1, 1));

    // check iteration of duplicate is fine
    ram::Tuple<RamDomain, 2> tup;
    tup[0] = 0;
    tup[1] = 0;
    auto x = br.begin();
    EXPECT_EQ(tup, *x);
    ++x;
    EXPECT_EQ(x, br.end());
}

TEST(BinRelTest, TransitivityTest) {
    // test (a,b) && (b, c) => (a,c) etc
    BinRel br;
    br.insert(1, 2);
    br.insert(2, 3);
    EXPECT_EQ(br.size(), 9);
    size_t count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br.size());
    EXPECT_TRUE(br.contains(1, 1));
    EXPECT_TRUE(br.contains(1, 2));
    EXPECT_TRUE(br.contains(1, 3));
    EXPECT_TRUE(br.contains(2, 1));
    EXPECT_TRUE(br.contains(2, 2));
    EXPECT_TRUE(br.contains(2, 3));
    EXPECT_TRUE(br.contains(3, 1));
    EXPECT_TRUE(br.contains(3, 2));
    EXPECT_TRUE(br.contains(3, 3));
}

TEST(BinRelTest, PairwiseIncremental) {
    BinRel br;

    const size_t N = 100;
    // test inserting ascending pairs still isolates them
    for (size_t i = 1; i < N; ++i) {
        br.insert(i, i);
        EXPECT_TRUE(br.contains(i, i));
        br.insert(i + (N + 1), i);
        EXPECT_TRUE(br.contains(i, i + (N + 1)));
        EXPECT_TRUE(br.contains(i + (N + 1), i + (N + 1)));
        EXPECT_TRUE(br.contains(i + (N + 1), i));
    }
    EXPECT_EQ(br.size(), (N - 1) * 4);
    size_t count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br.size());
}

TEST(BinRelTest, PairwiseDecremental) {
    BinRel br;

    const size_t N = 100;
    // test inserting descending pairs still isolates them
    for (size_t i = N; i > 1; --i) {
        br.insert(i, i);
        EXPECT_TRUE(br.contains(i, i));
        br.insert(i + (N + 1), i);
        EXPECT_TRUE(br.contains(i, i + (N + 1)));
        EXPECT_TRUE(br.contains(i + (N + 1), i + (N + 1)));
        EXPECT_TRUE(br.contains(i + (N + 1), i));
    }

    EXPECT_EQ(br.size(), (N - 1) * 4);
    size_t count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br.size());
}

TEST(BinRelTest, Shuffled) {
    BinRel br;

    size_t N = 100;
    // test inserting data "out of order" keeps isolation
    std::vector<int> data;
    for (size_t i = 0; i < N; i++) {
        data.push_back(i);
    }
    std::random_shuffle(data.begin(), data.end());

    for (auto x : data) {
        br.insert(x, x);
    }

    for (size_t i = 0; i < N; ++i) {
        EXPECT_TRUE(br.contains(i, i));
    }

    EXPECT_EQ(br.size(), N);

    // always check the iterator for size too
    size_t count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br.size());
}

TEST(BinRelTest, Extend) {
    // TODO, test running extend for a relation
    BinRel br;
    
    br.insert(0,1);
    br.insert(0,2);
    br.insert(0,3);
    br.insert(0,4);
    br.insert(0,5);
    br.insert(0,6);

    br.insert(8,9);

    br.insert(44, 70);
    
    br.insert(11,11);
    EXPECT_EQ(br.size(), (7*7) + (2*2) + (2*2) + (1*1));

    BinRel br2;
    br2.insert(33, 8);
    br2.insert(33, 99);
    br2.insert(33, 0);

    br2.insert(69, 68);
    br2.insert(69, 70);

    br2.insert(101, 102);
    EXPECT_EQ(br2.size(), (4*4) + (3*3) + (2*2));

    br2.extend(br);
    // br2 now should contain djsets: {0,1,2,3,4,5,6,8,9,33,99}, {68,69,70,44}, {11}
    // djset {0..6} merged with {33,8,99,0}, and {8,9} should also merge with that djset
    // djset {44,70} should be merged with {68,69,70}
    // dj set is on its own {11}, but IS new knowledge! so it's in the extended domain
    // shouldn't contain 101,102! this is not new delta knowledge
    EXPECT_TRUE(br2.contains(0,0));
    EXPECT_TRUE(br2.contains(0,1));
    EXPECT_TRUE(br2.contains(1,8));
    EXPECT_TRUE(br2.contains(9,4));
    EXPECT_TRUE(br2.contains(33,4));
    EXPECT_TRUE(br2.contains(33,99));

    EXPECT_TRUE(br2.contains(68,69));
    EXPECT_TRUE(br2.contains(70,44));

    EXPECT_TRUE(br2.contains(11,11));

    EXPECT_FALSE(br2.contains(0,69));
    EXPECT_FALSE(br2.contains(101,102));

    // check this hasn't changed size
    EXPECT_EQ(br.size(), (7*7) + (2*2) + (2*2) + (1*1));
    // check that it was properly extended 
    EXPECT_EQ(br2.size(), (11*11)+(4*4)+(1*1));
}

TEST(BinRelTest, Copy) {
    // TODO: test copy ctor (or assign? idk)
}

// TODO: rewrite this to not use .find (it is deprecated)
////TEST(BinRelTest, Copy) {
//
////    // test =assign keeps copy independence
////    BinRel br;
////
////    int N = 100;
////
////    std::vector<int> data;
////    for (int i = 0; i < N; i++) {
////        data.push_back(i);
////    }
////    std::random_shuffle(data.begin(), data.end());
////
////    for (int i = 0; i < N; i++) {
////        br.insert(data[i], data[i]);
////    }
////
////    EXPECT_EQ((size_t)N, br.size());
////
////    for (int i = 0; i < N; i++) {
////        ram::Tuple<RamDomain, 2> t;
////        t[0] = i;
////        t[1] = i;
////        EXPECT_TRUE(br.find(t) != br.end()) << "i=" << i;
////    }
////
////    BinRel br2;
////    EXPECT_EQ(0, br2.size());
////    EXPECT_FALSE(br2.contains(0, 0));
////
////    br2 = br;
////    EXPECT_EQ((size_t)N, br.size());
////    EXPECT_EQ((size_t)N, br2.size());
////
////    for (int i = 0; i < N; ++i) {
////        ram::Tuple<RamDomain, 2> t;
////        t[0] = i;
////        t[1] = i;
////        EXPECT_TRUE(br.find(t) != br.end());
////        EXPECT_TRUE(br2.find(t) != br2.end());
////    }
////
////    // construct a new one an insert into only one
////    ram::Tuple<RamDomain, 2> t;
////    t[0] = N + 1;
////    t[1] = N + 1;
////    EXPECT_FALSE(br.find(t) != br.end());
////    EXPECT_FALSE(br2.find(t) != br2.end());
////    br2.insert(t[0], t[1]);
////    EXPECT_FALSE(br.find(t) != br.end());
////    EXPECT_TRUE(br2.find(t) != br2.end());
////}
//
//// TEST(BinRelTest, CopyScope) {
////     //simply test whether scope is fine in scope changes
////     BinRel br1;
////     {
////         BinRel br2;
////         for (int i = 0; i < 5000; ++i) {
////             br2.insert(i,i);
////         }
//
////         br1 = br2;
////     }
//
////     EXPECT_EQ(5000, br1.size());
//// }
//

TEST(BinRelTest, Merge) {
    // test insertAll isolates data
    BinRel br;

    int N = 100;

    std::vector<int> data;
    for (int i = 0; i < N; i++) {
        data.push_back(i);
    }
    random_shuffle(data.begin(), data.end());

    for (int i = 0; i < N; i++) {
        br.insert(data[i], data[i]);
    }

    // also insert a joint pair
    br.insert(N - 1, N + 1);

    EXPECT_EQ((size_t)N + 3, br.size());

    BinRel br2;
    EXPECT_EQ(0, br2.size());

    size_t count = 0;
    for (auto x : br2) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br2.size());

    br2.insertAll(br);
    EXPECT_EQ((size_t)N + 3, br2.size());
    EXPECT_EQ((size_t)N + 3, br.size());
    count = 0;
    for (auto x : br2) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br2.size());

    br.clear();
    EXPECT_EQ((size_t)N + 3, br2.size());
    EXPECT_EQ(0, br.size());
    EXPECT_FALSE(br.begin() != br.end());

    count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br.size());

    br2.clear();
    EXPECT_EQ(0, br2.size());
    EXPECT_EQ(0, br.size());

    count = 0;
    for (auto x : br2) {
        ++count;
        testutil::ignore(x);
    }
    EXPECT_EQ(count, br2.size());
}

TEST(BinRelTest, IterEmpty) {
    // test iterating over an empty binrel fails
    BinRel br;
    for (auto x : br) {
        EXPECT_FALSE(true);
        testutil::ignore(x);
    }
    EXPECT_EQ(0, br.size());
}

TEST(BinRelTest, IterBasic) {
    BinRel br;
    br.insert(0, 0);
    br.insert(1, 1);
    br.insert(2, 2);

    size_t count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }

    EXPECT_EQ(count, br.size());
    // merge one disjoint set
    br.insert(0, 1);
    count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }

    EXPECT_EQ(count, br.size());
}


// XXX: doesn't exist anymore
// TODO (pnappa): make some tests for the range splices and shit
//TEST(BinRelTest, IterFindBetween) {
//    BinRel br;
//    br.insert(0, 1);
//    br.insert(1, 2);
//    br.insert(2, 3);
//
//    // try and perform findBetween on a single
//    ram::Tuple<RamDomain, 2> t1;
//    t1[0] = 1;
//    t1[1] = 0;
//    ram::Tuple<RamDomain, 2> t2;
//    t2[0] = 1;
//    t2[1] = 0;
//
//    size_t count = 0;
//    for (auto x = br.findBetween(t1, t2); x != br.end(); ++x) {
//        ++count;
//    }
//    EXPECT_EQ(count, 1);
//}

TEST(BinRelTest, IterRange) {
    // TODO: write some tests to use that templated range for different indexes too
    BinRel br;
    br.insert(0,1);
    br.insert(0,2);
    br.insert(0,3);
    br.insert(0,4);
    br.insert(0,5);
    br.insert(0,6);

    br.insert(8,9);
    br.insert(8,10);

    // this should return an iterator covering (3, 0), (3, 1), ..., (3, 6), it's a (3, *) iterator
    auto rangeIt = br.getBoundaries<1>({3, 18293018});

    std::vector<size_t> posteriorsCovered;
    for (auto tup : rangeIt) {
        posteriorsCovered.push_back(tup[1]); 
    }
    EXPECT_EQ(posteriorsCovered.size(), 7);

    // this should be of everything, so doesn't matter the args
    rangeIt = br.getBoundaries<0>({332, 888});
    posteriorsCovered.clear();
    for (auto tup : rangeIt) {
        posteriorsCovered.push_back(tup[1]); 
    }
    EXPECT_EQ(posteriorsCovered.size(), (7*7)+(3*3));

    // and now iterate over two levels (exactly one pretty much)
    rangeIt = br.getBoundaries<2>({2,3});
    posteriorsCovered.clear();
    for (auto tup : rangeIt) {
        posteriorsCovered.push_back(tup[1]); 
    }
    EXPECT_EQ(posteriorsCovered.size(), 1);
    EXPECT_EQ(posteriorsCovered.front(), 3);

    // and now the same, but for levels 1 and two, stuff that doesn't exist
    rangeIt = br.getBoundaries<1>({99, 99});
    posteriorsCovered.clear();
    for (auto tup : rangeIt) {
        posteriorsCovered.push_back(tup[1]); 
    }
    EXPECT_EQ(posteriorsCovered.size(), 0);

    rangeIt = br.getBoundaries<2>({8, 1});
    posteriorsCovered.clear();
    for (auto tup : rangeIt) {
        posteriorsCovered.push_back(tup[1]); 
    }
    EXPECT_EQ(posteriorsCovered.size(), 0);

    // and now the same for an empty binary relation
    br.clear();
    // repeat the same, but notice that we expect the size to be 0 always
    {
        auto rangeIt = br.getBoundaries<1>({3, 18293018});

        std::vector<size_t> posteriorsCovered;
        for (auto tup : rangeIt) {
            posteriorsCovered.push_back(tup[1]); 
        }
        EXPECT_EQ(posteriorsCovered.size(), 0);

        // this should be of everything, so doesn't matter the args
        rangeIt = br.getBoundaries<0>({332, 888});
        posteriorsCovered.clear();
        for (auto tup : rangeIt) {
            posteriorsCovered.push_back(tup[1]); 
        }
        EXPECT_EQ(posteriorsCovered.size(), 0);

        // and now iterate over two levels (exactly one pretty much)
        rangeIt = br.getBoundaries<2>({2,3});
        posteriorsCovered.clear();
        for (auto tup : rangeIt) {
            posteriorsCovered.push_back(tup[1]); 
        }
        EXPECT_EQ(posteriorsCovered.size(), 0);

        // and now the same, but for levels 1 and two, stuff that doesn't exist
        rangeIt = br.getBoundaries<1>({99, 99});
        posteriorsCovered.clear();
        for (auto tup : rangeIt) {
            posteriorsCovered.push_back(tup[1]); 
        }
        EXPECT_EQ(posteriorsCovered.size(), 0);

        rangeIt = br.getBoundaries<2>({8, 1});
        posteriorsCovered.clear();
        for (auto tup : rangeIt) {
            posteriorsCovered.push_back(tup[1]); 
        }
        EXPECT_EQ(posteriorsCovered.size(), 0);

    }
}

TEST(BinRelTest, IterPartition) {
    // test that the union equals the input

    // test single set binary rel
    BinRel br;
    std::vector<std::pair<RamDomain, RamDomain>> values;
    RamDomain N = 1000;
    for (RamDomain i = 0; i < N; ++i) {
        br.insert(i, i + 1);
    }

    EXPECT_EQ(size_t((N + 1) * (N + 1)), br.size());

    auto chunks = br.partition(400);
    // we can't make too many assumptions..
    EXPECT_TRUE(chunks.size() > 0);

    for (auto chunk : chunks) {
        for (auto x = chunk.begin(); x != chunk.end(); ++x) {
            values.push_back(std::make_pair((*x)[0], (*x)[1]));
        }
    }

    EXPECT_EQ(br.size(), values.size());

    br.clear();
    values.clear();
    chunks.clear();

    // many disjoint sets (note, can't use N, because even & odd numbers don't work the same..)
    for (RamDomain i = 0; i < 1000; i += 2) {
        br.insert(i, i + 1);
    }
    EXPECT_EQ((size_t)4 * 1000 / 2, br.size());

    chunks = br.partition(400);
    for (auto chunk : chunks) {
        for (auto x = chunk.begin(); x != chunk.end(); ++x) {
            values.push_back(std::make_pair((*x)[0], (*x)[1]));
        }
    }

    EXPECT_EQ(br.size(), values.size());
}

TEST(BinRelTest, ParallelTest) {
    // insert a lot of times into a disjoint set over multiple std::threads

    BinRel br;
    std::vector<std::thread> starts;
    // number of inserts per thread
    int N = 1000;
    // int N = 100000;

    starts.push_back(std::thread([&]() {
        for (RamDomain i = 0; i < N * 4; i += 4) br.insert(i, i + 4);
    }));

    starts.push_back(std::thread([&]() {
        for (RamDomain i = 1; i < N * 4; i += 4) br.insert(i, i + 4);
    }));

    starts.push_back(std::thread([&]() {
        for (RamDomain i = 2; i < N * 4; i += 4) br.insert(i, i + 4);
    }));

    starts.push_back(std::thread([&]() {
        for (RamDomain i = 3; i < N * 4; i += 4) br.insert(i, i + 4);
    }));

    for (auto& r : starts) r.join();

    EXPECT_EQ((size_t)(N + 1) * (N + 1) * 4, br.size());

    size_t count = 0;
    for (auto x : br) {
        ++count;
        testutil::ignore(x);
    }

    EXPECT_EQ(count, br.size());
}

TEST(BinRelTest, Scaling) {
    const int N = 10000;

    BinRel br;
    for (int i = 0; i < N; ++i) {
        br.insert(i,i);
    }

    EXPECT_EQ(N, br.size());
}

#ifdef _OPENMP
TEST(BinRelTest, ParallelScaling) {
    // use OpenMP this time

    // test with varying number of threads (100000 will likely catch a race condition)
    const int N = 100000;
    std::vector<int> data1;
    std::vector<int> data2;
    for (int i = 0; i < N; ++i) data1.push_back(i);
    for (int i = 0; i < N; ++i) data2.push_back(i);

    std::random_shuffle(data1.begin(), data1.end());
    std::random_shuffle(data2.begin(), data2.end());

    BinRel br;
#pragma omp parallel for
    for (int i = 0; i < N; i++) {
        // unfortunately, we can't do insert(data1, data2) as we won't know how many pairs...
        br.insert(data1[i], data1[i]);
        br.insert(data2[i], data2[i]);
    }

    EXPECT_EQ(N, br.size());
}
#endif



}  // namespace test
}  // namespace souffle
