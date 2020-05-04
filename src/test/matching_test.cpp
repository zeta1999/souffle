/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file matching_test.h
 *
 * Test cases for the computation of optimal indices.
 *
 ***********************************************************************/

#include "../RamIndexAnalysis.h"
#include "../RamRelation.h"
#include "test.h"

#include <cmath>
#include <fstream>
#include <functional>
#include <iostream>
#include <limits>
#include <random>
#include <sstream>

using namespace std;
using namespace souffle;

class TestAutoIndex : public MinIndexSelection {
public:
    TestAutoIndex() : MinIndexSelection() {}
    /** returns number of unique matchings */
    int getNumMatchings() {
        return matching.getNumMatchings();
    }
};

using Nodes = set<SearchSignature>;

TEST(Matching, StaticTest_1) {
    TestAutoIndex order;
    Nodes nodes;
    size_t arity = 5;
    order.addSearch(SearchSignature(arity, 31));
    nodes.insert(SearchSignature(arity, 31));
    order.addSearch(SearchSignature(arity, 23));
    nodes.insert(SearchSignature(arity, 23));
    order.addSearch(SearchSignature(arity, 15));
    nodes.insert(SearchSignature(arity, 15));
    order.addSearch(SearchSignature(arity, 7));
    nodes.insert(SearchSignature(arity, 7));
    order.addSearch(SearchSignature(arity, 5));
    nodes.insert(SearchSignature(arity, 5));
    order.addSearch(SearchSignature(arity, 3));
    nodes.insert(SearchSignature(arity, 3));
    order.addSearch(SearchSignature(arity, 1));
    nodes.insert(SearchSignature(arity, 1));

    order.solve();
    int num = order.getNumMatchings();

    EXPECT_EQ(num, 5);
}

TEST(Matching, StaticTest_2) {
    TestAutoIndex order;
    Nodes nodes;

    size_t arity = 7;
    order.addSearch(SearchSignature(arity, 121));
    nodes.insert(SearchSignature(arity, 121));
    order.addSearch(SearchSignature(arity, 104));
    nodes.insert(SearchSignature(arity, 104));
    order.addSearch(SearchSignature(arity, 53));
    nodes.insert(SearchSignature(arity, 53));
    order.addSearch(SearchSignature(arity, 49));
    nodes.insert(SearchSignature(arity, 49));
    order.addSearch(SearchSignature(arity, 39));
    nodes.insert(SearchSignature(arity, 39));
    order.addSearch(SearchSignature(arity, 33));
    nodes.insert(SearchSignature(arity, 33));
    order.addSearch(SearchSignature(arity, 32));
    nodes.insert(SearchSignature(arity, 32));
    order.addSearch(SearchSignature(arity, 23));
    nodes.insert(SearchSignature(arity, 23));
    order.addSearch(SearchSignature(arity, 11));
    nodes.insert(SearchSignature(arity, 11));
    order.addSearch(SearchSignature(arity, 7));
    nodes.insert(SearchSignature(arity, 7));

    order.solve();
    int num = order.getNumMatchings();

    EXPECT_EQ(num, 5);
}

TEST(Matching, TestOver64BitSignature) {
    TestAutoIndex order;
    Nodes nodes;

    size_t arity = 100;
    SearchSignature first(arity, 0);
    first.set(99, AttributeConstraint::Equal);
    first.set(75, AttributeConstraint::Equal);
    first.set(50, AttributeConstraint::Equal);
    first.set(25, AttributeConstraint::Equal);
    first.set(0, AttributeConstraint::Equal);

    SearchSignature second(arity, 0);
    second.set(99, AttributeConstraint::Equal);
    second.set(75, AttributeConstraint::Equal);
    second.set(50, AttributeConstraint::Equal);

    SearchSignature third(arity, 0);
    third.set(99, AttributeConstraint::Equal);
    third.set(75, AttributeConstraint::Equal);

    SearchSignature fourth(arity, 0);
    fourth.set(99, AttributeConstraint::Equal);

    SearchSignature fifth(arity, 0);
    fifth.set(25, AttributeConstraint::Equal);
    fifth.set(0, AttributeConstraint::Equal);

    nodes.insert(first);
    nodes.insert(second);
    nodes.insert(third);
    nodes.insert(fourth);
    nodes.insert(fifth);

    order.addSearch(first);
    order.addSearch(second);
    order.addSearch(third);
    order.addSearch(fourth);
    order.addSearch(fifth);

    order.solve();
    int num = order.getNumMatchings();

    EXPECT_EQ(num, 3);
}
