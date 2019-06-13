/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2017, The Souffle Developers and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file relation_test.cpp
 *
 * A test cases for the relation wrapper.
 *
 ***********************************************************************/

#include "test.h"

#include <iostream>

#include "Relation.h"

namespace souffle {
namespace test {

TEST(RelationTest, Order) {

	// test equality
	EXPECT_EQ(Order(), Order());
	EXPECT_EQ(Order(0), Order(0));
	EXPECT_EQ(Order(0,1), Order(0,1));
	EXPECT_EQ(Order(1,0), Order(1,0));

	EXPECT_NE(Order(0,1), Order(1,0));

	// test printing and creation
	EXPECT_EQ("[0,1,2,3]",toString(Order::create(4)));
}


TEST(RelationTest, BTree) {

	//Relation rel(2);
	Relation rel(Order(1,0));

	std::cout << "List data:\n";
	for (const auto& cur : rel.scan()) {
		std::cout << cur << "\n";
	}

	RamDomain data[] = { 12, 14 };
	TupleRef ref(data,2);
	EXPECT_FALSE(rel.contains(ref));
	rel.insert(ref);
	EXPECT_TRUE(rel.contains(ref));

	data[0] = 14;
	data[1] = 10;
	rel.insert(ref);

	std::cout << "List data:\n";
	for (const auto& cur : rel.scan()) {
		std::cout << cur << "\n";
	}
}

TEST(RelationTest, Brie) {

	//Relation rel(2);
	Relation rel(Order(1,0), &createBrieIndex);

	std::cout << "List data:\n";
	for (const auto& cur : rel.scan()) {
		std::cout << cur << "\n";
	}

	RamDomain data[] = { 12, 14 };
	TupleRef ref(data,2);
	EXPECT_FALSE(rel.contains(ref));
	rel.insert(ref);
	EXPECT_TRUE(rel.contains(ref));

	data[0] = 14;
	data[1] = 10;
	rel.insert(ref);

	std::cout << "List data:\n";
	for (const auto& cur : rel.scan()) {
		std::cout << cur << "\n";
	}
}

TEST(RelationTest, BTreeStress) {
	int N = 1000;

	//Relation rel(2);
	Relation rel(Order(1,0));

	RamDomain data[] = { 12, 14 };
	TupleRef ref(data,2);

	std::set<ram::Tuple<RamDomain,2>> set;

	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j) {
			data[0] = 2 * i;
			data[1] = 3 * i;
			rel.insert(ref);
			set.insert({data[0],data[1]});
		}
	}

	std::set<ram::Tuple<RamDomain,2>> check;
	for (const auto& cur : rel.scan()) {
		check.insert(cur.asTuple<2>());
	}
	EXPECT_EQ(set,check);
}

TEST(RelationTest, BrieStress) {
	int N = 1000;

	//Relation rel(2);
	Relation rel(Order(1,0), &createBrieIndex);

	RamDomain data[] = { 12, 14 };
	TupleRef ref(data,2);

	std::set<ram::Tuple<RamDomain,2>> set;

	for(int i=0; i<N; ++i) {
		for(int j=0; j<N; ++j) {
			data[0] = 2 * i;
			data[1] = 3 * i;
			rel.insert(ref);
			set.insert({data[0],data[1]});
		}
	}

	std::set<ram::Tuple<RamDomain,2>> check;
	for (const auto& cur : rel.scan()) {
		check.insert(cur.asTuple<2>());
	}
	EXPECT_EQ(set,check);
}

}  // namespace test
}  // namespace souffle
