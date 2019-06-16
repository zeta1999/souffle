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
	EXPECT_EQ(N,set.size());
	EXPECT_EQ(N,check.size());
	EXPECT_EQ(set,check);
}

TEST(RelationTest, NullaryRangeQuery) {
	const int D = 0;
	using Entry = ram::Tuple<RamDomain,D>;
	auto order = Order::create(0);

	Relation rel(D);
	std::vector<int> list;

	// all cases can be tested explicit
	for(const auto& cur : rel.range(order, Entry(), Entry())) {
		list.push_back(0);
	}
	EXPECT_TRUE(list.empty());

	rel.insert(Entry());
	list.clear();
	for(const auto& cur : rel.range(order, Entry(), Entry())) {
		list.push_back(0);
	}
	EXPECT_EQ(list.size(), 1);
}

TEST(RelationTest, BTreeRangeQuery) {
	const int N = 10;
	const int D = 2;
	using Entry = ram::Tuple<RamDomain,D>;

	Relation rel(D);
	std::set<Entry> ref;

	// fill relation and set
	Entry cur;
	for (int i=1; i<N; i++) {
		cur[0] = i;
		for (int j=1; j<N; j++) {
			cur[1] = j;
			rel.insert(cur);
			ref.insert(cur);
		}
	}

	// query boundaries of relation and set
	Entry low;
	Entry high;
	Order order = Order::create(2);
	for(low[0] = 0; low[0]<N+1; ++low[0]) {
		for (low[1]=0; low[1]<N+1; ++low[1]) {
			for (high[0]=0; high[0]<N+1; ++high[0]) {
				for (high[1]=0; high[1]<N+1; ++high[1]) {

					// get result from test relation
					std::vector<Entry> test;
					for (const auto& cur : rel.range(order,low,high)) {
						test.push_back(cur.asTuple<2>());
					}

					std::vector<Entry> reference;
					if (low < high) {
						auto a = ref.lower_bound(low);
						auto b = ref.lower_bound(high);
						for (auto it = a; it != b; ++it) {
							reference.push_back(*it);
						}
					}

					EXPECT_EQ(test,reference);
				}
			}
		}
	}
}


TEST(RelationTest, BrieRangeQuery_1d) {
	const int N = 100;
	const int D = 1;
	using Entry = ram::Tuple<RamDomain,D>;

	Relation rel(D,&createBrieIndex);
	std::set<Entry> ref;

	// fill relation and set
	Entry cur;
	for (int j=1; j<N; j++) {
		cur[0] = j;
		rel.insert(cur);
		ref.insert(cur);
	}

	// query boundaries of relation and set
	Entry low;
	Entry high;
	Order order = Order::create(1);
	for(low[0] = 0; low[0]<N+1; ++low[0]) {
		for (high[0]=0; high[0]<N+1; ++high[0]) {

			// get result from test relation
			std::vector<Entry> test;
			for (const auto& cur : rel.range(order,low,high)) {
				test.push_back(cur.asTuple<D>());
			}

			std::vector<Entry> reference;
			if (low < high) {
				auto a = ref.lower_bound(low);
				auto b = ref.lower_bound(high);
				for (auto it = a; it != b; ++it) {
					reference.push_back(*it);
				}
			}

			EXPECT_EQ(test,reference);
		}
	}
}

TEST(RelationTest, BrieRangeQuery_2d) {
	const int N = 10;
	const int D = 2;
	using Entry = ram::Tuple<RamDomain,D>;

	Relation rel(D,&createBrieIndex);
	std::set<Entry> ref;

	// fill relation and set
	Entry cur;
	for (int i=1; i<N; i++) {
		cur[0] = i;
		for (int j=1; j<N; j++) {
			cur[1] = j;
			rel.insert(cur);
			ref.insert(cur);
		}
	}

	// query boundaries of relation and set
	Entry low;
	Entry high;
	Order order = Order::create(D);
	for(low[0] = 0; low[0]<N+1; ++low[0]) {
	for (low[1]=0; low[1]<N+1; ++low[1]) {
		for (high[0]=0; high[0]<N+1; ++high[0]) {
		for (high[1]=0; high[1]<N+1; ++high[1]) {

			// get result from test relation
			std::vector<Entry> test;
			for (const auto& cur : rel.range(order,low,high)) {
				test.push_back(cur.asTuple<D>());
			}

			std::vector<Entry> reference;
			if (low < high) {
				auto a = ref.lower_bound(low);
				auto b = ref.lower_bound(high);
				for (auto it = a; it != b; ++it) {
					reference.push_back(*it);
				}
			}

			EXPECT_EQ(test,reference);
		}
		}
	}
	}
}


TEST(RelationTest, BrieRangeQuery_3d) {
	const int N = 10;
	const int D = 3;
	using Entry = ram::Tuple<RamDomain,D>;

	Relation rel(D,&createBrieIndex);
	std::set<Entry> ref;

	// fill relation and set
	Entry cur;
	for (int i=1; i<N; i++) {
		cur[0] = i;
		for (int j=1; j<N; j++) {
			cur[1] = j;
			for (int l=1; l<N; l++) {
				cur[2] = l;
				rel.insert(cur);
				ref.insert(cur);
			}
		}
	}

	// query boundaries of relation and set
	Entry low;
	Entry high;
	Order order = Order::create(D);
	for (low[0]=0; low[0]<N+1; ++low[0]) {
	for (low[1]=0; low[1]<N+1; ++low[1]) {
	for (low[2]=0; low[2]<N+1; ++low[2]) {
		for (high[0]=0; high[0]<N+1; ++high[0]) {
		for (high[1]=0; high[1]<N+1; ++high[1]) {
		for (high[2]=0; high[2]<N+1; ++high[2]) {

			// get result from test relation
			std::vector<Entry> test;
			for (const auto& cur : rel.range(order,low,high)) {
				test.push_back(cur.asTuple<D>());
			}

			std::vector<Entry> reference;
			if (low < high) {
				auto a = ref.lower_bound(low);
				auto b = ref.lower_bound(high);
				for (auto it = a; it != b; ++it) {
					reference.push_back(*it);
				}
			}

			EXPECT_EQ(test,reference);
		}
		}
		}
	}
	}
	}
}


}  // namespace test
}  // namespace souffle
