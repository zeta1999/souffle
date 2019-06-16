/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Relation.cpp
 *
 ***********************************************************************/

#include "Relation.h"
#include "Util.h"
#include "BTree.h"
#include "Brie.h"

namespace souffle {

	std::ostream& operator<<(std::ostream& out, TupleRef ref) {
		out << "[";
		for (std::size_t i=0; i<ref.size(); i++) {
			if (i > 0) out << ',';
			out << ref[i];
		}
		return out << "]";
	}

	Order Order::create(int arity) {
		Order res;
		res.order.resize(arity);
		for (int i=0; i<arity; i++) {
			res.order[i] = i;
		}
		return res;
	}

	std::size_t Order::size() const {
		return order.size();
	}

	bool Order::valid() const {
		// Check that all indices are in range.
		for (int i : order) {
			if (i < 0 || i >= int(order.size())) return false;
		}
		// Check that there are no duplicates.
		for (std::size_t i=0; i<order.size(); i++) {
			for (std::size_t j=i+1; j < order.size(); j++) {
				if (order[i] == order[j]) return false;
			}
		}
		return true;
	}

	bool Order::operator==(const Order& other) const {
		return order == other.order;
	}

	bool Order::operator!=(const Order& other) const {
		return !(*this == other);
	}

	bool Order::operator<(const Order& other) const {
		return order < other.order;
	}

	std::ostream& operator<<(std::ostream& out, const Order& order) {
		return out << "[" << join(order.order) << "]";
	}

	Relation::Relation(std::size_t arity, IndexFactory factory)
		: Relation(Order::create(arity), factory) {}

	Relation::Relation(const Order& order, IndexFactory factory) {
		auto& entry = indexes[order];
		entry = factory(order);
		main = entry.get();
	}

	void Relation::addIndex(const Order& order, IndexFactory factory) {
		auto pos = indexes.find(order);
		if (pos != indexes.end()) return;
		auto& index = indexes[order];
		index = factory(order);
		index->insert(*main);
	}

	void Relation::removeIndex(const Order& order) {
		assert(indexes.size() > 1);
		auto pos = indexes.find(order);
		if (pos == indexes.end()) return;
		std::unique_ptr<Index> index = std::move(pos->second);
		indexes.erase(pos);
		if (main == index.get()) {
			main = indexes.begin()->second.get();
		}
	}

	bool Relation::insert(TupleRef tuple) {
		if (!main->insert(tuple)) return false;
		for (const auto& cur : indexes) {
			if (cur.second.get() == main) continue;
			cur.second->insert(tuple);
		}
		return true;
	}

	void Relation::insert(const Relation& other) {
		// TODO: cover this in a smarter way
		for (const auto& cur : other.scan()) {
			insert(cur);
		}
	}

	bool Relation::contains(TupleRef tuple) {
		return main->contains(tuple);
	}

	Stream Relation::scan() const {
		return main->scan();
	}

	Stream Relation::range(const Order& order, TupleRef low, TupleRef high) const {
		auto pos = indexes.find(order);
		assert(pos != indexes.end());
		return pos->second->range(low,high);
	}

	void Relation::clear() {
		for (const auto& cur : indexes) {
			cur.second->clear();
		}
	}

	void Relation::swap(Relation& other) {
		indexes.swap(other.indexes);
	}


	namespace {

		// An index wrapper for nullary indexes. For those, no complex
		// nested data structure is required.
		class NullaryIndex : public Index {

			// indicates whether the one single element is present or not.
			bool present;

			// a source adaptation, iterating through the optionally present
			// entry in this relation.
			class Source : public Stream::Source {
				bool present;
			public:
				Source(bool present) : present(present) {}
				int load(TupleRef*, int max) override {
					if (!present) return 0;
					present = false;
					return 1;
				}
			};

		public:

			int arity() const override {
				return 0;
			}

			bool empty() const override {
				return !present;
			}

			std::size_t size() const override {
				return present ? 1 : 0;
			}

			bool insert(TupleRef tuple) override {
				assert(tuple.size() == 0);
				bool res = present;
				present = true;
				return res;
			}

			void insert(const Index& src) override {
				assert(src.arity() == 0);
				present = present | !src.empty();
			}

			bool contains(TupleRef tuple) const override {
				assert(tuple.size() == 0);
				return present;
			}

			Stream scan() const override {
				return std::make_unique<Source>(present);
			}

			Stream range(TupleRef low, TupleRef high) const override {
				return scan();
			}

			void clear() override {
				present = false;
			}

		};

		/**
		 * A generic data structure index adapter handling the boundary
		 * level order conversion as well as iteration through nested
		 * data structures.
		 *
		 * @tparam Structure the structure to be utilized
		 */
		template<typename Structure>
		class GenericIndex : public Index {

			using Entry = typename Structure::element_type;
			static constexpr int Arity = Entry::arity;

			// the order to be simulated
			Order order;

			// the internal data structure
			Structure data;

			// a source adapter for streaming through data
			class Source : public Stream::Source {

				const Order& order;

				// the begin and end of the stream
				using iter = typename Structure::iterator;
				iter cur;
				iter end;

				// an internal buffer for re-ordered elements
				std::vector<Entry> buffer;

			public:

				Source(const Order& order, iter begin, iter end)
					: order(order), cur(begin), end(end), buffer(Stream::BUFFER_SIZE) {}

				int load(TupleRef* out, int max) override {
					int c = 0;
					while(cur != end && c < max) {
						buffer[c] = order.decode(*cur);
						out[c] = buffer[c];
						++cur;
						++c;
					}
					return c;
				}
			};

		public:

			GenericIndex(const Order& order)
				: order(order) {}

			int arity() const override {
				return Arity;
			}

			bool empty() const override {
				return data.empty();
			}

			std::size_t size() const override {
				return data.size();
			}

			bool insert(TupleRef tuple) override {
				return data.insert(order.encode(tuple.asTuple<Arity>()));
			}

			void insert(const Index& src) override {
				// TODO: make smarter
				for (const auto& cur : src.scan()) {
					insert(cur);
				}
			}

			bool contains(TupleRef tuple) const override {
				return data.contains(order.encode(tuple.asTuple<Arity>()));
			}

			Stream scan() const override {
				return std::make_unique<Source>(order, data.begin(),data.end());
			}

			Stream range(TupleRef low, TupleRef high) const override {
				Entry a = order.encode(low.asTuple<Arity>());
				Entry b = order.encode(high.asTuple<Arity>());
				if (!(a < b)) {
					return std::make_unique<Source>(order, data.end(), data.end());
				}
				return std::make_unique<Source>(order,
					data.lower_bound(order.encode(low.asTuple<Arity>())),
					data.lower_bound(order.encode(high.asTuple<Arity>()))
				);
			}

			void clear() override {
				data.clear();
			}
		};

		/**
		 * A index adapter for B-trees, using the generic index adapter.
		 */
		template<std::size_t Arity>
		class BTreeIndex : public GenericIndex<btree_set<ram::Tuple<RamDomain,Arity>>> {
		public:
			using GenericIndex<btree_set<ram::Tuple<RamDomain,Arity>>>::GenericIndex;
		};

		/**
		 * A index adapter for Bries, using the generic index adapter.
		 */
		template<std::size_t Arity>
		class BrieIndex : public GenericIndex<Trie<Arity>> {
		public:
			using GenericIndex<Trie<Arity>>::GenericIndex;
		};

	} // end namespace


	std::unique_ptr<Index> createBTreeIndex(const Order& order) {
		switch(order.size()) {
		case 0: return std::make_unique<NullaryIndex>();
		case 1: return std::make_unique<BTreeIndex<1>>(order);
		case 2: return std::make_unique<BTreeIndex<2>>(order);
		case 3: return std::make_unique<BTreeIndex<3>>(order);
		case 4: return std::make_unique<BTreeIndex<4>>(order);
		case 5: return std::make_unique<BTreeIndex<5>>(order);
		case 6: return std::make_unique<BTreeIndex<6>>(order);
		case 7: return std::make_unique<BTreeIndex<7>>(order);
		case 8: return std::make_unique<BTreeIndex<8>>(order);
		case 9: return std::make_unique<BTreeIndex<9>>(order);
		case 10: return std::make_unique<BTreeIndex<10>>(order);
		case 11: return std::make_unique<BTreeIndex<11>>(order);
		case 12: return std::make_unique<BTreeIndex<12>>(order);
		}
		assert(false && "Requested arity not yet supported. Feel free to add it.");
	}


	std::unique_ptr<Index> createBrieIndex(const Order& order) {
		switch(order.size()) {
		case 0: return std::make_unique<NullaryIndex>();
		case 1: return std::make_unique<BrieIndex<1>>(order);
		case 2: return std::make_unique<BrieIndex<2>>(order);
		case 3: return std::make_unique<BrieIndex<3>>(order);
		case 4: return std::make_unique<BrieIndex<4>>(order);
		case 5: return std::make_unique<BrieIndex<5>>(order);
		case 6: return std::make_unique<BrieIndex<6>>(order);
		case 7: return std::make_unique<BrieIndex<7>>(order);
		case 8: return std::make_unique<BrieIndex<8>>(order);
		case 9: return std::make_unique<BrieIndex<9>>(order);
		case 10: return std::make_unique<BrieIndex<10>>(order);
		case 11: return std::make_unique<BrieIndex<11>>(order);
		case 12: return std::make_unique<BrieIndex<12>>(order);
		}
		assert(false && "Requested arity not yet supported. Feel free to add it.");
	}

} // end namespace souffle
