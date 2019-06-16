/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Relation.h
 *
 * Provides a type-erased, generic handle on relations.
 * Each relation consists of one or more indexes storing their content
 * in an order fashion using exchangeable data structure implementations.
 *
 ***********************************************************************/

#pragma once

#include <cassert>
#include <map>
#include <utility>
#include <memory>
#include <vector>

#include "CompiledTuple.h"
#include "RamTypes.h"

namespace souffle {

	/**
	 * A construction-time sized tuple instance easing tuple handling
	 * in non-compiled contexts.
	 */
	class DynTuple {

		std::vector<RamDomain> data;

	public:

		DynTuple(std::size_t arity) : data(arity) {}

		std::size_t size() const {
			return data.size();
		}

		RamDomain& operator[](std::size_t i) {
			return data[i];
		}

		const RamDomain& operator[](std::size_t i) const {
			return data[i];
		}

	};


	/**
	 * A type-erased reference to tuples. The reference does not
	 * own the referenced data. It is thus very light-weight to
	 * create and forward.
	 */
	class TupleRef {

		// The address of the first component of the tuple.
		const RamDomain* base;

		// The size of the tuple.
		std::size_t arity;

	public:

		TupleRef() = default;

		/**
		 * Creates a tuple reference from some externally maintained
		 */
		TupleRef(const RamDomain* base, std::size_t arity)
			: base(base), arity(arity) {}

		/**
		 * A constructor supporting the implicit conversion of
		 * a tuple into a reference.
		 */
		template<std::size_t Arity>
		TupleRef(const ram::Tuple<RamDomain,Arity>& tuple)
			: TupleRef(&tuple[0], Arity) {}

		TupleRef(const DynTuple& tuple)
			: TupleRef(&tuple[0], tuple.size()) {}

		template<std::size_t Arity>
		const ram::Tuple<RamDomain,Arity>& asTuple() const {
			assert(arity == Arity);
			return *reinterpret_cast<const ram::Tuple<RamDomain,Arity>*>(base);
		}

		/**
		 * Obtains the arity of the referenced tuple.
		 */
		std::size_t size() const {
			return arity;
		}

		/**
		 * Provides access to tuple components.
		 */
		const RamDomain& operator[](std::size_t i) const {
			return base[i];
		}

		friend std::ostream& operator<<(std::ostream& out, TupleRef ref);
	};


	/**
	 * An order to be enforced for storing tuples within
	 * indexes. The order is defined by the sequence of
	 * component to be considered in sorting tuples.
	 */
	class Order {
		std::vector<int> order;
	public:

		template<typename ... Positions>
		Order(Positions ... pos) : order{pos...} {
			assert(valid());
		}

		// Creates a natural order for the given arity.
		static Order create(int arity);

		std::size_t size() const;

		/**
		 * Determines whether this order is a valid order.
		 */
		bool valid() const;

		template<std::size_t Arity>
		ram::Tuple<RamDomain,Arity> encode(const ram::Tuple<RamDomain,Arity>& entry) const {
			ram::Tuple<RamDomain,Arity> res;
			for (std::size_t i=0; i<Arity; ++i) {
				res[i] = entry[order[i]];
			}
			return res;
		}

		template<std::size_t Arity>
		ram::Tuple<RamDomain,Arity> decode(const ram::Tuple<RamDomain,Arity>& entry) const {
			ram::Tuple<RamDomain,Arity> res;
			for (std::size_t i=0; i<Arity; ++i) {
				res[order[i]] = entry[i];
			}
			return res;
		}

		bool operator==(const Order& other) const;
		bool operator!=(const Order& other) const;
		bool operator<(const Order& other) const;

		friend std::ostream& operator<<(std::ostream& out, const Order& order);
	};


	/**
	 * An abstract perspective on a data range. A stream is a pair of iterators,
	 * referencing the begin and end of a traversible sequence. A stream can only
	 * be traversed once.
	 */
	class Stream {
	public:

		// the size of the internally maintained buffer, corresponding
		// to the maximum chunk size retrieved from the source
		constexpr static int BUFFER_SIZE = 128;

		// the 'interface' for data sources
		class Source {
		public:
			virtual ~Source() {};

			/**
			 * Requests the source to retrieve the next set of elements,
			 * to be stored in an array addressed by the first parameter.
			 * The second parameter states an upper limit for the number
			 * of elements to be retrieved.
			 *
			 * @return the number of elements retrieved, 0 if end has reached.
			 */
			virtual int load(TupleRef* trg, int max) =0;
		};

	private:

		// the source to read data from
		std::unique_ptr<Source> source;

		// an internal buffer for decoded elements
		std::vector<TupleRef> buffer;

		// the current position in the buffer
		int cur;

		// the end of valid elements in the buffer
		int limit;

	public:

		Stream(std::unique_ptr<Source>&& src)
			: source(std::move(src)), buffer(BUFFER_SIZE), cur(0) {
			loadNext();
		}

		template<typename S>
		Stream(std::unique_ptr<S>&& src)
			: Stream(std::unique_ptr<Source>(std::move(src))) {}

		/**
		 * The iterator exposed by this stream to iterate through
		 * its elements using a range-based for.
		 */
		class Iterator : public std::iterator<std::forward_iterator_tag,TupleRef> {
			Stream* stream;
		public:

			Iterator() : stream(nullptr) {}

			Iterator(Stream& stream) : stream(&stream) {
				if (stream.cur >= stream.limit) this->stream = nullptr;
			}

			Iterator& operator++() {
				++stream->cur;
				if (stream->cur < stream->limit) return *this;
				stream->loadNext();
				if (stream->cur >= stream->limit) stream = nullptr;
				return *this;
			}

			TupleRef operator*() const {
				return stream->buffer[stream->cur];
			}

			bool operator!=(Iterator& other) const {
				return stream != other.stream;
			}
		};

		// support for ranged based for loops
		Iterator begin() { return *this; }
		Iterator end() const { return {}; }

	private:

		/**
		 * Retrieves the next chunk of elements from the source.
		 */
		void loadNext() {
			limit = source->load(&buffer[0],BUFFER_SIZE);
			cur = 0;
		}
	};

	/**
	 * An index is an abstraction of a data structure
	 */
	class Index {
	public:

		virtual ~Index() {};

		/**
		 * Obtains the arity of the given index.
		 */
		virtual int arity() const =0;

		/**
		 * Tests whether this index is empty or not.
		 */
		virtual bool empty() const =0;

		/**
		 * Obtains the number of elements stored in this index.
		 */
		virtual std::size_t size() const =0;

		/**
		 * Inserts a tuple into this index.
		 */
		virtual bool insert(TupleRef tuple) =0;

		/**
		 * Inserts all elements of the given index.
		 */
		virtual void insert(const Index& src) =0;

		/**
		 * Tests whether the given tuple is present in this index or not.
		 */
		virtual bool contains(TupleRef tuple) const =0;

		/**
		 * Returns a stream covering the entire index content.
		 */
		virtual Stream scan() const =0;

		/**
		 * Returns a stream covering the elements
		 */
		virtual Stream range(TupleRef low, TupleRef high) const =0;

		/**
		 * Clears the content of this index, turning it empty.
		 */
		virtual void clear() =0;
	};

	// The type of index factory functions.
	using IndexFactory = std::unique_ptr<Index>(*)(const Order&);

	// A factory for BTree based index.
	std::unique_ptr<Index> createBTreeIndex(const Order&);

	// A factory for Brie based index.
	std::unique_ptr<Index> createBrieIndex(const Order&);

	/**
	 * A relation, composed of a collection of indexes.
	 */
	class Relation {

		// a map of managed indexes
		std::map<Order,std::unique_ptr<Index>> indexes;

		// a pointer to the main index within the managed index
		Index* main;

	public:

		/**
		 * Creates a relation of the given arity using the default order.
		 */
		Relation(std::size_t arity, IndexFactory factory = &createBTreeIndex);

		/**
		 * Creates a relation using the given order as an initial index.
		 */
		Relation(const Order& order, IndexFactory factory = &createBTreeIndex);

		/**
		 * Adds a new index for the given order. The order must have
		 * the same length as other index in this relation. Elements
		 * already present will get filled in this new index.
		 */
		void addIndex(const Order& order, IndexFactory factory = &createBTreeIndex);

		/**
		 * Drops an index from the maintained indexes. All but one index
		 * may be removed.
		 */
		void removeIndex(const Order& order);

		/**
		 * Add the given tuple to this relation.
		 */
		bool insert(TupleRef tuple);

		/**
		 * Add all entries of the given relation to this relation.
		 */
		void insert(const Relation& other);

		/**
		 * Tests whether this relation contains the given tuple.
		 */
		bool contains(TupleRef tuple);

		/**
		 * Obtains a stream to scan the entire relation.
		 */
		Stream scan() const;

		/**
		 * Obtains a stream covering the interval between the two given entries.
		 */
		Stream range(const Order& order, TupleRef low, TupleRef high) const;

		/**
		 * Removes the content of this relation, but retains the empty indexes.
		 */
		void clear();

		/**
		 * Swaps the content of this and the given relation, including the
		 * installed indexes.
		 */
		void swap(Relation& other);

	};


} // end namespace souffle
