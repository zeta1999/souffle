/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file CompiledSouffle.h
 *
 * Main include file for generated C++ classes of Souffle
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"
#include "SymbolTable.h"

#include <initializer_list>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>

#include <cassert>

namespace souffle {

class tuple;

/**
 * /brief Object-oriented wrapper class for Souffle's templatized relations.
 *
 * In a relation, tuples can be inserted into it. To access the stored tuples, iterator_base and iteraor are
 * used. A relation is manipulated by the souffle program (create new relation, load input etc).
 */
class Relation {
protected:
    /**
     * /brief Abstract iterator class.
     *
     * After tuples are inserted into a relation, they will be stored in consecutive memory space.
     * Intially, the iterator_base of a relation will point to the first tuple.
     * It can be moved to point to the next tuple until the end.
     * The tuple iterator_base is pointing to can be accessed.
     * However, users can not use this to access tuples.
     * Instead, they should use iterator which interacts with iterator_base.
     */
    class iterator_base {
    protected:
        /**
         * Required for identifying type of iterator
         * (NB: LLVM has no typeinfo).
         */
        uint32_t id;

    public:
        /**
         * Get the ID of the iterator object.
         * @return ID of the iterator object (unit32_t).
         */
        virtual uint32_t getId() const {
            return id;
        }
        /**
         * /brief Constructor.
         *
         * Create an instance of iterator_base and set its ID to be arg_id.
         * @param arg_id ID of a iterator object (unit32_t).
         */
        iterator_base(uint32_t arg_id) : id(arg_id) {}
        /**
         * /brief Destructor.
         */
        virtual ~iterator_base() = default;
        /**
         * /brief Overload the "++" operator.
         *
         * Increment the iterator_base so that it will now point to the next tuple.
         * It has to be defined by the child class.
         */
        virtual void operator++() = 0;
        /**
         * /brief Overload the "*" operator.
         *
         * Return the tuple that is pointed to by the iterator_base.
         * It has to be defined by the child class.
         * @return tuple Reference to a tuple object.
         */
        virtual tuple& operator*() = 0;
        /**
         * Overload the "==" operator.
         * @param o Reference to an object of the iterator_base class.
         * @return Boolean. True, if the ID of o is the same as the ID of the current object. False,
         * otherwise.
         */
        bool operator==(const iterator_base& o) const {
            return this->getId() == o.getId() && equal(o);
        }
        /**
         * Clone the iterator_base.
         * It has to be defined by the child class.
         * @return An iterator_base pointer.
         */
        virtual iterator_base* clone() const = 0;

    protected:
        /**
         * Check if the passed-in object of o is the the same as the current iterator_base.
         * @param o Reference to an object of the iterator_base class.
         * @return Boolean. True, if they are the same. False, otherwise.
         */
        virtual bool equal(const iterator_base& o) const = 0;
    };

public:
    /**
     * /brief Destructor.
     */
    virtual ~Relation() = default;

    /**
     * /brief Wrapper class for abstract iterator.
     *
     * Iterator can be used by the users to access the tuples stored in a relation.
     */
    class iterator {
    protected:
        /*
         * Iterator_base class pointer.
         */
        iterator_base* iter = nullptr;

    public:
        /**
         * /brief Constructor.
         */
        iterator() = default;
        /**
         * /brief Constructor.
         *
         * Initialize the iter to be the same as arg.
         * @param arg An iterator_base class pointer.
         */
        iterator(iterator_base* arg) : iter(arg) {}
        /**
         * /brief Destructor.
         *
         * Delete iter.
         */
        ~iterator() {
            delete iter;
        }
        /**
         * /brief Constructor.
         *
         * Initialize the iter to be the clone of arg.
         * @param o Reference to an iterator object.
         */
        iterator(const iterator& o) : iter(o.iter->clone()) {}
        /**
         * /brief Overload the "=" operator.
         */
        iterator& operator=(const iterator& o) {
            delete iter;
            iter = o.iter->clone();
            return *this;
        }
        /**
         * /brief Overload the "++" operator.
         *
         * Increment the iterator_base object that iter is pointing to so that iterator_base object points to
         * next tuple.
         * @return Reference to the iterator object which points to the next tuple in a relation.
         */
        iterator& operator++() {
            ++(*iter);
            return *this;
        }
        /**
         * /brief Overload the "*" operator.
         *
         * This will return the tuple that the iterator is pointing to.
         * @return Reference to a tuple object.
         */

        tuple& operator*() const {
            return *(*iter);
        }
        /**
         * /brief Overload the "==" operator.
         *
         * check if either the iter of o and the iter of current object are the same or the corresponding
         * iterator_base objects are the same.
         * @param o Reference to a iterator object.
         * @return Boolean. True, if either of them is true. False, otherwise.
         */
        bool operator==(const iterator& o) const {
            return (iter == o.iter) || (*iter == *o.iter);
        }
        /**
         * /brief Overload the "!=" operator.
         *
         * Check if the iterator object o is not the same as the current object.
         * @param o Reference to a iterator object.
         * @return Boolean. True, if they are not the same. False, otherwise.
         */
        bool operator!=(const iterator& o) const {
            return !(*this == o);
        }
    };

    /**
     * Insert a new tuple into the relation.
     * It has to be defined by the child class.
     * @param t Reference to a tuple class object.
     */
    virtual void insert(const tuple& t) = 0;

    /**
     * Check whether a tuple exists in a relation.
     * It has to be defined by the child class.
     * @param t Reference to a tuple object.
     * @return Boolean. True, if it exists. False, otherwise.
     */
    virtual bool contains(const tuple& t) const = 0;

    /**
     * To access the tuples in a relation, users need iterator.
     * Begin will return the iterator of a relation.
     * @return Iterator of a relation
     */
    virtual iterator begin() const = 0;
    /**
     * After the accessing process is done, eng function needs to be called.
     * @return Iterator of a relation.
     */
    virtual iterator end() const = 0;

    /**
     * Get the number of tuples in a relation.
     * @return the number of tuples in a relation (std::size_t).
     */
    virtual std::size_t size() const = 0;

    /**
     * Get the name of a relation.
     * @return the name of a relation (std::string).
     */
    virtual std::string getName() const = 0;
    /**
     * Get the attributes type of a relation.
     * @param the index of element in tuple (size_t).
     * @return Char pointer to the attributes type of a element in a tuple in a relation.
     */
    virtual const char* getAttrType(size_t) const = 0;
    /**
     * Get the attributes name of a relation.
     * @param the index of element in tuple (size_t).
     * @return Char pointer to the attributes name of a element in a tuple a relation.
     */
    virtual const char* getAttrName(size_t) const = 0;
    /**
     * Get the arity of a relation.
     * For example for a tuple(1 2) the arity is 2 and for a tuple (1 2 3) the arity is 3.
     * @return Arity of a relation (size_t).
     */
    virtual size_t getArity() const = 0;
    /**
     * Get the symbol table of a relation.
     * @return Reference to a symbolTable object.
     */
    virtual SymbolTable& getSymbolTable() const = 0;
    /**
     * Return the signature of a relation.
     */
    std::string getSignature() {
        if (getArity() == 0) {
            return "<>";
        }

        std::string signature = "<" + std::string(getAttrType(0));
        for (size_t i = 1; i < getArity(); i++) {
            signature += "," + std::string(getAttrType(i));
        }
        signature += ">";
        return signature;
    }

    /**
     * Eliminate all the tuples in relation.
     */
    virtual void purge() = 0;
};

/**
 * Defines a tuple for the OO interface such that
 * relations with varying columns can be accessed.
 */
/**
 * Tuples are stored in relation.
 * In Souffle, one piece of data is stored as a tuple.
 * For example if we have a relation called dog with attributes name, colour and age which are string, string
 * and interger type respectively. A piece of data it is storing can be (mydog, black,3). However, this is not
 * directly stored as a tuple. There will be a symbol table storing the actual content and associate them with
 * numbers. For example, |1|mydog| |2|black| |3|  3  | And when it stored as a tuple, (1, 2, 3) will be
 * stored.
 */
class tuple {
    /**
     * The relation to which the tuple belongs.
     */
    const Relation& relation;
    /**
     * Dynamic array used to store the elements in a tuple.
     */
    std::vector<RamDomain> array;
    /**
     * Pos stores the number of the element in a tuple.
     * For example, if we want to store "mydog, black and 3" and my dog and black have already been stored,
     * then it will be 2.
     */
    size_t pos;

public:
    /**
     * /brief Constructor.
     *
     * When given a relation pointer r pointing to an actual relation, it will set the above declared relation
     * to point to this relation. An dynamic array of space which is equal to the arity of the tuple will be
     * created. An pointer called data is pointing to the starting position of array.
     * @param r Relation pointer pointing to a relation
     */
    tuple(const Relation* r) : relation(*r), array(r->getArity()), pos(0), data(array.data()) {}
    /**
     * /brief Constructor.
     *
     * When given a tupple, similar to before, same thing will be set according to which relation the tuple
     * belongs to.
     * @param Reference to a tuple object.
     */
    tuple(const tuple& t) : relation(t.relation), array(t.array), pos(t.pos), data(array.data()) {}
    /**
     * Allows printing using WriteStream.
     */
    const RamDomain* data = nullptr;
    /**
     * Get the reference to the relation to which the tuple belongs.
     * @return Reference to a relation.
     */
    const Relation& getRelation() const {
        return relation;
    }
    /**
     * Return the number of elements in the tuple.
     * @return the number of elements in the tuple (size_t).
     */
    size_t size() const {
        return array.size();
    }

    /**
     * direct access to tuple elements via index
     * TODO: this interface should be hidden and
     * only be used by friendly classes such as
     * iterators; users should not use this interface.
     */

    /**
     * /brief Overload the operator [].
     *
     * Return the element in idx position of a tuple.
     * @param idx This is the idx of element in a tuple (size_t).
     */
    RamDomain& operator[](size_t idx) {
        return array[idx];
    }
    /**
     * /brief Overload the operator [].
     *
     * Return the element in idx position of a tuple. The returned element can not be changed.
     * @param idx This is the idx of element in a tuple (size_t).
     */
    const RamDomain& operator[](size_t idx) const {
        return array[idx];
    }

    /**
     * Reset stream pointer to the first element of a tuple.
     */
    void rewind() {
        pos = 0;
    }

    /**
     * Place a symbol into the current element of the tuple.
     * @param str Symbol to be added (std::string).
     * @return Reference to the tuple.
     */
    tuple& operator<<(const std::string& str) {
        assert(pos < size() && "exceeded tuple's size");
        assert(*relation.getAttrType(pos) == 's' && "wrong element type");
        array[pos++] = relation.getSymbolTable().lookup(str);
        return *this;
    }

    /**
     * Place a number into the current element of the tuple.
     * @param number Number to be added (RamDomain).
     * @return Reference to the tuple.
     */
    tuple& operator<<(RamDomain number) {
        assert(pos < size() && "exceeded tuple's size");
        assert((*relation.getAttrType(pos) == 'i' || *relation.getAttrType(pos) == 'r') &&
                "wrong element type");
        array[pos++] = number;
        return *this;
    }

    /**
     * Read a symbol from the tuple.
     */
    tuple& operator>>(std::string& str) {
        assert(pos < size() && "exceeded tuple's size");
        assert(*relation.getAttrType(pos) == 's' && "wrong element type");
        str = relation.getSymbolTable().resolve(array[pos++]);
        return *this;
    }

    /**
     * Read a number from the tuple.
     */
    tuple& operator>>(RamDomain& number) {
        assert(pos < size() && "exceeded tuple's size");
        assert((*relation.getAttrType(pos) == 'i' || *relation.getAttrType(pos) == 'r') &&
                "wrong element type");
        number = array[pos++];
        return *this;
    }

    /**
     * (insert) iterator for direct access to tuple's data (experimental).
     */
    decltype(array)::iterator begin() {
        return array.begin();
    }

    /**
     * Direct constructor using initialization list (experimental).
     */
    tuple(Relation* r, std::initializer_list<RamDomain> il) : relation(*r), array(il), pos(il.size()) {
        assert(il.size() == r->getArity() && "wrong tuple arity");
    }
};

/**
 * Abstract base class for generated Datalog programs.
 */
class SouffleProgram {
private:
    /**
     * Define a relation map for external access, when getRelation() called,
     * the target relation will search from this relation map,
     * relationMap stores all the relations in a map with its name
     * as the key and relation as the value.
     */
    std::map<std::string, Relation*> relationMap;

    /**
     * inputRelations stores all the input relation in a vector.
     */
    std::vector<Relation*> inputRelations;

    /**
     * outputRelations stores all the output relation in a vector.
     */
    std::vector<Relation*> outputRelations;

    /**
     * internalRelation stores all the relation in a vector that are neither an input or an output.
     */
    std::vector<Relation*> internalRelations;

    /**
     * allRelations store all the relation in a vector.
     */
    std::vector<Relation*> allRelations;

protected:
    /**
     * Add the relation to relationMap (with its name) and allRelations,
     * depends on the properties of the relation, if the relation is an input relation, it will be added to
     * inputRelations, else if the relation is an output relation, it will be added to outputRelations,
     * otherwise will add to internalRelations. (a relation could be both input and output at the same time.)
     * @param name the name of the relation (std::string).
     * @param rel a pointer to the relation (std::string).
     * @param isInput a bool argument, true if the relation is a input relation, else false (bool).
     * @param isOnput a bool argument, true if the relation is a ouput relation, else false (bool).
     */
    void addRelation(const std::string& name, Relation* rel, bool isInput, bool isOutput) {
        relationMap[name] = rel;
        allRelations.push_back(rel);
        if (isInput) {
            inputRelations.push_back(rel);
        }
        if (isOutput) {
            outputRelations.push_back(rel);
        }
        if (!isInput && !isOutput) {
            internalRelations.push_back(rel);
        }
    }

public:
    /**
     * /brief Destructor.
     *
     * Destructor of SouffleProgram.
     */
    virtual ~SouffleProgram() = default;

    /**
     * Execute the souffle program, without any loads or stores.
     */
    virtual void run(size_t stratumIndex = -1) {}

    /**
     * Execute program, loading inputs and storing outputs as required.
     * Read all input relations and store all output relations from the given directory. 
     * First argument is the input directory, second argument is the output directory, 
     * if no directory is given, the default is to use the current working directory. 
     * The implementation of this function occurs in the C++ code generated by Souffle. 
     * To view the generated C++ code, run Souffle with the `-g` option.
     */
    virtual void runAll(std::string inputDirectory = ".", std::string outputDirectory = ".",
            size_t stratumIndex = -1) = 0;

    /**
     * Read all input relations from the given directory. If no directory is given, the
     * default is to use the current working directory. The implementation of this function
     * occurs in the C++ code generated by Souffle. To view the generated C++ code, run
     * Souffle with the `-g` option.
     */
    virtual void loadAll(std::string inputDirectory = ".") = 0;

    /**
     * Store all output relations individually in .CSV file in the given directory, with
     * the relation name as the file name of the .CSV file. If no directory is given, the
     * default is to use the current working directory. The implementation of this function
     * occurs in the C++ code generated by Souffle. To view the generated C++ code, run
     * Souffle with the `-g` option.
     */
    virtual void printAll(std::string outputDirectory = ".") = 0;

    /**
     * Output all the input relations in stdout, without generating any files. (for debug purposes).
     */
    virtual void dumpInputs(std::ostream& out = std::cout) = 0;

    /**
     * Output all the output relations in stdout, without generating any files. (for debug purposes).
     */
    virtual void dumpOutputs(std::ostream& out = std::cout) = 0;

    /**
     * Get Relation by its name from relationMap, if relation not found, return a nullptr.
     * @param name The name of the target relation (const std::string).
     * @return The pointer of the target relation, or null pointer if the relation not found (Relation*).
     */
    Relation* getRelation(const std::string& name) const {
        auto it = relationMap.find(name);
        if (it != relationMap.end()) {
            return (*it).second;
        } else {
            return nullptr;
        }
    };

    /**
     * Return the size of the target relation from relationMap.
     * @param name The name of the target relation (const std::string).
     * @return The size of the target relation (std::size_t).
     */
    std::size_t getRelationSize(const std::string& name) const {
        return getRelation(name)->size();
    }

    /**
     * Return the name of the target relation from relationMap.
     * @param name The name of the target relation (const std::string).
     * @return The name of the target relation (std::string).
     */
    std::string getRelationName(const std::string& name) const {
        return getRelation(name)->getName();
    }

    /**
     * /see outputRelations.
     *
     * Getter of outputRelations, which this vector structure contains all output relations.
     * @return outputRelations (std::vector).
     */
    std::vector<Relation*> getOutputRelations() const {
        return outputRelations;
    }

    /**
     * /see inputRelations.
     *
     * Getter of inputRelations, which this vector structure contains all input relations.
     * @return intputRelations (std::vector)
     */
    std::vector<Relation*> getInputRelations() const {
        return inputRelations;
    }

    /**
     * /see internalRelations.
     *
     * Getter of internalRelations, which this vector structure contains all relations
     * that are neither an input relation or an output relation.
     * @return internalRelations (std::vector).
     */
    std::vector<Relation*> getInternalRelations() const {
        return internalRelations;
    }

    /**
     * /see allRelations.
     *
     * Getter of allRelations, which this vector structure contains all relations.
     * @return allRelations (std::vector).
     */
    std::vector<Relation*> getAllRelations() const {
        return allRelations;
    }

    /**
     * TODO
     */
    virtual void executeSubroutine(std::string name, const std::vector<RamDomain>& args,
            std::vector<RamDomain>& ret, std::vector<bool>& retErr) {}

    /**
     * Get the symbol table of the program.
     */
    virtual SymbolTable& getSymbolTable() = 0;

    /**
     * Remove all the tuples from the outputRelations.
     * A relation stores the master-copy collection of tuples and their 
     * indices in tables. (check CompiledRelation.h for more details.)
     * Which table is a single link-list structure. (check Table.h for more details.)
     * When purge() is called, it sets the head and tail of the table to nullptr, and for every elements
     * in the table, set the next element pointer points to the current element itself. 
     */
    void purgeOutputRelations() {
        for (Relation* relation : outputRelations) {
            relation->purge();
        }
    }

    /**
     * Remove all the tuples from the inputRelations.
     * A relation stores the master-copy collection of tuples and their 
     * indices in tables. (check CompiledRelation.h for more details.)
     * Which table is a single link-list structure. (check Table.h for more details.)
     * When purge() is called, it sets the head and tail of the table to nullptr, and for every elements
     * in the table, set the next element pointer points to the current element itself. 
     */
    void purgeInputRelations() {
        for (Relation* relation : inputRelations) {
            relation->purge();
        }
    }

    /**
     * Remove all the tuples from the internalRelations.
     * A relation stores the master-copy collection of tuples and their 
     * indices in tables. (check CompiledRelation.h for more details.)
     * Which table is a single link-list structure. (check Table.h for more details.)
     * When purge() is called, it sets the head and tail of the table to nullptr, and for every elements
     * in the table, set the next element pointer points to the current element itself. 
     */
    void purgeInternalRelations() {
        for (Relation* relation : internalRelations) {
            relation->purge();
        }
    }
};

/**
 * Abstract program factory class.
 */
class ProgramFactory {
protected:
    /**
     * Singly linked-list to store all program factories
     * Note that STL data-structures are not possible due
     * to "static initialization order fiasco (problem)".
     * (The problem of the order static objects get initialized, causing effect
     * such as program access static variables before they initialized.)
     * The static container needs to be a primitive type such as pointer
     * set to NULL.
     * Link to next factory.
     */
    ProgramFactory* link = nullptr;

    /**
     * Name of factory.
     */
    std::string name;

protected:
    /**
     * /brief Constructor.
     *
     * Constructor adds factory to static singly-linked list
     * for registration.
     */
    ProgramFactory(std::string name) : name(std::move(name)) {
        registerFactory(this);
    }

private:
    /**
     * Helper method for creating a factory map, which map key is the name of the program factory, map value
     * is the pointer of the ProgramFactory.
     * @return the factory registration map (std::map).
     */
    static inline std::map<std::string, ProgramFactory*>&
    getFactoryRegistry() { 
        static std::map<std::string, ProgramFactory*>
                factoryReg; 
        return factoryReg;
    }

protected:
    /**
     * Create and insert a factory into the factoryReg map.
     * @param factory Pointer of the program factory (ProgramFactory*).
     */
    static inline void registerFactory(
            ProgramFactory* factory) {
        auto& entry = getFactoryRegistry()[factory->name];
        assert(!entry && "double-linked/defined souffle analyis");
        entry = factory;
    }

    /**
     * Find a factory by its name, return the fatory if found, return nullptr if the
     * factory not found.
     * @param factoryName The factory name (const std::string).
     * @return The pointer of the target program factory, or null pointer if the program factory not found (ProgramFactory*). 
     */
    static inline ProgramFactory* find(const std::string& factoryName) {
        const auto& reg = getFactoryRegistry();
        auto pos = reg.find(factoryName);
        return (pos == reg.end())
                       ? nullptr
                       : pos->second;
    }

    /**
     * Create new instance (abstract).
     */
    virtual SouffleProgram* newInstance() = 0;

public:
    /**
     * /brief Destructor.
     *
     * Destructor of ProgramFactory.
     */
    virtual ~ProgramFactory() = default;

    /**
     * Create an instance by finding the name of the program factory, return nullptr if the instance not found.
     * @param name Instance name (const std::string).
     * @return The new instance(SouffleProgram*), or null pointer if the instance not found.
     */
    static SouffleProgram* newInstance(const std::string& name) {
        ProgramFactory* factory = find(name);
        if (factory != nullptr) { 
            return factory->newInstance(); 
        } else {
            return nullptr;
        }
    }
};
}  // namespace souffle
