/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Interpreter.h
 *
 * Declares the interpreter class for executing RAM programs.
 *
 ***********************************************************************/

#pragma once

#include "InterpreterContext.h"
#include "InterpreterRelation.h"
#include "RamTranslationUnit.h"
#include "RamTypes.h"
#include "RelationRepresentation.h"
#include "Logger.h"
#include "LVMCode.h"
#include "LVMGenerator.h"

#include <atomic>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>
#include <dlfcn.h>
#include <stack>

#define SOUFFLE_DLL "libfunctors.so"

namespace souffle {

class InterpreterProgInterface;

/**
 * Interpreter executing a RAM translation unit
 */
class Interpreter {
public:
    Interpreter(RamTranslationUnit& tUnit) : translationUnit(tUnit) {}
    virtual ~Interpreter() {
        for (auto& x : environment) {
            delete x.second;
        }
    }

    /** Get translation unit */
    RamTranslationUnit& getTranslationUnit() {
        return translationUnit;
    }

    /** Interface for executing the main program */
    virtual void executeMain();
    
    /** Clean the cache of main Program */
    void resetMainProgram() {
        mainProgram.reset();
    }

    /** Clean the cache of subroutine */
    void resetSubroutine(const std::string& name) {
        subroutines.erase(name);
    }

    /** Execute the subroutine */
    void executeSubroutine(const std::string& name, const std::vector<RamDomain>& arguments,
                           std::vector<RamDomain>& returnValues, std::vector<bool>& returnErrors) {
        InterpreterContext ctxt;
        ctxt.setReturnValues(returnValues);
        ctxt.setReturnErrors(returnErrors);
        ctxt.setArguments(arguments);

        if (subroutines.find(name) != subroutines.cend()) {
            execute(subroutines.at(name), ctxt);   
        } else {
            // Parse and cache the progrme
            LVMGenerator generator(translationUnit.getSymbolTable(), translationUnit.getP().getSubroutine(name));
            subroutines.emplace(std::make_pair(name, generator.getCodeStream()));
            execute(subroutines.at(name), ctxt);
        }
    }

    /** Print out the instruction stream */
    void printMain() {
        if (mainProgram.get() == nullptr) {
            LVMGenerator generator(translationUnit.getSymbolTable(), *translationUnit.getP().getMain());
            mainProgram = generator.getCodeStream();
        }
        mainProgram->print();
    }

protected:
    /** relation environment type */
    using relation_map = std::map<std::string, InterpreterRelation*>;
  
    using index_set = btree_multiset<const RamDomain*, InterpreterIndex::comparator, std::allocator<const RamDomain*>, 512>;

    /** Insert Logger */
    void insertTimerAt(size_t index, Logger* timer) {
        if (index >= timers.size()) {
            timers.resize((index + 1) * 2, nullptr);
        }
        timers[index] = timer;
    }
    
    /** Stop and destory logger */
    void stopTimerAt(size_t index) {
        assert(index < timers.size());
        timers[index]->~Logger();
    }

    /** Get symbol table */
    SymbolTable& getSymbolTable() {
        return translationUnit.getSymbolTable();
    }

    /** Get relation map */
    relation_map& getRelationMap() const {
        return const_cast<relation_map&>(environment);
    }

    /** Get Counter */
    int getCounter() {
        return counter;
    }

    /** Increment counter */
    int incCounter() {
        return counter ++;
    }

    /** Increment iteration number */
    void incIterationNumber() {
        iteration++;
    }

    /** Get Iteration Number */
    size_t getIterationNumber() const {
        return iteration;
    }
    /** Reset iteration number */
    void resetIterationNumber() {
        iteration = 0;
    }

    /** TODO not implemented yet */
    void createRelation(const RamRelation& id) { }

    /** Get relation */
    InterpreterRelation& getRelation(const std::string& name) {
        // look up relation
        auto pos = environment.find(name);
        assert(pos != environment.end());
        return *pos->second;
    }

    /** Get relation */
    inline InterpreterRelation& getRelation(const RamRelation& id) {
        return getRelation(id.getName());
    }

    /** Drop relation */  
    void dropRelation(const RamRelation& id) {
        InterpreterRelation& rel = getRelation(id);
        environment.erase(id.getName());
        delete &rel;
    }

    /** Drop relation */
    void dropRelation(const std::string& relName) {
        InterpreterRelation& rel = getRelation(relName);
        environment.erase(relName);
        delete &rel;
    }

    /** Swap relation */   
    void swapRelation(const RamRelation& ramRel1, const RamRelation& ramRel2) {
        InterpreterRelation* rel1 = &getRelation(ramRel1);
        InterpreterRelation* rel2 = &getRelation(ramRel2);
        environment[ramRel1.getName()] = rel2;
        environment[ramRel2.getName()] = rel1;
    }

    /** Swap relation */
    void swapRelation(const std::string& ramRel1, const std::string& ramRel2) {
        InterpreterRelation* rel1 = &getRelation(ramRel1);
        InterpreterRelation* rel2 = &getRelation(ramRel2);
        environment[ramRel1] = rel2;
        environment[ramRel2] = rel1;
    }

    /** load dll */
    void* loadDLL() {
        if (dll == nullptr) {
            // check environment variable
            std::string fname = SOUFFLE_DLL;
            dll = dlopen(SOUFFLE_DLL, RTLD_LAZY);
            if (dll == nullptr) {
                std::cerr << "Cannot find Souffle's DLL" << std::endl;
                exit(1);
            }
        }
        return dll;
    }

    // Lookup for IndexScan iter, resize the vector if idx > size */
    std::pair<index_set::iterator, index_set::iterator>& lookUpIndexScanIterator(size_t idx) {
        if (idx >= indexScanIteratorPool.size()) {
            indexScanIteratorPool.resize((idx+1) * 2);
        }
        return indexScanIteratorPool[idx];
    }


    /** Lookup for Scan iter, resize the vector if idx > size */
    std::pair<InterpreterRelation::iterator, InterpreterRelation::iterator>& lookUpScanIterator(size_t idx) {
        if (idx >= scanIteratorPool.size()) {
            scanIteratorPool.resize((idx+1) * 2);
        }
        return scanIteratorPool[idx];
    }

private:
    friend InterpreterProgInterface;

    /** RAM translation Unit */
    RamTranslationUnit& translationUnit;

    /** subroutines */
    std::map<std::string, std::unique_ptr<LVMCode>> subroutines;

    /** Main program */
    std::unique_ptr<LVMCode> mainProgram = nullptr;

    /** Execute given program */
    void execute(std::unique_ptr<LVMCode>& codeStream, InterpreterContext& ctxt, size_t ip = 0);

    /** Relation Environment */
    relation_map environment;

    /** Value stack */
    std::stack<RamDomain> stack;

    /** counters for atom profiling */
    std::map<std::string, std::map<size_t, size_t>> frequencies;

    /** counters for non-existence check */
    std::map<std::string, std::atomic<size_t>> reads;

    /** List of loggers for logtimer */
    std::vector<Logger*> timers;

    /** counter for $ operator */
    int counter = 0;

    /** iteration number (in a fix-point calculation) */
    size_t iteration = 0;

    /** Dynamic library for user-defined functors */
    void* dll = nullptr;

    /** List of iters for indexScan operation */
    std::vector<std::pair<index_set::iterator, index_set::iterator>> indexScanIteratorPool;

    /** List of iters for Scan operation */
    std::vector<std::pair<InterpreterRelation::iterator, InterpreterRelation::iterator>> scanIteratorPool;

    /** stratum */
    size_t level = 0;

};



}  // end of namespace souffle
