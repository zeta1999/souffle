/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file Interpreter.cpp
 *
 * Implementation of Souffle's interpreter.
 *
 ***********************************************************************/

#include "Interpreter.h"
#include "BTree.h"
#include "BinaryConstraintOps.h"
#include "FunctorOps.h"
#include "Global.h"
#include "IODirectives.h"
#include "IOSystem.h"
#include "InterpreterIndex.h"
#include "InterpreterRecords.h"
#include "Logger.h"
#include "ParallelUtils.h"
#include "ProfileEvent.h"
#include "RamExistenceCheckAnalysis.h"
#include "RamExpression.h"
#include "RamIndexScanKeys.h"
#include "RamNode.h"
#include "RamOperation.h"
#include "RamOperationDepth.h"
#include "RamProgram.h"
#include "RamProvenanceExistenceCheckAnalysis.h"
#include "RamVisitor.h"
#include "ReadStream.h"
#include "SignalHandler.h"
#include "SymbolTable.h"
#include "Util.h"
#include "WriteStream.h"
#include <algorithm>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <exception>
#include <fstream>
#include <functional>
#include <iostream>
#include <memory>
#include <regex>
#include <sstream>
#include <stdexcept>
#include <typeinfo>
#include <utility>
#include <ffi.h>

namespace souffle {


void LowLevelMachine::generatingInstructionStream() {
   const RamStatement& main = *translationUnit.getP().getMain();
   generator(main, 0);
   generator.cleanUp(); 
   generator(main, 0);
   generator.code.push_back(LVM_STOP);
}

void LowLevelMachine::eval() {
   size_t ip = 0;
   auto& code = generator.code;
   auto& symbolTable = generator.symbolTable;
   while (true) {
      switch (this->generator.code[ip]) {
         case LVM_Number:
            stack.push(code[ip+1]);
            ip += 2;
            break;
         case LVM_ElementAccess:
            stack.push(ctxt[code[ip+1]][code[ip+2]]);
            printf("Element Get %s\n", symbolTable.resolve((ctxt[code[ip+1]][code[ip+2]])).c_str());
            ip += 3;
            break;
         case LVM_AutoIncrement:
            incCounter();
            ip += 1;
            break;
         case LVM_OP_ORD:
            //Does nothing
            ip += 1;
            break;
         case LVM_OP_STRLEN: {
            RamDomain relNameId = stack.top();
            stack.pop();
            stack.push(symbolTable.resolve(relNameId).size());
            ip += 1;
            break;
         }
         case LVM_OP_NEG: {
            RamDomain val = stack.top();
            stack.pop();
            stack.push(-val);
            ip += 1;
            break;
         }
         case LVM_OP_BNOT: {
            RamDomain val = stack.top();
            stack.pop();
            stack.push(~val);
            ip += 1;
            break;
         }
         case LVM_OP_LNOT:{
            RamDomain val = stack.top();
            stack.pop();
            stack.push(!val);
            ip += 1;
            break;
         }
         case LVM_OP_TONUMBER: {
            RamDomain val = stack.top();
            stack.pop();
            RamDomain result = 0;
            try {
               result = stord(symbolTable.resolve(val));
            } catch (...) {
               std::cerr << "error: wrong string provided by to_number(\"";
               std::cerr << symbolTable.resolve(val);
               std::cerr << "\") functor.\n";
               raise(SIGFPE);
            }
            stack.push(result);
            ip += 1;
            break;
         }
         case LVM_OP_TOSTRING: {
            RamDomain val = stack.top();
            RamDomain result = symbolTable.lookup(std::to_string(val));
            stack.pop();
            stack.push(result);
            ip += 1;
            break;
         }
         case LVM_OP_ADD: {
            RamDomain x = stack.top();
            stack.pop();
            RamDomain y = stack.top();
            stack.pop();
            stack.push(x + y);
            ip += 1;
            break;
         }
         case LVM_OP_SUB: {
            RamDomain rhs = stack.top(); //rhs was pushed last, so on top
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs - rhs);
            ip += 1;
            break;
         }
         case LVM_OP_MUL: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs * rhs);
            ip += 1;
            break;
         }
         case LVM_OP_DIV: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs / rhs);
            ip += 1;
            break;
         }
         case LVM_OP_EXP: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(std::pow(lhs, rhs));
            ip += 1;
            break;
         }
         case LVM_OP_MOD: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs % rhs);
            ip += 1;
            break;
         }
         case LVM_OP_BAND: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs & rhs);
            ip += 1;
            break;
         }
         case LVM_OP_BOR: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs | rhs);
            ip += 1;
            break;
         }
         case LVM_OP_BXOR: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs ^ rhs);
            ip += 1;
            break;
         }
         case LVM_OP_LAND: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs && rhs);
            ip += 1;
            break;
         }
         case LVM_OP_LOR: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs || rhs);
            ip += 1;
            break;
         }
         case LVM_OP_MAX: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(std::max(lhs, rhs));
            ip += 1;
            break;
         }
         case LVM_OP_MIN: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(std::min(lhs, rhs));
            ip += 1;
            break;
         }
         case LVM_OP_CAT: {
            RamDomain s2 = stack.top();
            stack.pop();
            RamDomain s1 = stack.top();
            stack.pop();
            std::string cat = symbolTable.resolve(s1) + symbolTable.resolve(s2);
            stack.push(symbolTable.lookup(cat));
            ip += 1;
            break;
         }
         case LVM_OP_SUBSTR: {
            RamDomain len = stack.top();
            stack.pop();
            RamDomain idx = stack.top();
            stack.pop();
            RamDomain symbol = stack.top();
            stack.pop();
            std::string str = symbolTable.resolve(symbol);
            std::string sub_str;
            try {
               sub_str = str.substr(idx, len);
            } catch (...) {
               std::cerr << "warning: wrong index position provided by substr(\"";
               std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
            }
            stack.push(symbolTable.lookup(sub_str));

            ip += 1;
            break;
         }
         case LVM_OP_EQ: {
            printf("EQ\n");
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs == rhs);
            ip += 1;
            printf("EQ DONE\n");
            break;
         }
         case LVM_OP_NE: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs != rhs);
            ip += 1;
            break;
         
         }
         case LVM_OP_LT: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs < rhs);
            ip += 1;
            break;
         }
         case LVM_OP_LE: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs <= rhs);
            ip += 1;
            break;
         }
         case LVM_OP_GT: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs > rhs);
            ip += 1;
            break;
         }
         case LVM_OP_GE: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs >= rhs);
            ip += 1;
            break;
         }
         case LVM_OP_MATCH: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();

            const std::string& pattern = symbolTable.resolve(lhs);
            const std::string& text = symbolTable.resolve(rhs);
            bool result = false;

            try {
               result = !std::regex_match(text, std::regex(pattern));
            } catch (...) {
               std::cerr << "warning: wrong pattern provided for match(\"" << pattern << "\",\""
               << text << "\").\n";
            }
            stack.push(result);
            ip += 1;
            break;
         }
         case LVM_OP_NOT_MATCH: {
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();

            const std::string& pattern = symbolTable.resolve(lhs);
            const std::string& text = symbolTable.resolve(rhs);
            bool result = false;

            try {
               result = std::regex_match(text, std::regex(pattern));
            } catch (...) {
               std::cerr << "warning: wrong pattern provided for match(\"" << pattern << "\",\""
               << text << "\").\n";
            }
            stack.push(result);
            ip += 1;
            break;
         }
         case LVM_OP_CONTAINS:{ 
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            const std::string& pattern = symbolTable.resolve(lhs);
            const std::string& text = symbolTable.resolve(rhs);
            stack.push (text.find(pattern) != std::string::npos);
            ip += 1;
            break;
         }
         case LVM_OP_NOT_CONTAINS:{
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            const std::string& pattern = symbolTable.resolve(lhs);
            const std::string& text = symbolTable.resolve(rhs);
            stack.push (text.find(pattern) == std::string::npos);
            ip += 1;
            break;
         }
         case LVM_UserDefinedOperator:{ //TODO Later
            ip += 3;
            break;
         }
         case LVM_PackRecord: { //TODO confirm
            RamDomain arity = stack.top();
            stack.pop();
            RamDomain data[arity];
            for (size_t i = 0; i < arity; ++i) {
               data[arity-i-1] =  stack.top();
               stack.pop();
            }
            
            stack.push(pack(data, arity));
            ip += 2;
            break;
         }
         case LVM_Argument: { //TODO Later: ctxt.getArgument return type
            ctxt.getArgument(code[ip+1]);
            ip += 2;
            break;
         }
         case LVM_Conjunction: { //TODO Confirm, diff with OP_LAND?
            RamDomain rhs = stack.top();
            stack.pop();
            RamDomain lhs = stack.top();
            stack.pop();
            stack.push(lhs && rhs);
            ip += 1;
            break;
         }
         case LVM_Negation: { //TODO Confirm, diff with OP_Neg?
            RamDomain val = stack.top();
            stack.pop();
            stack.push(-val);
            ip += 1;
            break;
         }
         case LVM_EmptinessCheck: {
            printf("EmptinessCheck\n");
            std::string relName = symbolTable.resolve(code[ip+1]);
            stack.push(getRelation(relName).empty());
            ip += 2;
            printf("EmptinessCheck Done\n");
            break;
         }
         case LVM_ExistenceCheck: {
            std::string relName = symbolTable.resolve(code[ip+1]);
            std::string patterns = symbolTable.resolve(code[ip+2]);
            printf("Existence Check %s in %s\n", patterns.c_str(), relName.c_str());
            const InterpreterRelation& rel = getRelation(relName);
            size_t arity = rel.getArity();
            
            //TODO a profile action here
            
            // for total we use the exists test
            if (patterns.find("_") == std::string::npos) {
               RamDomain tuple[arity];
               for (size_t i = 0; i < arity; i++) {
                  tuple[arity-i-1] =  stack.top();
                  stack.pop();   //TODO Confirm, value can never be null. 
                                 //Check visitExistenceCheck
                  printf("%s\t", symbolTable.resolve(tuple[arity-i-1]).c_str());
               }
               putchar('\n');
               stack.push(rel.exists(tuple));
               ip += 3;
               break;
            } else { // for partial we search for lower and upper boundaries
               RamDomain low[arity]; 
               RamDomain high[arity]; 
               
               for (size_t i = 0; i < arity; i++) {
                  if (patterns[arity-i-1] == 'V') {
                     low[arity-i-1] = stack.top();
                     stack.pop(); 
                     high[arity-i-1] = low[arity-i-1];
                  } else {
                     low[arity-i-1] = MIN_RAM_DOMAIN;
                     high[arity-i-1] = MAX_RAM_DOMAIN;   
                  }
               }

               // obtain index TODO do as a function
               SearchColumns res = 0;
               for (size_t i = 0; i < arity; ++i) {
                  if (patterns[i] == 'V') {
                     res |= (1 << i); 
                  } 
               }
               auto idx = rel.getIndex(res);
               auto range = idx->lowerUpperBound(low, high);

               stack.push(range.first != range.second);
               ip += 3;
               break;
            }

            break;
         }
         case LVM_ProvenanceExistenceCheck: { //TODO Later
            ip += 3;
            break;
         }
         case LVM_Constraint: //TODO no need
            break;
         case LVM_Scan: //TODO No need
            printf("Scan\n");
            ip += 1;
            break;
         case LVM_IndexScan:  //TODO Later
            ip += 1;
            break;
         case LVM_UnpackRecord: //TODO Later
            ip += 5;
            break;
         case LVM_Filter: //TODO NO need
            printf("Filter\n");
            ip += 1;
            break;
         case LVM_Project: {
            RamDomain arity = code[ip+1];
            std::string relName = symbolTable.resolve(code[ip+2]);
            RamDomain tuple[arity];
            for (size_t i = 0; i < arity; ++i) {
               tuple[arity-i-1] = stack.top(); 
               stack.pop();
            }
            InterpreterRelation& rel = getRelation(relName);
            printf("Project into %s\n", relName.c_str());
            rel.insert(tuple);
            ip += 3;
            break;
         }
         case LVM_Return: {
            RamDomain size = code[ip+1];
            RamDomain vals[size];
            for (size_t i = 0; i < size; ++i) {
               vals[size-i-1] = stack.top();
               stack.pop(); 
            }
            for (size_t i = 0; i < size; ++i) {
               (vals[i] == 0 ? ctxt.addReturnValue(0, true)
                              :ctxt.addReturnValue(vals[i]));
            }
            ip += 2;
            break;
         }
         case LVM_Sequence: {
            ip += 1;
            break;
         }
         case LVM_Parallel: { //TODO Later
            ip += (1 + code[ip+1] + 1);
            break;
         }
         case LVM_Stop_Parallel: { //TODO later
            ip += 2;
            break;
         }
         case LVM_Loop: {  //TODO Does nothing, for debugging
            ip += 2;            
            break;
         }
         case LVM_Exit: {
            RamDomain val = stack.top(); 
            stack.pop();
            if (val){
               ip = code[ip+1];
               break;
            } 

            ip += 2;
            break;
         }
         case LVM_LogTimer: {
            ip += 1;
            break;
         }
         case LVM_DebugInfo: {
            ip += 1;
            break;
         }
         case LVM_Stratum: {
            printf("Level:%d\n", level++);
            ip += 1;
            break;
         }
         case LVM_Create: {
            //TODO RelationRepresentation is not used here ?? 
            InterpreterRelation* res = nullptr;
            std::string relName = symbolTable.resolve(code[ip+1]);
            printf("%s\n", relName.c_str());
            auto arity = code[ip+2];
            assert(environment.find(relName) == environment.end());
            if (code[ip+3] == LVM_EQREL) {
               res = new InterpreterEqRelation(arity);
            } else {
               res = new InterpreterRelation(arity);
            }
            std::vector<std::string> attributeTypes(arity);
            for (int i = 0; i < code[ip+2]; ++i) {
               attributeTypes.push_back(symbolTable.resolve(code[ip+4+i]));
            }
            res->addAttributes(attributeTypes);
            environment[relName] = res;
            ip += 3 + code[ip+2] + 1;
            break;
         }
         case LVM_Clear: {
            std::string relName = symbolTable.resolve(code[ip+1]);
            auto& rel = getRelation(relName);
            rel.purge();
            ip += 2;
            break;
         }
         case LVM_Drop: {
            std::string relName = symbolTable.resolve(code[ip+1]);
            dropRelation(relName);
            ip += 2;
            break;
         }
         case LVM_LogSize: {
            ip += 2;
            break;
         }
         case LVM_Load: {
            std::string relName = symbolTable.resolve(code[ip+1]);
            auto IOs = generator.IODirectivesPool[code[ip+2]];

            for (auto io : IOs) {
                try {
                    InterpreterRelation& relation = getRelation(relName);
                    std::vector<bool> symbolMask;
                    for (auto& cur : relation.getAttributeTypeQualifiers()) {
                        symbolMask.push_back(cur[0] == 's');
                    }
                    IOSystem::getInstance()
                            .getReader(symbolMask, symbolTable, io,
                                    Global::config().has("provenance"))  //TODO
                            ->readAll(relation);
                } catch (std::exception& e) {
                    std::cerr << "Error loading data: " << e.what() << "\n";
                }
            }
            ip += 3;
            break;
         }
         case LVM_Store: {    //TODO minor bug, the index of the tuple is also printed
            std::string relName = symbolTable.resolve(code[ip+1]);

            InterpreterRelation& r = getRelation(relName);
            for (const RamDomain* c : r) {
               for (size_t i = 0; i < r.getArity(); ++i) {
                  printf("\t%s", symbolTable.resolve(c[i]).c_str());
               }
            }
            putchar('\n');

            auto IOs = generator.IODirectivesPool[code[ip+2]];

            for (auto& io : IOs) {
                try {
                    InterpreterRelation& relation = getRelation(relName);
                    std::vector<bool> symbolMask;
                    for (auto& cur : relation.getAttributeTypeQualifiers()) {
                        symbolMask.push_back(cur[0] == 's');
                    }
                    IOSystem::getInstance().getWriter(symbolMask, symbolTable, io,
                                     Global::config().has("provenance")) //TODO
                            ->writeAll(relation);
                } catch (std::exception& e) {
                    std::cerr << "Error loading data: " << e.what() << "\n";
                }
            }
            ip += 3;
            break;
         }
         case LVM_Fact: {
            std::string relName = symbolTable.resolve(code[ip+1]);
            auto arity = code[ip+2];
            RamDomain tuple[arity];
            for (size_t i = 0; i < arity; ++i) {
               tuple[i] = stack.top();
               stack.pop();
            }
            getRelation(relName).insert(tuple);
            ip += 3;
            break;
         }
         case LVM_Merge: {
            std::string source = symbolTable.resolve(code[ip+1]);
            std::string target = symbolTable.resolve(code[ip+2]);
            // get involved relation
            InterpreterRelation& src = getRelation(source);
            InterpreterRelation& trg = getRelation(target);

            if (dynamic_cast<InterpreterEqRelation*>(&trg)) {
                // expand src with the new knowledge generated by insertion.
                src.extend(trg);
            }
            // merge in all elements
            trg.insert(src);

            ip += 3;
            break;
         }
         case LVM_Swap: {
            std::string firstRel = symbolTable.resolve(code[ip+1]);
            std::string secondRel = symbolTable.resolve(code[ip+2]);
            swapRelation(firstRel, secondRel);
            ip += 3;
            break;
         }
         case LVM_Query: //TODO
            ip += 1;
            break;
         case LVM_Goto: 
            ip = code[ip+1];
            break;
         case LVM_Jmpnz: {
            RamDomain val = stack.top();
            stack.pop();
            size_t t = ip;
            ip = (val != 0 ? code[ip+1] : ip + 2);
            printf("%ld:Jmpnz, next = %ld, val is %d\n",t, ip, val);
            break;
         }
         case LVM_Jmpez: {
            RamDomain val = stack.top();
            stack.pop();
            size_t t = ip;
            ip = (val == 0 ? code[ip+1] : ip + 2);
            printf("%ld:Jmpez, next = %ld, val is %d\n",t, ip, val);
            break;
         }
         case LVM_ITER_TypeScan: {
            printf("ITER_TypeScan\n");
            RamDomain idx = code[ip+1];
            
            lookUpScanIterator(idx);
            std::string relName = symbolTable.resolve(code[ip+2]);
            InterpreterRelation& rel = getRelation(relName);
            scanIteratorPool[idx] = std::pair<InterpreterRelation::iterator, InterpreterRelation::iterator>(rel.begin(), rel.end());

            printf("ITER_TypeScan Done\n");
            ip += 3;
            break;                         
         }
         case LVM_ITER_TypeIndexScan: {
            printf("ITER_TypeIndexScan\n");
            RamDomain idx = code[ip+1];
            std::string relName = symbolTable.resolve(code[ip+2]);
            InterpreterRelation& rel = getRelation(relName);
            std::string pattern = symbolTable.resolve(code[ip+3]);

            // create pattern tuple for range query
            auto arity = rel.size();
            RamDomain low[arity];
            RamDomain hig[arity];
            for (size_t i = 0; i < arity; i++) {
                if (pattern[arity-i-1] == 'V'){
                    low[arity-i-1] = stack.top();
                    stack.pop();
                    hig[arity-i-1] = low[arity-i-1];
                } else {
                    low[arity-i-1] = MIN_RAM_DOMAIN;
                    hig[arity-i-1] = MAX_RAM_DOMAIN;
                }
            }

            // obtain index
            // TODO Do as function
            SearchColumns keys = 0;
            for (size_t i = 0; i < arity; i++) {
               if (pattern[i] == 'V') {
                  keys |= (1 << i);
               }
            }
            auto index = rel.getIndex(keys);

            // get iterator range
            lookUpIndexScanIterator(idx); //TODO Imrpove
            indexScanIteratorPool[idx] = index->lowerUpperBound(low, hig);
            ip += 4;
            printf("ITER_TypeIndexScan Done\n");
            break;
         }
         case LVM_ITER_NotAtEnd: {  //TODO Change name to notAtEnd
            printf("ITER_AtEnd\n");
            RamDomain idx = code[ip+1];
            switch(code[ip+2]) {
               case LVM_ITER_TypeScan: {
                  auto iter = scanIteratorPool[idx];
                  stack.push(iter.first != iter.second);        
                  break;
               } 
               case LVM_ITER_TypeIndexScan: {
                  auto iter = indexScanIteratorPool[idx];
                  stack.push(iter.first != iter.second);        
                  break;
               }
               default:
                  break;
            }   
            printf("ITER_AtEnd Done\n");
            ip += 3;
            break;
         }
         case LVM_ITER_Select: { //TODO improve
            printf("Select\n");
            RamDomain idx = code[ip+1];
            RamDomain id = code[ip+3];
            switch(code[ip+2]) {
               case LVM_ITER_TypeScan: {
                  auto iter = scanIteratorPool[idx];
                  ctxt[id] = *iter.first;
                  break;
               } 
               case LVM_ITER_TypeIndexScan: {
                  auto iter = indexScanIteratorPool[idx];
                  ctxt[id] = *iter.first;
                  break;
               }
               default:
                  break;
            }   
            ip += 4;
            printf("Select Done\n");
            break;
         }
         case LVM_ITER_Inc: {
            printf("Increase Iter\n");
            RamDomain idx = code[ip+1];
            switch(code[ip+2]) {
               case LVM_ITER_TypeScan: {
                  ++scanIteratorPool[idx].first;
                  break;
               } 
               case LVM_ITER_TypeIndexScan: {
                  ++indexScanIteratorPool[idx].first;
                  break;
               }
               default:
                  printf("Unknown iter\n");
                  break;
            }   
            ip += 3;
            break;
         }
         case LVM_Match:{  
            RamDomain idx = code[ip+1];
            RamDomain id = code[ip+3];
            std::string pattern = symbolTable.resolve(code[ip+4]);


            ip += 4;
            break;
         }
         case LVM_LT: //TODO Don't Need
            ip += 1;
            break;
         case LVM_STOP: //TODO
            return;
         default:
            break;
      }
   }
}


/** Evaluate RAM Expression */
RamDomain Interpreter::evalVal(const RamExpression& value, const InterpreterContext& ctxt) {
    class ValueEvaluator : public RamVisitor<RamDomain> {
        Interpreter& interpreter;
        const InterpreterContext& ctxt;

    public:
        ValueEvaluator(Interpreter& interp, const InterpreterContext& ctxt)
                : interpreter(interp), ctxt(ctxt) {}

        RamDomain visitNumber(const RamNumber& num) override {
            return num.getConstant();
        }

        RamDomain visitElementAccess(const RamElementAccess& access) override {
            return ctxt[access.getIdentifier()][access.getElement()];
        }

        RamDomain visitAutoIncrement(const RamAutoIncrement&) override {
            return interpreter.incCounter();
        }

        // intrinsic functors
        RamDomain visitIntrinsicOperator(const RamIntrinsicOperator& op) override {
            const auto& args = op.getArguments();

            switch (op.getOperator()) {
                /** Unary Functor Operators */
                case FunctorOp::ORD:
                    return visit(args[0]);
                case FunctorOp::STRLEN:
                    return interpreter.getSymbolTable().resolve(visit(args[0])).size();
                case FunctorOp::NEG:
                    return -visit(args[0]);
                case FunctorOp::BNOT:
                    return ~visit(args[0]);
                case FunctorOp::LNOT:
                    return !visit(args[0]);
                case FunctorOp::TONUMBER: {
                    RamDomain result = 0;
                    try {
                        result = stord(interpreter.getSymbolTable().resolve(visit(args[0])));
                    } catch (...) {
                        std::cerr << "error: wrong string provided by to_number(\"";
                        std::cerr << interpreter.getSymbolTable().resolve(visit(args[0]));
                        std::cerr << "\") functor.\n";
                        raise(SIGFPE);
                    }
                    return result;
                }
                case FunctorOp::TOSTRING:
                    return interpreter.getSymbolTable().lookup(std::to_string(visit(args[0])));

                /** Binary Functor Operators */
                case FunctorOp::ADD: {
                    return visit(args[0]) + visit(args[1]);
                }
                case FunctorOp::SUB: {
                    return visit(args[0]) - visit(args[1]);
                }
                case FunctorOp::MUL: {
                    return visit(args[0]) * visit(args[1]);
                }
                case FunctorOp::DIV: {
                    return visit(args[0]) / visit(args[1]);
                }
                case FunctorOp::EXP: {
                    return std::pow(visit(args[0]), visit(args[1]));
                }
                case FunctorOp::MOD: {
                    return visit(args[0]) % visit(args[1]);
                }
                case FunctorOp::BAND: {
                    return visit(args[0]) & visit(args[1]);
                }
                case FunctorOp::BOR: {
                    return visit(args[0]) | visit(args[1]);
                }
                case FunctorOp::BXOR: {
                    return visit(args[0]) ^ visit(args[1]);
                }
                case FunctorOp::LAND: {
                    return visit(args[0]) && visit(args[1]);
                }
                case FunctorOp::LOR: {
                    return visit(args[0]) || visit(args[1]);
                }
                case FunctorOp::MAX: {
                    return std::max(visit(args[0]), visit(args[1]));
                }
                case FunctorOp::MIN: {
                    return std::min(visit(args[0]), visit(args[1]));
                }
                case FunctorOp::CAT: {
                    return interpreter.getSymbolTable().lookup(
                            interpreter.getSymbolTable().resolve(visit(args[0])) +
                            interpreter.getSymbolTable().resolve(visit(args[1])));
                }

                /** Ternary Functor Operators */
                case FunctorOp::SUBSTR: {
                    auto symbol = visit(args[0]);
                    const std::string& str = interpreter.getSymbolTable().resolve(symbol);
                    auto idx = visit(args[1]);
                    auto len = visit(args[2]);
                    std::string sub_str;
                    try {
                        sub_str = str.substr(idx, len);
                    } catch (...) {
                        std::cerr << "warning: wrong index position provided by substr(\"";
                        std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
                    }
                    return interpreter.getSymbolTable().lookup(sub_str);
                }

                /** Undefined */
                default: {
                    assert(false && "unsupported operator");
                    return 0;
                }
            }
        }

        RamDomain visitUserDefinedOperator(const RamUserDefinedOperator& op) override {
            // get name and type
            const std::string& name = op.getName();
            const std::string& type = op.getType();

            // load DLL (if not done yet)
            void* handle = interpreter.loadDLL();
            void (*fn)() = (void (*)())dlsym(handle, name.c_str());
            if (fn == nullptr) {
                std::cerr << "Cannot find user-defined operator " << name << " in " << SOUFFLE_DLL
                          << std::endl;
                exit(1);
            }

            // prepare dynamic call environment
            size_t arity = op.getArgCount();
            ffi_cif cif;
            ffi_type* args[arity];
            void* values[arity];
            RamDomain intVal[arity];
            const char* strVal[arity];
            ffi_arg rc;

            /* Initialize arguments for ffi-call */
            for (size_t i = 0; i < arity; i++) {
                RamDomain arg = visit(op.getArgument(i));
                if (type[i] == 'S') {
                    args[i] = &ffi_type_pointer;
                    strVal[i] = interpreter.getSymbolTable().resolve(arg).c_str();
                    values[i] = &strVal[i];
                } else {
                    args[i] = &ffi_type_uint32;
                    intVal[i] = arg;
                    values[i] = &intVal[i];
                }
            }

            // call external function
            if (type[arity] == 'N') {
                // Initialize for numerical return value
                if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arity, &ffi_type_uint32, args) != FFI_OK) {
                    std::cerr << "Failed to prepare CIF for user-defined operator ";
                    std::cerr << name << std::endl;
                    exit(1);
                }
            } else {
                // Initialize for string return value
                if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, arity, &ffi_type_pointer, args) != FFI_OK) {
                    std::cerr << "Failed to prepare CIF for user-defined operator ";
                    std::cerr << name << std::endl;
                    exit(1);
                }
            }
            ffi_call(&cif, fn, &rc, values);
            RamDomain result;
            if (type[arity] == 'N') {
                result = ((RamDomain)rc);
            } else {
                result = interpreter.getSymbolTable().lookup(((const char*)rc));
            }

            return result;
        }

        // -- records --
        RamDomain visitPackRecord(const RamPackRecord& op) override {
            auto values = op.getArguments();
            auto arity = values.size();
            RamDomain data[arity];
            for (size_t i = 0; i < arity; ++i) {
                data[i] = visit(values[i]);
            }
            return pack(data, arity);
        }

        // -- subroutine argument
        RamDomain visitArgument(const RamArgument& arg) override {
            return ctxt.getArgument(arg.getArgCount());
        }

        // -- safety net --

        RamDomain visitNode(const RamNode& node) override {
            std::cerr << "Unsupported node type: " << typeid(node).name() << "\n";
            assert(false && "Unsupported Node Type!");
            return 0;
        }
    };

    // create and run evaluator
    return ValueEvaluator(*this, ctxt)(value);
}

/** Evaluate RAM Condition */
bool Interpreter::evalCond(const RamCondition& cond, const InterpreterContext& ctxt) {
    class ConditionEvaluator : public RamVisitor<bool> {
        Interpreter& interpreter;
        const InterpreterContext& ctxt;
        RamExistenceCheckAnalysis* existCheckAnalysis;
        RamProvenanceExistenceCheckAnalysis* provExistCheckAnalysis;

    public:
        ConditionEvaluator(Interpreter& interp, const InterpreterContext& ctxt)
                : interpreter(interp), ctxt(ctxt),
                  existCheckAnalysis(interp.getTranslationUnit().getAnalysis<RamExistenceCheckAnalysis>()),
                  provExistCheckAnalysis(
                          interp.getTranslationUnit().getAnalysis<RamProvenanceExistenceCheckAnalysis>()) {}

        // -- connectors operators --

        bool visitConjunction(const RamConjunction& conj) override {
            return visit(conj.getLHS()) && visit(conj.getRHS());
        }

        bool visitNegation(const RamNegation& neg) override {
            return !visit(neg.getOperand());
        }

        // -- relation operations --

        bool visitEmptinessCheck(const RamEmptinessCheck& emptiness) override {
            return interpreter.getRelation(emptiness.getRelation()).empty();
        }

        bool visitExistenceCheck(const RamExistenceCheck& exists) override {
            const InterpreterRelation& rel = interpreter.getRelation(exists.getRelation());

            // construct the pattern tuple
            auto arity = rel.getArity();
            auto values = exists.getValues();

            if (Global::config().has("profile") && !exists.getRelation().isTemp()) {
                interpreter.reads[exists.getRelation().getName()]++;
            }
            // for total we use the exists test
            if (existCheckAnalysis->isTotal(&exists)) {
                RamDomain tuple[arity];
                for (size_t i = 0; i < arity; i++) {
                    tuple[i] = (values[i]) ? interpreter.evalVal(*values[i], ctxt) : MIN_RAM_DOMAIN;
                }

                return rel.exists(tuple);
            }

            // for partial we search for lower and upper boundaries
            RamDomain low[arity];
            RamDomain high[arity];
            for (size_t i = 0; i < arity; i++) {
                low[i] = (values[i]) ? interpreter.evalVal(*values[i], ctxt) : MIN_RAM_DOMAIN;
                high[i] = (values[i]) ? low[i] : MAX_RAM_DOMAIN;
            }

            // obtain index
            auto idx = rel.getIndex(existCheckAnalysis->getKey(&exists));
            auto range = idx->lowerUpperBound(low, high);
            return range.first != range.second;  // if there is something => done
        }

        bool visitProvenanceExistenceCheck(const RamProvenanceExistenceCheck& provExists) override {
            const InterpreterRelation& rel = interpreter.getRelation(provExists.getRelation());

            // construct the pattern tuple
            auto arity = rel.getArity();
            auto values = provExists.getValues();

            // for partial we search for lower and upper boundaries
            RamDomain low[arity];
            RamDomain high[arity];
            for (size_t i = 0; i < arity - 2; i++) {
                low[i] = (values[i]) ? interpreter.evalVal(*values[i], ctxt) : MIN_RAM_DOMAIN;
                high[i] = (values[i]) ? low[i] : MAX_RAM_DOMAIN;
            }

            low[arity - 2] = MIN_RAM_DOMAIN;
            low[arity - 1] = MIN_RAM_DOMAIN;
            high[arity - 2] = MAX_RAM_DOMAIN;
            high[arity - 1] = MAX_RAM_DOMAIN;

            // obtain index
            auto idx = rel.getIndex(provExistCheckAnalysis->getKey(&provExists));
            auto range = idx->lowerUpperBound(low, high);
            return range.first != range.second;  // if there is something => done
        }

        // -- comparison operators --
        bool visitConstraint(const RamConstraint& relOp) override {
            RamDomain lhs = interpreter.evalVal(*relOp.getLHS(), ctxt);
            RamDomain rhs = interpreter.evalVal(*relOp.getRHS(), ctxt);
            switch (relOp.getOperator()) {
                case BinaryConstraintOp::EQ:
                    return lhs == rhs;
                case BinaryConstraintOp::NE:
                    return lhs != rhs;
                case BinaryConstraintOp::LT:
                    return lhs < rhs;
                case BinaryConstraintOp::LE:
                    return lhs <= rhs;
                case BinaryConstraintOp::GT:
                    return lhs > rhs;
                case BinaryConstraintOp::GE:
                    return lhs >= rhs;
                case BinaryConstraintOp::MATCH: {
                    RamDomain l = interpreter.evalVal(*relOp.getLHS(), ctxt);
                    RamDomain r = interpreter.evalVal(*relOp.getRHS(), ctxt);
                    const std::string& pattern = interpreter.getSymbolTable().resolve(l);
                    const std::string& text = interpreter.getSymbolTable().resolve(r);
                    bool result = false;
                    try {
                        result = std::regex_match(text, std::regex(pattern));
                    } catch (...) {
                        std::cerr << "warning: wrong pattern provided for match(\"" << pattern << "\",\""
                                  << text << "\").\n";
                    }
                    return result;
                }
                case BinaryConstraintOp::NOT_MATCH: {
                    RamDomain l = interpreter.evalVal(*relOp.getLHS(), ctxt);
                    RamDomain r = interpreter.evalVal(*relOp.getRHS(), ctxt);
                    const std::string& pattern = interpreter.getSymbolTable().resolve(l);
                    const std::string& text = interpreter.getSymbolTable().resolve(r);
                    bool result = false;
                    try {
                        result = !std::regex_match(text, std::regex(pattern));
                    } catch (...) {
                        std::cerr << "warning: wrong pattern provided for !match(\"" << pattern << "\",\""
                                  << text << "\").\n";
                    }
                    return result;
                }
                case BinaryConstraintOp::CONTAINS: {
                    RamDomain l = interpreter.evalVal(*relOp.getLHS(), ctxt);
                    RamDomain r = interpreter.evalVal(*relOp.getRHS(), ctxt);
                    const std::string& pattern = interpreter.getSymbolTable().resolve(l);
                    const std::string& text = interpreter.getSymbolTable().resolve(r);
                    return text.find(pattern) != std::string::npos;
                }
                case BinaryConstraintOp::NOT_CONTAINS: {
                    RamDomain l = interpreter.evalVal(*relOp.getLHS(), ctxt);
                    RamDomain r = interpreter.evalVal(*relOp.getRHS(), ctxt);
                    const std::string& pattern = interpreter.getSymbolTable().resolve(l);
                    const std::string& text = interpreter.getSymbolTable().resolve(r);
                    return text.find(pattern) == std::string::npos;
                }
                default:
                    assert(false && "unsupported operator");
                    return false;
            }
        }

        bool visitNode(const RamNode& node) override {
            std::cerr << "Unsupported node type: " << typeid(node).name() << "\n";
            assert(false && "Unsupported Node Type!");
            return false;
        }
    };

    // run evaluator
    return ConditionEvaluator(*this, ctxt)(cond);
}

/** Evaluate RAM operation */
void Interpreter::evalOp(const RamOperation& op, const InterpreterContext& args) {
    class OperationEvaluator : public RamVisitor<void> {
        Interpreter& interpreter;
        InterpreterContext& ctxt;
        RamIndexScanKeysAnalysis* keysAnalysis;

    public:
        OperationEvaluator(Interpreter& interp, InterpreterContext& ctxt)
                : interpreter(interp), ctxt(ctxt),
                  keysAnalysis(interp.getTranslationUnit().getAnalysis<RamIndexScanKeysAnalysis>()) {}

        // -- Operations -----------------------------

        void visitNestedOperation(const RamNestedOperation& nested) override {
            visit(nested.getOperation());
        }

        void visitSearch(const RamSearch& search) override {
            visitNestedOperation(search);

            if (Global::config().has("profile") && !search.getProfileText().empty()) {
                interpreter.frequencies[search.getProfileText()][interpreter.getIterationNumber()]++;
            }
        }

        void visitScan(const RamScan& scan) override {
            // get the targeted relation
            const InterpreterRelation& rel = interpreter.getRelation(scan.getRelation());

            // use simple iterator
            for (const RamDomain* cur : rel) {
                ctxt[scan.getIdentifier()] = cur;
                visitSearch(scan);
            }

           
        }

        void visitIndexScan(const RamIndexScan& scan) override {
            // get the targeted relation
            const InterpreterRelation& rel = interpreter.getRelation(scan.getRelation());

            // create pattern tuple for range query
            auto arity = rel.getArity();
            RamDomain low[arity];
            RamDomain hig[arity];
            auto pattern = scan.getRangePattern();
            for (size_t i = 0; i < arity; i++) {
                if (pattern[i] != nullptr) {
                    low[i] = interpreter.evalVal(*pattern[i], ctxt);
                    hig[i] = low[i];
                } else {
                    low[i] = MIN_RAM_DOMAIN;
                    hig[i] = MAX_RAM_DOMAIN;
                }
            }

            // obtain index
            auto idx = rel.getIndex(keysAnalysis->getRangeQueryColumns(&scan), nullptr);

            // get iterator range
            auto range = idx->lowerUpperBound(low, hig);

            // conduct range query
            for (auto ip = range.first; ip != range.second; ++ip) {
                const RamDomain* data = *(ip);
                ctxt[scan.getIdentifier()] = data;
                visitSearch(scan);
            }
        }

        void visitUnpackRecord(const RamUnpackRecord& lookup) override {
            // get reference
            RamDomain ref = ctxt[lookup.getReferenceLevel()][lookup.getReferencePosition()];

            // check for null
            if (isNull(ref)) {
                return;
            }

            // update environment variable
            auto arity = lookup.getArity();
            const RamDomain* tuple = unpack(ref, arity);

            // save reference to temporary value
            ctxt[lookup.getIdentifier()] = tuple;

            // run nested part - using base class visitor
            visitSearch(lookup);
        }

        void visitAggregate(const RamAggregate& aggregate) override {
            // get the targeted relation
            const InterpreterRelation& rel = interpreter.getRelation(aggregate.getRelation());

            // initialize result
            RamDomain res = 0;
            switch (aggregate.getFunction()) {
                case RamAggregate::MIN:
                    res = MAX_RAM_DOMAIN;
                    break;
                case RamAggregate::MAX:
                    res = MIN_RAM_DOMAIN;
                    break;
                case RamAggregate::COUNT:
                    res = 0;
                    break;
                case RamAggregate::SUM:
                    res = 0;
                    break;
            }

            // init temporary tuple for this level
            auto arity = rel.getArity();

            // get lower and upper boundaries for iteration
            const auto& pattern = aggregate.getPattern();
            RamDomain low[arity];
            RamDomain hig[arity];

            for (size_t i = 0; i < arity; i++) {
                if (pattern[i] != nullptr) {
                    low[i] = interpreter.evalVal(*pattern[i], ctxt);
                    hig[i] = low[i];
                } else {
                    low[i] = MIN_RAM_DOMAIN;
                    hig[i] = MAX_RAM_DOMAIN;
                }
            }

            // obtain index
            auto idx = rel.getIndex(aggregate.getRangeQueryColumns());

            // get iterator range
            auto range = idx->lowerUpperBound(low, hig);

            // check for emptiness
            if (aggregate.getFunction() != RamAggregate::COUNT) {
                if (range.first == range.second) {
                    return;  // no elements => no min/max
                }
            }

            // iterate through values
            for (auto ip = range.first; ip != range.second; ++ip) {
                // link tuple
                const RamDomain* data = *(ip);
                ctxt[aggregate.getIdentifier()] = data;

                // count is easy
                if (aggregate.getFunction() == RamAggregate::COUNT) {
                    ++res;
                    continue;
                }

                // aggregation is a bit more difficult

                // eval target expression
                RamDomain cur = interpreter.evalVal(*aggregate.getExpression(), ctxt);

                switch (aggregate.getFunction()) {
                    case RamAggregate::MIN:
                        res = std::min(res, cur);
                        break;
                    case RamAggregate::MAX:
                        res = std::max(res, cur);
                        break;
                    case RamAggregate::COUNT:
                        res = 0;
                        break;
                    case RamAggregate::SUM:
                        res += cur;
                        break;
                }
            }

            // write result to environment
            RamDomain tuple[1];
            tuple[0] = res;
            ctxt[aggregate.getIdentifier()] = tuple;

            // run nested part - using base class visitor
            visitSearch(aggregate);
        }

        void visitFilter(const RamFilter& filter) override {
            // check condition
            if (interpreter.evalCond(filter.getCondition(), ctxt)) {
                // process nested
                visitNestedOperation(filter);
            }

            if (Global::config().has("profile") && !filter.getProfileText().empty()) {
                interpreter.frequencies[filter.getProfileText()][interpreter.getIterationNumber()]++;
            }
        }

        void visitProject(const RamProject& project) override {
            // create a tuple of the proper arity (also supports arity 0)
            auto arity = project.getRelation().getArity();
            const auto& values = project.getValues();
            RamDomain tuple[arity];
            for (size_t i = 0; i < arity; i++) {
                assert(values[i]);
                tuple[i] = interpreter.evalVal(*values[i], ctxt);
            }

            // insert in target relation
            InterpreterRelation& rel = interpreter.getRelation(project.getRelation());
            rel.insert(tuple);
        }

        // -- return from subroutine --
        void visitReturn(const RamReturn& ret) override {
            for (auto val : ret.getValues()) {
                if (val == nullptr) {
                    ctxt.addReturnValue(0, true);
                } else {
                    ctxt.addReturnValue(interpreter.evalVal(*val, ctxt));
                }
            }
        }

        // -- safety net --
        void visitNode(const RamNode& node) override {
            std::cerr << "Unsupported node type: " << typeid(node).name() << "\n";
            assert(false && "Unsupported Node Type!");
        }
    };

    // create and run interpreter for operations
    InterpreterContext ctxt(translationUnit.getAnalysis<RamOperationDepthAnalysis>()->getDepth(&op));
    ctxt.setReturnValues(args.getReturnValues());
    ctxt.setReturnErrors(args.getReturnErrors());
    ctxt.setArguments(args.getArguments());
    OperationEvaluator(*this, ctxt).visit(op);
}

/** Evaluate RAM statement */
void Interpreter::evalStmt(const RamStatement& stmt) {
    class StatementEvaluator : public RamVisitor<bool> {
        Interpreter& interpreter;

    public:
        StatementEvaluator(Interpreter& interp) : interpreter(interp) {}

        // -- Statements -----------------------------

        bool visitSequence(const RamSequence& seq) override {
            // process all statements in sequence
            for (const auto& cur : seq.getStatements()) {
                if (!visit(cur)) {
                    return false;
                }
            }

            // all processed successfully
            return true;
        }

        bool visitParallel(const RamParallel& parallel) override {
            // get statements to be processed in parallel
            const auto& stmts = parallel.getStatements();

            // special case: empty
            if (stmts.empty()) {
                return true;
            }

            // special handling for a single child
            if (stmts.size() == 1) {
                return visit(stmts[0]);
            }

            // parallel execution
            bool cond = true;
#pragma omp parallel for reduction(&& : cond)
            for (size_t i = 0; i < stmts.size(); i++) {
                cond = cond && visit(stmts[i]);
            }
            return cond;
        }

        bool visitLoop(const RamLoop& loop) override {
            interpreter.resetIterationNumber();
            while (visit(loop.getBody())) {
                interpreter.incIterationNumber();
            }
            interpreter.resetIterationNumber();
            return true;
        }

        bool visitExit(const RamExit& exit) override {
            return !interpreter.evalCond(exit.getCondition());
        }

        bool visitLogTimer(const RamLogTimer& timer) override {
            if (timer.getRelation() == nullptr) {
                Logger logger(timer.getMessage().c_str(), interpreter.getIterationNumber());
                return visit(timer.getStatement());
            } else {
                const InterpreterRelation& rel = interpreter.getRelation(*timer.getRelation());
                Logger logger(timer.getMessage().c_str(), interpreter.getIterationNumber(),
                        std::bind(&InterpreterRelation::size, &rel));
                return visit(timer.getStatement());
            }
        }

        bool visitDebugInfo(const RamDebugInfo& dbg) override {
            SignalHandler::instance()->setMsg(dbg.getMessage().c_str());
            return visit(dbg.getStatement());
        }

        bool visitStratum(const RamStratum& stratum) override {
            // TODO (lyndonhenry): should enable strata as subprograms for interpreter here

            // Record relations created in each stratum
            if (Global::config().has("profile")) {
                std::map<std::string, size_t> relNames;
                visitDepthFirst(stratum, [&](const RamCreate& create) {
                    relNames[create.getRelation().getName()] = create.getRelation().getArity();
                });
                for (const auto& cur : relNames) {
                    // Skip temporary relations, marked with '@'
                    if (cur.first[0] == '@') {
                        continue;
                    }
                    ProfileEventSingleton::instance().makeStratumRecord(
                            stratum.getIndex(), "relation", cur.first, "arity", std::to_string(cur.second));
                }
            }
            return visit(stratum.getBody());
        }

        bool visitCreate(const RamCreate& create) override {
            interpreter.createRelation(create.getRelation());
            return true;
        }

        bool visitClear(const RamClear& clear) override {
            InterpreterRelation& rel = interpreter.getRelation(clear.getRelation());
            rel.purge();
            return true;
        }

        bool visitDrop(const RamDrop& drop) override {
            interpreter.dropRelation(drop.getRelation());
            return true;
        }

        bool visitLogSize(const RamLogSize& size) override {
            const InterpreterRelation& rel = interpreter.getRelation(size.getRelation());
            ProfileEventSingleton::instance().makeQuantityEvent(
                    size.getMessage(), rel.size(), interpreter.getIterationNumber());
            return true;
        }

        bool visitLoad(const RamLoad& load) override {
            for (IODirectives ioDirectives : load.getIODirectives()) {
                try {
                    InterpreterRelation& relation = interpreter.getRelation(load.getRelation());
                    std::vector<bool> symbolMask;
                    for (auto& cur : load.getRelation().getAttributeTypeQualifiers()) {
                        symbolMask.push_back(cur[0] == 's');
                    }
                    IOSystem::getInstance()
                            .getReader(symbolMask, interpreter.getSymbolTable(), ioDirectives,
                                    Global::config().has("provenance"))
                            ->readAll(relation);
                } catch (std::exception& e) {
                    std::cerr << "Error loading data: " << e.what() << "\n";
                }
            }
            return true;
        }
        bool visitStore(const RamStore& store) override {
            for (IODirectives ioDirectives : store.getIODirectives()) {
                try {
                    std::vector<bool> symbolMask;
                    for (auto& cur : store.getRelation().getAttributeTypeQualifiers()) {
                        symbolMask.push_back(cur[0] == 's');
                    }
                    IOSystem::getInstance()
                            .getWriter(symbolMask, interpreter.getSymbolTable(), ioDirectives,
                                    Global::config().has("provenance"))
                            ->writeAll(interpreter.getRelation(store.getRelation()));
                } catch (std::exception& e) {
                    std::cerr << e.what();
                    exit(1);
                }
            }
            return true;
        }

        bool visitFact(const RamFact& fact) override {
            auto arity = fact.getRelation().getArity();
            RamDomain tuple[arity];
            auto values = fact.getValues();

            for (size_t i = 0; i < arity; ++i) {
                tuple[i] = interpreter.evalVal(*values[i]);
            }

            interpreter.getRelation(fact.getRelation()).insert(tuple);
            return true;
        }

        bool visitQuery(const RamQuery& insert) override {
            // run generic query executor

            const RamCondition* c = insert.getCondition();
            if (c != nullptr) {
                if (interpreter.evalCond(*insert.getCondition())) {
                    interpreter.evalOp(insert.getOperation());
                }
            } else {
                interpreter.evalOp(insert.getOperation());
            }
            return true;
        }

        bool visitMerge(const RamMerge& merge) override {
            // get involved relation
            InterpreterRelation& src = interpreter.getRelation(merge.getSourceRelation());
            InterpreterRelation& trg = interpreter.getRelation(merge.getTargetRelation());

            if (dynamic_cast<InterpreterEqRelation*>(&trg)) {
                // expand src with the new knowledge generated by insertion.
                src.extend(trg);
            }
            // merge in all elements
            trg.insert(src);

            // done
            return true;
        }

        bool visitSwap(const RamSwap& swap) override {
            interpreter.swapRelation(swap.getFirstRelation(), swap.getSecondRelation());
            return true;
        }

        // -- safety net --

        bool visitNode(const RamNode& node) override {
            auto lease = getOutputLock().acquire();
            (void)lease;
            std::cerr << "Unsupported node type: " << typeid(node).name() << "\n";
            assert(false && "Unsupported Node Type!");
            return false;
        }
    };

    // create and run interpreter for statements
    StatementEvaluator(*this).visit(stmt);
}

/** Execute main program of a translation unit */
void Interpreter::executeMain() {
    SignalHandler::instance()->set();
    if (Global::config().has("verbose")) {
        SignalHandler::instance()->enableLogging();
    }
    const RamStatement& main = *translationUnit.getP().getMain();

    if (!Global::config().has("profile")) {
        evalStmt(main);
    } else {
        ProfileEventSingleton::instance().setOutputFile(Global::config().get("profile"));
        // Prepare the frequency table for threaded use
        visitDepthFirst(main, [&](const RamSearch& node) {
            if (!node.getProfileText().empty()) {
                frequencies.emplace(node.getProfileText(), std::map<size_t, size_t>());
            }
        });
        // Enable profiling for execution of main
        ProfileEventSingleton::instance().startTimer();
        ProfileEventSingleton::instance().makeTimeEvent("@time;starttime");
        // Store configuration
        for (const auto& cur : Global::config().data()) {
            ProfileEventSingleton::instance().makeConfigRecord(cur.first, cur.second);
        }
        // Store count of relations
        size_t relationCount = 0;
        visitDepthFirst(main, [&](const RamCreate& create) {
            if (create.getRelation().getName()[0] != '@') {
                ++relationCount;
                reads[create.getRelation().getName()] = 0;
            }
        });
        ProfileEventSingleton::instance().makeConfigRecord("relationCount", std::to_string(relationCount));

        // Store count of rules
        size_t ruleCount = 0;
        visitDepthFirst(main, [&](const RamQuery& rule) { ++ruleCount; });
        ProfileEventSingleton::instance().makeConfigRecord("ruleCount", std::to_string(ruleCount));

        evalStmt(main);
        ProfileEventSingleton::instance().stopTimer();
        for (auto const& cur : frequencies) {
            for (auto const& iter : cur.second) {
                ProfileEventSingleton::instance().makeQuantityEvent(cur.first, iter.second, iter.first);
            }
        }
        for (auto const& cur : reads) {
            ProfileEventSingleton::instance().makeQuantityEvent(
                    "@relation-reads;" + cur.first, cur.second, 0);
        }
    }
    SignalHandler::instance()->reset();
}

/** Execute subroutine */
void Interpreter::executeSubroutine(const RamStatement& stmt, const std::vector<RamDomain>& arguments,
        std::vector<RamDomain>& returnValues, std::vector<bool>& returnErrors) {
    InterpreterContext ctxt;
    ctxt.setReturnValues(returnValues);
    ctxt.setReturnErrors(returnErrors);
    ctxt.setArguments(arguments);

    // run subroutine
    const RamOperation& op = static_cast<const RamQuery&>(stmt).getOperation();
    evalOp(op, ctxt);
}

void LowLevelMachine::print() {
   size_t ip = 0;
   auto& code = generator.code;
   auto& symbolTable = generator.symbolTable;
   while (true) {
      switch (this->generator.code[ip]) {
         case LVM_Number:
            printf("%ld\tLVM_Number\t%d\n", ip, code[ip+1]);
            ip += 2;
            break;
         case LVM_ElementAccess:
            printf("%ld\tLVM_ElementAccess\t%d\t%d\n",
                  ip, code[ip+1], code[ip+2]);
            ip += 3;
            break;
         case LVM_AutoIncrement:
            printf("%ld\tLVM_AutoIncrement\t\n", ip);
            ip += 1;
            break;
         case LVM_OP_ORD:
            printf("%ld\tLVM_OP_PRD\t\n", ip);
            ip += 1;
            break;
         case LVM_OP_STRLEN: {
            printf("%ld\tLVM_OP_STRLEN\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_NEG: {
            printf("%ld\tLVM_OP_NEG\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_BNOT: {
            printf("%ld\tLVM_OP_BNOT\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_LNOT:{
            printf("%ld\tLVM_OP_LNOT\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_TONUMBER: {
            printf("%ld\tLVM_OP_TONUMBER\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_TOSTRING: {
            printf("%ld\tLVM_OP_TOSTRING\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_ADD: {
            printf("%ld\tLVM_OP_ADD\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_SUB: {
            printf("%ld\tLVM_OP_SUB\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_MUL: {
            printf("%ld\tLVM_OP_MUL\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_DIV: {
            printf("%ld\tLVM_OP_DIV\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_EXP: {
            printf("%ld\tLVM_OP_EXP\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_MOD: {
            printf("%ld\tLVM_OP_MOD\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_BAND: {
            printf("%ld\tLVM_OP_BAND\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_BOR: {
            printf("%ld\tLVM_OP_BOR\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_BXOR: {
            printf("%ld\tLVM_OP_BXOR\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_LAND: {
            printf("%ld\tLVM_OP_LAND\t\n", ip);
            ip += 1; 
            break;
         }
         case LVM_OP_LOR: {
            printf("%ld\tLVM_OP_LOR\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_MAX: {
            printf("%ld\tLVM_OP_MAX\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_MIN: {
            printf("%ld\tLVM_OP_MIN\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_CAT: {
            printf("%ld\tLVM_OP_CAT\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_SUBSTR: {
            printf("%ld\tLVM_OP_SUBSTR\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_EQ: {
            printf("%ld\tLVM_OP_EQ\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_NE: {
            printf("%ld\tLVM_OP_NE\t\n", ip);
            ip += 1;
            break;
         
         }
         case LVM_OP_LT: {
            printf("%ld\tLVM_OP_LT\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_LE: {
            printf("%ld\tLVM_OP_LE\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_GT: {
            printf("%ld\tLVM_OP_GT\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_GE: {
            printf("%ld\tLVM_OP_GE\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_MATCH: {
            printf("%ld\tLVM_OP_MATCH\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_NOT_MATCH: {
            printf("%ld\tLVM_OP_NOT_MATCH\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_CONTAINS:{ 
            printf("%ld\tLVM_OP_CONTAINS\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_OP_NOT_CONTAINS:{
            printf("%ld\tLVM_OP_CONTAINS\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_UserDefinedOperator:{ //TODO Later
            printf("%ld\tLVM_UserDefinedOperator\n", ip);
            printf("\t%s\t%s\t\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  symbolTable.resolve(code[ip+2]).c_str());
            ip += 3;
            break;
         }
         case LVM_PackRecord: { 
            printf("%ld\tLVM_PackRecord\t%d\n", ip, code[ip+1]);
            ip += 2;
            break;
         }
         case LVM_Argument: { 
            printf("%ld\tLVM_Argument\t%d\n", ip, code[ip+1]);
            ip += 2;
            break;
         }
         case LVM_Conjunction: { 
            printf("%ld\tLVM_Conjunction\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_Negation: { 
            printf("%ld\tLVM_Negation\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_EmptinessCheck: {
            printf("%ld\tLVM_EmptinessCheck\t\n", ip);
            printf("\t%s\n",
                  symbolTable.resolve(code[ip+1]).c_str());
            ip += 2;
            break;
         }
         case LVM_ExistenceCheck: {
            printf("%ld\tLVM_ExistenceCheck\t\n", ip);
            printf("\t%s\t%s\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  symbolTable.resolve(code[ip+2]).c_str());
            ip += 3;
            break;
         }
         case LVM_ProvenanceExistenceCheck: { //TODO Later
            printf("%ld\tLVM_ProvenanceExitenceChekck\t\n", ip);
            printf("\t%s\t%s\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  symbolTable.resolve(code[ip+2]).c_str());
            ip += 3;
            break;
         }
         case LVM_Constraint:
            break;
         case LVM_Scan:
            printf("%ld\tLVM_Scan\t\n", ip);
            ip += 1;
            break;
         case LVM_IndexScan:
            printf("%ld\tLVM_IndexScan\t\n", ip);
            ip += 1;
            break;
         case LVM_UnpackRecord:
            printf("%ld\tLVM_UnpackRecord\t%d %d %d %d\t\n",
                  ip, code[ip+1], code[ip+2], code[ip+3], code[ip+4]);
            ip += 5;
            break;
         case LVM_Filter:
            printf("%ld\tLVM_Filter\t\n", ip);
            ip += 1;
            break;
         case LVM_Project:
            printf("%ld\tLVM_Project\t%d\t\n", ip, code[ip+1]);
            printf("\t%s\t\n",
                  symbolTable.resolve(code[ip+2]).c_str());
            ip += 3;
            break;
         case LVM_Return: {
            printf("%ld\tLVM_Return\t%d\t\n", ip, code[ip+1]);
            ip += 2;
            break;
         }
         case LVM_Sequence: {
            printf("%ld\tLVM_Sequence\n", ip);
            ip += 1;
            break;
         }
         case LVM_Parallel: { 
            printf("%ld\tLVM_Parallel\t%d\t\n", ip, code[ip+1]);
            for (int i = 0; i < code[ip+1]; ++i) {
               printf("%d\t", code[ip+i]);
            }
            putchar('\n');
            ip += (1 + code[ip+1] + 1);
            break;
         }
         case LVM_Stop_Parallel: { 
            printf("%ld\tLVM_Stop_Parallel\t%d\t\n", ip, code[ip+1]);
            ip += 2;
            break;
         }
         case LVM_Loop: {
            printf("%ld\tLVM_LOOP\n",ip);
            ip += 1;
            break;
         }
         case LVM_Exit: {
            printf("%ld\tLVM_Exit\t%d\n", ip, code[ip+1]);
            ip += 2;
            break;
         }
         case LVM_LogTimer: {
            printf("%ld\tLVM_LogTimer\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_DebugInfo: {
            printf("%ld\tLVM_DebugInfo\t\n", ip);
            ip += 1;
            break;
         }
         case LVM_Stratum: 
            printf("%ld\tLVM_Stratum\t%d\n", ip, level++);
            ip += 1;
            break;
         case LVM_Create: {
            printf("%ld\tLVM_Create\t Name:%s Arity:%d Struct:%d\n", ip, 
                  symbolTable.resolve(code[ip+1]).c_str(),
                  code[ip+2], code[ip+3]);
            for (int i = 0; i < code[ip+2]; ++i) {
               printf("\t%s", symbolTable.resolve(code[ip+4+i]).c_str());
            }
            putchar('\n');
            ip += 3 + code[ip+2] + 1;
            break;
         }
         case LVM_Clear: {
            printf("%ld\tLVM_Clear\t\n", ip);
            printf("\t%s\t\n",
                  symbolTable.resolve(code[ip+1]).c_str());
            ip += 2;
            break;
         }
         case LVM_Drop: {
            printf("%ld\tLVM_Drop\t\n", ip);
            printf("\t%s\t\n",
                  symbolTable.resolve(code[ip+1]).c_str());
            ip += 2;
            break;
         }
         case LVM_LogSize: {
            printf("%ld\tLVM_LogSize\t\n", ip);
            printf("\t%s\t\n",
                  symbolTable.resolve(code[ip+1]).c_str());
            ip += 2;
            break;
         }
         case LVM_Load: {
            printf("%ld\tLVM_Load\t\n", ip);
            printf("\t%s\tIOidx:%d\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  code[ip+2]);
            ip += 3;
            break;
         }
         case LVM_Store:
            printf("%ld\tLVM_Store\t\n", ip);
            printf("\t%s\tIOidx:%d\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  code[ip+2]);
            ip += 3;
            break;
         case LVM_Fact: {
            printf("%ld\tLVM_Fact\t\n", ip);
            printf("\t%s\t%d\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  code[ip+2]);
            ip += 3;
            break;
         }
         case LVM_Merge: {
            printf("%ld\tLVM_Merge\t\n", ip);
            printf("\t%s\t%s\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  symbolTable.resolve(code[ip+2]).c_str());
            ip += 3;
            break;
         }
         case LVM_Swap: {
            printf("%ld\tLVM_Swap\t\n", ip);
            printf("\t%s\t%s\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  symbolTable.resolve(code[ip+2]).c_str());
            ip += 3;
            break;
         }
         case LVM_Query: 
            printf("%ld\tLVM_Query\t\n", ip);
            ip += 1;
            break;
         case LVM_Goto: 
            printf("%ld\tLVM_GOTO\t%d\n", ip, code[ip+1]);
            ip += 2;
            break;
         case LVM_Jmpnz: 
            printf("%ld\tLVM_Jmpnz\t%d\n", ip, code[ip+1]);
            ip += 2;
            break;
         case LVM_Jmpez: 
            printf("%ld\tLVM_Jmpez\t%d\n", ip, code[ip+1]);
            ip += 2;
            break;
         case LVM_ITER_TypeIndexScan:
            printf("%ld\tLVM_ITER_TypeIndexScan\t%s\n", ip, symbolTable.resolve(code[ip+2]).c_str());
            ip += 4;
            break;
         case LVM_ITER_TypeScan: {
            printf("%ld\tLVM_ITER_TypeScan\t%s\n", ip, symbolTable.resolve(code[ip+2]).c_str());
            ip += 3;
            break;                         
         }
         case LVM_ITER_NotAtEnd:
            printf("%ld\tLVM_AtEnd\t%d\tType:%d\n", ip, code[ip+1], code[ip+2]);
            ip += 3;
            break;
         case LVM_ITER_Select:   
            printf("%ld\tLVM_ITER_Select\t\n", ip);
            printf("\t%d\t%d\t%d\n",
                  code[ip+1],
                  code[ip+2],
                  code[ip+3]);
            ip += 4;
            break;
         case LVM_ITER_Inc:   
            printf("%ld\tLVM_ITER_Inc\tIter:%d\tType:%d\n", ip, code[ip+1], code[ip+2]);
            ip += 3;
            break;
         case LVM_Match:   
            printf("%ld\tLVM_Match\t\n", ip);
            printf("\t%s\t%d\t%s\n",
                  symbolTable.resolve(code[ip+1]).c_str(),
                  code[ip+2],
                  symbolTable.resolve(code[ip+3]).c_str());
            ip += 4;
            break;
         case LVM_LT:
            printf("%ld\tLVM_LT\n", ip);
            ip += 1;
            break;
         case LVM_STOP: 
            printf("%ld\tLVM_STOP\n", ip);
            level = 0;
            return;
         default:
            printf("Unkown\n");
            break;
      }
   }
}

}  // end of namespace souffle
