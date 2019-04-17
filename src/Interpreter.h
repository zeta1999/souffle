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
#include "RamCondition.h"
#include "RamRelation.h"
#include "RamStatement.h"
#include "RamTranslationUnit.h"
#include "RamTypes.h"
#include "RelationRepresentation.h"
#include "RamVisitor.h" 
#include "RamOperationDepth.h" 
#include "Logger.h"

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

enum LVM_Type {
    // Expressions
    LVM_Number,
    LVM_ElementAccess,
    LVM_AutoIncrement,

    /** Unary Functor Operations */
    LVM_OP_ORD,
    LVM_OP_STRLEN,
    LVM_OP_NEG,
    LVM_OP_BNOT,
    LVM_OP_LNOT,
    LVM_OP_TONUMBER,
    LVM_OP_TOSTRING,
    /** Binary Functor Operators */ 
    LVM_OP_ADD,
    LVM_OP_SUB,
    LVM_OP_MUL,
    LVM_OP_DIV,
    LVM_OP_EXP,
    LVM_OP_MOD,
    LVM_OP_BAND,
    LVM_OP_BOR,
    LVM_OP_BXOR,
    LVM_OP_LAND,
    LVM_OP_LOR,
    LVM_OP_MAX,
    LVM_OP_MIN,
    LVM_OP_CAT,
    /** Ternary Functor Operators */
    LVM_OP_SUBSTR,

    // LVM Constraint Op
    LVM_OP_EQ,
    LVM_OP_NE,
    LVM_OP_LT,
    LVM_OP_LE,
    LVM_OP_GT,
    LVM_OP_GE,
    LVM_OP_MATCH,
    LVM_OP_NOT_MATCH,
    LVM_OP_CONTAINS,
    LVM_OP_NOT_CONTAINS,

    LVM_UserDefinedOperator,
    LVM_PackRecord,
    LVM_Argument,
    LVM_Aggregate_COUNT,
    LVM_Aggregate_Return,

    // LVM Conditions
    LVM_Conjunction,
    LVM_Negation,
    LVM_EmptinessCheck,
    LVM_ExistenceCheck,
    LVM_ProvenanceExistenceCheck,
    LVM_Constraint,

    // LVM Operations;
    LVM_Scan,
    LVM_IndexScan,
    LVM_UnpackRecord,
    LVM_Aggregate,
    LVM_Filter,
    LVM_Project,
    LVM_Return,
    LVM_Search,

    // LVM Stmts
    LVM_Sequence,
    LVM_Parallel,
    LVM_Stop_Parallel,
    LVM_Loop,
    LVM_IncIterationNumber,
    LVM_ResetIterationNumber,
    LVM_Exit,
    LVM_LogTimer,
    LVM_StopLogTimer,
    LVM_DebugInfo,
    LVM_Stratum,
    LVM_Create,
    LVM_Clear,
    LVM_Drop,
    LVM_LogSize,
    LVM_Load,
    LVM_Store,
    LVM_Fact,
    LVM_Merge,
    LVM_Swap,
    LVM_Query,

    // LVM internal operation
    LVM_Goto,               
    LVM_Jmpnz,             
    LVM_Jmpez,            
    LVM_ITER_Select,        
    LVM_ITER_Inc,          
    LVM_ITER_NotAtEnd,    
    LVM_STOP,
    LVM_NOP,    // Padding operation

    // LVM Relation Struct Representation
    LVM_BTREE,
    LVM_BRIE,
    LVM_EQREL,
    LVM_DEFAULT,

    LVM_ITER_TypeScan,
    LVM_ITER_TypeIndexScan,
};

class InterpreterProgInterface;
class RamOperation;
class RamExpression;
class SymbolTable;
class LVMGenerator;

/**
 * Interpreter executing a RAM translation unit
 */

class Interpreter {
    class LVMGenerator : public RamVisitor<void, size_t> {
    public:
       LVMGenerator(SymbolTable& symbolTable, const RamStatement& entry) : symbolTable(symbolTable) {
           (*this)(entry, 0);
           (*this).cleanUp(); 
           (*this)(entry, 0);
           code.push_back(LVM_STOP);
       }

       void cleanUp() {
          code.clear();
          IODirectivesPool.clear();  
          currentAddressLabel = 0;
          scanIteratorIndex = 0;
          indexScanIteratorIndex = 0;
       }

       std::vector<RamDomain> code;            /** Instructions stream */
       SymbolTable& symbolTable;                /** Class for converting string to number and vice versa */ 
       
       /** Store reference to IODirectives */
       std::vector<std::vector<IODirectives>> IODirectivesPool;
       size_t IODirectivesCounter = 0;

       /** Address Table */
       size_t currentAddressLabel = 0;
       size_t getNewAddressLabel() { return currentAddressLabel++; }
       std::vector<size_t> addressMap;

       /** Iter */
       size_t scanIteratorIndex = 0;
       size_t indexScanIteratorIndex = 0;
       size_t getNewScanIterator() {return scanIteratorIndex++; }
       size_t getNewIndexScanIterator() {return indexScanIteratorIndex++; }

       /** Timer */
       size_t timerIndex = 0;
       size_t getNewTimer() {return timerIndex++; }

       /* Return the value of the addressLabel. 
        * Return 0 if label doesn't exits. 
        */
       size_t lookupAddress(size_t addressLabel) {
          if (addressLabel < addressMap.size()) {
             return addressMap[addressLabel];
          }
          return 0;
       }
        
       /** Set the value of address label */
       void setAddress(size_t addressLabel, size_t value) {
          if (addressLabel >= addressMap.size()) {
             addressMap.resize((addressLabel + 1) * 2); 
          } 
          addressMap[addressLabel] = value;
       }

       // Visit RAM Expressions
       void visitNumber(const RamNumber& num, size_t exitAddress) override {
          code.push_back(LVM_Number); 
          code.push_back(num.getConstant());
       } 

       void visitElementAccess(const RamElementAccess& access, size_t exitAddress) override {
          code.push_back(LVM_ElementAccess); 
          code.push_back(access.getIdentifier()); 
          code.push_back(access.getElement()); 
       }
       
       void visitAutoIncrement(const RamAutoIncrement& inc, size_t exitAddress) override {
          code.push_back(LVM_AutoIncrement); 
       }
       

       void visitIntrinsicOperator(const RamIntrinsicOperator& op, size_t exitAddress) override {
          const auto& args = op.getArguments();
          switch (op.getOperator()) {
             /** Unary Functor Operators */
             case FunctorOp::ORD:       
                visit(args[0], exitAddress); 
                code.push_back(LVM_OP_ORD);       
                break;
             case FunctorOp::STRLEN:    
                visit(args[0], exitAddress); 
                code.push_back(LVM_OP_STRLEN);    
                break;
             case FunctorOp::NEG:       
                visit(args[0], exitAddress); 
                code.push_back(LVM_OP_NEG);       
                break;
             case FunctorOp::BNOT:      
                visit(args[0], exitAddress); 
                code.push_back(LVM_OP_BNOT);
                break;
             case FunctorOp::LNOT:      
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_LNOT);
                break;
             case FunctorOp::TONUMBER:  
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_TONUMBER);
                break;
             case FunctorOp::TOSTRING:  
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_TOSTRING);
                break;

             /** Binary Functor Operators */ 
             case FunctorOp::ADD:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_ADD);
                break;
             case FunctorOp::SUB:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_SUB);
                break;
             case FunctorOp::MUL: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_MUL);
                break;
             case FunctorOp::DIV: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_DIV);
                break;
             case FunctorOp::EXP: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_EXP);
                break;
             case FunctorOp::MOD: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_MOD);
                break;
             case FunctorOp::BAND: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_BAND);
                break;
             case FunctorOp::BOR: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_BOR);
                break;
             case FunctorOp::BXOR: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_BXOR);
                break;
             case FunctorOp::LAND: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_LAND);
                break;
             case FunctorOp::LOR: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_LOR);
                break;
             case FunctorOp::MAX: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_MAX);
                break;
             case FunctorOp::MIN: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_MIN);
                break;
             case FunctorOp::CAT: 
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_CAT);
                break;

             /** Ternary Functor Operators */
             case FunctorOp::SUBSTR: 
                visit(args[0], exitAddress); 
                visit(args[1], exitAddress); 
                visit(args[2], exitAddress); 
                code.push_back(LVM_OP_SUBSTR);
                break;

             /** Undefined */
             default: 
                assert(false && "unsupported operator");
                return;
          }
       }
       
       void visitUserDefinedOperator(const RamUserDefinedOperator& op, size_t exitAddress) override {
          for (size_t i = 0; i < op.getArgCount(); i++) {
             visit(op.getArgument(i), exitAddress);
          } 
          code.push_back(LVM_UserDefinedOperator);
          code.push_back(symbolTable.lookup(op.getName()));
          code.push_back(symbolTable.lookup(op.getType()));
        } 

        void visitPackRecord(const RamPackRecord& pack, size_t exitAddress) override {
           auto values = pack.getArguments();
           for (size_t i = 0; i < values.size(); ++i) {
              visit(values[i], exitAddress);
           }
           code.push_back(LVM_PackRecord);
           code.push_back(values.size()); 
        } 
       
        void visitArgument(const RamArgument& arg, size_t exitAddress) override {
           code.push_back(LVM_Argument);
           code.push_back(arg.getArgCount()); 
        }

        /** Visit RAM Conditions */

        void visitConjunction(const RamConjunction& conj, size_t exitAddress) override {
           visit(conj.getLHS(), exitAddress);
           visit(conj.getRHS(), exitAddress);
           code.push_back(LVM_Conjunction);
        }
       
        void visitNegation(const RamNegation& neg, size_t exitAddress) override {
           visit(neg.getOperand(), exitAddress);
           code.push_back(LVM_Negation); 
        }
       
        void visitEmptinessCheck(const RamEmptinessCheck& emptiness, size_t exitAddress) override {
           code.push_back(LVM_EmptinessCheck); 
           code.push_back(symbolTable.lookup(emptiness.getRelation().getName()));
        }

        void visitExistenceCheck(const RamExistenceCheck& exists, size_t exitAddress) override {
           auto values = exists.getValues();
           auto arity = exists.getRelation().getArity();
           std::string types;
           for (size_t i = 0; i < arity; ++i) {
              if (values[i]) {
                 visit(values[i], exitAddress); 
              }
              types += (values[i] == nullptr ? "_" : "V");
           }
           code.push_back(LVM_ExistenceCheck); 
           code.push_back(symbolTable.lookup(exists.getRelation().getName())); 
           code.push_back(symbolTable.lookup(types));
        }
       
        void visitProvenanceExistenceCheck(const RamProvenanceExistenceCheck& provExists, size_t exitAddress) override {
           auto values = provExists.getValues();
           auto arity = provExists.getRelation().getArity();
           std::string types;
           for (size_t i = 0; i < arity - 2; ++i) {
              if(values[i]) {
                 visit(values[i], exitAddress);
              } 
              types += (values[i] == nullptr ? "_" : "V");
           }
           code.push_back(LVM_ProvenanceExistenceCheck);
           code.push_back(symbolTable.lookup(provExists.getRelation().getName())); 
           code.push_back(symbolTable.lookup(types));
        }
       
        void visitConstraint(const RamConstraint& relOp, size_t exitAddress) override {
           code.push_back(LVM_Constraint);
           visit(relOp.getLHS(), exitAddress);
           visit(relOp.getRHS(), exitAddress);
           switch (relOp.getOperator()) {
             case BinaryConstraintOp::EQ:
                code.push_back(LVM_OP_EQ);
                break;
             case BinaryConstraintOp::NE:
                code.push_back(LVM_OP_NE);
                break;
             case BinaryConstraintOp::LT:
                code.push_back(LVM_OP_LT);
                break;
             case BinaryConstraintOp::LE:
                code.push_back(LVM_OP_LE);
                break;
             case BinaryConstraintOp::GT:
                code.push_back(LVM_OP_GT);
                break;
             case BinaryConstraintOp::GE:
                code.push_back(LVM_OP_GE);
                break;
             case BinaryConstraintOp::MATCH: 
                code.push_back(LVM_OP_MATCH);
                break;
             case BinaryConstraintOp::NOT_MATCH:
                code.push_back(LVM_OP_NOT_MATCH);
                break;
             case BinaryConstraintOp::CONTAINS:
                code.push_back(LVM_OP_CONTAINS);
                break;
             case BinaryConstraintOp::NOT_CONTAINS: 
                code.push_back(LVM_OP_NOT_CONTAINS);
                break;
             default:
                assert(false && "unsupported operator");
               
          }
        }


        /** Visit RAM Operations */
       
       void visitNestedOperation(const RamNestedOperation& nested, size_t exitAddress) override {
          visit(nested.getOperation(), exitAddress);
       }

       void visitSearch(const RamSearch& search, size_t exitAddress) override {
          code.push_back(LVM_Search);
          if (search.getProfileText().empty()) {
             code.push_back(0);
          } else {
             code.push_back(1);
          }
          code.push_back(symbolTable.lookup(search.getProfileText()));
          visitNestedOperation(search, exitAddress);
       }
       
       void visitScan(const RamScan& scan, size_t exitAddress) override {
          code.push_back(LVM_Scan); 
          size_t counterLabel = getNewScanIterator();
          code.push_back(LVM_ITER_TypeScan);
          code.push_back(counterLabel);
          code.push_back(symbolTable.lookup(scan.getRelation().getName()));
          size_t address_L0 = code.size();

          code.push_back(LVM_ITER_NotAtEnd);
          code.push_back(counterLabel);
          code.push_back(LVM_ITER_TypeScan);
          code.push_back(LVM_Jmpez);
          size_t L1 = getNewAddressLabel();
          code.push_back(lookupAddress(L1));

          code.push_back(LVM_ITER_Select);
          code.push_back(counterLabel);
          code.push_back(LVM_ITER_TypeScan);
          code.push_back(scan.getIdentifier());
          
          visitSearch(scan, exitAddress);
          code.push_back(LVM_ITER_Inc);
          code.push_back(counterLabel);
          code.push_back(LVM_ITER_TypeScan);
          code.push_back(LVM_Goto);
          code.push_back(address_L0);

          setAddress(L1, code.size());
       }
       
       void visitIndexScan(const RamIndexScan& scan, size_t exitAddress) override {
          //TODO For now just eval pattern every times
          code.push_back(LVM_IndexScan);
          size_t counterLabel = getNewIndexScanIterator();
          size_t L1 = getNewAddressLabel();

          auto patterns = scan.getRangePattern();
          std::string types;
          auto arity = scan.getRelation().getArity();
          for (size_t i = 0; i < arity; i ++) {
             if (patterns[i]) {
                visit(patterns[i], exitAddress);
             }
             types += (patterns[i] == nullptr? "_" : "V");
          }

          code.push_back(LVM_ITER_TypeIndexScan);
          code.push_back(counterLabel);
          code.push_back(symbolTable.lookup(scan.getRelation().getName())); 
          code.push_back(symbolTable.lookup(types));

          size_t address_L0 = code.size();

          code.push_back(LVM_ITER_NotAtEnd);
          code.push_back(counterLabel);
          code.push_back(LVM_ITER_TypeIndexScan);
          code.push_back(LVM_Jmpez);
          code.push_back(lookupAddress(L1));

          code.push_back(LVM_ITER_Select);
          code.push_back(counterLabel);
          code.push_back(LVM_ITER_TypeIndexScan);
          code.push_back(scan.getIdentifier());

          visitSearch(scan, exitAddress);

          code.push_back(LVM_ITER_Inc);
          code.push_back(counterLabel);
          code.push_back(LVM_ITER_TypeIndexScan);
          code.push_back(LVM_Goto);
          code.push_back(address_L0);
          setAddress(L1, code.size());
       }
       
       void visitUnpackRecord(const RamUnpackRecord& lookup, size_t exitAddress) override {
          code.push_back(LVM_UnpackRecord);
          code.push_back(lookup.getReferenceLevel());
          code.push_back(lookup.getReferencePosition());
          code.push_back(lookup.getArity()); 
          code.push_back(lookup.getIdentifier());
          visitSearch(lookup, exitAddress);
       }

       void visitAggregate(const RamAggregate& aggregate, size_t exitAddress) override {
          code.push_back(LVM_Aggregate);
          auto patterns = aggregate.getPattern();
          std::string types;
          auto arity = aggregate.getRelation().getArity();
          for (size_t i = 0; i < arity; i ++) {
             if (patterns[i]) {
                visit(patterns[i], exitAddress);
             }
             types += (patterns[i] == nullptr? "_" : "V");
          }
          size_t counterLabel = getNewIndexScanIterator();
          size_t L1 = getNewAddressLabel();
          size_t L2 = getNewAddressLabel();
          code.push_back(LVM_ITER_TypeIndexScan);
          code.push_back(counterLabel);
          code.push_back(symbolTable.lookup(aggregate.getRelation().getName())); 
          code.push_back(symbolTable.lookup(types));

          if (aggregate.getFunction() == RamAggregate::COUNT) {   // To count, there is no need to iterate
            code.push_back(LVM_Aggregate_COUNT);
            code.push_back(counterLabel);
            code.push_back(LVM_Goto);
            code.push_back(lookupAddress(L1));
          } else {
             code.push_back(LVM_ITER_NotAtEnd);    // First check, if the range is empty, does nothing 
             code.push_back(counterLabel);
             code.push_back(LVM_ITER_TypeIndexScan);
             code.push_back(LVM_Jmpez);
             code.push_back(lookupAddress(L2));

             switch (aggregate.getFunction()) { // Init value
                case RamAggregate::MIN:
                     code.push_back(LVM_Number);
                     code.push_back(MAX_RAM_DOMAIN);
                     break;
                case RamAggregate::MAX:
                     code.push_back(LVM_Number);
                     code.push_back(MIN_RAM_DOMAIN);
                     break;
                case RamAggregate::COUNT:
                     break;
                case RamAggregate::SUM:
                     code.push_back(LVM_Number);
                     code.push_back(0);
                     break;
             }

             size_t address_L0 = code.size();

             code.push_back(LVM_ITER_NotAtEnd);    // Start the formal for loop if the relation is non-empty
             code.push_back(counterLabel);
             code.push_back(LVM_ITER_TypeIndexScan);
             code.push_back(LVM_Jmpez);
             code.push_back(lookupAddress(L1));

             code.push_back(LVM_ITER_Select);
             code.push_back(counterLabel);
             code.push_back(LVM_ITER_TypeIndexScan);
             code.push_back(aggregate.getIdentifier());

             visit(aggregate.getExpression(), exitAddress);

             switch (aggregate.getFunction()) {
                case RamAggregate::MIN:
                     code.push_back(LVM_OP_MIN);  //TODO replace with LVM_OP
                     break;
                case RamAggregate::MAX:
                     code.push_back(LVM_OP_MAX);  //TODO replace with LVM_OP
                     break;
                case RamAggregate::COUNT:
                     assert (false);
                     break;
                case RamAggregate::SUM:
                     code.push_back(LVM_OP_ADD);  //TODO replace with LVM_OP
                     break;
             }
             
             code.push_back(LVM_ITER_Inc);
             code.push_back(counterLabel);
             code.push_back(LVM_ITER_TypeIndexScan);
             code.push_back(LVM_Goto);
             code.push_back(address_L0);
          }

          setAddress(L1, code.size());
          
          code.push_back(LVM_Aggregate_Return);  // TODO Well.. Improve?
          code.push_back(aggregate.getIdentifier());

          visitSearch(aggregate, exitAddress);
          setAddress(L2, code.size());
       }
       
       void visitFilter(const RamFilter& filter, size_t exitAddress) override {
          code.push_back(LVM_Filter);
            
          // Profile Action
          code.push_back(symbolTable.lookup(filter.getProfileText()));

          size_t L0 = getNewAddressLabel();

          visit(filter.getCondition(), exitAddress);

          code.push_back(LVM_Jmpez);
          code.push_back(lookupAddress(L0));

          visitNestedOperation(filter, exitAddress);

          setAddress(L0, code.size());
       }
        
       void visitProject(const RamProject& project, size_t exitAddress) override {
          size_t arity = project.getRelation().getArity();
          std::string relationName = project.getRelation().getName();
          auto values = project.getValues();
          for (size_t i = 0; i < values.size(); ++i) {
             assert(values[i]);
             visit(values[i], exitAddress);
          }
          code.push_back(LVM_Project);
          code.push_back(arity);
          code.push_back(symbolTable.lookup(relationName));
       }
       void visitReturn(const RamReturn& ret, size_t exitAddress) override {
          //The value must be pushed in correct order
          std::string types;
          auto expressions = ret.getValues();
          size_t size = expressions.size();
          for (int i = size - 1; i >= 0; --i) {
             if (expressions[i] == nullptr) {
                 types += '_';
             } else {
                 types += 'V';
                 visit(expressions[i], exitAddress);
             }
          }
          code.push_back(LVM_Return);
          code.push_back(ret.getValues().size());
          code.push_back(symbolTable.lookup(types));
       }

       /** Visit RAM stmt*/
       
       void visitSequence(const RamSequence& seq, size_t exitAddress) override {
          code.push_back(LVM_Sequence);
          for (const auto& cur : seq.getStatements()) {
             visit(cur, exitAddress); 
          } 
       }
       
       void visitParallel(const RamParallel& parallel, size_t exitAddress) override {
          //size_t num_blocks=  parallel.getStatements().size();
          for (const auto& cur : parallel.getStatements()) {
             visit(cur, exitAddress); 
          } 

          //TODO Implement real parallel
       }
       
       void visitLoop(const RamLoop& loop, size_t exitAddress) override {
          size_t address_L0 = code.size(); 
          code.push_back(LVM_Loop);

          size_t L1 = getNewAddressLabel();
          size_t address_L1 = lookupAddress(L1);
          visit(loop.getBody(), address_L1);
          code.push_back(LVM_IncIterationNumber);
          code.push_back(LVM_Goto);
          code.push_back(address_L0);
          code.push_back(LVM_ResetIterationNumber);
          setAddress(L1, code.size());
       }
       
       void visitExit(const RamExit& exit, size_t exitAddress) override {
          visit(exit.getCondition(), exitAddress);
          code.push_back(LVM_Jmpnz); // Jmp if condition is true
          code.push_back(exitAddress);
       }
       
       void visitLogTimer(const RamLogTimer& timer, size_t exitAddress) override {
          code.push_back(LVM_LogTimer);
          size_t timerIndex = getNewTimer();
          code.push_back(symbolTable.lookup(timer.getMessage()));
          if (timer.getRelation() == nullptr) {
             code.push_back(0);
             code.push_back(LVM_NOP);
             code.push_back(timerIndex);
          } else {
             code.push_back(1);
             code.push_back(symbolTable.lookup(timer.getRelation()->getName())); //TODO getRelation return type not consitent
             code.push_back(timerIndex);
          }
          visit(timer.getStatement(), exitAddress);
          code.push_back(LVM_StopLogTimer);
          code.push_back(timerIndex);
       }
       
       void visitDebugInfo(const RamDebugInfo& dbg, size_t exitAddress) override {
          code.push_back(LVM_DebugInfo);
          code.push_back(symbolTable.lookup(dbg.getMessage()));
          visit(dbg.getStatement(), exitAddress);
       }

       void visitStratum(const RamStratum& stratum, size_t exitAddress) override {
          code.push_back(LVM_Stratum); 
          visit(stratum.getBody(), exitAddress);
       }
       
       void visitCreate(const RamCreate& create, size_t exitAddress) override {
          code.push_back(LVM_Create);
          code.push_back(symbolTable.lookup(create.getRelation().getName()));
          code.push_back(create.getRelation().getArity());
          switch (create.getRelation().getRepresentation()) {
                case RelationRepresentation::BTREE:
                    code.push_back(LVM_BTREE);
                    break;
                case RelationRepresentation::BRIE:
                    code.push_back(LVM_BRIE);
                    break;
                case RelationRepresentation::EQREL:
                    code.push_back(LVM_EQREL);
                    break;
                case RelationRepresentation::DEFAULT:
                    code.push_back(LVM_DEFAULT);
                default:
                    break;
          }

          auto attributeTypes = create.getRelation().getAttributeTypeQualifiers();
          for (auto type : attributeTypes) {
            code.push_back(symbolTable.lookup(type));
          }
       }

       void visitClear(const RamClear& clear, size_t exitAddress) override {
          code.push_back(LVM_Clear);
          code.push_back(symbolTable.lookup(clear.getRelation().getName()));
       }
       
       void visitDrop(const RamDrop& drop, size_t exitAddress) override {
          code.push_back(LVM_Drop); 
          code.push_back(symbolTable.lookup(drop.getRelation().getName()));
       }
       
       void visitLogSize(const RamLogSize& size, size_t exitAddress) override {
          code.push_back(LVM_LogSize);
          code.push_back(symbolTable.lookup(size.getRelation().getName()));
          code.push_back(symbolTable.lookup(size.getMessage()));
       }

       void visitLoad(const RamLoad& load, size_t exitAddress) override {
          code.push_back(LVM_Load); 
          code.push_back(symbolTable.lookup(load.getRelation().getName()));

          /** TODO Need a better way to store IOs.*/
          IODirectivesPool.push_back(load.getIODirectives());
          code.push_back(IODirectivesPool.size() - 1);
       }

       void visitStore(const RamStore& store, size_t exitAddress) override {
          code.push_back(LVM_Store);
          code.push_back(symbolTable.lookup(store.getRelation().getName()));

          /** TODO: Need a better way to store IOs.*/
          IODirectivesPool.push_back(store.getIODirectives());
          code.push_back(IODirectivesPool.size() - 1);
       }
       
       void visitFact(const RamFact& fact, size_t exitAddress) override {
          size_t arity = fact.getRelation().getArity();
          auto values = fact.getValues();
          for (size_t i = 0; i < arity; ++i) {
             visit(values[i], exitAddress);       // Values cannot be null here
          }
          std::string targertRelation = fact.getRelation().getName();
          code.push_back(LVM_Fact);
          code.push_back(symbolTable.lookup(targertRelation));
          code.push_back(arity);
       }

       void visitQuery(const RamQuery& insert, size_t exitAddress) override {
          code.push_back(LVM_Query);
          if (insert.getCondition() == nullptr) {
             code.push_back(LVM_Number);
             code.push_back(true);  // Push true
          } else {
             visit(insert.getCondition(), exitAddress);
          }
          size_t L0 = getNewAddressLabel();
          code.push_back(LVM_Jmpez);
          code.push_back(lookupAddress(L0));
          visit(insert.getOperation(), exitAddress);
          setAddress(L0, code.size());
       }

       void visitMerge(const RamMerge& merge, size_t exitAddress) override {
          std::string source = merge.getSourceRelation().getName();
          std::string target = merge.getTargetRelation().getName();
          code.push_back(LVM_Merge);
          code.push_back(symbolTable.lookup(source));
          code.push_back(symbolTable.lookup(target));
       }

       void visitSwap(const RamSwap& swap, size_t exitAddress) override {
          std::string first = swap.getFirstRelation().getName(); 
          std::string second = swap.getSecondRelation().getName(); 
          code.push_back(LVM_Swap);
          code.push_back(symbolTable.lookup(first));
          code.push_back(symbolTable.lookup(second));
       }

       void visitNode(const RamNode& node, size_t exitAddress) override {
          assert(false && "Unknown Node type");
          /** Unknown Node */
       }

    };
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

   /** Entry for executing the main program */
   void executeMain();

   /** Execute main program */
   void execute(const LVMGenerator& generator, InterpreterContext& ctxt);

   /** Execute the subroutine */
   void executeSubroutine(const RamStatement& stmt, const std::vector<RamDomain>& arguments,
           std::vector<RamDomain>& returnValues, std::vector<bool>& returnErrors) {

      InterpreterContext ctxt;
      LVMGenerator generator(translationUnit.getSymbolTable(), stmt);
      ctxt.setReturnValues(returnValues);
      ctxt.setReturnErrors(returnErrors);
      ctxt.setArguments(arguments);
      //print(generator);
      execute(generator, ctxt);
   }
   
   /** Print out the instruction stream */
   // TODO Cache the program code
   void printMain() const {
      LVMGenerator generator(translationUnit.getSymbolTable(), *translationUnit.getP().getMain());
      print(generator);
   }

   /** Print out the instruction stream */
   void print(const LVMGenerator& g) const;

protected:
   /** Main Program Instruction Stream */

   /** relation environment type */
   using relation_map = std::map<std::string, InterpreterRelation*>; 
   
   /** index_set for iter */
   using index_set = btree_multiset<const RamDomain*, InterpreterIndex::comparator, std::allocator<const RamDomain*>, 512>;

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
   
   /** Drop relation */  // TODO old interface
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

    /** Swap relation */   //TODO old interface
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

    //InterpreterContext ctxt(translationUnit.getAnalysis<RamOperationDepthAnalysis>()->getDepth(&op));
    

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

   void insertTimerAt(size_t index, Logger* timer) {
      if (index >= timers.size()) {
         timers.resize((index + 1) * 2, nullptr);
      }
      timers[index] = timer;
   }

   void stopTimerAt(size_t index) {
      assert(index < timers.size());
      timers[index]->~Logger();
   }

private:
   friend InterpreterProgInterface;

   /** RAM translation Unit */
   RamTranslationUnit& translationUnit;

   /** Cached subroutine */
   std::map<std::string, LVMGenerator&> subroutines;

   /** Relation Environment */
   relation_map environment;
   
   /** Value stack */
   std::stack<RamDomain> stack;

   /** counters for atom profiling */
   std::map<std::string, std::map<size_t, size_t>> frequencies;

   /** counters for non-existence check */
   std::map<std::string, std::atomic<size_t>> reads;

   /** counter for $ operator */
   int counter = 0;

   /** iteration number (in a fix-point calculation) */
   size_t iteration = 0;

   /** Dynamic library for user-defined functors */
   void* dll = nullptr;
   
   /** Iters for the indexScan operation */
   std::vector<std::pair<index_set::iterator, index_set::iterator>> indexScanIteratorPool;
   
   /** Iters for the Scan operation */
   std::vector<std::pair<InterpreterRelation::iterator, InterpreterRelation::iterator>> scanIteratorPool;

   /** Timer table for logtimer */
   std::vector<Logger*> timers;

   /** for stratum */ 
   int level = 0;

};


class Interpreter_ {
public:
    Interpreter_(RamTranslationUnit& tUnit) : translationUnit(tUnit), counter(0), iteration(0), dll(nullptr) {}
    virtual ~Interpreter_() {
        for (auto& x : environment) {
            delete x.second;
        }
    }

    /** Get translation unit */
    RamTranslationUnit& getTranslationUnit() {
        return translationUnit;
    }

    /** Execute main program */
    void executeMain();

    /* Execute subroutine */
    void executeSubroutine(const RamStatement& stmt, const std::vector<RamDomain>& arguments,
            std::vector<RamDomain>& returnValues, std::vector<bool>& returnErrors);

protected:
    /** relation environment type */
    using relation_map = std::map<std::string, InterpreterRelation*>;

    /** Evaluate value */
    RamDomain evalVal(const RamExpression& value, const InterpreterContext& ctxt = InterpreterContext());

    /** Evaluate operation */
    void evalOp(const RamOperation& op, const InterpreterContext& args = InterpreterContext());

    /** Evaluate conditions */
    bool evalCond(const RamCondition& cond, const InterpreterContext& ctxt = InterpreterContext());

    /** Evaluate statement */
    void evalStmt(const RamStatement& stmt);

    /** Get symbol table */
    SymbolTable& getSymbolTable() {
        return translationUnit.getSymbolTable();
    }

    /** Get counter */
    int getCounter() const {
        return counter;
    }

    /** Get Iteration Number */
    size_t getIterationNumber() const {
        return iteration;
    }

    /** Increment counter */
    int incCounter() {
        return counter++;
    }

    /** Increment iteration number */
    void incIterationNumber() {
        iteration++;
    }

    /** Reset iteration number */
    void resetIterationNumber() {
        iteration = 0;
    }

    void createRelation(const RamRelation& id) {
        InterpreterRelation* res = nullptr;
        assert(environment.find(id.getName()) == environment.end());
        if (id.getRepresentation() == RelationRepresentation::EQREL) {
            res = new InterpreterEqRelation(id.getArity());
        } else {
            res = new InterpreterRelation(id.getArity());
        }
        environment[id.getName()] = res;
    }

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

    /** Get relation map */
    relation_map& getRelationMap() const {
        return const_cast<relation_map&>(environment);
    }

    /** Drop relation */
    void dropRelation(const RamRelation& id) {
        InterpreterRelation& rel = getRelation(id);
        environment.erase(id.getName());
        delete &rel;
    }

    /** Swap relation */
    void swapRelation(const RamRelation& ramRel1, const RamRelation& ramRel2) {
        InterpreterRelation* rel1 = &getRelation(ramRel1);
        InterpreterRelation* rel2 = &getRelation(ramRel2);
        environment[ramRel1.getName()] = rel2;
        environment[ramRel2.getName()] = rel1;
    }

    /** Load dll */
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

private:
    friend InterpreterProgInterface;

    /** RAM translation Unit */
    RamTranslationUnit& translationUnit;

    /** relation environment */
    relation_map environment;

    /** counters for atom profiling */
    std::map<std::string, std::map<size_t, size_t>> frequencies;

    /** counters for non-existence checks */
    std::map<std::string, std::atomic<size_t>> reads;

    /** counter for $ operator */
    int counter;

    /** iteration number (in a fix-point calculation) */
    size_t iteration;

    /** Dynamic library for user-defined functors */
    void* dll;
};

}  // end of namespace souffle
