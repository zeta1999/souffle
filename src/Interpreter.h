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
#include "RamVisitor.h" // TODO
#include "RamOperationDepth.h" //TODO

#include <atomic>
#include <cassert>
#include <cstdlib>
#include <iostream>
#include <map>
#include <string>
#include <utility>
#include <vector>
#include <dlfcn.h>

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
    LVM_OP_NOT_CONTAIN,

    LVM_UserDefinedOperator,
    LVM_PackRecord,
    LVM_Argument,

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
    LVM_Filter,
    LVM_Project,
    LVM_Return,

    // LVM Stmts
    LVM_Sequence,
    LVM_Parallel,
    LVM_Stop_Parallel,
    LVM_Loop,
    LVM_Exit,
    LVM_LogTimer,
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

    // LVM internal operation
    LVM_Goto,               // LVM_Goto  <address>  
    LVM_Jmpnz,              // LVM_Jmpnz <address>
    LVM_Jmpez,              // LVM_Jmpez <address>
    LVM_ITER_LT,            // LVM_ITER_LT <iter Counter> <LVM_Number>. 
                            // Needed because iter counter is a not LVM_Number, need special handle
    LVM_ITER_Counter,       // LVM_ITER_Counter <i>
                            // Load iter counter at index i
    LVM_ITER_Select,        // TODO: Replacable by LVM_ElementAccess
    LVM_ITER_Inc,
    LVM_Match,              
    LVM_LT,
};

class InterpreterProgInterface;
class RamOperation;
class RamExpression;
class SymbolTable;
class LVMGenerator;

/**
 * Interpreter executing a RAM translation unit
 */

class LowLevelMachine {
    class LVMGenerator : public RamVisitor<void, size_t> {
    public:
       LVMGenerator() {}

       void cleanUp() {
          code.clear();
          IODirectivesPool.clear();  
          relationPool.clear();
          currentAddressLabel = 0;
          currentCounterLabel = 0;
       }

       std::vector<RamDomain> code;            /** Instructions stream */
       SymbolTable symbolTable;                /** Class for converting string to number and vice versa */ 
       
       /** Store reference to IODirectives */
       std::vector<std::vector<IODirectives>> IODirectivesPool;
       size_t IODirectivesCounter = 0;

       /** Store reference to relation, used by RN_Create */
       std::vector<RamRelation> relationPool;
       size_t relationCounter = 0;

       /** Address Table */
       size_t currentAddressLabel;
       size_t getNewAddressLabel() { return currentAddressLabel++; }
       std::vector<size_t> addressMap;

       /** Loop Iter */
       size_t currentCounterLabel = 0;
       size_t getNewCounterLabel() {return currentCounterLabel++; }

       /* Return the value of the addressLabel. 
        * Return 0 if label doesn't exits. ??
        */
       size_t lookupAddress(size_t addressLabel) {
          if (addressLabel < addressMap.size()) {
             return addressMap[addressLabel];
          }
          return 0;
       }

       size_t setAddress(size_t addressLabel, size_t value) {
          if (addressLabel > addressMap.size()) {
             addressMap.resize(addressLabel + 1); 
          } 
          addressMap[addressLabel] = value;
       }

       RamOperationDepthAnalysis depthAnalyzer;


       // Visit RAM Expressions
      
       /*
        * Syntax: [RN_Number, value]
        *
        * Semantic: Push the [value] onto the stack
        */
       void visitNumber(const RamNumber& num, size_t exitAddress) override {
          code.push_back(LVM_Number); 
          code.push_back(num.getConstant());
       } 

       /*
        * Syntax: [RN_ElementAccess, identifier, element]
        *
        * Semantic: Push the ctxt[identifier][element] onto the stack
        */
       void visitElementAccess(const RamElementAccess& access, size_t exitAddress) override {
          code.push_back(LVM_ElementAccess); 
          code.push_back(access.getIdentifier()); 
          code.push_back(access.getElement()); 
       }
       
       /* 
        * Syntax: [RN_AutoIncrement]
        *
        * Semantic: Increase counter by one.
        */
       void visitAutoIncrement(const RamAutoIncrement& inc, size_t exitAddress) override {
          code.push_back(LVM_AutoIncrement); 
       }
       

       /*
        * Syntax: [RN_IntrinsicOperatr, Operator]
        *
        * Semantic: Pop n value from stack, do the operation,
        * push the result onto the stack.
        */
       void visitIntrinsicOperator(const RamIntrinsicOperator& op, size_t exitAddress) override {
          const auto& args = op.getArguments();
          switch (op.getOperator()) {
             /** Unary Functor Operators */
             case FunctorOp::ORD:       visit(args[0], exitAddress); code.push_back(LVM_OP_ORD);       break;
             case FunctorOp::STRLEN:    visit(args[0], exitAddress); code.push_back(LVM_OP_STRLEN);    break;
             case FunctorOp::NEG:       visit(args[0], exitAddress); code.push_back(LVM_OP_NEG);       break;
             case FunctorOp::BNOT:      visit(args[0], exitAddress); code.push_back(LVM_OP_BNOT);      break;
             case FunctorOp::LNOT:      visit(args[0], exitAddress); code.push_back(LVM_OP_LNOT);      break;
             case FunctorOp::TONUMBER:  visit(args[0], exitAddress); code.push_back(LVM_OP_TONUMBER);  break;
             case FunctorOp::TOSTRING:  visit(args[0], exitAddress); code.push_back(LVM_OP_TOSTRING);  break;

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
       
       /* Syntax: [RN_UserDefinedOperator, OperationName, Types]
        *
        * Semantic: Find and Perform the user-defined operator, return type is Types[0]
        * Arguments' types are Types[1 - n], push result back to the stack
        */
       void visitUserDefinedOperator(const RamUserDefinedOperator& op, size_t exitAddress) override {
          for (size_t i = 0; i < op.getArgCount(); i++) {
             visit(op.getArgument(i), exitAddress);
          } 
          code.push_back(LVM_UserDefinedOperator);
          code.push_back(symbolTable.lookup(op.getName()));
          code.push_back(symbolTable.lookup(op.getType()));
        } 

        /*
         * Syntax: [RN_PackRecords, size]
         *
         * Semantic: pop [size] values from the stack, pack the record.
         */
        void visitPackRecord(const RamPackRecord& pack, size_t exitAddress) override {
           auto values = pack.getArguments();
           for (size_t i = 0; i < values.size(); ++i) {
              visit(values[i], exitAddress);
           }
           code.push_back(LVM_PackRecord);
           code.push_back(values.size()); 
        } 
       
        /*
         * Syntax: [RN_Argument, size]
         *
         * Semantic: For subroutine
         */
        void visitArgument(const RamArgument& arg, size_t exitAddress) override {
           code.push_back(LVM_Argument);
           code.push_back(arg.getArgCount()); 
        }

        /** Visit RAM Conditions */

        /*
         * Syntax: [RN_Conjunction]
         *
         * Semantic: Pop two values from stack, &
         */
        void visitConjunction(const RamConjunction& conj, size_t exitAddress) override {
           visit(conj.getLHS(), exitAddress);
           visit(conj.getRHS(), exitAddress);
           code.push_back(LVM_Conjunction);
        }
       
        /*
         * Syntax: [Rn_Negation]
         *
         * Semantic: Pop one value from stack, neg
         */
        void visitNegation(const RamNegation& neg, size_t exitAddress) override {
           visit(neg.getOperand(), exitAddress);
           code.push_back(LVM_Negation); 
        }
       
        /*
         * Syntax: [RN_EmptinessCheck, relationName]
         *
         * Semantic: Check if [relation] is empty, push bool onto the stack.
         */
        void visitEmptinessCheck(const RamEmptinessCheck& emptiness, size_t exitAddress) override {
           code.push_back(LVM_EmptinessCheck); 
           code.push_back(symbolTable.lookup(emptiness.getRelation().getName()));
        }

        /*
         * Syntax: [RN_ExistenceCheck, relation, pattern]
         *
         * Semantic: Check if a [pattern] exists in a [relation], push bool onto the stack.
         *
         */
        void visitExistenceCheck(const RamExistenceCheck& exists, size_t exitAddress) override {
           auto values = exists.getValues();
           std::string types;
           for (size_t i = 0; i < values.size(); ++i) {
              visit(values[i], exitAddress); 
              types += (values[i] == nullptr ? "_" : "V");
           }
           code.push_back(LVM_ExistenceCheck); 
           code.push_back(symbolTable.lookup(exists.getRelation().getName())); 
           code.push_back(symbolTable.lookup(types));
        }
       
        /*
         * Syntax: [RN_ExistenceCheck, relation, pattern]
         *
         * Semantic: umm. TODO
         *
         */
        void visitProvenanceExistenceCheck(const RamProvenanceExistenceCheck& provExists, size_t exitAddress) override {
           auto values = provExists.getValues();
           std::string types;
           for (size_t i = 0; i < values.size(); ++i) {
              visit(values[i], exitAddress);
              types += (values[i] == nullptr ? "_" : "V");
           }
           code.push_back(LVM_ProvenanceExistenceCheck);
           code.push_back(symbolTable.lookup(provExists.getRelation().getName())); 
           code.push_back(symbolTable.lookup(types));
        }
       
        /*
         * Syntax: [RN_Constraint, operator]
         *
         * Semantic: pop two values, do operator, push result back
         *
         */
        void visitConstraint(const RamConstraint& relOp, size_t exitAddress) override {
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
                code.push_back(LVM_OP_NOT_CONTAIN);
                break;
             default:
                assert(false && "unsupported operator");
               
          }
        }


        /** Visit RAM Operations */

       void visitNestedOperation(const RamNestedOperation& nested, size_t exitAddress) override {
          /** Does nothing */
       }

       void visitSearch(const RamSearch& search, size_t exitAddress) override {
          /** Does nothing */
       }
       
       /*
        *
        *                for (i = 0; i < realtion.size(); ++i){
        *                    t.i = relation[i];
        * Imperative         do netsed operation
        *                }
        *
        *
        * L0: LVM_Loop_Counter    <i>
        *     LVM_NUMBER  <relation.size()>
        *     LVM_LT
        *     LVM_JMPEZ  <L1>
        *     LVM_SELECT  <relation.name()>   <i>    <identifier>
        *     visit (scan.getOperation, exitAddress)
        *     LVM_Loop_Inc        <i>
        *     JMP L0
        * L1:
        *
        */
       void visitScan(const RamScan& scan, size_t exitAddress) override {
          size_t address_L0 = code.size();
          size_t counterLabel = getNewCounterLabel();
          code.push_back(LVM_ITER_Counter);
          code.push_back(counterLabel);

          code.push_back(LVM_Number);
          code.push_back(symbolTable.lookup(scan.getRelation().getName()));

          code.push_back(LVM_ITER_LT);
          code.push_back(LVM_Jmpez);
          size_t L1 = getNewAddressLabel();
          code.push_back(lookupAddress(L1));

          code.push_back(LVM_ITER_Select);
          code.push_back(symbolTable.lookup(scan.getRelation().getName()));
          code.push_back(counterLabel);
          code.push_back(scan.getIdentifier());

          visit(scan.getOperation(), L1);
          code.push_back(LVM_ITER_Inc);
          code.push_back(counterLabel);
          code.push_back(LVM_Goto);
          code.push_back(address_L0);

          setAddress(L1, code.size());
       }
       
       /*
        *
        *  for (i = 0; i < realtion.size(); ++i){
        *    if (match_pattern(relation[i])){
        *       t.i = realtion[i]
        *       do nested operation.
        *    } else {
        *       continue;
        *    }
        *  }
        *
        *
        * L0: LVM_Loop_Counter    <i>
        *     LVM_NUMBER  <relation.size()>
        *     LVM_LT
        *     LVM_JMPEZ  <L2>
        *     LVM_MATCH  <relation.name()>   <i>    <Pattern>
        *     LVM_JMPEZ  <L1>
        *     LVM_SELECT <relation.name()>   <i>    <identifier>
        *     visit (scan.getOperation, exitAddress)
        * L1: LVM_Inc    <i>
        *     JMP L0
        * L2:
        *
        */
       void visitIndexScan(const RamIndexScan& scan, size_t exitAddress) override {
          auto patterns = scan.getRangePattern();
          std::string types;
          auto arity = scan.getRelation().getArity();
          for (size_t i = 0; i < arity; i ++) {
             visit(patterns[i], exitAddress);
             types += (patterns[i] == nullptr? "_" : "V");
          }

          size_t counterLabel = getNewCounterLabel();
          size_t address_L0 = code.size();
          size_t L1 = getNewAddressLabel();
          size_t L2 = getNewAddressLabel();

          code.push_back(LVM_ITER_Counter);
          code.push_back(counterLabel);
          code.push_back(LVM_Number);
          code.push_back(symbolTable.lookup(scan.getRelation().getName())); //TODO can't get relation size.
                                              //TODO concurrent insert?
          code.push_back(LVM_Jmpez);
          code.push_back(lookupAddress(L2));
          code.push_back(LVM_Match);
          code.push_back(symbolTable.lookup(scan.getRelation().getName()));   
          code.push_back(counterLabel);
          //code.push_back(patterns); TODO Implement patterns here
          code.push_back(LVM_Jmpez);
          code.push_back(lookupAddress(L1));
          
          code.push_back(LVM_ITER_Select);
          code.push_back(counterLabel);
          code.push_back(scan.getIdentifier());
          visit(scan.getOperation(), exitAddress);
          setAddress(L1, code.size());
          
          code.push_back(LVM_Goto);
          code.push_back(address_L0);

          setAddress(L2, code.size());
       }
       
       /*
        * Syntax: [RN_UnpackRecord, referenceLevel, referencePos, arity, identifier, operation]
        *
        * Semantic: Look up record at ctxt[level][pos]
        *
        */
       void visitUnpackRecord(const RamUnpackRecord& lookup, size_t exitAddress) override {
          code.push_back(LVM_UnpackRecord);
          code.push_back(lookup.getReferenceLevel());
          code.push_back(lookup.getReferencePosition());
          code.push_back(lookup.getArity()); 
          code.push_back(lookup.getIdentifier());
          visit(lookup.getOperation(), exitAddress);
       }

       /*
        * Syntax: [RN_Aggregate
        * TODO 
        */
       void visitAggregate(const RamAggregate& aggregate, size_t exitAddress) override {
       }
       
       /*
        * If (condition) {
        *    do nested operation
        * }
        *
        *    <condition Expressions>
        *    LVM_Jmpez L0
        *    visit(nestedOperation)
        * L0:
        */
       void visitFilter(const RamFilter& filter, size_t exitAddress) override {
          size_t L0 = getNewAddressLabel();
          visit(filter.getCondition(), exitAddress);
          code.push_back(LVM_Jmpez);
          code.push_back(L0);
          visit(filter.getOperation(), exitAddress);
          setAddress(L0, code.size());
       }
        
       /*
        *
        *    Values
        *    LVM_Project
        *    <arity>
        *    <realtionName>
        */
       
       void visitProject(const RamProject& project, size_t exitAddress) override {
          size_t arity = project.getRelation().getArity();
          std::string relationName = project.getRelation().getName();
          auto values = project.getValues();
          for (size_t i = 0; i < values.size(); ++i) {
             visit(values[i], exitAddress);
          }
          code.push_back(LVM_Project);
          code.push_back(arity);
          code.push_back(symbolTable.lookup(relationName));
       }
        
       /*
        * Syntax: [RN_Return, size]
        *
        * Semantic: Pop [size] values from stack, add to ctxt.return
        */
       void visitReturn(const RamReturn& ret, size_t exitAddress) override {
          for (auto expr : ret.getValues()) {
             if (expr == nullptr) {
                code.push_back(RN_Number);
                code.push_back(1);
             } else {
                visit(expr, exitAddress); 
             }
          }

          code.push_back(LVM_Return);
          code.push_back(ret.getValues().size());
       }

       /** Visit RAM stmt*/
       
       /* Syntax: [RN_Sequence, stmt, stmt ... ]
        *
        * Semantic: A sequence of Statement
        */
       void visitSequence(const RamSequence& seq, size_t exitAddress) override {
          code.push_back(LVM_Sequence);
          for (const auto& cur : seq.getStatements()) {
             visit(cur, exitAddress); 
          } 
       }
       
       /* 
        *
        */
       void visitParallel(const RamParallel& parallel, size_t exitAddress) override {
          size_t address_L0 = code.size();
          size_t num_blocks=  parallel.getStatements().size();
          code.push_back(LVM_Parallel); 
          code.push_back(num_blocks);

          std::vector<size_t> labels(num_blocks);
          for (size_t i = 0; i < num_blocks; ++ i) {
             labels[i] = getNewAddressLabel();
             code.push_back(lookupAddress(labels[i]));
          }
          
          size_t L1 = getNewAddressLabel();
          code.push_back(LVM_Goto);
          code.push_back(lookupAddress(L1));
       
          for (size_t i = 0; i < num_blocks; ++ i) {
             setAddress(labels[i], code.size());
             visit(parallel.getStatements()[i], exitAddress);
             code.push_back(LVM_Stop_Parallel);
             code.push_back(address_L0);
          }

          setAddress(L1, code.size());
       }
       
       /* Syntax: [L0: LVM_Loop
        *              body
        *              LVM_GOTO L0 
        *          L1: ... ]
        * 
        * Semantic: Infinitely execute the body. 
        * Provide an exitAddress for possible RN_exits to jump to label E.
        */
       void visitLoop(const RamLoop& loop, size_t exitAddress) override {
          size_t address_L0 = code.size(); 
          size_t L1 = getNewAddressLabel();
          size_t address_L1 = lookupAddress(L1);
          visit(loop.getBody(), address_L1);
          code.push_back(LVM_Goto);
          code.push_back(address_L0);
          setAddress(L1, code.size() - 1);
       }
       
       /* Syntax: [RN_Exit, address]
        *
        * Semantic: If the top of the stack is TRUE, jump to address.
        */
       void visitExit(const RamExit& exit, size_t exitAddress) override {
          visit(exit.getCondition(), exitAddress);
          code.push_back(LVM_Jmpnz); // Jmp if condition is true
          code.push_back(exitAddress);
       }
       
       /* Syntax: [RN_LogTimer, relation?, body]
        *
        * Semantic:  relation can be null
        */
       void visitLogTimer(const RamLogTimer& timer, size_t exitAddress) override {
          code.push_back(LVM_LogTimer);
          //TODO: How to handle possible nullptr
       }
       
       /* Syntax: [RN_DebugInfo, body]
        *
        * Semantic: Start debug, continue to body
        */
       void visitDebugInfo(const RamDebugInfo& dbg, size_t exitAddress) override {
          code.push_back(LVM_DebugInfo);
          visit(dbg.getStatement(), exitAddress);
       }

       /* Syntax: [RN_Stratum, body]
        *
        * Semantic: Enter new stratum
        */
       void visitStratum(const RamStratum& stratum, size_t exitAddress) override {
          code.push_back(LVM_Stratum); 
          visit(stratum.getBody(), exitAddress);
       }
       
       /* Syntax: [RN_Create, relation_idx]
        *
        * Semantic: lookup the relation in relationPool[relation_idx], insert 
        * into environment.
        *
        */
       void visitCreate(const RamCreate& create, size_t exitAddress) override {
          code.push_back(LVM_Create);
          /** TODO Better way to store a relation */
          relationPool.push_back(create.getRelation());
          code.push_back(relationCounter++);
       }

       /* Syntax: [RN_Clear, relation]
        *
        * Semantic: Clean all the tuples in a relation.
        */
       void visitClear(const RamClear& clear, size_t exitAddress) override {
          code.push_back(LVM_Clear);
          code.push_back(symbolTable.lookup(clear.getRelation().getName()));
       }
       
       /* Syntax: [RN_Drop, relation]
        *
        * Semantic: Delete relation from the environment
        */
       void visitDrop(const RamDrop& drop, size_t exitAddress) override {
          code.push_back(LVM_Drop); 
          code.push_back(symbolTable.lookup(drop.getRelation().getName()));
       }
       
       /* Syntax: [RN_LogSize, relation]
        *
        * Semantic: ??
        */
       void visitLogSize(const RamLogSize& size, size_t exitAddress) override {
          code.push_back(LVM_LogSize);
          code.push_back(symbolTable.lookup(size.getRelation().getName()));
       }

       /* Syntax: [RN_Store, RelationName, SourceIOs_idx]
        *
        * Semantic: Load [Relation] from [SourceIOs]
        *
        * The SourceIOs_Idx indicate the index of IOs in the IODirectivesPool.
        */
       void visitLoad(const RamLoad& load, size_t exitAddress) override {
          code.push_back(LVM_Load); 
          code.push_back(symbolTable.lookup(load.getRelation().getName()));

          /** TODO Need a better way to store IOs.*/
          IODirectivesPool.push_back(load.getIODirectives());
          code.push_back(IODirectivesCounter++);
       }

       /*
        * Syntax: [RN_Store, RelationName, DestinationIOs_Idx]
        *
        * Semantic: Store [Relation] into [DestinationIOs]
        * 
        * The DestinationIOs_Idx indicate the index of the IOs in the IODirectivesPool.
        */
       void visitStore(const RamStore& store, size_t exitAddress) override {
          code.push_back(LVM_Store);
          code.push_back(symbolTable.lookup(store.getRelation().getName()));

          /** TODO: Need a better way to store IOs.*/
          IODirectivesPool.push_back(store.getIODirectives());
          code.push_back(IODirectivesCounter++);
       }
       
       /*
        * Syntax: [RN_Fact, targetRelationName, arity]
        *
        * Semantic: Pop [arity] values from stack, insert into [targetRelationName]
        */
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

       /*
        * Syntax: [RN_Query, depth]
        *
        * Semantic: Start loops
        */
       void visitQuery(const RamQuery& insert, size_t exitAddress) override {
          //size_t depth = depthAnalyzer.getDepth(insert.getOperation());
          code.push_back(RN_Query);
          //code.push_back(depth);
          //visit(insert.getOperation());
       }

       /*
        * Syntax: [RN_Merge, sourceName, targetName]
        *
        * Semantic: Merge [source] with [target].
        */
       void visitMerge(const RamMerge& merge, size_t exitAddress) override {
          std::string source = merge.getSourceRelation().getName();
          std::string target = merge.getTargetRelation().getName();
          code.push_back(LVM_Merge);
          code.push_back(symbolTable.lookup(source));
          code.push_back(symbolTable.lookup(target));
       }

       /*
        * Syntax: [RN_Swap, FirstRelation, SecondRelation]
        *
        * Semantic: Swap [first] with [second].
        */
       void visitSwap(const RamSwap& swap, size_t exitAddress) override {
          std::string first = swap.getFirstRelation().getName(); 
          std::string second = swap.getSecondRelation().getName(); 
          code.push_back(LVM_Swap);
          code.push_back(symbolTable.lookup(first));
          code.push_back(symbolTable.lookup(second));
       }

       void visitNode(const RamNode& node, size_t exitAddress) override {
          /** Unknown Node */
       }

    };
public:
   LowLevelMachine(RamTranslationUnit& tUnit) : translationUnit(tUnit) {}

   /** Get LVM instruction stream */
   void generatingInstructionStream() {
      const RamStatement& main = *translationUnit.getP().getMain();
      generator(main, 0);
      generator.code.clear(); 
      generator(main, 0);
   }

private:

   using relation_map = std::map<std::string, InterpreterRelation*>; 

   /** RAM */
   RamTranslationUnit& translationUnit;

   /** Relation Environment */
   relation_map environment;
   
   /** Value stack */
   std::vector<RamDomain> stack;

   /** LVMGenerator */
   LVMGenerator generator;
};


class Interpreter {
public:
    Interpreter(RamTranslationUnit& tUnit) : translationUnit(tUnit), counter(0), iteration(0), dll(nullptr) {}
    virtual ~Interpreter() {
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
