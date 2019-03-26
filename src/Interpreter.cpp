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

enum LVM_Type {
    // Expressions
    LVM_Number;
    LVM_ElementAccess;
    LVM_AutoIncrement;
    /** Unary Functor Operations */
    LVM_OP_ORD;
    LVM_OP_STRLEN;
    LVM_OP_NEG;
    LVM_OP_BNOT;
    LVM_OP_LNOT;
    LVM_OP_TONUMBER;
    LVM_OP_TOSTRING;
    /** Binary Functor Operators */ 
    LVM_OP_ADD;
    LVM_OP_SUB;
    LVM_OP_MUL;
    LVM_OP_DIV;
    LVM_OP_EXP;
    LVM_OP_MOD;
    LVM_OP_BAND;
    LVM_OP_BOR;
    LVM_OP_BXOR;
    LVM_OP_LAND;
    LVM_OP_LOR;
    LVM_OP_MAX;
    LVM_OP_MIN;
    LVM_OP_CAT;
    /** Ternary Functor Operators */
    LVM_OP_SUBSTR;

    // LVM Constraint Op
    
    LVM_OP_EQ;
    LVM_OP_NE;
    LVM_OP_LT;
    LVM_OP_LE;
    LVM_OP_GT;
    LVM_OP_GE;
    LVM_OP_MATCH;
    LVM_OP_NOT_MATCH;
    LVM_OP_CONTAINS;
    LVM_OP_NOT_CONTAIN;


    LVM_UserDefinedOperator;
    LVM_PackRecord;
    LVM_Argument;

    // LVM Conditions
    LVM_Conjunction;
    LVM_Negation;
    LVM_EmptinessCheck;
    LVM_ExistenceCheck;
    LVM_ProvenanceExistenceCheck;
    LVM_Constraint;

    // LVM Operations;
    LVM_Scan;
    LVM_IndexScan;
    LVM_UnpackRecord;
    LVM_Filter;
    LVM_Project;
    LVM_Return;

    // LVM Stmts
    LVM_Sequence;
    LVM_Parallel;
    LVM_Stop_Parallel;
    LVM_Loop;
    LVM_Exit;
    LVM_LogTimer;
    LVM_DebugInfo;
    LVM_Stratum;
    LVM_Create;
    LVM_Clear;
    LVM_Drop;
    LVM_LogSize;
    LVM_Load;
    LVM_Store;
    LVM_Fact;
    LVM_Number;
    LVM_Merge;
    LVM_Swap;

    // LVM
    LVM_Goto;
    LVM_Jmpnz;
};

class LVMGenerator : public RamVisitor<void, size_t exitAddress> {
   std::vector<RamDomain> &code;            /** Instructions stream */
   SymbolTable &symbolTable;                /** Class for converting string to number and vice versa */ 
   std::vector<size_t> jumpAdresses;        
   
   /** Store reference to IODirectives */
   std::vector<const std::vector<IODirectives>&> IODirectivesPool;
   size_t IODirectivesCounter = 0;

   /** Store reference to relation, used by RN_Create */
   std::vector<Relation&> relationPool;
   size_t relationCounter = 0;

   /** Address Table */
   size_t  currentAddress;
   size_t getNewLabel() { return currentLabel++; }
   std::vector<size_t> addressMap;

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
         case FunctorOp::ORD:       visit(args[0], exit_address); code.push_back(LVM_OP_ORD);       break;
         case FunctorOp::STRLEN:    visit(args[0], exit_address); code.push_back(LVM_OP_STRLEN);    break;
         case FunctorOp::NEG:       visit(args[0], exit_address); code.push_back(LVM_OP_NEG);       break;
         case FunctorOp::BNOT:      visit(args[0], exit_address); code.push_back(LVM_OP_BNOT);      break;
         case FunctorOp::LNOT:      visit(args[0], exit_address); code.push_back(LVM_OP_LNOT);      break;
         case FunctorOp::TONUMBER:  visit(args[0], exit_address); code.push_back(LVM_OP_TONUMBER);  break;
         case FunctorOp::TOSTRING:  visit(args[0], exit_address); code.push_back(LVM_OP_TOSTRING);  break;

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
            visit(args[0], exit_address); 
            visit(args[1], exit_address); 
            visit(args[2], exit_address); 
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
      code.push_back(symbolTable.lookup(op.getName());
      code.push_back(symbolTable.lookup(op.getType());
    } 

    /*
     * Syntax: [RN_PackRecords, size]
     *
     * Semantic: pop [size] values from the stack, pack the record.
     */
    void visitPackRecord(const RamPackRecord& pack, size_t exitAddress) override {
       auto values = pack.getArguments();
       for (size_t i = 0; i < values.size(); ++i) {
          visit(values[i]);
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
       code.push_back(symbolTabel.lookup(emptiness.getRelation().getName()));
    }

    /*
     * Syntax: [RN_ExistenceCheck, relation, pattern]
     *
     * Semantic: Check if a [pattern] exists in a [relation], push bool onto the stack.
     *
     */
    void visitExistenceCheck(const RamExistenceCheck& exists, size_t exitAddress) override {
       auto values = exists.getValues();
       std::string type;
       for (size_t i = 0; i < values.size(); ++i) {
          visit(values[i]);      /** why pattern is named 'value' ? */
          types += (values[i] == nullptr ? "_" : "V");
       }
       code.push_back(LVM_ExistenceCheck); 
       code.push_back(symbolTabel.lookup(exists.getRelation().getName())); 
       code.push_back(symbolTable.lookup(type));
    }
   
    /*
     * Syntax: [RN_ExistenceCheck, relation, pattern]
     *
     * Semantic: umm. TODO
     *
     */
    void visitProvenanceExistenceCheck(const RamProvenanceExistenceCheck& provExists, size_t exitAddress) override {
       auto values = provExists.getValues();
       std::string type;
       for (size_t i = 0; i < values.size(); ++i) {
          visit(values[i]);
          types += (values[i] == nullptr ? "_" : "V");
       }
       code.push_back(LVM_ProvenanceExistenceCheck);
       code.push_back(symbolTabel.lookup(exists.getRelation().getName())); 
       code.push_back(symbolTable.lookup(type));
    }
   
    /*
     * Syntax: [RN_Constraint, operator]
     *
     * Semantic: pop two values, do operator, push result back
     *
     */
    void visitConstraint(const RamConstraint& relOp) override {
       visit(relOp.getLHS());
       visit(relOp.getRHS());
       switch (relOp.getOperator()) {
         case BinaryConstraintOp::EQ:
            code.push(LVM_OP_EQ);
            break;
         case BinaryConstraintOp::NE:
            code.push(LVM_OP_NE);
            break;
         case BinaryConstraintOp::LT:
            code.push(LVM_OP_LT);
            break;
         case BinaryConstraintOp::LE:
            code.push(LVM_OP_LE);
            break;
         case BinaryConstraintOp::GT:
            code.push(LVM_OP_GT);
            break;
         case BinaryConstraintOp::GE:
            code.push(LVM_OP_GE);
            break;
         case BinaryConstraintOp::MATCH: 
            code.push(LVM_OP_MATCH);
            break;
         case BinaryConstraintOp::NOT_MATCH:
            code.push(LVM_OP_NOT_MATCH);
            break;
         case BinaryConstraintOp::CONTAINS:
            code.push(LVM_OP_CONTAINS);
            break;
         case BinaryConstraintOp::NOT_CONTAINS: 
            code.push(LVM_OP_NOT_CONTAIN):
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
    * Syntax: [RN_Scan, relation, identifier, operation]
    *
    * Semantic: Perform [operation] on each tuple in the [relation]
    *
    */
   void visitScan(const RamScan& scan, size_t exitAddress) override {
      code.push_back(LVM_Scan);
      code.push_back(symbolTabel.lookup(scan.getRelation().getName()));
      code.push_back(scan.getIdentifier());
      visit(scan.getOperation(), exitAddress);
   }
   
   /*
    * Syntax: [RN_IndexScan, relation, identifier, pattern, operation]
    *
    * Semantuc: Perform [operation] on each tuple in the [relation] that match the [pattern]
    */
   void visitIndexScan(const RamIndexScan& scan, size_t exitAddress) override {
      auto patterns = scan.getRangePattern();
      std::string types;
      auto arity = scan.getRelation().getArity();
      for (size_t i = 0; i < arity; i ++) {
         visit(patterns[i], exitAddress);
         types += (patterns[i] == nullptr? "_" : "V");
      }
      code.push_back(LVM_IndexScan);
      code.push_back(symbolTabel.lookup(scan.getRelation().getName()));
      code.push_back(scan.getIdentifier());
      code.push_back(types);
      visit(scan.getOperation(), exitAddress);
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
    * Syntax: [RN_filter, condtion, operation]
    *
    */
   void visitFilter(const RamFilter& filter, size_t exitAddress) override {
      code.push_back(LVM_Filter);
      visit(filter.getCondition(), exitAddress);
      visit(filter.getOperation(), exitAddress);
   }
    
   /*
    * Syntax: [RN_Project, arity, relation] 
    *
    * Semantic: Create new tuple, insert into relation
    */
   
   void visitProject(const RamProject& project, size_t exitAddress) override {
      size_t arity = project.getRelation.getArity();
      std::string relationName = project.getRelation().getName();
      code.push_back(LVM_Project);
      code.push_back(arity);
      code.push_back(symbolTabel.lookup(relationName));
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

      code.push(LVM_Return);
      code.push(ret.getValues().size());
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
   
   /* Syntax: [RN_Parallel, num_stmts, stmts...]
    *
    * Semantic: Execute the [stmts] in parallel, wait untill all stmts are done.
    */
   void visitParallel(const RamParallel& parallel, size_t exitAddress) override {
      size_t address_L0 = code.size();
      size_t num_blocks=  parallel.getStatements().size();
      code.push_back(LVM_Parallel); 
      code.push_back(num_blocks);

      std::vector<size_t> labels(num_blocks);
      for (size_t i = 0; i < num_blocks; ++ i) {
         labels[i] = getNewLabel();
         code.push_back(lookupAddress(labels[i]));
      }
      
      size_t L1 = getNewLabel();
      code.push_back(LVM_Goto);
      code.push_back(lookupAddress(L1));
   
      for (size_t i = 0; i < num_blocks; ++ i) {
         setAddress(label[i], code.size());
         visit(parallel.getStatements()[i]);
         code.push_back(LVM_Stop_Parallel);
         code.push_back(L0);
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
      size_t address_L0 = code.size(); // TODO: 0 is ok?
      size_t L1 = getNewLabel();
      size_t address_L1 = lookupAddress(L1);
      visit(loop.getBody(), address_L1);
      code.push(LVM_Goto);
      code.push(address_L0);
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
      code.push_back(clear.getRelation().getName());
   }
   
   /* Syntax: [RN_Drop, relation]
    *
    * Semantic: Delete relation from the environment
    */
   void visitDrop(const RamDrop& drop, size_t exitAddress) override {
      code.push_back(LVM_Drop); 
      code.push_back(symbolTabel.lookup(drop.getRelation().getName()));
   }
   
   /* Syntax: [RN_LogSize, relation]
    *
    * Semantic: ??
    */
   void visitLogSize(const RamLogSizr& size, size_t exitAddress) override {
      code.push_back(LVM_LogSize);
      code.push_back(symbolTabel.lookup(size.getRelation().getName()));
   }

   /* Syntax: [RN_Store, RelationName, SourceIOs_idx]
    *
    * Semantic: Load [Relation] from [SourceIOs]
    *
    * The SourceIOs_Idx indicate the index of IOs in the IODirectivesPool.
    */
   void visitLoad(const RamLoad& load, size_t exitAddress) override {
      code.push_back(LVM_Load); 
      code.push_back(symTable.lookup(load.getRelation().getName()));

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
      code.push_back(symbolTabel.lookup(store.getRelation().getName()));

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
      size_t depth = depthAnalyzer.getDepth(insert.getOperation());
      code.push_back(RN_Query);
      code.push_back(depth);
      visit(insert.getOperation());
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
      code.push_back(symbolTabel.lookup(source));
      code.push_back(symbolTabel.lookup(target));
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
      code.push_back(symbolTabel.lookup(first));
      code.push_back(symbolTabel.lookup(second));
   }

   void visitNode(const RamNode& node, size_t exitAddress) override {
      /** Unknown Node */
   }

};

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

}  // end of namespace souffle
