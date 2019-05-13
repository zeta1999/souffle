/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RamTransformer.h
 *
 * Defines the interface for RAM transformation passes.
 *
 ***********************************************************************/

#pragma once

#include <memory>
#include <string>
#include <vector>

namespace souffle {

class RamTranslationUnit;

/**
 * Abstract Transformer Class for RAM
 */

class RamTransformer {
public:
    virtual ~RamTransformer() = default;

    /** Apply transformer */
    bool apply(RamTranslationUnit& translationUnit);

    /** Get name of transformer */
    virtual std::string getName() const = 0;

protected:
    /** Transform RAM translation unit */
    virtual bool transform(RamTranslationUnit& translationUnit) = 0;
};

/**
 * Composite loop transformer iterating until the encapsulated transformer
 * does not change the translation unit.
 */

class RamSequenceTransformer {
public:
   RamSequenceTransformer(std::vector<std::unique_ptr<RamTransformer>> tSeq) : transformerSequence(std::move(tSeq)){ }
   
   std::string getName() const {
       return "RamSequenceTransformer";
   }

   bool transform(RamTranslationUnit& tU) {
       bool changed = false; 
       for(auto const &cur: transformerSequence) {
	       if(cur->apply(tU)) { 
		       changed = true; 
	       }
       }
       return changed; 
   }

protected:
   std::vector<std::unique_ptr<RamTransformer>> transformerSequence; 
};

/**
 * Composite loop transformer checking for the change of a transformation.
 */

class RamIfTransformer {
public:
   RamIfTransformer(std::unique_ptr<RamTransformer> tPredicate, std::unique_ptr<RamTransformer> tBody) : 
	       transformerPredicate(std::move(tPredicate)), transformerBody(std::move(tBody)) { }
   
   std::string getName() const {
       return "RamIfTransformer";
   }

   bool transform(RamTranslationUnit& tU) {
       if(transformerPredicate->apply(tU)) {
          transformerBody->apply(tU);  
	  return true; 
       } else {
	  return false;
       }
   }

protected:
   std::unique_ptr<RamTransformer> transformerPredicate;
   std::unique_ptr<RamTransformer> transformerBody;
};


/**
 * Composite sequence transformer that applies sequences of transformations 
 */

class RamLoopTransformer {
public:
   RamLoopTransformer(std::unique_ptr<RamTransformer> tLoop) : transformerLoop(std::move(tLoop)){ }

   std::string getName() const {
       return "RamLoopTransformer";
   }

   bool transform(RamTranslationUnit& tU) {
       while(transformerLoop->apply(tU));
       return false;
   }

protected:
   std::unique_ptr<RamTransformer> transformerLoop;
};



}  // end of namespace souffle
