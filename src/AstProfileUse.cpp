/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstProfileUse.cpp
 *
 * Implements an AstAnalysis that provides profile information 
 * from a profile log file for profile-guided optimisations. 
 *
 ***********************************************************************/

#include "AstProfileUse.h"
#include "AstClause.h"
#include "AstLiteral.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "AstRelationIdentifier.h"
#include "AstTranslationUnit.h"
#include "AstUtils.h"
#include "AstVisitor.h"
#include "Global.h"
#include "Util.h"
#include <algorithm>
#include <iterator>
#include <set>
#include <string>

namespace souffle {

AstProfileUse::AstProfileUse() : programRun(std::make_shared<profile::ProgramRun>(profile::ProgramRun())) {
}

void AstProfileUse::run(const AstTranslationUnit& translationUnit) {
  if (Global::config().has("profile-use")) {
     std::string filename = Global::config().get("profile-use");
     profile::Reader(filename, programRun).processFile();
  }
}

void AstProfileUse::print(std::ostream& os) const {
}


size_t AstProfileUse::getRelationSize(const AstRelationIdentifier *rel) {
   if(profile::Relation *profRel = programRun->getRelation(rel->getName())){
       return profRel->getTotNum_tuples(); 
   } else {
       throw std::runtime_error("failed to find");
   }
}

}  // end of namespace souffle
