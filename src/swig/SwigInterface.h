#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <string>
#include <map>

#include "souffle/RamTypes.h"
#include "souffle/SymbolTable.h"
#include "souffle/SouffleInterface.h"

#pragma once

//Abstract base class for generated Datalog programs
class SWIGSouffleProgram {
   souffle::SouffleProgram *prog;
    
public:
   SWIGSouffleProgram(souffle::SouffleProgram *p) : prog(p) { 
    std::cout << "SWIG Instance generated.\n";
   }
   virtual ~SWIGSouffleProgram() { delete prog;  std::cout << "SWIG Instance deleted.\n";} 

   // execute Datalog program
   void run() { prog->run(); } 

   //execute Datalog program, loading inputs and storing outputs as requires.
   void runAll(const std::string &inputDirectory, const std::string &outputDirectory) 
         { prog->runAll(inputDirectory,outputDirectory); }

   // load all relations
   void loadAll(const std::string &inputDirectory) { prog->loadAll(inputDirectory); }
   
   // print all relations
   void printAll(const std::string &outputDirectory) { prog->printAll(outputDirectory); }      
    
   // dump input relations 
   void dumpInputs(std::ostream& out = std::cout) { prog->dumpInputs(out); }

   // dump output relations 
   void dumpOutputs(std::ostream& out = std::cout) { prog->dumpOutputs(out); }
};

//Create instance
SWIGSouffleProgram *newInstance(const std::string &name);