#include "SwigInterface.h"

SWIGSouffleProgram *newInstance(const std::string &name) {
     std::cout << "Instance " << name << "\n";
     souffle::SouffleProgram *prog = souffle::ProgramFactory::newInstance(name);
     SWIGSouffleProgram *p = new SWIGSouffleProgram(prog); 
     return p;
}
