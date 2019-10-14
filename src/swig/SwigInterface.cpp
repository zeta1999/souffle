#include "SwigInterface.h"

/**
 * Enables an instance of name to be created
 */
SWIGSouffleProgram* newInstance(const std::string& name) {
    souffle::SouffleProgram* prog = souffle::ProgramFactory::newInstance(name);
    SWIGSouffleProgram* p = new SWIGSouffleProgram(prog);
    return p;
}
