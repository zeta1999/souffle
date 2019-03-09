#include "AstTypeAnalysis.h"
#include "AstArgument.h"
#include "AstLiteral.h"
#include <ostream>

namespace souffle {

void TypeSolution::print(std::ostream& out) const {
    for (const auto& pair : typeMapping) {
        assert(pair.first != nullptr && "nullptr argument in type solution");
        assert(pair.second != nullptr && "nullptr analysis type in type solution");
        out << "type(" << *pair.first << ") = " << *pair.second << std::endl;
    }
}

} // end of namespace souffle
