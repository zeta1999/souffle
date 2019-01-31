#include "TypeSystem.h"

class AnalysisType {
public:
    // Check the type is not a bottom or top type
    bool isValid();

    // Get the primitive type that is a supertype of this
    PrimtiveType getPrimitive(); // This will fail on the top type

    // Get the constant type that is a subtype of this
    ConstantType getConstant(); // This will fail on the bottom type
};

class PrimitiveType : AnalysisType {};

class ConstantType : AnalysisType {};

class TypeLattice {
public:
    // Initialise the type lattice from the types found in the type environment
    TypeLattice(TypeEnvironment& env);

    // Find the highest common subtype (intersection)
    AnalysisType meet(AnalysisType first, AnalysisType second);

    // Find the lowest common supertype (union)
    AnalysisType join(AnalysisType first, AnalysisType second);
}
