class TypeEnvironment;
class Type;

// Forward declarations
class PrimitiveType;
class ConstantType;

class AnalysisType {
public:
    // Check the type is not a bottom or top type
    bool isValid();

    // Get the primitive type that is a supertype of this
    PrimitiveType getPrimitive();  // This will fail on the top type

    // Get the constant type that is a subtype of this
    ConstantType getConstant();  // This will fail on the bottom type
};

class PrimitiveType : AnalysisType {};

class ConstantType : AnalysisType {};

class TypeLattice {
public:
    // Initialise the type lattice from the types found in the type environment
    TypeLattice(const TypeEnvironment& env);

    // Find the highest common subtype (intersection)
    AnalysisType meet(AnalysisType first, AnalysisType second);

    // Find the lowest common supertype (union)
    AnalysisType join(AnalysisType first, AnalysisType second);

    // Check if the first is a subtype of the second
    bool isSubtype(AnalysisType first, AnalysisType second) const;

    // Get the constant number type
    ConstantType getNumberConstant() const;

    // Get the constant symbol type
    ConstantType getSymbolConstant() const;

    // Get the constant record type
    ConstantType getRecordConstant() const;

    // Get the top number type
    PrimitiveType getNumberType() const;

    // Get the top symbol type
    PrimitiveType getSymbolType() const;

    // Get the top record type
    PrimitiveType getRecordType() const;

    // Get the contained type environment
    TypeEnvironment getEnvironment() const;

    // Pack a type environment type into a lattice type
    AnalysisType convert(const Type& other);
};
