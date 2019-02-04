#pragma once

#include "AstType.h"
#include <ostream>

namespace souffle {

class TypeEnvironment;
class Type;

// Forward declarations
class PrimitiveAType;
class ConstantAType;
class TypeLattice;

enum class Kind = {SYMBOL, NUMBER, RECORD};

class AnalysisType {
public:
    // Check the type is not a bottom or top type
    bool isValid();

    // Get the primitive type that is a supertype of this, or top if none exists
    PrimitiveAType getPrimitive();

    // Get the constant type that is a subtype of this, or bot is none exists
    ConstantAType getConstant();

    void print(std::ostream& os);
};

class PrimitiveAType : AnalysisType {
private:
    Kind kind;

public:
    PrimitiveAType(Kind kind) : kind(kind) {}
    bool isValid() {
        return (kind != Kind::RECORD);
    }
    PrimitiveAType getPrimitive() {
        return PrimitiveAType(kind);
    }
    ConstantAType getConstant() {
        return ConstantAType(kind);
    }
    void print(std::ostream& os) {
        switch (kind) {
            case Kind::SYMBOL:
                os << "symbol";
                break;
            case Kind::NUMBER:
                os << "number";
                break;
            case Kind::RECORD:
                os << "record";
                break;
        }
    }
};

class ConstantAType : AnalysisType {
private:
    Kind kind;

public:
    ConstantAType(Kind kind) : kind(kind) {}
    bool isValid() {
        return true;
    }
    PrimitiveAType getPrimitive() {
        return PrimitiveAType(kind);
    }
    ConstantAType getConstant() {
        return ConstantAType(kind);
    }
    void print(std::ostream& os) {
        switch (kind) {
            case Kind::SYMBOL:
                os << "const_symbol";
                break;
            case Kind::NUMBER:
                os << "const_number";
                break;
            case Kind::RECORD:
                os << "nil";
                break;
        }
    }
};

class BaseAType : AnalysisType {};

class RecordAType : AnalysisType {};

class UnionAType : AnalysisType {};

class TopAType : AnalysisType {};

class BotAType : AnalysisType {};

class BotPrimAType : AnalysisType {};

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

    // Get the contained type environment
    TypeEnvironment getEnvironment() const;

    // Pack a type environment type into a lattice type
    AnalysisType convert(const Type& other);

    // Get a type from its identifier
    const AnalysisType& getType(const AstTypeIdentifier& ident) const;
};

}  // end of namespace souffle
