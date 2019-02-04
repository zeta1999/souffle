#pragma once

#include "AstType.h"
#include "Util.h"
#include <sstream>

namespace souffle {

class TypeEnvironment;
class Type;

// Forward declarations
class PrimitiveAType;
class ConstantAType;
class TypeLattice;

enum class Kind { SYMBOL, NUMBER, RECORD };

class AnalysisType {
public:
    // Check the type is not a bottom or top type
    bool isValid() {
        return false;
    }

    virtual void print(std::ostream& os);
};

class TopAType : AnalysisType {
public:
    TopAType() {}
    void print(std::ostream& os) {
        os << "top";
    }
};

class BotAType : AnalysisType {
public:
    BotAType() {}
    void print(std::ostream& os) {
        os << "bottom";
    }
};

// Type that is not top or bottom
class InnerAType : AnalysisType {
public:
    bool isValid() {
        return true;
    }

    virtual Kind getKind();

    // Get the primitive type that is a supertype of this
    PrimitiveAType getPrimitive() {
        return PrimitiveAType(this->getKind());
    }

    // Get the constant type that is a subtype of this
    ConstantAType getConstant() {
        return ConstantAType(this->getKind());
    }
};

class PrimitiveAType : InnerAType {
private:
    Kind kind;

public:
    PrimitiveAType(Kind kind) : kind(kind) {}
    bool isValid() {
        return (kind != Kind::RECORD);
    }
    Kind getKind() {
        return kind;
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

class ConstantAType : InnerAType {
private:
    Kind kind;

public:
    ConstantAType(Kind kind) : kind(kind) {}
    Kind getKind() {
        return kind;
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

class BotPrimAType : InnerAType {
private:
    Kind kind;

public:
    BotPrimAType(Kind kind) : kind(kind) {}
    bool isValid() {
        return false;
    }
    Kind getKind() {
        return kind;
    }
    void print(std::ostream& os) {
        switch (kind) {
            case Kind::SYMBOL:
                os << "bot_symbol";
                break;
            case Kind::NUMBER:
                os << "bot_number";
                break;
            case Kind::RECORD:
                os << "bot_record";
                break;
        }
    }
};

class BaseAType : InnerAType {
private:
    Kind kind;
    AstTypeIdentifier name;

public:
    BaseAType(Kind kind, AstTypeIdentifier name) : kind(kind), name(name) {
        assert(kind != Kind::RECORD && "Base types are symbols and numbers only");
    }
    Kind getKind() {
        return kind;
    }
    void print(std::ostream& os) {
        os << name;
    }
};

class RecordAType : InnerAType {
private:
    AstTypeIdentifier name;
    std::vector<InnerAType> fields;

public:
    RecordAType(AstTypeIdentifier name) : name(name), fields() {}
    RecordAType(AstTypeIdentifier name, std::vector<InnerAType> fields) : name(name), fields(fields) {}
    void addField(InnerAType field) {
        fields.push_back(field);
    }
    Kind getKind() {
        return Kind::RECORD;
    }
    void print(std::ostream& os) {
        os << name;
    }
};

class UnionAType : InnerAType {
private:
    std::string representation;
    std::vector<BaseAType> bases;

public:
    UnionAType(std::vector<BaseAType> bases) : representation(), bases(bases) {
        std::stringstream repr;
        repr << join(bases, "|");
        representation = repr.str();
        assert(!bases.empty() && "Empty union is not allowed");
        assert(bases.size() > 1 && "Union with one element is just a base type");
        Kind kind = bases.front().getKind();
        for (BaseAType b : bases) {
            assert(b.getKind() == kind && "All components of union have the same type");
        }
    }
    UnionAType(std::vector<BaseAType> bases, AstTypeIdentifier name) : bases(bases) {
        std::stringstream repr;
        repr << name;
        representation = repr.str();
        assert(!bases.empty() && "Empty union is not allowed");
        assert(bases.size() > 1 && "Union with one element is just a base type");
        Kind kind = bases.front().getKind();
        for (BaseAType b : bases) {
            assert(b.getKind() == kind && "All components of union have the same type");
        }
    }
    Kind getKind() {
        return bases.front().getKind();
    }
    void print(std::ostream& os) {
        os << representation;
    }
};

class TypeLattice {
private:
    TypeEnvironment env;

public:
    // Initialise the type lattice from the types found in the type environment
    TypeLattice(const TypeEnvironment& env) : env(env) {
        // TODO
    }

    // Find the highest common subtype (intersection)
    AnalysisType meet(AnalysisType first, AnalysisType second);

    // Find the lowest common supertype (union)
    AnalysisType join(AnalysisType first, AnalysisType second);

    // Check if the first is a subtype of the second
    bool isSubtype(AnalysisType first, AnalysisType second) const;

    // Get the contained type environment
    TypeEnvironment getEnvironment() const {
        return env;
    }

    // Pack a type environment type into a lattice type
    InnerAType convert(const Type& other);

    // Get a type from its identifier
    const InnerAType& getType(const AstTypeIdentifier& ident) const;
};

}  // end of namespace souffle
