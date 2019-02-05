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
    friend class TypeLattice;
protected:
    const TypeLattice& lattice;
protected:
    AnalysisType(const TypeLattice& lattice) : lattice(lattice) {}
public:
    // Check the type is not a bottom or top type
    bool isValid() const {
        return false;
    }

    virtual void print(std::ostream& os) const = 0;

    friend std::ostream& operator<<(std::ostream& out, const AnalysisType& type) {
        type.print(out);
        return out;
    }
};

class TopAType : public AnalysisType {
    friend class TypeLattice;
private:
    TopAType(const TypeLattice& lattice) : AnalysisType(lattice) {}
public:
    void print(std::ostream& os) const override {
        os << "top";
    }
};

class BotAType : public AnalysisType {
    friend class TypeLattice;
private:
    BotAType(const TypeLattice& lattice) : AnalysisType(lattice) {}
public:
    void print(std::ostream& os) const override {
        os << "bottom";
    }
};

// Type that is not top or bottom
class InnerAType : public AnalysisType {
    friend class TypeLattice;
protected:
    InnerAType(const TypeLattice& lattice) : AnalysisType(lattice) {}
public:
    bool isValid() const {
        return true;
    }

    virtual Kind getKind() const = 0;

    // Get the primitive type that is a supertype of this
    const PrimitiveAType& getPrimitive();

    // Get the constant type that is a subtype of this
    const ConstantAType& getConstant();
};

class PrimitiveAType : public InnerAType {
    friend class TypeLattice;
private:
    Kind kind;
private:
    PrimitiveAType(const TypeLattice& lattice, Kind kind) : InnerAType(lattice), kind(kind) {}
public:
    bool isValid() const {
        return (kind != Kind::RECORD);
    }
    Kind getKind() const override {
        return kind;
    }
    void print(std::ostream& os) const override {
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

class ConstantAType : public InnerAType {
    friend class TypeLattice;
private:
    Kind kind;
private:
    ConstantAType(const TypeLattice& lattice, Kind kind) : InnerAType(lattice), kind(kind) {}
public:
    Kind getKind() const override {
        return kind;
    }
    void print(std::ostream& os) const override {
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

class BotPrimAType : public InnerAType {
    friend class TypeLattice;
private:
    Kind kind;
private:
    BotPrimAType(const TypeLattice& lattice, Kind kind) : InnerAType(lattice), kind(kind) {}
public:
    bool isValid() const {
        return false;
    }
    Kind getKind() const override {
        return kind;
    }
    void print(std::ostream& os) const override {
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

class BaseAType : public InnerAType {
    friend class TypeLattice;
private:
    Kind kind;
    AstTypeIdentifier name;
private:
    BaseAType(const TypeLattice& lattice, Kind kind, AstTypeIdentifier name) : InnerAType(lattice), kind(kind), name(name) {
        assert(kind != Kind::RECORD && "Base types are symbols and numbers only");
    }
public:
    Kind getKind() const override {
        return kind;
    }
    void print(std::ostream& os) const override {
        os << name;
    }
};

class RecordAType : public InnerAType {
    friend class TypeLattice;
private:
    AstTypeIdentifier name;
    std::vector<InnerAType*> fields;
private:
    RecordAType(const TypeLattice& lattice, AstTypeIdentifier name, std::vector<InnerAType*> fields) : InnerAType(lattice), name(name), fields(fields) {}
public:
    Kind getKind() const override {
        return Kind::RECORD;
    }
    void print(std::ostream& os) const override {
        os << name;
    }
};

class UnionAType : public InnerAType {
    friend class TypeLattice;
private:
    std::string representation;
    std::vector<BaseAType*> bases;
private:
    UnionAType(const TypeLattice& lattice, std::vector<BaseAType*> bases);
    UnionAType(const TypeLattice& lattice, std::vector<BaseAType*> bases, AstTypeIdentifier name);
public:
    Kind getKind() const override {
        return bases.front()->getKind();
    }
    void print(std::ostream& os) const override {
        os << representation;
    }
};

class TypeLattice {
private:
    const TypeEnvironment& env;

public:
    // Initialise the type lattice from the types found in the type environment
    TypeLattice(const TypeEnvironment& env) : env(env) {
        // TODO
    }

    // Find the highest common subtype (intersection)
    const AnalysisType& meet(const AnalysisType& first, const AnalysisType& second);

    // Find the lowest common supertype (union)
    const AnalysisType& join(const AnalysisType& first, const AnalysisType& second);

    // Check if the first is a subtype of the second
    bool isSubtype(const AnalysisType& first, const AnalysisType& second) const;

    // Get the contained type environment
    const TypeEnvironment& getEnvironment() const {
        return env;
    }

    // Pack a type environment type into a lattice type
    const InnerAType& convert(const Type& other) const;

    // Get a type from its identifier
    const InnerAType& getType(const AstTypeIdentifier& ident) const;

    // Get a primitive type
    const PrimitiveAType& getPrimitive(Kind kind) const;

    // Get a constant type
    const ConstantAType& getConstant(Kind kind) const;
};

}  // end of namespace souffle
