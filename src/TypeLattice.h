#pragma once

#include "AstType.h"
#include "Util.h"
#include <cassert>
#include <deque>
#include <sstream>
#include <vector>

namespace souffle {

class TypeAnalysis;
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
    const TypeLattice* lattice;

protected:
    AnalysisType(const TypeLattice* lattice) : lattice(lattice) {}

public:
    // Check the type is not a bottom or top type
    virtual bool isValid() const = 0;

    // Check if a type is numeric
    bool isNumeric() const {
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
    TopAType(const TypeLattice* lattice) : AnalysisType(lattice) {}

public:
    bool isValid() const override {
        return false;
    }

    void print(std::ostream& os) const override {
        os << "top";
    }
};

class BotAType : public AnalysisType {
    friend class TypeLattice;

private:
    BotAType(const TypeLattice* lattice) : AnalysisType(lattice) {}

public:
    bool isValid() const override {
        return false;
    }

    void print(std::ostream& os) const override {
        os << "bottom";
    }
};

// Type that is not top or bottom
class InnerAType : public AnalysisType {
    friend class TypeLattice;

protected:
    InnerAType(const TypeLattice* lattice) : AnalysisType(lattice) {}

public:
    bool isValid() const override {
        return true;
    }

    bool isNumeric() const {
        return getKind() == Kind::NUMBER;
    }

    virtual Kind getKind() const = 0;

    // Get the primitive type that is a supertype of this
    const PrimitiveAType* getPrimitive() const;

    // Get the constant type that is a subtype of this
    const ConstantAType* getConstant() const;
};

class PrimitiveAType : public InnerAType {
    friend class TypeLattice;

private:
    Kind kind;

private:
    PrimitiveAType(const TypeLattice* lattice, Kind kind) : InnerAType(lattice), kind(kind) {}

public:
    bool isValid() const override {
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
    ConstantAType(const TypeLattice* lattice, Kind kind) : InnerAType(lattice), kind(kind) {}

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
    BotPrimAType(const TypeLattice* lattice, Kind kind) : InnerAType(lattice), kind(kind) {}

public:
    bool isValid() const override {
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
    BaseAType(const TypeLattice* lattice, Kind kind, AstTypeIdentifier name)
            : InnerAType(lattice), kind(kind), name(name) {
        assert(kind != Kind::RECORD && "Base type cannot be a record");
    }

public:
    Kind getKind() const override {
        return kind;
    }
    void print(std::ostream& os) const override {
        os << name;
    }
};

// TODO (#380): When record unions are allowed, records inherit from base types
class RecordAType : public InnerAType {
    friend class TypeLattice;

private:
    AstTypeIdentifier name;
    std::vector<const InnerAType*> fields{};

private:
    RecordAType(const TypeLattice* lattice, AstTypeIdentifier name) : InnerAType(lattice), name(name) {}

    void addField(const InnerAType* field) {
        assert(field->isValid() && "Field type must be valid");
        fields.push_back(field);
    }

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
    std::set<const BaseAType*> bases;

private:
    UnionAType(const TypeLattice* lattice, std::set<const BaseAType*> bases);
    UnionAType(const TypeLattice* lattice, std::set<const BaseAType*> bases, AstTypeIdentifier name);
    const std::set<const BaseAType*>& getBases() const {
        return bases;
    }
    void setName(AstTypeIdentifier name);

public:
    Kind getKind() const override {
        return (*bases.begin())->getKind();
    }
    void print(std::ostream& os) const override {
        os << representation;
    }
};

class TypeLattice {
    friend class TypeAnalysis;

private:
    bool valid;
    const TypeEnvironment* env;
    TopAType top;
    BotAType bot;
    std::map<Kind, PrimitiveAType> primitives{};
    std::map<Kind, ConstantAType> constants{};
    std::map<Kind, BotPrimAType> botprims{};
    std::deque<BaseAType> bases{};
    std::deque<RecordAType> records{};
    std::deque<UnionAType> unions{};
    std::map<AstTypeIdentifier, const InnerAType*> aliases{};

private:
    const InnerAType* addType(const Type* type);

    // Initialise the type lattice from the types found in the type environment
    TypeLattice(const TypeEnvironment* env);

    // Temporary constructor, produces invalid lattice
    TypeLattice() : valid(false), env(), top(this), bot(this) {}

    // Makes previously invalid lattice valid again
    void setEnvironment(const TypeEnvironment* env);

public:
    TypeLattice(const TypeLattice& lattice) = delete;

    bool isValid() const {
        return valid;
    }

    // Find the highest common subtype (intersection)
    const AnalysisType* meet(const AnalysisType* first, const AnalysisType* second);

    // Find the lowest common supertype (union)
    const AnalysisType* join(const AnalysisType* first, const AnalysisType* second);

    // Check if the first is a subtype of the second
    bool isSubtype(const AnalysisType* first, const AnalysisType* second) const;

    // Get the contained type environment
    const TypeEnvironment* getEnvironment() const {
        return env;
    }

    // Get a lattice type from its type environment type
    const InnerAType* getType(const Type& type) const;

    // Get a type from its identifier
    const InnerAType* getType(const AstTypeIdentifier& ident) const {
        assert(aliases.count(ident) > 0 && "Type is not in lattice");
        return aliases.find(ident)->second;
    }

    // Get a primitive type
    const PrimitiveAType* getPrimitive(Kind kind) const {
        return &primitives.find(kind)->second;
    }

    // Get a constant type
    const ConstantAType* getConstant(Kind kind) const {
        return &constants.find(kind)->second;
    };

    // Get a bottom primitive type
    const BotPrimAType* getBotPrim(Kind kind) const {
        return &botprims.find(kind)->second;
    }

    // Get the top type
    const TopAType* getTop() const {
        return &top;
    };
};

}  // end of namespace souffle
