/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstIO.h
 *
 * Define the classes representing IO operations.
 *
 ***********************************************************************/

#pragma once

#include "AstNode.h"
#include "AstRelationIdentifier.h"

#include <map>
#include <string>

namespace souffle {

/**
 * @class AstIO
 * @brief Intermediate representation of IO operations.
 */
class AstIO : public AstNode {
public:
    AstIO(const AstIO& io) : name(io.name), kvps(io.kvps) {}
    AstIO() = default;

    void print(std::ostream& os) const override {
        os << name;
        if (!kvps.empty()) {
           os << "(" << join(kvps, ",") << ")"; 
        }
    }

    /** relation name of I/O-directive */
    const AstRelationIdentifier& getName() const {
        return name; 
    }

    /** set relation name of I/O-directive */
    void setName(const AstRelationIdentifier& name) {
        this->name = name;
    }

    /** add key-value pair */
    void addKVP(const std::string& key, const std::string& value) {
        kvps[key] = unescape(value);
    }

    /** get I/O-directive map */
    const std::map<std::string, std::string>& getIODirectiveMap() const {
        return kvps;
    }

    AstIO* clone() const override {
        auto res = new AstIO();
        res->name = name;
        res->kvps = kvps;
        res->setSrcLoc(getSrcLoc());
        return res;
    }

    std::string unescape(const std::string& inputString) const {
        std::string unescaped = unescape(inputString, "\\\"", "\"");
        unescaped = unescape(unescaped, "\\t", "\t");
        unescaped = unescape(unescaped, "\\r", "\r");
        unescaped = unescape(unescaped, "\\n", "\n");
        return unescaped;
    }

    std::string unescape(
            const std::string& inputString, const std::string& needle, const std::string& replacement) const {
        std::string result = inputString;
        size_t pos = 0;
        while ((pos = result.find(needle, pos)) != std::string::npos) {
            result = result.replace(pos, needle.length(), replacement);
            pos += replacement.length();
        }
        return result;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstIO*>(&node));
        const auto& other = static_cast<const AstIO&>(node);
        return other.name == name && other.kvps == kvps;
    }

    /** relation name of I/O directive */
    AstRelationIdentifier name;

    /** key-value pair map */
    std::map<std::string, std::string> kvps;
};

/**
 * @class AstStore
 * @brief Intermediate representation of a store operation.
 */
class AstStore : public AstIO {
public:
    AstStore(const AstIO& io) : AstIO(io) {
        setSrcLoc(io.getSrcLoc());
    }
    AstStore() = default;

    void print(std::ostream& os) const override {
        os << ".output ";
        AstIO::print(os);
    }

    AstStore* clone() const override {
        auto res = new AstStore();
        res->name = name;
        res->kvps = kvps;
        res->setSrcLoc(getSrcLoc());
        return res;
    }
};

/**
 * @class AstLoad
 * @brief Intermediate representation of a store operation.
 */
class AstLoad : public AstIO {
public:
    AstLoad(const AstIO& io) : AstIO(io) {
        setSrcLoc(io.getSrcLoc());
    }
    AstLoad() = default;

    void print(std::ostream& os) const override {
        os << ".input ";
        AstIO::print(os);
    }

    AstLoad* clone() const override {
        auto res = new AstLoad();
        res->name = name;
        res->kvps = kvps;
        res->setSrcLoc(getSrcLoc());
        return res;
    }
};

/**
 * @class AstPrintSize
 * @brief Intermediate representation of a summary store operation.
 */
class AstPrintSize : public AstStore {
public:
    AstPrintSize(const AstIO& io) : AstStore(io) {
        addKVP("IO", "stdoutprintsize");
    }
    AstPrintSize() {
        addKVP("IO", "stdoutprintsize");
    }

    void print(std::ostream& os) const override {
        os << ".printsize ";
        AstIO::print(os);
    }

    AstPrintSize* clone() const override {
        auto res = new AstPrintSize();
        res->name = name;
        res->kvps = kvps;
        res->setSrcLoc(getSrcLoc());
        return res;
    }
};

}  // end of namespace souffle
