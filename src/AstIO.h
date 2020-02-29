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
#include "AstQualifiedName.h"

#include <map>
#include <string>

namespace souffle {

/**
 * @class AstIO
 * @brief Intermediate representation of IO operations.
 */
class AstIO : public AstNode {
public:
    AstIO(const AstIO& io) : name(io.name), kvps(io.kvps) {
        this->setSrcLoc(io.getSrcLoc());
    }
    AstIO() = default;

    void print(std::ostream& os) const override {
        auto temp = kvps;
        temp.erase("operation");
        os << "." << kvps.at("operation") << " ";
        os << name;
        if (!temp.empty()) {
            os << "(" << join(temp, ",", [](std::ostream& out, const auto& arg) {
                out << arg.first << "=\"" << arg.second << "\"";
            }) << ")";
        }
    }

    /** relation name of I/O-directive */
    const AstQualifiedName& getQualifiedName() const {
        return name;
    }

    /** set relation name of I/O-directive */
    void setQualifiedName(const AstQualifiedName& name) {
        this->name = name;
    }

    /** add key-value pair */
    void addKVP(const std::string& key, const std::string& value) {
        kvps[key] = unescape(value);
    }

    /** get value */
    const std::string& getKVP(const std::string& key) const {
        return kvps.at(key);
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

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstIO*>(&node));
        const auto& other = static_cast<const AstIO&>(node);
        return other.name == name && other.kvps == kvps;
    }

    /** relation name of I/O directive */
    AstQualifiedName name;

    /** key-value pair map */
    // TODO (b-scholz): replace map by an I/O directive
    std::map<std::string, std::string> kvps;
};

}  // end of namespace souffle
