/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstQualifiedName.h
 *
 * Defines a class for qualified names so that components can be accessed.
 * Qualified names are used for types and relations.
 *
 ***********************************************************************/

#pragma once

#include "Util.h"

#include <algorithm>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

/**
 * Qualified name class, e.g.,
 *       problem.graph.edge
 * used to access names inside and outside of components.
 */
class AstQualifiedName {
public:
    AstQualifiedName(const std::string& name = "") : qualifiers(toVector(name)) {}
    AstQualifiedName(const char* name) : AstQualifiedName(std::string(name)) {}
    AstQualifiedName(std::vector<std::string> qualifiers) : qualifiers(std::move(qualifiers)) {}
    AstQualifiedName(const AstQualifiedName&) = default;
    AstQualifiedName(AstQualifiedName&&) = default;

    AstQualifiedName& operator=(const AstQualifiedName&) = default;
    AstQualifiedName& operator=(AstQualifiedName&&) = default;

    /** append qualifier */
    void append(const std::string& qualifier) {
        qualifiers.push_back(qualifier);
    }

    /** prepend qualifier */
    void prepend(const std::string& qualifier) {
        qualifiers.insert(qualifiers.begin(), qualifier);
    }

    /** check whether qualified name is the empty name */
    bool empty() const {
        return qualifiers.empty();
    }

    /** get qualifiers */
    const std::vector<std::string>& getQualifiers() const {
        return qualifiers;
    }

    /** get qualified name as a string */
    std::string toString() const {
        std::stringstream ss;
        ss << join(qualifiers, ".");
        return ss.str();
    }

    bool operator==(const AstQualifiedName& other) const {
        return qualifiers == other.qualifiers;
    }

    bool operator!=(const AstQualifiedName& other) const {
        return !(*this == other);
    }

    bool operator<(const AstQualifiedName& other) const {
        return std::lexicographical_compare(
                qualifiers.begin(), qualifiers.end(), other.qualifiers.begin(), other.qualifiers.end());
    }

    void print(std::ostream& out) const {
        out << join(qualifiers, ".");
    }

    friend std::ostream& operator<<(std::ostream& out, const AstQualifiedName& qualifiedName);

public:
    /* list of name qualifiers */
    std::vector<std::string> qualifiers;
};

inline std::ostream& operator<<(std::ostream& out, const AstQualifiedName& qualifiedName) {
    qualifiedName.print(out);
    return out;
}

/** prefix operator for qualified names */
inline AstQualifiedName operator+(const std::string& name, const AstQualifiedName& id) {
    AstQualifiedName res = id;
    res.prepend(name);
    return res;
}

}  // namespace souffle
