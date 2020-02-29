/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file IODirective.h
 *
 ***********************************************************************/

#pragma once

#include <map>
#include <sstream>
#include <string>

namespace souffle {



/**
 * @class IODirective
 * @brief Describes an I/O operation in RAM, synthesiser, and interpreter
 * 
 * TODO (b-scholz): confused interface; either we have a pure generic interface
 *                  using get/set or specialize it to all aspects of I/O. 
 *                  at the moment this is a mixed approach and quite bad. 
 */
class IODirective {
public:
    IODirective() = default;
    IODirective(const std::map<std::string, std::string>& directiveMap) {
        for (const auto& pair : directiveMap) {
            directives[pair.first] = pair.second;
        }
    }
    virtual ~IODirective() = default;

    /**
     * @brief check whether registry is empty
     */ 
    bool isEmpty() {
        return directives.empty();
    }

    /**
     * @brief get value for a given key
     */
    const std::string& get(const std::string& key) const {
        if (directives.count(key) == 0) {
            throw std::invalid_argument("Requested IO directive <" + key + "> was not specified");
        }
        return directives.at(key);
    }

    /**
     * @brief get value for a given key; if not found, return default value. 
     */
    const std::string& getOr(const std::string& key, const std::string& defaultValue) const {
        if (this->has(key)) {
            return directives.at(key);
        }
        return defaultValue;
    }

    /**
     * @brief set value for a given key
     */
    void set(const std::string& key, const std::string& value) {
        directives[key] = value;
    }

    /**
     * @brief check whether key exists
     */
    bool has(const std::string& key) const {
        return directives.count(key) > 0;
    }

    // TODO (b-scholz): remove this method 
    const std::string& getIOType() const {
        return get("IO");
    }

    // TODO (b-scholz): remove this method 
    void setIOType(const std::string& type) {
        directives["IO"] = type;
    }

    // TODO (b-scholz): remove this method 
    const std::string& getFileName() const {
        return get("filename");
    }

    // TODO (b-scholz): remove this method 
    void setFileName(const std::string& filename) {
        directives["filename"] = filename;
    }

    // TODO (b-scholz): remove this method 
    const std::string& getRelationName() const {
        return get("name");
    }

    // TODO (b-scholz): remove this method 
    void setRelationName(const std::string& name) {
        directives["name"] = name;
    }


    // TODO (b-scholz): printing is clumsy; use join; should be in protected section
    void print(std::ostream& out) const {
        auto cur = directives.begin();
        if (cur == directives.end()) {
            return;
        }

        out << "{{\"" << cur->first << "\",\"" << escape(cur->second) << "\"}";
        ++cur;
        for (; cur != directives.end(); ++cur) {
            out << ",{\"" << cur->first << "\",\"" << escape(cur->second) << "\"}";
        }
        out << '}';
    }

    friend std::ostream& operator<<(std::ostream& out, const IODirective& ioDirectives) {
        ioDirectives.print(out);
        return out;
    }

    bool operator==(const IODirective& other) const {
        return directives == other.directives;
    }

    bool operator!=(const IODirective& other) const {
        return directives != other.directives;
    }

private:
    // TODO (b-scholz): this method has no state => move to Util.h
    std::string escape(const std::string& inputString) const {
        std::string escaped = escape(inputString, "\"", "\\\"");
        escaped = escape(escaped, "\t", "\\t");
        escaped = escape(escaped, "\r", "\\r");
        escaped = escape(escaped, "\n", "\\n");
        return escaped;
    }

    // TODO (b-scholz): this method has no state => move to Util.h
    std::string escape(
            const std::string& inputString, const std::string& needle, const std::string& replacement) const {
        std::string result = inputString;
        size_t pos = 0;
        while ((pos = result.find(needle, pos)) != std::string::npos) {
            result = result.replace(pos, needle.length(), replacement);
            pos += replacement.length();
        }
        return result;
    }

    /** key/value store */ 
    std::map<std::string, std::string> directives;
};
}  // namespace souffle
