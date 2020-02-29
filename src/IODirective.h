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
 * Declaration of the I/O directive class
 *
 ***********************************************************************/

#pragma once

#include "Util.h"
#include <map>
#include <sstream>
#include <string>

namespace souffle {

/**
 * @class IODirective
 * @brief An IO directive is a registry describing a single I/O operation;
 *        the registry contains key/value pairs describing the nature of the
 *        I/O operation.
 *
 */
class IODirective {
public:
    IODirective() = default;
    IODirective(const std::map<std::string, std::string>& directiveMap) {
        for (const auto& pair : directiveMap) {
            registry[pair.first] = pair.second;
        }
    }
    virtual ~IODirective() = default;

    /**
     * @brief check whether registry is empty
     */
    bool isEmpty() const {
        return registry.empty();
    }

    /**
     * @brief get value for a given key
     */
    const std::string& get(const std::string& key) const {
        if (registry.count(key) == 0) {
            throw std::invalid_argument("Requested IO directive <" + key + "> was not specified");
        }
        return registry.at(key);
    }

    /**
     * @brief get value for a given key; if not found, return default value.
     */
    const std::string& getOr(const std::string& key, const std::string& defaultValue) const {
        if (this->has(key)) {
            return registry.at(key);
        }
        return defaultValue;
    }

    /**
     * @brief set value for a given key
     */
    void set(const std::string& key, const std::string& value) {
        registry[key] = value;
    }

    /**
     * @brief check whether key exists
     */
    bool has(const std::string& key) const {
        return registry.count(key) > 0;
    }

    // TODO (b-scholz): printing is clumsy; use join; should be in protected section

    friend std::ostream& operator<<(std::ostream& out, const IODirective& ioDirective) {
        ioDirective.print(out);
        return out;
    }

    bool operator==(const IODirective& other) const {
        return registry == other.registry;
    }

    bool operator!=(const IODirective& other) const {
        return registry != other.registry;
    }

protected:
    // TODO (b-scholz): clumsy printing / use join
    void print(std::ostream& out) const {
        auto cur = registry.begin();
        if (cur == registry.end()) {
            return;
        }
        out << "{{\"" << cur->first << "\",\"" << escape(cur->second) << "\"}";
        ++cur;
        for (; cur != registry.end(); ++cur) {
            out << ",{\"" << cur->first << "\",\"" << escape(cur->second) << "\"}";
        }
        out << '}';
    }

    /** key/value registry for a I/O directive */
    std::map<std::string, std::string> registry;
};
}  // namespace souffle
