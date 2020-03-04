/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file RWOperation.h
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
 * @class RWOperation
 * @brief An IO directive is a registry describing a single I/O operation;
 *        the registry contains key/value pairs describing the nature of the
 *        I/O operation.
 *
 */
class RWOperation {
public:
    RWOperation() = default;
    RWOperation(const std::map<std::string, std::string>& directiveMap) {
        for (const auto& pair : directiveMap) {
            registry[pair.first] = pair.second;
        }
    }
    virtual ~RWOperation() = default;

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

protected:
    /** key/value registry for a I/O directive */
    std::map<std::string, std::string> registry;
};
}  // namespace souffle
