/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include <string>

namespace souffle {
namespace profile {

/*
 * Class to hold information about souffle Atom profile information
 */
class Atom {
public:
    const std::string identifier;
    const std::string rule;
    const size_t level;
    const size_t frequency;

    Atom(std::string identifier, std::string rule, size_t level, size_t frequency)
            : identifier(std::move(identifier)), rule(std::move(rule)), level(level), frequency(frequency) {}

    bool operator<(const Atom& other) const {
        if (rule != other.rule) {
            return rule < other.rule;
        } else if (identifier != other.identifier) {
            return identifier < other.identifier;
        }
        return level < other.level;
    }
};

}  // namespace profile
}  // namespace souffle
