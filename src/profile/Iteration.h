/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2016, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "Rule.h"
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <vector>

namespace souffle {
namespace profile {

/*
 * Represents recursive profile data
 */
class Iteration {
private:
    double starttime = 0;
    double endtime = 0;
    size_t numTuples = 0;
    double copytime = 0;
    std::string locator = "";

    std::unordered_map<std::string, std::shared_ptr<Rule>> rules;

public:
    Iteration() : rules() {}

    void addRule(const std::string& ruleKey, std::shared_ptr<Rule>& rule) {
        rules[ruleKey] = rule;
    }

    const std::unordered_map<std::string, std::shared_ptr<Rule>>& getRules() const {
        return rules;
    }

    std::string toString() const {
        std::ostringstream output;

        output << getRuntime() << "," << numTuples << "," << copytime << ",";
        output << " recRule:";
        for (auto& rul : rules) {
            output << rul.second->toString();
        }
        output << "\n";
        return output.str();
    }

    double getRuntime() const {
        return endtime - starttime;
    }

    double getStarttime() const {
        return starttime;
    }

    double getEndtime() const {
        return endtime;
    }

    size_t size() const {
        return numTuples;
    }

    void setNumTuples(long numTuples) {
        this->numTuples = numTuples;
    }

    double getCopytime() const {
        return copytime;
    }

    void setCopytime(double copy_time) {
        this->copytime = copy_time;
    }

    void setStarttime(double time) {
        starttime = time;
    }

    void setEndtime(double time) {
        endtime = time;
    }

    const std::string& getLocator() const {
        return locator;
    }

    void setLocator(std::string locator) {
        this->locator = locator;
    }
};

}  // namespace profile
}  // namespace souffle
