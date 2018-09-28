/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2016, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "Iteration.h"
#include "Rule.h"
#include <memory>
#include <sstream>
#include <string>
#include <unordered_map>
#include <utility>
#include <vector>

namespace souffle {
namespace profile {

/*
 * Stores the iterations and rules of a given relation
 */
class Relation {
private:
    const std::string name;
    double starttime = 0;
    double endtime = 0;
    double loadtime = 0;
    double savetime = 0;
    long prev_num_tuples = 0;
    long num_tuples = 0;
    size_t preMaxRSS = 0;
    size_t postMaxRSS = 0;
    const std::string id;
    std::string locator;
    int rul_id = 0;
    int rec_id = 0;

    std::vector<std::shared_ptr<Iteration>> iterations;

    std::unordered_map<std::string, std::shared_ptr<Rule>> ruleMap;

    bool ready = true;

public:
    Relation(std::string name, std::string id) : name(std::move(name)), id(std::move(id)) {
        ruleMap = std::unordered_map<std::string, std::shared_ptr<Rule>>();
        iterations = std::vector<std::shared_ptr<Iteration>>();
    }

    std::string createID() {
        return "N" + id.substr(1) + "." + std::to_string(++rul_id);
    }

    std::string createRecID(std::string name) {
        for (auto& iter : iterations) {
            for (auto& rul : iter->getRul_rec()) {
                if (rul.second->getName().compare(name) == 0) {
                    return rul.second->getId();
                }
            }
        }
        return "C" + id.substr(1) + "." + std::to_string(++rec_id);
    }

    inline double getLoadtime() {
        return loadtime;
    }

    inline double getSavetime() {
        return savetime;
    }

    inline double getStarttime() {
        for (auto& iteration : iterations) {
            if (starttime == 0 || starttime > iteration->getStarttime()) {
                starttime = iteration->getStarttime();
            }
        }
        return starttime;
    }

    inline double getEndtime() {
        for (auto& iteration : iterations) {
            if (endtime < iteration->getEndtime()) {
                endtime = iteration->getEndtime();
            }
        }
        return endtime;
    }

    inline double getNonRecTime() {
        return endtime - starttime;
    }

    double getRecTime() {
        double result = 0;
        for (auto& iter : iterations) {
            result += iter->getRuntime();
        }
        return result;
    }

    double getCopyTime() {
        double result = 0;
        for (auto& iter : iterations) {
            result += iter->getCopy_time();
        }
        return result;
    }

    size_t getNum_tuplesRel() {
        size_t result = 0;
        for (auto& iter : iterations) {
            result += iter->getNum_tuples();
        }
        return num_tuples + result;
    }

    size_t getNum_tuplesRul() {
        size_t result = 0;
        for (auto& rul : ruleMap) {
            result += rul.second->getNum_tuples();
        }
        for (auto& iter : iterations) {
            for (auto& rul : iter->getRul_rec()) {
                result += rul.second->getNum_tuples();
            }
        }
        return result;
    }

    inline size_t getTotNum_tuples() {
        return getNum_tuplesRel();
    }

    inline size_t getMaxRSSDiff() {
        return postMaxRSS - preMaxRSS;
    }

    size_t getTotNumRec_tuples() {
        size_t result = 0;
        for (auto& iter : iterations) {
            for (auto& rul : iter->getRul_rec()) {
                result += rul.second->getNum_tuples();
            }
        }
        return result;
    }

    inline void setLoadtime(double loadtime) {
        this->loadtime = loadtime;
    }

    inline void setSavetime(double savetime) {
        this->savetime = savetime;
    }

    inline void setStarttime(double time) {
        starttime = time;
    }

    inline void setEndtime(double time) {
        endtime = time;
    }

    inline void setNum_tuples(long num_tuples) {
        this->num_tuples = num_tuples;
    }

    inline void setPostMaxRSS(size_t maxRSS) {
        this->postMaxRSS = maxRSS > postMaxRSS ? maxRSS : postMaxRSS;
    }

    inline void setPreMaxRSS(size_t maxRSS) {
        if (preMaxRSS == 0) {
            preMaxRSS = maxRSS;
            return;
        }
        this->preMaxRSS = maxRSS < preMaxRSS ? maxRSS : postMaxRSS;
    }

    std::string toString() {
        std::ostringstream output;
        output << "{\n\"" << name << "\":[" << getNonRecTime() << "," << num_tuples
               << "],\n\n\"onRecRules\":[\n";
        for (auto& rul : ruleMap) {
            output << rul.second->toString();
        }
        output << "\n],\n\"iterations\":\n";
        output << "[";
        if (iterations.empty()) {
            output << ", ";
        }
        for (auto& iter : iterations) {
            output << iter->toString();
            output << ", ";
        }
        std::string retStr = output.str();
        // substring to remove the last comma
        return retStr.substr(0, retStr.size() - 2) + "]\n}";
    }

    inline std::string getName() {
        return name;
    }

    /**
     * Return a map of Rules, indexed by srcLocator.
     *
     * @return the ruleMap
     */
    inline std::unordered_map<std::string, std::shared_ptr<Rule>>& getRuleMap() {
        return this->ruleMap;
    }

    std::vector<std::shared_ptr<Rule>> getRuleRecList() {
        std::vector<std::shared_ptr<Rule>> temp = std::vector<std::shared_ptr<Rule>>();
        for (auto& iter : iterations) {
            for (auto& rul : iter->getRul_rec()) {
                temp.push_back(rul.second);
            }
        }
        return temp;
    }

    inline std::vector<std::shared_ptr<Iteration>>& getIterations() {
        return this->iterations;
    }

    inline std::string getId() {
        return id;
    }

    inline std::string getLocator() {
        return locator;
    }

    inline void setLocator(std::string locator) {
        this->locator = locator;
    }

    inline bool isReady() {
        return this->ready;
    }

    inline void setReady(bool ready) {
        this->ready = ready;
    }

    inline long getPrev_num_tuples() {
        return prev_num_tuples;
    }

    inline void setPrev_num_tuples(long prev_num_tuples) {
        this->prev_num_tuples = prev_num_tuples;
    }
};

}  // namespace profile
}  // namespace souffle
