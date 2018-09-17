/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2016, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "../ProfileEvent.h"
#include "HtmlString.h"
#include "OutputProcessor.h"
#include "Reader.h"
#include "Table.h"
#include "UserInputReader.h"
#include <algorithm>
#include <cstdio>
#include <iostream>
#include <memory>
#include <string>
#include <thread>
#include <vector>
#include <dirent.h>
#include <sys/ioctl.h>
#include <sys/stat.h>

namespace souffle {
namespace profile {

/*
 * Text User interface for SouffleProf
 * OutputProcessor creates a ProgramRun object
 * ProgramRun -> Reader.h ProgramRun stores all the data
 * OutputProcessor grabs the data and makes tables
 * Tui displays the data
 * TODO: move parts of the code into other classes, especially the outputJson function
 */
class Tui {
private:
    OutputProcessor out;
    bool loaded;
    std::string f_name;
    bool alive = false;
    std::thread updater;
    int sort_col = 0;
    int precision = -1;
    Table rel_table_state;
    Table rul_table_state;
    std::shared_ptr<Reader> reader;
    InputReader linereader;
    /// Limit results shown. Default value chosen to approximate unlimited
    size_t resultLimit = 20000;

    struct Usage {
        uint64_t time;
        uint32_t maxRSS;
        uint64_t systemtime;
        uint64_t usertime;
        bool operator<(const Usage& other) const {
            return time < other.time;
        }
    };

public:
    Tui(std::string filename, bool live, bool gui) {
        this->f_name = filename;

        std::shared_ptr<ProgramRun>& run = out.getProgramRun();

        this->reader = std::make_shared<Reader>(filename, run);

        this->alive = false;
        updateDB();
        this->loaded = reader->isLoaded();
    }

    Tui() {
        std::shared_ptr<ProgramRun>& run = out.getProgramRun();
        this->reader = std::make_shared<Reader>(run);
        this->loaded = true;
        this->alive = true;
        updateDB();
        updater = std::thread([this]() {
            // Update the display every 30s. Check for input every 0.5s
            std::chrono::milliseconds interval(30000);
            auto nextUpdateTime = std::chrono::high_resolution_clock::now();
            do {
                std::this_thread::sleep_for(std::chrono::milliseconds(500));
                if (nextUpdateTime < std::chrono::high_resolution_clock::now()) {
                    runCommand({});
                    nextUpdateTime = std::chrono::high_resolution_clock::now() + interval;
                }
            } while (reader->isLive() && !linereader.hasReceivedInput());
        });
    }

    ~Tui() {
        if (updater.joinable()) {
            updater.join();
        }
    }

    void runCommand(std::vector<std::string> c) {
        static bool firstRun = true;
        if (linereader.hasReceivedInput() && c.empty()) {
            return;
        }
        if (!loaded) {
            std::cout << "Error: File cannot be loaded\n";
            return;
        }

        if (alive) {
            updateDB();
            // remake tables to get new data
            rul_table_state = out.getRulTable();
            rel_table_state = out.getRelTable();

            setupTabCompletion();
        }

        // If we have not received any input yet in live mode then run top.
        if ((!linereader.hasReceivedInput() && c.empty()) || c[0].compare("top") == 0) {
            // Move up 3 lines and overwrite the previous top output.
            if (!firstRun && !linereader.hasReceivedInput()) {
                std::cout << "\x1b[A\x1b[A\x1b[A";
            } else if (firstRun) {
                firstRun = false;
            }
            top();
        } else if (c[0].compare("rel") == 0) {
            if (c.size() == 2) {
                relRul(c[1]);
            } else if (c.size() == 1) {
                rel();
            } else {
                std::cout << "Invalid parameters to rel command.\n";
            }
        } else if (c[0].compare("rul") == 0) {
            if (c.size() > 1) {
                if (c.size() == 3 && c[1].compare("id") == 0) {
                    std::printf("%7s%2s%s\n\n", "ID", "", "NAME");
                    id(c[2]);
                } else if (c.size() == 2 && c[1].compare("id") == 0) {
                    id("0");
                } else if (c.size() == 2) {
                    verRul(c[1]);
                } else {
                    std::cout << "Invalid parameters to rul command.\n";
                }
            } else {
                rul();
            }
        } else if (c[0].compare("graph") == 0) {
            if (c.size() == 3 && c[1].find(".") == std::string::npos) {
                iterRel(c[1], c[2]);
            } else if (c.size() == 3 && c[1].at(0) == 'C') {
                iterRul(c[1], c[2]);
            } else if (c.size() == 4 && c[1].compare("ver") == 0 && c[2].at(0) == 'C') {
                verGraph(c[2], c[3]);
            } else {
                std::cout << "Invalid parameters to graph command.\n";
            }
        } else if (c[0].compare("usage") == 0) {
            if (c.size() > 1) {
                if (c[1][0] == 'R') {
                    usageRelation(c[1]);
                } else {
                    usageRule(c[1]);
                }
            } else {
                usage();
            }
        } else if (c[0].compare("help") == 0) {
            help();
        } else if (c[0].compare("limit") == 0) {
            if (c.size() == 1) {
                setResultLimit(20000);
            } else {
                try {
                    setResultLimit(std::stoul(c[1]));
                } catch (...) {
                    std::cout << "Invalid parameters to limit command.\n";
                }
            }
        } else {
            std::cout << "Unknown command. Use \"help\" for a list of commands.\n";
        }
    }

    void runProf() {
        if (!loaded && !f_name.empty()) {
            std::cout << "Error: File cannot be floaded\n";
            return;
        }
        if (loaded) {
            std::cout << "SouffleProf\n";
            top();
        }

        linereader.setPrompt("\n> ");
        setupTabCompletion();

        while (true) {
            if (!loaded) {
                loadMenu();
                if (!f_name.empty()) {
                    std::cout << "Error loading file.\n";
                }
            }
            std::string untrimmedInput = linereader.getInput();
            std::string input = Tools::trimWhitespace(untrimmedInput);

            std::cout << std::endl;
            if (input.empty()) {
                std::cout << "Unknown command. Type help for a list of commands.\n";
                continue;
            }

            linereader.addHistory(input.c_str());

            std::vector<std::string> c = Tools::split(input, " ");

            if (c[0] == "q" || c[0] == "quit") {
                quit();
                break;
                //            } else if (c[0] == "load" || c[0] == "open") {
                //                if (c.size() == 2) {
                //                    load(c[0], c[1]);
                //                } else {
                //                    loadMenu();
                //                }
                //        } else if (c[0] == "save") {
                //            if (c.size() == 1) {
                //                std::cout << "Enter file name to save.\n";
                //            } else if (c.size() == 2) {
                //                save(c[1]);
                //            }
            } else if (c[0] == "sort") {
                if (c.size() == 2 && std::stoi(c[1]) < 7) {
                    sort_col = std::stoi(c[1]);
                } else {
                    std::cout << "Invalid column, please select a number between 0 and 6.\n";
                }
            } else {
                runCommand(c);
            }
        }
    }

    void outputJson() {
        std::cout << "SouffleProf\n";
        std::cout << "Generating JSON files...\n";

        std::string workingdir = Tools::getworkingdir();
        if (workingdir.size() == 0) {
            std::cerr << "Error getting working directory.\nTry run the profiler using an absolute path."
                      << std::endl;
            throw 1;
        }
        DIR* dir;
        bool exists = false;

        if ((dir = opendir((workingdir + std::string("/profiler_html")).c_str())) != nullptr) {
            exists = true;
            closedir(dir);
        }
        if (!exists) {
            std::string sPath = workingdir + std::string("/profiler_html");
            mode_t nMode = 0733;  // UNIX style permissions
            int nError = 0;
            nError = mkdir(sPath.c_str(), nMode);
            if (nError != 0) {
                std::cerr
                        << "directory ./profiler_html/ failed to be created. Please create it and try again.";
                exit(2);
            }
        }

        std::string new_file = workingdir + std::string("/profiler_html/");
        if (Tools::file_exists(new_file)) {
            int i = 1;
            while (Tools::file_exists(new_file + std::to_string(i) + ".html")) {
                i++;
            }

            new_file = new_file + std::to_string(i) + ".html";
        }

        std::ofstream outfile(new_file);

        HtmlString html;

        outfile << html.get_first_half();

        std::shared_ptr<ProgramRun>& run = out.getProgramRun();
        std::string source_loc;

        auto comma = [&outfile](bool& first, const std::string& delimiter = ", ") {
            if (!first) {
                outfile << delimiter;
            } else {
                first = false;
            }
        };
        outfile << R"_(data={"top":[)_" << run->getDoubleRuntime() << "," << run->getTotNumTuples() << "],\n";
        outfile << R"_("rel":{)_";
        bool firstRow = true;
        for (auto& _row : rel_table_state.getRows()) {
            comma(firstRow, ",\n");

            Row row = *_row;
            outfile << '"' << row[6]->toString(0) << R"_(": [)_";
            outfile << '"' << Tools::cleanJsonOut(row[5]->toString(0)) << R"_(", )_";
            outfile << '"' << Tools::cleanJsonOut(row[6]->toString(0)) << R"_(", )_";
            outfile << row[0]->getDoubVal() << ", ";
            outfile << row[1]->getDoubVal() << ", ";
            outfile << row[2]->getDoubVal() << ", ";
            outfile << row[3]->getDoubVal() << ", ";
            outfile << row[4]->getLongVal() << ", ";
            outfile << '"' << Tools::cleanJsonOut(row[7]->toString(0)) << R"_(", [)_";

            source_loc = row[7]->toString(0);

            bool firstCol = true;
            for (auto& _rel_row : rul_table_state.getRows()) {
                Row rel_row = *_rel_row;
                if (rel_row[7]->toString(0) == row[5]->toString(0)) {
                    comma(firstCol);
                    outfile << '"' << rel_row[6]->toString(0) << '"';
                }
            }
            outfile << "], ";
            std::vector<std::shared_ptr<Iteration>> iter =
                    run->getRelation_map()[row[5]->toString(0)]->getIterations();
            outfile << R"_({"tot_t": [)_";
            firstCol = true;
            for (auto& i : iter) {
                comma(firstCol);
                outfile << i->getRuntime();
            }
            outfile << R"_(], "copy_t": [)_";
            firstCol = true;
            for (auto& i : iter) {
                comma(firstCol);
                outfile << i->getCopy_time();
            }
            outfile << R"_(], "tuples": [)_";
            firstCol = true;
            for (auto& i : iter) {
                comma(firstCol);
                outfile << i->getNum_tuples();
            }
            outfile << "]}]";
        }
        outfile << "},\n";
        outfile << R"_("rul": {)_";

        firstRow = true;
        for (auto& _row : rul_table_state.getRows()) {
            Row row = *_row;

            std::vector<std::string> part = Tools::split(row[6]->toString(0), ".");
            std::string strRel = "R" + part[0].substr(1);
            Table ver_table = out.getVersions(strRel, row[6]->toString(0));

            std::string src;
            if (ver_table.rows.size() > 0) {
                if (ver_table.rows[0]->cells[9] != nullptr) {
                    src = (*ver_table.rows[0])[9]->toString(0);
                } else {
                    src = "-";
                }
            } else {
                src = row[10]->toString(-1);
            }
            comma(firstRow);
            outfile << "\n ";

            outfile << '"' << row[6]->toString(0) << R"_(": [)_";
            outfile << '"' << Tools::cleanJsonOut(row[5]->toString(0)) << R"_(", )_";
            outfile << '"' << Tools::cleanJsonOut(row[6]->toString(0)) << R"_(", )_";
            outfile << row[0]->getDoubVal() << ", ";
            outfile << row[1]->getDoubVal() << ", ";
            outfile << row[2]->getDoubVal() << ", ";
            outfile << row[3]->getDoubVal() << ", ";
            outfile << row[4]->getLongVal() << ", ";

            outfile << '"' << src << R"_(", )_";
            outfile << "[";

            bool has_ver = false;
            bool firstCol = true;
            for (auto& _ver_row : ver_table.getRows()) {
                comma(firstCol);
                has_ver = true;
                Row ver_row = *_ver_row;
                outfile << '[';
                outfile << '"' << Tools::cleanJsonOut(ver_row[5]->toString(0)) << R"_(", )_";
                outfile << '"' << Tools::cleanJsonOut(ver_row[6]->toString(0)) << R"_(", )_";
                outfile << ver_row[0]->getDoubVal() << ", ";
                outfile << ver_row[1]->getDoubVal() << ", ";
                outfile << ver_row[2]->getDoubVal() << ", ";
                outfile << ver_row[3]->getDoubVal() << ", ";
                outfile << ver_row[4]->getLongVal() << ", ";
                outfile << '"' << src << R"_(", )_";
                outfile << ver_row[8]->getLongVal();
                outfile << ']';
            }

            outfile << "], ";

            if (row[6]->toString(0).at(0) != 'C') {
                outfile << "{}, {}]";
            } else {
                outfile << R"_({"tot_t": [)_";

                std::vector<uint64_t> iteration_tuples;
                bool firstCol = true;
                for (auto& i : run->getRelation_map()[row[7]->toString(0)]->getIterations()) {
                    bool add = false;
                    double tot_time = 0.0;
                    uint64_t tot_num = 0.0;
                    for (auto& rul : i->getRul_rec()) {
                        if (rul.second->getId() == row[6]->toString(0)) {
                            tot_time += rul.second->getRuntime();

                            tot_num += rul.second->getNum_tuples();
                            add = true;
                        }
                    }
                    if (add) {
                        comma(firstCol);
                        outfile << tot_time;
                        iteration_tuples.push_back(tot_num);
                    }
                }
                outfile << R"_(], "tuples": [)_";
                firstCol = true;
                for (auto& i : iteration_tuples) {
                    comma(firstCol);
                    outfile << i;
                }

                outfile << "]}, {";

                if (has_ver) {
                    outfile << R"_("tot_t": [)_";

                    firstCol = true;
                    for (auto& row : ver_table.rows) {
                        comma(firstCol);
                        outfile << (*row)[0]->getDoubVal();
                    }
                    outfile << R"_(], "copy_t": [)_";

                    firstCol = true;
                    for (auto& row : ver_table.rows) {
                        comma(firstCol);
                        outfile << (*row)[3]->getDoubVal();
                    }
                    outfile << R"_(], "tuples": [)_";

                    firstCol = true;
                    for (auto& row : ver_table.rows) {
                        comma(firstCol);
                        outfile << (*row)[4]->getLongVal();
                    }
                    outfile << ']';
                }
                outfile << "}]";
            }
        }
        outfile << "\n}, \n";

        std::string source_file_loc = Tools::split(source_loc, " ").at(0);
        std::ifstream source_file(source_file_loc);
        if (!source_file.is_open()) {
            std::cout << "Error opening \"" << source_file_loc << "\", creating GUI without source locator."
                      << std::endl;
        } else {
            std::string str;
            outfile << R"_("code": [)_";
            bool firstCol = true;
            while (getline(source_file, str)) {
                comma(firstCol, ",\n");
                outfile << '"' << Tools::cleanJsonOut(str) << '"';
            }
            outfile << "],\n";
            source_file.close();
        }

        // Add usage statistics
        auto usages = getUsageStats(100);

        outfile << R"_("usage": [)_";
        firstRow = true;
        Usage previousUsage = *usages.begin();
        for (auto usage : usages) {
            comma(firstRow);
            outfile << '[';
            outfile << usage.time << ", ";
            outfile << (usage.usertime - previousUsage.usertime) / 1000000.0 << ", ";
            outfile << (usage.systemtime - previousUsage.systemtime) / 1000000.0 << ", ";
            outfile << usage.maxRSS * 1024 << ", ";
            outfile << '"';
            bool firstCol = true;
            for (auto& cur : out.getProgramRun()->getRelationsAtTime(
                         previousUsage.time / 1000000.0, usage.time / 1000000.0)) {
                comma(firstCol);
                outfile << cur->getName();
            }
            outfile << '"';
            outfile << ']';
            previousUsage = usage;
        }
        outfile << "]};\n";
        outfile << html.get_second_half();

        std::cout << "file output to: " << new_file << std::endl;
    }

    void loadMenu() {
        std::cout << "Please 'load' a file or 'open' from Previous Runs.\n";
        std::cout << "Previous Runs:\n";

        DIR* dir;
        struct dirent* ent;
        if ((dir = opendir("./old_runs")) != nullptr) {
            while ((ent = readdir(dir)) != nullptr) {
                // if the file doesnt exist in the working directory, it is in old_runs (to remove . and ..)
                if (!Tools::file_exists(ent->d_name)) {
                    printf("- %s\n", ent->d_name);
                }
            }
            closedir(dir);
        }
    }

    void quit() {
        if (updater.joinable()) {
            updater.join();
        }
    }

    void save(std::string save_name) {
        if (loaded) {
            // std::shared_ptr<ProgramRun>& run = out.getProgramRun();
            // Reader saver(this->f_name, run, false, false);
            // saver.save(save_name);
            std::cout << "Save not implemented.\n";
            // std::cout << "Save success.\n";
        } else {
            std::cout << "Save failed.\n";
        }
    }

    static void help() {
        std::cout << "\nAvailable profiling commands:" << std::endl;
        std::printf("  %-30s%-5s %s\n", "rel", "-", "display relation table.");
        std::printf("  %-30s%-5s %s\n", "rel <relation id>", "-", "display all rules of a given relation.");
        std::printf("  %-30s%-5s %s\n", "rul", "-", "display rule table");
        std::printf("  %-30s%-5s %s\n", "rul <rule id>", "-", "display all version of given rule.");
        std::printf("  %-30s%-5s %s\n", "rul id", "-", "display all rules names and ids.");
        std::printf(
                "  %-30s%-5s %s\n", "rul id <rule id>", "-", "display the rule name for the given rule id.");
        std::printf("  %-30s%-5s %s\n", "graph <relation id> <type>", "-",
                "graph a relation by type: (tot_t/copy_t/tuples).");
        std::printf("  %-30s%-5s %s\n", "graph <rule id> <type>", "-",
                "graph recursive(C) rule by type(tot_t/tuples).");
        std::printf("  %-30s%-5s %s\n", "graph ver <rule id> <type>", "-",
                "graph recursive(C) rule versions by type(tot_t/copy_t/tuples).");
        std::printf("  %-30s%-5s %s\n", "top", "-", "display top-level summary of program run.");
        std::printf("  %-30s%-5s %s\n", "usage [relation id|rule id]", "-",
                "display CPU usage graphs for a relation or rule.");
        std::printf("  %-30s%-5s %s\n", "help", "-", "print this.");

        std::cout << "\nInteractive mode only commands:" << std::endl;
        std::printf("  %-30s%-5s %s\n", "load <filename>", "-", "load the given profiler log file.");
        std::printf("  %-30s%-5s %s\n", "open", "-", "list stored souffle log files.");
        std::printf("  %-30s%-5s %s\n", "open <filename>", "-", "open the given stored log file.");
        std::printf("  %-30s%-5s %s\n", "save <filename>", "-", "store a copy of the souffle log file.");
        //    if (alive) std::printf("  %-30s%-5s %s\n", "stop", "-",
        //                "stop the current live run.");
        std::printf("  %-30s%-5s %s\n", "limit <row count>", "-", "limit number of results shown.");
        std::printf("  %-30s%-5s %s\n", "sort <col number>", "-", "sort tables by given column number.");
        std::printf("  %-30s%-5s %s\n", "q", "-", "exit program.");
    }

    void usageRelation(std::string id) {
        std::vector<std::vector<std::string>> rel_table = out.formatTable(rel_table_state, precision);
        std::string name = "";
        bool found = false;
        for (auto& row : rel_table) {
            if (row[5] == id || row[6] == id) {
                name = row[5];
                found = true;
                break;
            }
        }
        if (!found) {
            std::cout << "Relation does not exist.\n";
            return;
        }

        Relation* rel = out.getProgramRun()->getRelation(name);
        usage(rel->getEndtime() * 1000000, rel->getStarttime() * 1000000);
    }

    void usageRule(std::string id) {
        std::vector<std::vector<std::string>> rul_table = out.formatTable(rul_table_state, precision);
        std::string relName = "";
        std::string srcLocator = "";
        bool found = false;
        for (auto& row : rul_table) {
            if (row[5] == id || row[6] == id) {
                relName = row[7];
                srcLocator = row[10];
                found = true;
                break;
            }
        }
        if (!found) {
            std::cout << "Rule does not exist.\n";
            return;
        }

        auto* rel = out.getProgramRun()->getRelation(relName);
        if (rel == nullptr) {
            std::cout << "Relation ceased to exist. Odd." << std::endl;
            return;
        }
        if (rel->getRuleMap().count(srcLocator) == 0) {
            std::cout << "Rule ceased to exist. Odd." << std::endl;
            return;
        }

        auto& rul = rel->getRuleMap().at(srcLocator);
        usage(rul->getEndtime() * 1000000, rul->getStarttime() * 1000000);
    }

    std::set<Usage> getUsageStats(size_t width = size_t(-1)) {
        std::set<Usage> usages;
        DirectoryEntry* usageStats = dynamic_cast<DirectoryEntry*>(
                ProfileEventSingleton::instance().getDB().lookupEntry({"program", "usage", "timepoint"}));
        if (usageStats == nullptr || usageStats->getKeys().size() < 2) {
            return usages;
        }
        uint64_t endTime = 0;
        uint64_t startTime = 0;
        uint64_t timeStep = 0;
        // Translate the string ordered text usage stats to a time ordered binary form.
        std::set<Usage> allUsages;
        for (auto& currentKey : usageStats->getKeys()) {
            Usage currentUsage;
            currentUsage.time = std::stol(currentKey);
            currentUsage.systemtime = dynamic_cast<SizeEntry*>(
                    usageStats->readDirectoryEntry(currentKey)->readEntry("systemtime"))
                                              ->getSize();
            currentUsage.usertime = dynamic_cast<SizeEntry*>(
                    usageStats->readDirectoryEntry(currentKey)->readEntry("usertime"))
                                            ->getSize();
            currentUsage.maxRSS =
                    dynamic_cast<SizeEntry*>(usageStats->readDirectoryEntry(currentKey)->readEntry("maxRSS"))
                            ->getSize();

            // Duplicate times are possible
            if (allUsages.find(currentUsage) != allUsages.end()) {
                auto& existing = *allUsages.find(currentUsage);
                currentUsage.systemtime = std::max(existing.systemtime, currentUsage.systemtime);
                currentUsage.usertime = std::max(existing.usertime, currentUsage.usertime);
                currentUsage.maxRSS = std::max(existing.maxRSS, currentUsage.maxRSS);
                allUsages.erase(currentUsage);
            }
            allUsages.insert(currentUsage);
        }

        // cpu times aren't quite recorded in a monotonic way, so skip the invalid ones.
        for (auto it = ++allUsages.begin(); it != allUsages.end(); ++it) {
            auto previous = std::prev(it);
            if (it->usertime < previous->usertime || it->systemtime < previous->systemtime ||
                    it->time == previous->time) {
                it = allUsages.erase(it);
                --it;
            }
        }

        // Extract our overall stats
        if (startTime == 0) {
            startTime = allUsages.begin()->time;
        }
        if (endTime == 0) {
            endTime = allUsages.rbegin()->time;
        }

        // If we don't have enough records, just return what we can
        if (allUsages.size() < width) {
            return allUsages;
        }

        timeStep = (endTime - startTime) / width;

        // Store the timepoints we need for the graph
        for (uint32_t i = 1; i <= width; ++i) {
            auto it = allUsages.upper_bound(Usage{startTime + timeStep * i});
            if (it != allUsages.begin()) {
                --it;
            }
            usages.insert(*it);
        }

        return usages;
    }

    void usage(uint64_t endTime = 0, uint64_t startTime = 0) {
        uint32_t width = getTermWidth() - 8;
        uint32_t height = 20;

        std::set<Usage> usages = getUsageStats(width);

        if (usages.size() < 2) {
            for (uint8_t i = 0; i < height + 2; ++i) {
                std::cout << std::endl;
            }
            std::cout << "Insufficient data for usage statistics." << std::endl;
            return;
        }

        double maxIntervalUsage = 0;

        // Extract our overall stats
        if (startTime == 0) {
            startTime = usages.begin()->time;
        }
        if (endTime == 0) {
            endTime = usages.rbegin()->time;
        }

        if (usages.size() < width) {
            width = usages.size();
        }

        // Find maximum so we can normalise the graph
        Usage previousUsage{0, 0, 0, 0};
        for (auto& currentUsage : usages) {
            double usageDiff = currentUsage.systemtime - previousUsage.systemtime + currentUsage.usertime -
                               previousUsage.usertime;
            usageDiff /= (currentUsage.time - previousUsage.time);
            if (usageDiff > maxIntervalUsage) {
                maxIntervalUsage = usageDiff;
            }
            previousUsage = currentUsage;
        }

        double intervalUsagePercent = 100.0 * maxIntervalUsage;
        std::printf("%11s\n", "cpu total");
        std::printf("%11s\n", Tools::formatTime(usages.rbegin()->usertime / 1000000.0).c_str());

        // Add columns to the graph
        char grid[height][width];
        for (uint32_t i = 0; i < height; ++i) {
            for (uint32_t j = 0; j < width; ++j) {
                grid[i][j] = ' ';
            }
        }

        previousUsage = {0, 0, 0, 0};
        uint32_t col = 0;
        for (const Usage& currentUsage : usages) {
            uint64_t curHeight = 0;
            uint64_t curSystemHeight = 0;
            // Usage may be 0
            if (maxIntervalUsage != 0) {
                curHeight = (currentUsage.systemtime - previousUsage.systemtime + currentUsage.usertime -
                             previousUsage.usertime);
                curHeight /= currentUsage.time - previousUsage.time;
                curHeight *= height / maxIntervalUsage;

                curSystemHeight = currentUsage.systemtime - previousUsage.systemtime;
                curSystemHeight /= currentUsage.time - previousUsage.time;
                curSystemHeight *= height / maxIntervalUsage;
            }
            for (uint32_t row = 0; row < curHeight; ++row) {
                grid[row][col] = '*';
            }
            for (uint32_t row = curHeight - curSystemHeight; row < curHeight; ++row) {
                grid[row][col] = '+';
            }
            previousUsage = currentUsage;
            ++col;
        }

        // Print array
        for (int32_t row = height - 1; row >= 0; --row) {
            printf("%6s%% ", Tools::formatNum(0, intervalUsagePercent * (row + 1) / height).c_str());
            for (uint32_t col = 0; col < width; ++col) {
                std::cout << grid[row][col];
            }
            std::cout << std::endl;
        }
        for (uint32_t col = 0; col < 8; ++col) {
            std::cout << ' ';
        }
        for (uint32_t col = 0; col < width; ++col) {
            std::cout << '-';
        }
        std::cout << std::endl;
    }

    void setupTabCompletion() {
        linereader.clearTabCompletion();

        linereader.appendTabCompletion("rel");
        linereader.appendTabCompletion("rul");
        linereader.appendTabCompletion("rul id");
        linereader.appendTabCompletion("graph ");
        linereader.appendTabCompletion("top");
        linereader.appendTabCompletion("help");
        linereader.appendTabCompletion("usage");
        linereader.appendTabCompletion("limit ");

        // add rel tab completes after the rest so users can see all commands first
        for (auto& row : out.formatTable(rel_table_state, precision)) {
            linereader.appendTabCompletion("rel " + row[5]);
            linereader.appendTabCompletion("graph " + row[5] + " tot_t");
            linereader.appendTabCompletion("graph " + row[5] + " copy_t");
            linereader.appendTabCompletion("graph " + row[5] + " tuples");
            linereader.appendTabCompletion("usage " + row[5]);
        }
    }

    void top() {
        std::shared_ptr<ProgramRun>& run = out.getProgramRun();
        if (alive) run->update();
        auto* totalRelationsEntry =
                dynamic_cast<TextEntry*>(ProfileEventSingleton::instance().getDB().lookupEntry(
                        {"program", "configuration", "relationCount"}));
        auto* totalRulesEntry =
                dynamic_cast<TextEntry*>(ProfileEventSingleton::instance().getDB().lookupEntry(
                        {"program", "configuration", "ruleCount"}));
        size_t totalRelations = 0;
        if (totalRelationsEntry != nullptr) {
            totalRelations = std::stoul(totalRelationsEntry->getText());
        } else {
            totalRelations = run->getRelation_map().size();
        }
        size_t totalRules = 0;
        if (totalRulesEntry != nullptr) {
            totalRules = std::stoul(totalRulesEntry->getText());
        } else {
            totalRules = rul_table_state.getRows().size();
        }
        std::printf("%11s%10s%10s%10s%10s%20s\n", "runtime", "loadtime", "savetime", "relations", "rules",
                "tuples generated");

        std::printf("%11s%10s%10s%10s%10s%14s\n", run->getRuntime().c_str(),
                run->formatTime(run->getTotLoadtime()).c_str(),
                run->formatTime(run->getTotSavetime()).c_str(), run->formatNum(0, totalRelations).c_str(),
                run->formatNum(0, totalRules).c_str(),
                run->formatNum(precision, run->getTotNumTuples()).c_str());

        // Progress bar
        // Determine number of relations processed
        size_t processedRelations = run->getRelation_map().size();
        size_t screenWidth = getTermWidth() - 10;
        if (alive && totalRelationsEntry != nullptr) {
            std::cout << "Progress ";
            for (size_t i = 0; i < screenWidth; ++i) {
                if (screenWidth * processedRelations / totalRelations > i) {
                    std::cout << '#';
                } else {
                    std::cout << '_';
                }
            }
        }
        std::cout << std::endl;

        usage();
    }

    void setResultLimit(size_t limit) {
        resultLimit = limit;
    }

    void rel() {
        rel_table_state.sort(sort_col);
        std::cout << " ----- Relation Table -----\n";
        std::printf("%8s%8s%8s%8s%8s%8s%8s%15s%6s%1s%s\n\n", "TOT_T", "NREC_T", "REC_T", "COPY_T", "LOAD_T",
                "SAVE_T", "RSSDiff", "TUPLES", "ID", "", "NAME");
        size_t count = 0;
        for (auto& row : out.formatTable(rel_table_state, precision)) {
            if (++count > resultLimit) {
                std::cout << (rel_table_state.getRows().size() - resultLimit) << " rows not shown"
                          << std::endl;
                break;
            }
            std::printf("%8s%8s%8s%8s%8s%8s%8s%15s%6s%1s%s\n", row[0].c_str(), row[1].c_str(), row[2].c_str(),
                    row[3].c_str(), row[9].c_str(), row[10].c_str(), row[11].c_str(), row[4].c_str(),
                    row[6].c_str(), "", row[5].c_str());
        }
    }

    void rul() {
        rul_table_state.sort(sort_col);
        std::cout << "  ----- Rule Table -----\n";
        std::printf("%8s%8s%8s%15s    %s\n\n", "TOT_T", "NREC_T", "REC_T", "TUPLES", "ID RELATION");
        size_t count = 0;
        for (auto& row : out.formatTable(rul_table_state, precision)) {
            if (++count > resultLimit) {
                std::cout << (rul_table_state.getRows().size() - resultLimit) << " rows not shown"
                          << std::endl;
                break;
            }
            std::printf("%8s%8s%8s%15s%8s %s\n", row[0].c_str(), row[1].c_str(), row[2].c_str(),
                    row[4].c_str(), row[6].c_str(), row[7].c_str());
        }
    }

    void id(std::string col) {
        rul_table_state.sort(6);
        std::vector<std::vector<std::string>> table = out.formatTable(rul_table_state, precision);

        if (col.compare("0") == 0) {
            std::printf("%7s%2s%s\n\n", "ID", "", "NAME");
            for (auto& row : table) {
                std::printf("%7s%2s%s\n", row[6].c_str(), "", row[5].c_str());
            }
        } else {
            for (auto& row : table) {
                if (row[6].compare(col) == 0) {
                    std::printf("%7s%2s%s\n", row[6].c_str(), "", row[5].c_str());
                }
            }
        }
    }

    void relRul(std::string str) {
        rul_table_state.sort(sort_col);

        std::vector<std::vector<std::string>> rul_table = out.formatTable(rul_table_state, precision);
        std::vector<std::vector<std::string>> rel_table = out.formatTable(rel_table_state, precision);

        std::cout << "  ----- Rules of a Relation -----\n";
        std::printf("%8s%8s%8s%10s%8s %s\n\n", "TOT_T", "NREC_T", "REC_T", "TUPLES", "ID", "NAME");
        std::string name = "";
        bool found = false;  // workaround to make it the same as java (row[5] seems to have priority)
        for (auto& row : rel_table) {
            if (row[5].compare(str) == 0) {
                std::printf("%8s%8s%8s%10s%8s %s\n", row[0].c_str(), row[1].c_str(), row[2].c_str(),
                        row[4].c_str(), row[6].c_str(), row[5].c_str());
                name = row[5];
                found = true;
                break;
            }
        }
        if (!found) {
            for (auto& row : rel_table) {
                if (row[6].compare(str) == 0) {
                    std::printf("%8s%8s%8s%8s%10s%8s %s\n", row[0].c_str(), row[1].c_str(), row[2].c_str(),
                            row[3].c_str(), row[4].c_str(), row[6].c_str(), row[5].c_str());
                    name = row[5];
                    break;
                }
            }
        }
        std::cout << " ---------------------------------------------------------\n";
        for (auto& row : rul_table) {
            if (row[7].compare(name) == 0) {
                std::printf("%8s%8s%8s%10s%8s %s\n", row[0].c_str(), row[1].c_str(), row[2].c_str(),
                        row[4].c_str(), row[6].c_str(), row[7].c_str());
            }
        }
        std::string src = "";
        std::shared_ptr<ProgramRun>& run = out.getProgramRun();
        if (run->getRelation(name) != nullptr) {
            src = run->getRelation(name)->getLocator();
        }
        std::cout << "\nSrc locator: " << src << "\n\n";
        for (auto& row : rul_table) {
            if (row[7].compare(name) == 0) {
                std::printf("%7s%2s%s\n", row[6].c_str(), "", row[5].c_str());
            }
        }
    }

    void verRul(std::string str) {
        if (str.find(".") == std::string::npos) {
            std::cout << "Rule does not exist\n";
            return;
        }
        std::vector<std::string> part = Tools::split(str, ".");
        std::string strRel = "R" + part[0].substr(1);

        Table ver_table = out.getVersions(strRel, str);
        ver_table.sort(sort_col);

        rul_table_state.sort(sort_col);  // why isnt it sorted in the original java?!?

        std::vector<std::vector<std::string>> rul_table = out.formatTable(rul_table_state, precision);

        bool found = false;
        std::string ruleName;
        std::string srcLocator;
        // Check that the rule exists, and print it out if so.
        for (auto& row : rul_table) {
            if (row[6].compare(str) == 0) {
                std::cout << row[5] << std::endl;
                found = true;
                ruleName = row[5];
                srcLocator = row[10];
            }
        }

        // If the rule exists, print out the source locator.
        if (found) {
            if (ver_table.rows.size() > 0) {
                if (ver_table.rows[0]->cells[9] != nullptr) {
                    std::cout << "Src locator-: " << (*ver_table.rows[0])[9]->getStringVal() << "\n\n";
                } else {
                    std::cout << "Src locator-: -\n\n";
                }
            } else if (rul_table.size() > 0) {
                std::cout << "Src locator-: " << rul_table[0][10] << "\n\n";
            }
        }

        // Print out the versions of this rule.
        std::cout << "  ----- Rule Versions Table -----\n";
        std::printf("%8s%8s%8s%10s%6s\n\n", "TOT_T", "NREC_T", "REC_T", "TUPLES", "VER");
        for (auto& row : rul_table) {
            if (row[6].compare(str) == 0) {
                std::printf("%8s%8s%8s%10s%6s\n", row[0].c_str(), row[1].c_str(), row[2].c_str(),
                        row[4].c_str(), "");
            }
        }
        std::cout << "   ---------------------------------------------\n";
        for (auto& _row : ver_table.rows) {
            Row row = *_row;

            std::printf("%8s%8s%8s%10s%6s\n", row[0]->toString(precision).c_str(),
                    row[1]->toString(precision).c_str(), row[2]->toString(precision).c_str(),
                    row[4]->toString(precision).c_str(), row[8]->toString(precision).c_str());
            Table atom_table = out.getVersionAtoms(strRel, srcLocator, row[8]->getLongVal());
            verAtoms(atom_table);
        }

        if (!ver_table.rows.empty()) {
            return;
        }

        Table atom_table = out.getAtomTable(strRel, str);
        verAtoms(atom_table, ruleName);
    }

    void iterRel(std::string c, std::string col) {
        std::vector<std::vector<std::string>> table = out.formatTable(rel_table_state, -1);
        std::vector<std::shared_ptr<Iteration>> iter;
        for (auto& row : table) {
            if (row[6].compare(c) == 0) {
                std::printf("%4s%2s%s\n\n", row[6].c_str(), "", row[5].c_str());
                std::shared_ptr<ProgramRun>& run = out.getProgramRun();
                iter = run->getRelation_map()[row[5]]->getIterations();
                if (col.compare("tot_t") == 0) {
                    std::vector<double> list;
                    for (auto& i : iter) {
                        list.emplace_back(i->getRuntime());
                    }
                    std::printf("%4s   %s\n\n", "NO", "RUNTIME");
                    graphD(list);
                } else if (col.compare("copy_t") == 0) {
                    std::vector<double> list;
                    for (auto& i : iter) {
                        list.emplace_back(i->getCopy_time());
                    }
                    std::printf("%4s   %s\n\n", "NO", "COPYTIME");
                    graphD(list);
                } else if (col.compare("tuples") == 0) {
                    std::vector<uint64_t> list;
                    for (auto& i : iter) {
                        list.emplace_back(i->getNum_tuples());
                    }
                    std::printf("%4s   %s\n\n", "NO", "TUPLES");
                    graphL(list);
                }
                return;
            }
        }
        for (auto& row : table) {
            if (row[5].compare(c) == 0) {
                std::printf("%4s%2s%s\n\n", row[6].c_str(), "", row[5].c_str());
                std::shared_ptr<ProgramRun>& run = out.getProgramRun();
                iter = run->getRelation_map()[row[5]]->getIterations();
                if (col.compare("tot_t") == 0) {
                    std::vector<double> list;
                    for (auto& i : iter) {
                        list.emplace_back(i->getRuntime());
                    }
                    std::printf("%4s   %s\n\n", "NO", "RUNTIME");
                    graphD(list);
                } else if (col.compare("copy_t") == 0) {
                    std::vector<double> list;
                    for (auto& i : iter) {
                        list.emplace_back(i->getCopy_time());
                    }
                    std::printf("%4s   %s\n\n", "NO", "COPYTIME");
                    graphD(list);
                } else if (col.compare("tuples") == 0) {
                    std::vector<uint64_t> list;
                    for (auto& i : iter) {
                        list.emplace_back(i->getNum_tuples());
                    }
                    std::printf("%4s   %s\n\n", "NO", "TUPLES");
                    graphL(list);
                }
                return;
            }
        }
    }

    void iterRul(std::string c, std::string col) {
        std::vector<std::vector<std::string>> table = out.formatTable(rul_table_state, precision);
        std::vector<std::shared_ptr<Iteration>> iter;
        for (auto& row : table) {
            if (row[6].compare(c) == 0) {
                std::printf("%6s%2s%s\n\n", row[6].c_str(), "", row[5].c_str());
                std::shared_ptr<ProgramRun>& run = out.getProgramRun();
                iter = run->getRelation_map()[row[7]]->getIterations();
                if (col.compare("tot_t") == 0) {
                    std::vector<double> list;
                    for (auto& i : iter) {
                        bool add = false;
                        double tot_time = 0.0;
                        for (auto& rul : i->getRul_rec()) {
                            if (rul.second->getId().compare(c) == 0) {
                                tot_time += rul.second->getRuntime();
                                add = true;
                            }
                        }
                        if (add) {
                            list.emplace_back(tot_time);
                        }
                    }
                    std::printf("%4s   %s\n\n", "NO", "RUNTIME");
                    graphD(list);
                } else if (col.compare("tuples") == 0) {
                    std::vector<uint64_t> list;
                    for (auto& i : iter) {
                        bool add = false;
                        uint64_t tot_num = 0L;
                        for (auto& rul : i->getRul_rec()) {
                            if (rul.second->getId().compare(c) == 0) {
                                tot_num += rul.second->getNum_tuples();
                                add = true;
                            }
                        }
                        if (add) {
                            list.emplace_back(tot_num);
                        }
                    }
                    std::printf("%4s   %s\n\n", "NO", "TUPLES");
                    graphL(list);
                }
                break;
            }
        }
    }

    void verGraph(std::string c, std::string col) {
        if (c.find('.') == std::string::npos) {
            std::cout << "Rule does not exist";
            return;
        }

        std::vector<std::string> part = Tools::split(c, ".");
        std::string strRel = "R" + part[0].substr(1);

        Table ver_table = out.getVersions(strRel, c);
        std::printf("%6s%2s%s\n\n", (*ver_table.rows[0])[6]->toString(0).c_str(), "",
                (*ver_table.rows[0])[5]->toString(0).c_str());
        if (col.compare("tot_t") == 0) {
            std::vector<double> list;
            for (auto& row : ver_table.rows) {
                list.emplace_back((*row)[0]->getDoubVal());
            }
            std::printf("%4s   %s\n\n", "NO", "RUNTIME");
            graphD(list);
        } else if (col.compare("copy_t") == 0) {
            std::vector<double> list;
            for (auto& row : ver_table.rows) {
                list.emplace_back((*row)[3]->getDoubVal());
            }
            std::printf("%4s   %s\n\n", "NO", "COPYTIME");
            graphD(list);
        } else if (col.compare("tuples") == 0) {
            std::vector<uint64_t> list;
            for (auto& row : ver_table.rows) {
                list.emplace_back((*row)[4]->getLongVal());
            }
            std::printf("%4s   %s\n\n", "NO", "TUPLES");
            graphL(list);
        }
    }

    void graphD(std::vector<double> list) {
        double max = 0;
        for (auto& d : list) {
            if (d > max) {
                max = d;
            }
        }

        std::sort(list.begin(), list.end());
        std::reverse(list.begin(), list.end());
        int i = 0;
        for (auto& d : list) {
            uint32_t len = 67 * (d / max);
            std::string bar = "";
            for (uint32_t j = 0; j < len; j++) {
                bar += "*";
            }

            if (std::isnan(d)) {
                std::printf("%4d        NaN | %s\n", i++, bar.c_str());
            } else {
                std::printf("%4d %10.8f | %s\n", i++, d, bar.c_str());
            }
        }
    }

    void graphL(std::vector<uint64_t> list) {
        uint64_t max = 0;
        for (auto& l : list) {
            if (l > max) {
                max = l;
            }
        }
        std::sort(list.begin(), list.end());
        std::reverse(list.begin(), list.end());
        int i = 0;
        for (auto& l : list) {
            uint32_t len = 64 * ((double)l / (double)max);
            std::string bar = "";
            for (uint32_t j = 0; j < len; j++) {
                bar += "*";
            }

            std::printf("%4d %8s | %s\n", i++, out.formatNum(precision, l).c_str(), bar.c_str());
        }
    }

    static bool string_sort(std::vector<std::string> a, std::vector<std::string> b) {
        return a[0] > b[0];
    }

protected:
    void verAtoms(Table& atomTable, const std::string& ruleName = "") {
        // If there are no subrules then just print out any atoms found
        // If we do find subrules, then label atoms with their subrules.
        if (atomTable.rows.empty()) {
            return;
        }
        bool firstRun = true;
        std::string lastRule = ruleName;
        for (auto& _row : atomTable.rows) {
            Row& row = *_row;
            std::string rule = row[0]->toString(precision);
            if (rule != lastRule) {
                lastRule = rule;
                std::cout << "     " << row[0]->toString(precision) << std::endl;
                firstRun = true;
            }
            if (firstRun) {
                std::printf("      %-16s%s\n", "FREQ", "ATOM");
                firstRun = false;
            }
            std::printf(
                    "      %-16s%s\n", row[3]->toString(precision).c_str(), row[1]->getStringVal().c_str());
        }
        std::cout << '\n';
    }
    void updateDB() {
        reader->processFile();
        rul_table_state = out.getRulTable();
        rel_table_state = out.getRelTable();
    }

    uint32_t getTermWidth() {
        struct winsize w;
        ioctl(0, TIOCGWINSZ, &w);
        uint32_t width = w.ws_col > 0 ? w.ws_col : 80;
        return width;
    }
};

}  // namespace profile
}  // namespace souffle
