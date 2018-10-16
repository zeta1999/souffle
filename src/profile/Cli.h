/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2016, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "StringUtils.h"
#include "Tui.h"

#include <iostream>
#include <map>
#include <string>
#include <vector>

namespace souffle {
namespace profile {

/*
 * CLI to parse command line arguments and start up the TUI to either run a single command,
 * generate the GUI file or run the TUI
 */
class Cli {
public:
    std::map<char, std::string> args;

    Cli(int argc, char* argv[]) : args() {
        int c;
        while ((c = getopt(argc, argv, "c:hj::")) != -1) {
            if (optarg != nullptr) {
                if (*optarg == '=') {
                    args[c] = optarg + 1;
                } else {
                    args[c] = optarg;
                }
            } else {
                args[c] = c;
            }
        }
        if (optind < argc && args.count('f') == 0) {
            args['f'] = argv[optind];
        }
    }

    void parse() {
        if (args.size() == 0) {
            std::cout << "No arguments provided.\nTry souffle-profile -h for help.\n";
            exit(1);
        }

        if (args.count('h') != 0 || args.count('f') == 0) {
            std::cout << "Souffle Profiler" << std::endl
                      << "Usage: souffle-profile [-f] <log-file> [ -h | -c <command> [options] | -j ]"
                      << std::endl
                      << "<log-file>            The log file to profile." << std::endl
                      << "-c <command>          Run the given command on the log file, try with  "
                         "'-c help' for a list"
                      << std::endl
                      << "                      of commands." << std::endl
                      << "-j <filename<         Generate a GUI (html/js) version of the profiler."
                      << std::endl
                      << "-h                    Print this help message." << std::endl;
            exit(0);
        }
        std::string filename = args['f'];

        if (args.count('c') != 0) {
            Tui(filename, false, false).runCommand(Tools::split(args['c'], " "));
        } else if (args.count('j') != 0) {
            if (args['j'] == "j") {
                Tui(filename, false, true).outputHtml();
            } else {
                Tui(filename, false, true).outputHtml(args['j']);
            }
        } else {
            Tui(filename, true, false).runProf();
        }
    }
};

}  // namespace profile
}  // namespace souffle
