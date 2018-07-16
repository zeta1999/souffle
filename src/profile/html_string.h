/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2016, The Souffle Developers. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

#pragma once

#include "StringUtils.h"
#include "htmlCssChartist.h"
#include "htmlCssStyle.h"
#include "htmlJsChartistMin.h"
#include "htmlJsChartistPlugin.h"
#include "htmlJsMain.h"
#include "htmlJsTableSort.h"
#include "htmlJsUtil.h"
#include "htmlMain.h"
#include <iostream>
#include <sstream>
#include <string>

namespace souffle {
namespace profile {

/*
 * Class containing a copy of the gui_src directory (apart from testtabledata) packaged into one html file
 * so that a data variable can be inserted in the middle of the two strings and written to a file.
 *
 * TODO: test reading directly from gui_src files
 * TODO: after reading from gui_src a js/css minification process could be added to reduce file size
 *  - not necessary at this point as the packaged file is ~100kb
 */
class html_string {
private:
    std::string first_half;
    std::string second_half;

public:
    html_string() {
        bool adding_to_first = true;

        std::stringstream infile(html::htmlMain);
        std::string line;
        std::string output;
        while (std::getline(infile, line)) {
            output = "";
            if (line.find("<link") == 0) {
                std::vector<std::string> src = Tools::split(line, "href=\"");
                if (src.size() > 1) {
                    output = "<style>\n";
                    std::string filename = Tools::split(src.at(1), "\"").at(0);
                    if (filename == "style.css") {
                        output += html::cssStyle;
                    } else if (filename == "chartiststyle.css") {
                        output += html::cssChartist;
                    }
                    output += "\n</style>\n";
                    std::cout << output;
                } else {
                    output = line;
                }
            } else if (line.find("<script") == 0) {
                std::vector<std::string> src = Tools::split(line, "src=\"");
                if (src.size() > 1) {
                    output = "<script>\n";
                    std::string filename = Tools::split(src.at(1), "\"").at(0);
                    if (filename == "testtabledata.js") {
                        this->first_half += output;
                        adding_to_first = false;
                        output = "\n</script>\n";
                    } else if (filename == "main.js") {
                        output += html::jsMain;
                        output += "\n</script>\n";
                    } else if (filename == "chartist.min.js") {
                        output += html::jsChartistMin;
                        output += "\n</script>\n";
                    } else if (filename == "chartist-plugin-tooltip.js") {
                        output += html::jsChartistPlugin;
                        output += "\n</script>\n";
                    } else if (filename == "tablesort.js") {
                        output += html::jsTableSort;
                        output += "\n</script>\n";
                    } else if (filename == "util.js") {
                        output += html::jsUtil;
                        output += "\n</script>\n";
                    }
                    std::cout << output;
                } else {
                    output = line;
                }
            } else {
                output = line;
            }
            if (output.empty()) continue;

            if (adding_to_first)
                this->first_half += output;
            else
                this->second_half += output;
        }
    }

    inline std::string get_first_half() {
        return this->first_half;
    }

    inline std::string get_second_half() {
        return this->second_half;
    }
};

}  // namespace profile
}  // namespace souffle
