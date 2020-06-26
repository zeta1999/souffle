/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2014, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ReadStreamJSON.h
 *
 ***********************************************************************/

#pragma once

#include "RamTypes.h"
#include "ReadStream.h"
#include "SymbolTable.h"
#include "utility/ContainerUtil.h"
#include "utility/FileUtil.h"
#include "utility/StringUtil.h"

#include <fstream>
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <iostream>
#include <map>
#include <memory>
#include <sstream>
#include <stdexcept>
#include <string>
#include <vector>
#include <sstream>

namespace souffle {
class RecordTable;

class ReadStreamJSON : public ReadStream {
public:
    ReadStreamJSON(std::istream& file, const std::map<std::string, std::string>& rwOperation,
            SymbolTable& symbolTable, RecordTable& recordTable)
            : ReadStream(rwOperation, symbolTable, recordTable), file(file){}

protected:
    std::istream& file;
    std::map<int, int> inputMap;
    std::unique_ptr<RamDomain[]> readNextTuple() override {
        if (file.eof()) {
            return nullptr;
        }

        std::unique_ptr<RamDomain[]> tuple = std::make_unique<RamDomain[]>(typeAttributes.size());
        std::string error = "";
        std::ostringstream stringInput;
        stringInput << file.rdbuf();
        Json jsonObj = Json::parse(stringInput.str(), error, json11::COMMENTS);
        if (error.length() > 0) {
            fatal("cannot deserialize json: %s\n", error);
        }
    }

};
}