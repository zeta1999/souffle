/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file InterpreterNode.h
 *
 * Declares the Interpreter Node class. The interpreter node class
 * is a compact executable representation of RAM nodes for interpretation.  
 * There are two main reasons for the class: 
 *  - node types are exposed in form of enums so that fast switch-statements 
 *    can be employed for interpretation (visitor patterns with their
 *    double-dispatch are too slow). 
 *  - nodes are decorated with data so that frequent on-the-fly data-structure
 *    lookups are avoided. 
 * Every interpreter node is associated with a unique RAM node.  
 ***********************************************************************/

#pragma once

#include "RamNode.h"

/** interpreter node types
 */

enum InterpreterNodeType {
    I_Number,
    I_TupleElement,
    I_AutoIncrement,
    I_IntrinsicOperator,
    I_UserDefinedOperator,
    I_PackRecord,
    I_SubroutineArgument,
    I_True,
    I_False,
    I_Conjunction,
    I_Negation
};

/**
 * @class InterpreterNode
 * @brief This is a shadow node for a RamNode that is enriched for
 *        with local information so that the interpreter is executing
 *        quickly.
 */

class InterpreterNode {
public:
    InterpreterNode(enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds, std::vector<void*> dat)
            : type(ty), shadow(sdw), children(std::move(chlds)), data(dat){};

    /** @brief get node type */
    inline enum InterpreterNodeType getType() {
        return type;
    }

    /** @brief get shadow node, i.e., RAM node */
    inline const RamNode* getShadow() {
        return shadow;
    }

    /** @brief get children of node */
    inline const InterpreterNode* getChild(std::size_t i) {
        return children[i].get();
    }

    /** @brief get data */
    inline void* getData(std::size_t i) {
        return data[i].get();
    }

protected:
    /** node type */
    enum InterpreterNodeType type;

    /** shadow node */
    const RamNode* shadow;

    /** children */
    std::vector<std::unique_ptr<InterpreterNode>> children;

    /** data pointers */
    std::vector<void*> data;
};
