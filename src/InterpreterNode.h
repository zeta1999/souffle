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

namespace souffle {

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
    I_Negation,
    I_EmptinessCheck,
    I_ExistenceCheck,
    I_ProvenanceExistenceCheck,
    I_Constraint,
    I_TupleOperation,
    I_Scan,
    I_ParallelScan,
    I_IndexScan,
    I_ParallelIndexScan,
    I_Choice,
    I_ParallelChoice,
    I_IndexChoice,
    I_ParallelIndexChoice,
    I_UnpackRecord,
    I_Aggregate,
    I_IndexAggregate,
    I_Break,
    I_Filter,
    I_Project,
    I_SubroutineReturnValue,
    I_Sequence,
    I_Parallel,
    I_Loop,
    I_Exit,
    I_LogRelationTimer,
    I_LogTimer,
    I_DebugInfo,
    I_Stratum,
    I_Create,
    I_Clear,
    I_Drop,
    I_LogSize,
    I_Load,
    I_Store,
    I_Fact,
    I_Query,
    I_Merge,
    I_Swap,
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
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, std::vector<void*> dat = {})
            : type(ty), shadow(sdw), children(std::move(chlds)), data(dat){};

    /** @brief get node type */
    inline enum InterpreterNodeType getType() const {
        return type;
    }

    /** @brief get shadow node, i.e., RAM node */
    inline const RamNode* getShadow() const {
        return shadow;
    }

    /** @brief get children of node */
    inline const InterpreterNode* getChild(std::size_t i) const {
        return children[i].get();
    }

    /** @brief get data */
    inline void* getData(std::size_t i) const {
        return data[i];
    }

    /** @brief get list of all children */
    const std::vector<std::unique_ptr<InterpreterNode>>& getChildren() const {
        return children;
    }

protected:
    /** node type */
    enum InterpreterNodeType type;

    /** shadow node */
    const RamNode* shadow;

    /** children */
    std::vector<std::unique_ptr<InterpreterNode>> children;

    /** data pointers */
    std::vector<void*> data;    //TODO deleting via a void pointer is undefined behavoir. A better approach?
};
}  // namespace souffle
