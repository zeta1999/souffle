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

#include "RamTypes.h"
#include <cassert>
#include <cstddef>
#include <memory>
#include <utility>
#include <vector>

namespace souffle {
class InterpreterPreamble;
class InterpreterRelation;
class RamNode;

enum InterpreterNodeType {
    I_Constant,
    I_TupleElement,
    I_AutoIncrement,
    I_IntrinsicOperator,
    I_UserDefinedOperator,
    I_NestedIntrinsicOperator,
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
    I_ParallelAggregate,
    I_IndexAggregate,
    I_ParallelIndexAggregate,
    I_Break,
    I_Filter,
    I_Project,
    I_SubroutineReturn,
    I_Sequence,
    I_Parallel,
    I_Loop,
    I_Exit,
    I_LogRelationTimer,
    I_LogTimer,
    I_DebugInfo,
    I_Clear,
    I_LogSize,
    I_IO,
    I_Query,
    I_Extend,
    I_Swap,
    I_Call
};

/**
 * @class InterpreterNode
 * @brief This is a shadow node for a RamNode that is enriched for
 *        with local information so that the interpreter is executing
 *        quickly.
 */

class InterpreterNode {
public:
    using RelationHandle = std::unique_ptr<InterpreterRelation>;

    InterpreterNode(enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : type(ty), shadow(sdw), children(std::move(chlds)), relHandle(relHandle) {}
    virtual ~InterpreterNode() = default;

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

    /** @brief get preamble */
    inline InterpreterPreamble* getPreamble() const {
        return preamble.get();
    }

    /** @brief set preamble */
    inline void setPreamble(const std::shared_ptr<InterpreterPreamble>& p) {
        preamble = p;
    }

    /** @brief get list of all children */
    const std::vector<std::unique_ptr<InterpreterNode>>& getChildren() const {
        return children;
    }

    /** @brief get relation from handle */
    InterpreterRelation* getRelation() const {
        assert(relHandle && "No relation cached\n");
        return (*relHandle).get();
    }

protected:
    enum InterpreterNodeType type;
    const RamNode* shadow;
    std::vector<std::unique_ptr<InterpreterNode>> children;
    RelationHandle* const relHandle;
    std::shared_ptr<InterpreterPreamble> preamble = nullptr;
};

/**
 * @class InterpreterSuperInstruction
 * @brief This class contains information for super-instruction.
 *        Used to eliminate Number and TupleElement in index operation
 *        TODO a more general comment
 *        TODO intergrate with superinstparent.
 */
class InterpreterSuperInstruction {
public:
    InterpreterSuperInstruction(size_t i) {
        first.resize(i);
        second.resize(i);
    }

    InterpreterSuperInstruction(const InterpreterSuperInstruction&) = delete;
    InterpreterSuperInstruction& operator=(const InterpreterSuperInstruction&) = delete;
    InterpreterSuperInstruction(InterpreterSuperInstruction&&) = default;

    std::vector<RamDomain> first;
    std::vector<RamDomain> second;
    std::vector<std::array<size_t, 3>> tupleFirst;
    std::vector<std::array<size_t, 3>> tupleSecond;
    std::vector<std::pair<size_t, std::unique_ptr<InterpreterNode>>> exprFirst;
    std::vector<std::pair<size_t, std::unique_ptr<InterpreterNode>>> exprSecond;
};

class InterpreterSuperInstParent {
public:
    InterpreterSuperInstParent(InterpreterSuperInstruction superInst) : superInst(std::move(superInst)) {}

    const InterpreterSuperInstruction& getSuperInst() const {
        return superInst;
    }

protected:
    const InterpreterSuperInstruction superInst;
};

class InterpreterViewNode {
public:
    InterpreterViewNode(size_t id) : viewId(id) {}

    inline const size_t getViewId() const {
        return viewId;
    }

protected:
    size_t viewId;
};

class InterpreterBinRelNode {
public:
    InterpreterBinRelNode(size_t src, size_t target) : src(src), target(target) {}

    inline const size_t getSourceId() const {
        return src;
    }

    inline const size_t getTargetId() const {
        return target;
    }

protected:
    const size_t src;
    const size_t target;
};

class InterpreterNestedNode {
public:
    InterpreterNestedNode(std::unique_ptr<InterpreterNode> nested) : nested(std::move(nested)) {}

    inline const InterpreterNode* getCondtion() const {
        return nested.get();
    };

protected:
    std::unique_ptr<InterpreterNode> nested;
};

class InterpreterChoiceNode {
public:
    InterpreterChoiceNode(std::unique_ptr<InterpreterNode> cond) : cond(std::move(cond)) {}

    inline const InterpreterNode* getCondtion() const {
        return cond.get();
    };

protected:
    std::unique_ptr<InterpreterNode> cond;
};

class InterpreterConstant : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterTupleElement : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterAutoIncrement : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterIntrinsicOperator : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterUserDefinedOperator : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterNestedIntrinsicOperator : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterPackRecord : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterSubroutineArgument : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterTrue : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterFalse : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterConjunction : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterNegation : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterEmptinessCheck : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterExistenceCheck : public InterpreterNode,
                                  public InterpreterSuperInstParent,
                                  public InterpreterViewNode {
public:
    InterpreterExistenceCheck(bool isTotalSearch, size_t viewId, InterpreterSuperInstruction superInst,
            enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId),
              isTotalSearch(isTotalSearch) {}

    const bool isTotal() const {
        return isTotalSearch;
    }

private:
    bool isTotalSearch = false;
};

class InterpreterProvenanceExistenceCheck : public InterpreterNode,
                                            public InterpreterSuperInstParent,
                                            public InterpreterViewNode {
public:
    InterpreterProvenanceExistenceCheck(size_t viewId, InterpreterSuperInstruction superInst,
            enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId) {}
};

class InterpreterConstraint : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterTupleOperation : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterScan : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterParallelScan : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterIndexScan : public InterpreterNode,
                             public InterpreterSuperInstParent,
                             public InterpreterViewNode {
public:
    InterpreterIndexScan(size_t viewId, InterpreterSuperInstruction superInst, enum InterpreterNodeType ty,
            const RamNode* sdw, std::vector<std::unique_ptr<InterpreterNode>> chlds = {},
            RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId) {}
};

class InterpreterParallelIndexScan : public InterpreterNode,
                                     public InterpreterSuperInstParent,
                                     public InterpreterViewNode {
public:
    InterpreterParallelIndexScan(size_t indexPos, InterpreterSuperInstruction superInst,
            enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)

            : InterpreterNode(ty, sdw, std::move(chlds), relHandle),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(indexPos) {}
};

class InterpreterChoice : public InterpreterNode, public InterpreterNestedNode {
public:
    InterpreterChoice(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            std::unique_ptr<InterpreterNode> nested, std::unique_ptr<InterpreterNode> cond)
            : InterpreterNode(ty, sdw, std::vector<std::unique_ptr<InterpreterNode>>{}, relHandle),
              InterpreterNestedNode(std::move(nested)), cond(std::move(cond)) {}

    inline const InterpreterNode* getCondtion() const {
        return cond.get();
    };

protected:
    std::unique_ptr<InterpreterNode> cond;
};

class InterpreterParallelChoice : public InterpreterChoice {
    using InterpreterChoice::InterpreterChoice;
};

class InterpreterIndexChoice : public InterpreterChoice,
                               public InterpreterSuperInstParent,
                               public InterpreterViewNode {
public:
    InterpreterIndexChoice(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            std::unique_ptr<InterpreterNode> nested, std::unique_ptr<InterpreterNode> cond, size_t viewId,
            InterpreterSuperInstruction superInst)
            : InterpreterChoice(ty, sdw, relHandle, std::move(nested), std::move(cond)),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId) {}
};

class InterpreterParallelIndexChoice : public InterpreterIndexChoice {
    using InterpreterIndexChoice::InterpreterIndexChoice;
};

class InterpreterUnpackRecord : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterAggregate : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterParallelAggregate : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterIndexAggregate : public InterpreterNode,
                                  public InterpreterSuperInstParent,
                                  public InterpreterViewNode {
public:
    InterpreterIndexAggregate(size_t viewId, InterpreterSuperInstruction superInst,
            enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId) {}
};

class InterpreterParallelIndexAggregate : public InterpreterNode,
                                          public InterpreterSuperInstParent,
                                          public InterpreterViewNode {
public:
    InterpreterParallelIndexAggregate(size_t viewId, InterpreterSuperInstruction superInst,
            enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId) {}
};

class InterpreterBreak : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterFilter : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterProject : public InterpreterNode, public InterpreterSuperInstParent {
public:
    InterpreterProject(InterpreterSuperInstruction superInst, enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle),
              InterpreterSuperInstParent(std::move(superInst)) {}
};

class InterpreterSubroutineReturn : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterSequence : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterParallel : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterLoop : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterExit : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterLogRelationTimer : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterLogTimer : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterDebugInfo : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterClear : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterLogSize : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterIO : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterQuery : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterExtend : public InterpreterNode, public InterpreterBinRelNode {
public:
    InterpreterExtend(size_t src, size_t target, enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle), InterpreterBinRelNode(src, target) {}
};

class InterpreterSwap : public InterpreterNode, public InterpreterBinRelNode {
public:
    InterpreterSwap(size_t src, size_t target, enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle), InterpreterBinRelNode(src, target) {}
};

class InterpreterCall : public InterpreterNode {
public:
    InterpreterCall(size_t subroutineId, enum InterpreterNodeType ty, const RamNode* sdw,
            std::vector<std::unique_ptr<InterpreterNode>> chlds = {}, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, std::move(chlds), relHandle), subroutineId(subroutineId) {}

    const size_t getSubroutineId() const {
        return subroutineId;
    }

private:
    const size_t subroutineId;
};

}  // namespace souffle
