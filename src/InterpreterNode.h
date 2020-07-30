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

    InterpreterNode(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle = nullptr)
            : type(ty), shadow(sdw), relHandle(relHandle) {}
    virtual ~InterpreterNode() = default;

    /** @brief get node type */
    inline enum InterpreterNodeType getType() const {
        return type;
    }

    /** @brief get shadow node, i.e., RAM node */
    inline const RamNode* getShadow() const {
        return shadow;
    }

    /** @brief get relation from handle */
    InterpreterRelation* getRelation() const {
        assert(relHandle && "No relation cached\n");
        return (*relHandle).get();
    }

protected:
    enum InterpreterNodeType type;
    const RamNode* shadow;
    RelationHandle* const relHandle;
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

class InterpreterCompundNode : public InterpreterNode {
    using NodePtrVec = std::vector<std::unique_ptr<InterpreterNode>>;

public:
    InterpreterCompundNode(enum InterpreterNodeType ty, const RamNode* sdw, NodePtrVec children = {},
            RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, relHandle), children(std::move(children)) {}

    /** @brief get children of node */
    inline const InterpreterNode* getChild(std::size_t i) const {
        return children[i].get();
    }

    /** @brief get list of all children */
    const NodePtrVec& getChildren() const {
        return children;
    }

protected:
    NodePtrVec children;
};

class InterpreterBinaryNode : public InterpreterNode {
public:
    InterpreterBinaryNode(enum InterpreterNodeType ty, const RamNode* sdw,
            std::unique_ptr<InterpreterNode> lhs, std::unique_ptr<InterpreterNode> rhs,
            RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, relHandle), lhs(std::move(lhs)), rhs(std::move(rhs)) {}

    inline const InterpreterNode* getLhs() const {
        return lhs.get();
    }

    inline const InterpreterNode* getRhs() const {
        return rhs.get();
    }

protected:
    std::unique_ptr<InterpreterNode> lhs;
    std::unique_ptr<InterpreterNode> rhs;
};

class InterpreterUnaryNode : public InterpreterNode {
public:
    InterpreterUnaryNode(enum InterpreterNodeType ty, const RamNode* sdw,
            std::unique_ptr<InterpreterNode> child, RelationHandle* relHandle = nullptr)
            : InterpreterNode(ty, sdw, relHandle), child(std::move(child)) {}

    inline const InterpreterNode* getChild() const {
        return child.get();
    }

protected:
    std::unique_ptr<InterpreterNode> child;
};

class InterpreterAbstractParallel {
public:
    /** @brief get preamble */
    inline InterpreterPreamble* getPreamble() const {
        return preamble.get();
    }

    /** @brief set preamble */
    inline void setPreamble(const std::shared_ptr<InterpreterPreamble>& p) {
        preamble = p;
    }

protected:
    std::shared_ptr<InterpreterPreamble> preamble = nullptr;
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

    inline size_t getViewId() const {
        return viewId;
    }

protected:
    size_t viewId;
};

class InterpreterBinRelNode {
public:
    InterpreterBinRelNode(size_t src, size_t target) : src(src), target(target) {}

    inline size_t getSourceId() const {
        return src;
    }

    inline size_t getTargetId() const {
        return target;
    }

protected:
    const size_t src;
    const size_t target;
};

class InterpreterNestedNode {
public:
    InterpreterNestedNode(std::unique_ptr<InterpreterNode> nested) : nested(std::move(nested)) {}

    inline const InterpreterNode* getNestesOperation() const {
        return nested.get();
    };

protected:
    std::unique_ptr<InterpreterNode> nested;
};

class InterpreterCondNode {
public:
    InterpreterCondNode(std::unique_ptr<InterpreterNode> cond) : cond(std::move(cond)) {}

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

class InterpreterIntrinsicOperator : public InterpreterCompundNode {
    using InterpreterCompundNode::InterpreterCompundNode;
};

class InterpreterUserDefinedOperator : public InterpreterCompundNode {
    using InterpreterCompundNode::InterpreterCompundNode;
};

class InterpreterNestedIntrinsicOperator : public InterpreterCompundNode {
    using InterpreterCompundNode::InterpreterCompundNode;
};

class InterpreterPackRecord : public InterpreterCompundNode {
    using InterpreterCompundNode::InterpreterCompundNode;
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

class InterpreterConjunction : public InterpreterBinaryNode {
    using InterpreterBinaryNode::InterpreterBinaryNode;
};

class InterpreterNegation : public InterpreterUnaryNode {
    using InterpreterUnaryNode::InterpreterUnaryNode;
};

class InterpreterEmptinessCheck : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterExistenceCheck : public InterpreterNode,
                                  public InterpreterSuperInstParent,
                                  public InterpreterViewNode {
public:
    InterpreterExistenceCheck(enum InterpreterNodeType ty, const RamNode* sdw, bool totalSearch,
            size_t viewId, InterpreterSuperInstruction superInst)
            : InterpreterNode(ty, sdw), InterpreterSuperInstParent(std::move(superInst)),
              InterpreterViewNode(viewId), totalSearch(totalSearch) {}

    bool isTotalSearch() const {
        return totalSearch;
    }

private:
    const bool totalSearch;
};

class InterpreterProvenanceExistenceCheck : public InterpreterNode,
                                            public InterpreterSuperInstParent,
                                            public InterpreterViewNode {
public:
    InterpreterProvenanceExistenceCheck(enum InterpreterNodeType ty, const RamNode* sdw,
            size_t viewId, InterpreterSuperInstruction superInst)
            : InterpreterNode(ty, sdw), InterpreterSuperInstParent(std::move(superInst)),
              InterpreterViewNode(viewId) {}
};

class InterpreterConstraint : public InterpreterBinaryNode {
    using InterpreterBinaryNode::InterpreterBinaryNode;
};

class InterpreterTupleOperation : public InterpreterUnaryNode {
    using InterpreterUnaryNode::InterpreterUnaryNode;
};

class InterpreterScan : public InterpreterNode, public InterpreterNestedNode {
public:
    InterpreterScan(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            std::unique_ptr<InterpreterNode> nested)
            : InterpreterNode(ty, sdw, relHandle), InterpreterNestedNode(std::move(nested)) {}
};

class InterpreterParallelScan : public InterpreterScan, public InterpreterAbstractParallel {
    using InterpreterScan::InterpreterScan;
};

class InterpreterIndexScan : public InterpreterScan,
                             public InterpreterSuperInstParent,
                             public InterpreterViewNode {
public:
    InterpreterIndexScan(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            std::unique_ptr<InterpreterNode> nested, size_t viewId, InterpreterSuperInstruction superInst)
            : InterpreterScan(ty, sdw, relHandle, std::move(nested)),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId) {}
};

class InterpreterParallelIndexScan : public InterpreterIndexScan, public InterpreterAbstractParallel {
public:
    using InterpreterIndexScan::InterpreterIndexScan;
};

class InterpreterChoice : public InterpreterNode, public InterpreterCondNode, public InterpreterNestedNode {
public:
    InterpreterChoice(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            std::unique_ptr<InterpreterNode> cond, std::unique_ptr<InterpreterNode> nested)
            : InterpreterNode(ty, sdw, relHandle), InterpreterCondNode(std::move(cond)),
              InterpreterNestedNode(std::move(nested)) {}
};

class InterpreterParallelChoice : public InterpreterChoice, public InterpreterAbstractParallel {
    using InterpreterChoice::InterpreterChoice;
};

class InterpreterIndexChoice : public InterpreterChoice,
                               public InterpreterSuperInstParent,
                               public InterpreterViewNode {
public:
    InterpreterIndexChoice(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            std::unique_ptr<InterpreterNode> cond, std::unique_ptr<InterpreterNode> nested, size_t viewId,
            InterpreterSuperInstruction superInst)
            : InterpreterChoice(ty, sdw, relHandle, std::move(cond), std::move(nested)),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId) {}
};

class InterpreterParallelIndexChoice : public InterpreterIndexChoice, public InterpreterAbstractParallel {
    using InterpreterIndexChoice::InterpreterIndexChoice;
};

class InterpreterUnpackRecord : public InterpreterNode, public InterpreterNestedNode {
public:
    InterpreterUnpackRecord(enum InterpreterNodeType ty, const RamNode* sdw,
            std::unique_ptr<InterpreterNode> expr, std::unique_ptr<InterpreterNode> nested)
            : InterpreterNode(ty, sdw), InterpreterNestedNode(std::move(nested)), expr(std::move(expr)) {}

    inline const InterpreterNode* getExpr() const {
        return expr.get();
    }

protected:
    std::unique_ptr<InterpreterNode> expr;
};

class InterpreterAggregate : public InterpreterNode,
                             public InterpreterCondNode,
                             public InterpreterNestedNode {
public:
    InterpreterAggregate(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            std::unique_ptr<InterpreterNode> expr, std::unique_ptr<InterpreterNode> filter,
            std::unique_ptr<InterpreterNode> nested)
            : InterpreterNode(ty, sdw, relHandle), 
              InterpreterCondNode(std::move(filter)), InterpreterNestedNode(std::move(nested)), expr(std::move(expr)) {}

    inline const InterpreterNode* getExpr() const {
        return expr.get();
    }

protected:
    std::unique_ptr<InterpreterNode> expr;
};

class InterpreterParallelAggregate : public InterpreterAggregate, public InterpreterAbstractParallel {
    using InterpreterAggregate::InterpreterAggregate;
};

class InterpreterIndexAggregate : public InterpreterAggregate,
                                  public InterpreterSuperInstParent,
                                  public InterpreterViewNode {
public:
    InterpreterIndexAggregate(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            std::unique_ptr<InterpreterNode> expr, std::unique_ptr<InterpreterNode> filter,
            std::unique_ptr<InterpreterNode> nested, size_t viewId, InterpreterSuperInstruction superInst)
            : InterpreterAggregate(ty, sdw, relHandle, std::move(expr), std::move(filter), std::move(nested)),
              InterpreterSuperInstParent(std::move(superInst)), InterpreterViewNode(viewId) {}
};

class InterpreterParallelIndexAggregate : public InterpreterIndexAggregate,
                                          public InterpreterAbstractParallel {
    using InterpreterIndexAggregate::InterpreterIndexAggregate;
};

class InterpreterBreak : public InterpreterNode, public InterpreterCondNode, public InterpreterNestedNode {
public:
    InterpreterBreak(enum InterpreterNodeType ty, const RamNode* sdw, std::unique_ptr<InterpreterNode> cond,
            std::unique_ptr<InterpreterNode> nested)
            : InterpreterNode(ty, sdw, nullptr), InterpreterCondNode(std::move(cond)),
              InterpreterNestedNode(std::move(nested)) {}
};

class InterpreterFilter : public InterpreterNode, public InterpreterCondNode, public InterpreterNestedNode {
public:
    InterpreterFilter(enum InterpreterNodeType ty, const RamNode* sdw, std::unique_ptr<InterpreterNode> cond,
            std::unique_ptr<InterpreterNode> nested)
            : InterpreterNode(ty, sdw, nullptr), InterpreterCondNode(std::move(cond)),
              InterpreterNestedNode(std::move(nested)) {}
};

class InterpreterProject : public InterpreterNode, public InterpreterSuperInstParent {
public:
    InterpreterProject(enum InterpreterNodeType ty, const RamNode* sdw, RelationHandle* relHandle,
            InterpreterSuperInstruction superInst)
            : InterpreterNode(ty, sdw, relHandle), InterpreterSuperInstParent(std::move(superInst)) {}
};

class InterpreterSubroutineReturn : public InterpreterCompundNode {
    using InterpreterCompundNode::InterpreterCompundNode;
};

class InterpreterSequence : public InterpreterCompundNode {
    using InterpreterCompundNode::InterpreterCompundNode;
};

class InterpreterParallel : public InterpreterCompundNode {
    using InterpreterCompundNode::InterpreterCompundNode;
};

class InterpreterLoop : public InterpreterUnaryNode {
    using InterpreterUnaryNode::InterpreterUnaryNode;
};

class InterpreterExit : public InterpreterUnaryNode {
    using InterpreterUnaryNode::InterpreterUnaryNode;
};

class InterpreterLogRelationTimer : public InterpreterUnaryNode {
    using InterpreterUnaryNode::InterpreterUnaryNode;
};

class InterpreterLogTimer : public InterpreterUnaryNode {
    using InterpreterUnaryNode::InterpreterUnaryNode;
};

class InterpreterDebugInfo : public InterpreterUnaryNode {
    using InterpreterUnaryNode::InterpreterUnaryNode;
};

class InterpreterClear : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterCall : public InterpreterNode {
public:
    InterpreterCall(enum InterpreterNodeType ty, const RamNode* sdw, size_t subroutineId)
            : InterpreterNode(ty, sdw), subroutineId(subroutineId) {}

    size_t getSubroutineId() const {
        return subroutineId;
    }

private:
    const size_t subroutineId;
};

class InterpreterLogSize : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterIO : public InterpreterNode {
    using InterpreterNode::InterpreterNode;
};

class InterpreterQuery : public InterpreterUnaryNode, public InterpreterAbstractParallel {
    using InterpreterUnaryNode::InterpreterUnaryNode;
};

class InterpreterExtend : public InterpreterNode, public InterpreterBinRelNode {
public:
    InterpreterExtend(enum InterpreterNodeType ty, const RamNode* sdw, size_t src, size_t target)
            : InterpreterNode(ty, sdw), InterpreterBinRelNode(src, target) {}
};

class InterpreterSwap : public InterpreterNode, public InterpreterBinRelNode {
public:
    InterpreterSwap(enum InterpreterNodeType ty, const RamNode* sdw, size_t src, size_t target)
            : InterpreterNode(ty, sdw), InterpreterBinRelNode(src, target) {}
};

}  // namespace souffle
