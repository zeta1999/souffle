#include "RamVisitor.h"

#pragma once

namespace souffle {

class InterpreterNode;

/**
 * This class contains information for views (Hints) creation for RamQuery and RamParallel operation.
 */
class InterpreterPreamble {
public:
    /**
     * Add outer-most filter operation which require a view.
     */
    void addViewOperationForFilter(std::unique_ptr<InterpreterNode> node) {
        outerFilterViewOps.push_back(std::move(node));
    }

    /**
     * add outer-most filter operation which does not require a view.
     */
    void addViewFreeOperationForFilter(std::unique_ptr<InterpreterNode> node) {
        outerFilterViewFreeOps.push_back(std::move(node));
    }

    /**
     * Add nested operation which require a view (Hints).
     * This node cannot come from the outer-most filter operation.
     */
    void addViewOperationForNested(std::unique_ptr<InterpreterNode> op) {
        nestedViewOps.push_back(std::move(op));
    }

    /**
     * Return outer-most filter operations.
     */
    const std::vector<std::unique_ptr<InterpreterNode>>& getOuterFilterViewOps() {
        return outerFilterViewOps;
    }

    /**
     * Return views for outer-most filter operations.
     */
    const std::vector<std::unique_ptr<InterpreterNode>>& getOuterFilterViewFreeOps() {
        return outerFilterViewFreeOps;
    }

    /**
     * Return nested operations
     */
    std::vector<std::unique_ptr<InterpreterNode>>& getViewsInNestedOperation() {
        return nestedViewOps;
    }

    std::vector<std::array<size_t, 3>>& getViewInfoForFilter() {
        return viewInfoForFilter;
    }
    std::vector<std::array<size_t, 3>>& getViewInfoForNested() {
        return viewInfoForNested;
    }

    void addViewInfoForFilter(size_t relId, size_t indexPos, size_t viewPos) {
        viewInfoForFilter.push_back({relId, indexPos, viewPos});
    }

    void addViewInfoForNested(size_t relId, size_t indexPos, size_t viewPos) {
        viewInfoForNested.push_back({relId, indexPos, viewPos});
    }

    bool isParallel = false;

private:
    std::vector<std::unique_ptr<InterpreterNode>> outerFilterViewOps;
    std::vector<std::unique_ptr<InterpreterNode>> outerFilterViewFreeOps;
    std::vector<std::unique_ptr<InterpreterNode>> nestedViewOps;
    std::vector<std::array<size_t, 3>> viewInfoForFilter;
    std::vector<std::array<size_t, 3>> viewInfoForNested;
};

}  // namespace souffle
