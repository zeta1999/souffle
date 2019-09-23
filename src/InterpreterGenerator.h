#include "InterpreterNode.h"
#include "InterpreterPreamble.h"
#include "InterpreterRecords.h"
#include "RamIndexAnalysis.h"
#include "RamVisitor.h"

#pragma once

namespace souffle {

class NodeGenerator : public RamVisitor<std::unique_ptr<InterpreterNode>> {
    using NodePtr = std::unique_ptr<InterpreterNode>;
    using NodePtrVec = std::vector<std::unique_ptr<InterpreterNode>>;

public:
    NodeGenerator(RamIndexAnalysis* isa) : isa(isa) {}

    NodePtr generateTree(const RamNode& root) {
        // Encode all relation, indexPos and viewId.
        visitDepthFirst(root, [&](const RamNode& node) {
            if (dynamic_cast<const RamQuery*>(&node)) {
                newQueryBlock();  // TODO New block after entering parallel statement?
            }
            if (const auto* create = dynamic_cast<const RamCreate*>(&node)) {
                encodeRelation(create->getRelation());
            }
            if (const auto* indexSearch = dynamic_cast<const RamIndexOperation*>(&node)) {
                encodeIndexPos(*indexSearch);
                encodeView(indexSearch);
            } else if (const auto* exists = dynamic_cast<const RamExistenceCheck*>(&node)) {
                encodeIndexPos(*exists);
                encodeView(exists);
            } else if (const auto* provExists = dynamic_cast<const RamProvenanceExistenceCheck*>(&node)) {
                encodeIndexPos(*provExists);
                encodeView(provExists);
            }
        });
        // Parse program
        return visit(root);
    }

    std::unique_ptr<InterpreterNode> visitNumber(const RamNumber& num) override {
        return std::make_unique<InterpreterNode>(I_Number, &num);
    }

    std::unique_ptr<InterpreterNode> visitTupleElement(const RamTupleElement& access) override {
        return std::make_unique<InterpreterNode>(I_TupleElement, &access);
    }

    std::unique_ptr<InterpreterNode> visitAutoIncrement(const RamAutoIncrement& inc) override {
        return std::make_unique<InterpreterNode>(I_AutoIncrement, &inc);
    }

    std::unique_ptr<InterpreterNode> visitIntrinsicOperator(const RamIntrinsicOperator& op) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& arg : op.getArguments()) {
            children.push_back(std::move(visit(arg)));
        }
        return std::make_unique<InterpreterNode>(I_IntrinsicOperator, &op, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitUserDefinedOperator(const RamUserDefinedOperator& op) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& arg : op.getArguments()) {
            children.push_back(std::move(visit(arg)));
        }
        return std::make_unique<InterpreterNode>(I_UserDefinedOperator, &op, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitPackRecord(const RamPackRecord& pr) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& arg : pr.getArguments()) {
            children.push_back(std::move(visit(arg)));
        }
        return std::make_unique<InterpreterNode>(I_PackRecord, &pr, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitSubroutineArgument(const RamSubroutineArgument& arg) override {
        return std::make_unique<InterpreterNode>(I_SubroutineArgument, &arg);
    }

    // -- connectors operators --
    std::unique_ptr<InterpreterNode> visitTrue(const RamTrue& ltrue) override {
        return std::make_unique<InterpreterNode>(I_True, &ltrue);
    }

    std::unique_ptr<InterpreterNode> visitFalse(const RamFalse& lfalse) override {
        return std::make_unique<InterpreterNode>(I_False, &lfalse);
    }

    std::unique_ptr<InterpreterNode> visitConjunction(const RamConjunction& conj) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;

        children.push_back(visit(conj.getLHS()));
        children.push_back(visit(conj.getRHS()));
        return std::make_unique<InterpreterNode>(I_Conjunction, &conj, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitNegation(const RamNegation& neg) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(neg.getOperand()));
        return std::make_unique<InterpreterNode>(I_Negation, &neg, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitEmptinessCheck(const RamEmptinessCheck& emptiness) override {
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(emptiness.getRelation())));
        return std::make_unique<InterpreterNode>(I_EmptinessCheck, &emptiness,
                (std::vector<std::unique_ptr<InterpreterNode>>){}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitExistenceCheck(const RamExistenceCheck& exists) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : exists.getValues()) {
            children.push_back(isRamUndefValue(value) ? nullptr : visit(value));
        }
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(exists.getRelation())));
        data.push_back(new size_t(encodeView(&exists)));
        return std::make_unique<InterpreterNode>(
                I_ExistenceCheck, &exists, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitProvenanceExistenceCheck(
            const RamProvenanceExistenceCheck& provExists) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : provExists.getValues()) {
            children.push_back(isRamUndefValue(value) ? nullptr : visit(value));
        }
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(provExists.getRelation())));
        data.push_back(new size_t(encodeView(&provExists)));
        return std::make_unique<InterpreterNode>(
                I_ProvenanceExistenceCheck, &provExists, std::move(children), std::move(data));
    }

    // -- comparison operators --
    std::unique_ptr<InterpreterNode> visitConstraint(const RamConstraint& relOp) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(relOp.getLHS()));
        children.push_back(visit(relOp.getRHS()));
        return std::make_unique<InterpreterNode>(I_Constraint, &relOp, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitNestedOperation(const RamNestedOperation& nested) override {
        return visit(nested.getOperation());
    }

    std::unique_ptr<InterpreterNode> visitTupleOperation(const RamTupleOperation& search) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(search.getOperation()));
        return std::make_unique<InterpreterNode>(I_TupleOperation, &search, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitScan(const RamScan& scan) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visitTupleOperation(scan));
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(scan.getRelation())));
        return std::make_unique<InterpreterNode>(I_Scan, &scan, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitParallelScan(const RamParallelScan& pScan) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visitTupleOperation(pScan));
        std::vector<void*> data;
        data.push_back(parentQueryPreamble);
        data.push_back(new size_t(encodeRelation(pScan.getRelation())));
        return std::make_unique<InterpreterNode>(
                I_ParallelScan, &pScan, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitIndexScan(const RamIndexScan& scan) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        // TODO: Pattern + NestOperation are put in the same vector. good?
        for (const auto& value : scan.getRangePattern()) {
            children.push_back(isRamUndefValue(value) ? nullptr : visit(value));
        }
        children.push_back(visitTupleOperation(scan));
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(scan.getRelation())));
        data.push_back(new size_t(encodeView(&scan)));
        return std::make_unique<InterpreterNode>(I_IndexScan, &scan, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitParallelIndexScan(const RamParallelIndexScan& piscan) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        // TODO: Pattern + NestOperation are put in the same vector. good?
        for (const auto& value : piscan.getRangePattern()) {
            children.push_back(isRamUndefValue(value) ? nullptr : visit(value));
        }
        children.push_back(visitTupleOperation(piscan));
        std::vector<void*> data;
        data.push_back(parentQueryPreamble);
        data.push_back(new size_t(encodeRelation(piscan.getRelation())));
        data.push_back(new size_t(encodeIndexPos(piscan)));
        return std::make_unique<InterpreterNode>(
                I_ParallelIndexScan, &piscan, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitChoice(const RamChoice& choice) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(choice.getCondition()));
        children.push_back(visitTupleOperation(choice));
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(choice.getRelation())));
        return std::make_unique<InterpreterNode>(I_Choice, &choice, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitParallelChoice(const RamParallelChoice& pchoice) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(pchoice.getCondition()));
        children.push_back(visitTupleOperation(pchoice));
        std::vector<void*> data{parentQueryPreamble, new size_t(encodeRelation(pchoice.getRelation()))};
        return std::make_unique<InterpreterNode>(
                I_ParallelChoice, &pchoice, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitIndexChoice(const RamIndexChoice& choice) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : choice.getRangePattern()) {
            children.push_back(isRamUndefValue(value) ? nullptr : visit(value));
        }
        children.push_back(visit(choice.getCondition()));
        children.push_back(visitTupleOperation(choice));
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(choice.getRelation())));
        data.push_back(new size_t(encodeView(&choice)));
        return std::make_unique<InterpreterNode>(
                I_IndexChoice, &choice, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitParallelIndexChoice(
            const RamParallelIndexChoice& ichoice) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : ichoice.getRangePattern()) {
            children.push_back(isRamUndefValue(value) ? nullptr : visit(value));
        }
        children.push_back(visit(ichoice.getCondition()));
        children.push_back(visit(ichoice.getOperation()));
        std::vector<void*> data;
        data.push_back(parentQueryPreamble);
        data.push_back(new size_t(encodeRelation(ichoice.getRelation())));
        data.push_back(new size_t(encodeIndexPos(ichoice)));
        return std::make_unique<InterpreterNode>(
                I_ParallelIndexChoice, &ichoice, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitUnpackRecord(
            const RamUnpackRecord& lookup) override {  // get reference
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(lookup.getExpression()));
        children.push_back(visitTupleOperation(lookup));
        createRecordMap(lookup.getArity());
        return std::make_unique<InterpreterNode>(I_UnpackRecord, &lookup, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitAggregate(const RamAggregate& aggregate) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(aggregate.getCondition()));
        children.push_back(visit(aggregate.getExpression()));
        children.push_back(visitTupleOperation(aggregate));
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(aggregate.getRelation())));
        return std::make_unique<InterpreterNode>(
                I_Aggregate, &aggregate, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitIndexAggregate(const RamIndexAggregate& aggregate) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : aggregate.getRangePattern()) {
            children.push_back(isRamUndefValue(value) ? nullptr : visit(value));
        }
        children.push_back(visit(aggregate.getCondition()));
        children.push_back(visit(aggregate.getExpression()));
        children.push_back(visitTupleOperation(aggregate));
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(aggregate.getRelation())));
        data.push_back(new size_t(encodeView(&aggregate)));
        return std::make_unique<InterpreterNode>(
                I_IndexAggregate, &aggregate, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitBreak(const RamBreak& breakOp) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(breakOp.getCondition()));
        children.push_back(visit(breakOp.getOperation()));
        return std::make_unique<InterpreterNode>(I_Break, &breakOp, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitFilter(const RamFilter& filter) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(filter.getCondition()));
        children.push_back(visit(filter.getOperation()));
        return std::make_unique<InterpreterNode>(I_Filter, &filter, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitProject(const RamProject& project) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : project.getValues()) {
            children.push_back(visit(value));
        }
        std::vector<void*> data{new size_t(encodeRelation(project.getRelation()))};
        return std::make_unique<InterpreterNode>(I_Project, &project, std::move(children), std::move(data));
    }

    // -- return from subroutine --
    std::unique_ptr<InterpreterNode> visitSubroutineReturnValue(
            const RamSubroutineReturnValue& ret) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : ret.getValues()) {
            children.push_back(isRamUndefValue(value) ? nullptr : visit(value));
        }
        return std::make_unique<InterpreterNode>(I_SubroutineReturnValue, &ret, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitSequence(const RamSequence& seq) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : seq.getStatements()) {
            children.push_back(visit(value));
        }
        return std::make_unique<InterpreterNode>(I_Sequence, &seq, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitParallel(const RamParallel& parallel) override {
        // Parallel statements are executed in sequence for now.
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (const auto& value : parallel.getStatements()) {
            children.push_back(visit(value));
        }
        return std::make_unique<InterpreterNode>(I_Parallel, &parallel, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitLoop(const RamLoop& loop) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(loop.getBody()));
        return std::make_unique<InterpreterNode>(I_Loop, &loop, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitExit(const RamExit& exit) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(exit.getCondition()));
        return std::make_unique<InterpreterNode>(I_Exit, &exit, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitLogRelationTimer(const RamLogRelationTimer& timer) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(timer.getStatement()));
        std::vector<void*> data{new size_t(encodeRelation(timer.getRelation()))};
        return std::make_unique<InterpreterNode>(
                I_LogRelationTimer, &timer, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitLogTimer(const RamLogTimer& timer) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(timer.getStatement()));
        return std::make_unique<InterpreterNode>(I_LogTimer, &timer, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitDebugInfo(const RamDebugInfo& dbg) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(dbg.getStatement()));
        return std::make_unique<InterpreterNode>(I_DebugInfo, &dbg, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitStratum(const RamStratum& stratum) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(stratum.getBody()));
        return std::make_unique<InterpreterNode>(I_Stratum, &stratum, std::move(children));
    }

    std::unique_ptr<InterpreterNode> visitCreate(const RamCreate& create) override {
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(create.getRelation())));
        return std::make_unique<InterpreterNode>(
                I_Create, &create, std::vector<std::unique_ptr<InterpreterNode>>{}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitClear(const RamClear& clear) override {
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(clear.getRelation())));
        return std::make_unique<InterpreterNode>(I_Clear, &clear, NodePtrVec{}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitDrop(const RamDrop& drop) override {
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(drop.getRelation())));
        return std::make_unique<InterpreterNode>(I_Drop, &drop, NodePtrVec{}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitLogSize(const RamLogSize& size) override {
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(size.getRelation())));
        return std::make_unique<InterpreterNode>(I_LogSize, &size, NodePtrVec{}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitLoad(const RamLoad& load) override {
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(load.getRelation())));
        return std::make_unique<InterpreterNode>(I_Load, &load, NodePtrVec{}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitStore(const RamStore& store) override {
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(store.getRelation())));
        return std::make_unique<InterpreterNode>(I_Store, &store, NodePtrVec{}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitFact(const RamFact& fact) override {
        std::vector<std::unique_ptr<InterpreterNode>> children;
        for (auto& val : fact.getValues()) {
            children.push_back(visit(val));
        }
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(fact.getRelation())));
        return std::make_unique<InterpreterNode>(I_Fact, &fact, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitQuery(const RamQuery& query) override {
        InterpreterPreamble* preamble = new InterpreterPreamble();
        parentQueryPreamble = preamble;
        // split terms of conditions of outer-most filter operation
        // into terms that require a context and terms that
        // do not require a view
        const RamOperation* next = &query.getOperation();
        std::vector<const RamCondition*> freeOfView;
        if (const auto* filter = dynamic_cast<const RamFilter*>(&query.getOperation())) {
            next = &filter->getOperation();
            // Check terms of outer filter operation whether they can be pushed before
            // the view-generation for speed improvements
            auto conditions = toConjunctionList(&filter->getCondition());
            for (auto const& cur : conditions) {
                bool needView = false;
                visitDepthFirst(*cur, [&](const RamNode& node) {
                    if (requireView(&node)) {
                        needView = true;
                        const auto& rel = getRelationRefForView(&node);
                        preamble->addViewInfoForFilter(
                                encodeRelation(rel), indexTable[&node], encodeView(&node));
                    }
                });

                if (needView == true) {
                    preamble->addViewOperationForFilter(visit(*cur));
                } else {
                    preamble->addViewFreeOperationForFilter(visit(*cur));
                }
            }
        }

        visitDepthFirst(*next, [&](const RamNode& node) {
            if (requireView(&node) == true) {
                const auto& rel = getRelationRefForView(&node);
                preamble->addViewInfoForNested(encodeRelation(rel), indexTable[&node], encodeView(&node));
            };
        });

        visitDepthFirst(*next, [&](const RamAbstractParallel& node) { preamble->isParallel = true; });

        std::vector<void*> data;
        data.push_back(preamble);

        std::vector<std::unique_ptr<InterpreterNode>> children;
        children.push_back(visit(*next));

        return std::make_unique<InterpreterNode>(I_Query, &query, std::move(children), std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitMerge(const RamMerge& merge) override {
        std::vector<void*> data{new size_t(encodeRelation(merge.getFirstRelation())),
                new size_t(encodeRelation(merge.getSecondRelation()))};
        return std::make_unique<InterpreterNode>(I_Merge, &merge, NodePtrVec{}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitSwap(const RamSwap& swap) override {
        std::vector<void*> data;
        data.push_back(new size_t(encodeRelation(swap.getFirstRelation())));
        data.push_back(new size_t(encodeRelation(swap.getSecondRelation())));
        return std::make_unique<InterpreterNode>(I_Swap, &swap, NodePtrVec{}, std::move(data));
    }

    std::unique_ptr<InterpreterNode> visitUndefValue(const RamUndefValue& undef) override {
        return nullptr;  // TODO Aggregate with no expression
        assert(false && "Compilation error");
        return 0;
    }

    std::unique_ptr<InterpreterNode> visitNode(const RamNode& node) override {
        std::cerr << "Unsupported node type: " << typeid(node).name() << "\n";
        assert(false && "Unsupported Node Type!");
        return 0;
    }

private:
    std::unordered_map<const RamNode*, size_t> indexTable;
    template <class RamNode>
    size_t encodeIndexPos(RamNode& node) {
        const MinIndexSelection& orderSet = isa->getIndexes(node.getRelation());
        SearchSignature signature = isa->getSearchSignature(&node);
        // A zero signature is equivalent as a full order signature.
        if (signature == 0) {
            signature = (1 << node.getRelation().getArity()) - 1;
        }
        auto i = orderSet.getLexOrderNum(signature);
        indexTable[&node] = i;
        return i;
    };
    RamIndexAnalysis* isa;

    InterpreterPreamble* parentQueryPreamble = nullptr;
    void newQueryBlock() {
        viewTable.clear();
        viewId = 0;
    }
    size_t viewId = 0;
    size_t getNextViewId() {
        return viewId++;
    }
    std::unordered_map<const RamNode*, size_t> viewTable;
    size_t encodeView(const RamNode* node) {
        auto pos = viewTable.find(node);
        if (pos != viewTable.end()) {
            return pos->second;
        }
        size_t id = getNextViewId();
        viewTable[node] = id;
        return id;
    }

    size_t relId = 0;
    size_t getNewRelId() {
        return relId++;
    }
    std::unordered_map<const RamRelation*, size_t> relTable;
    size_t encodeRelation(const RamRelation& rel) {
        auto pos = relTable.find(&rel);
        if (pos != relTable.end()) {
            return pos->second;
        }
        size_t id = getNewRelId();
        relTable[&rel] = id;
        return id;
    }

    /**
     * Find all operations under the root node that require a view.
     * Return a list of InterpreterNodes that points to the original RAMNodes.
     */
    std::vector<std::unique_ptr<InterpreterNode>> findAllViews(const RamNode& node) {
        std::vector<std::unique_ptr<InterpreterNode>> res;
        visitDepthFirst(node, [&](const RamNode& node) {
            if (requireView(&node) == true) {
                res.push_back(visit(node));
            };
        });
        return res;
    }

    /**
     * Return true if the given operation requires a view.
     */
    bool requireView(const RamNode* node) {
        if (dynamic_cast<const RamAbstractExistenceCheck*>(node) != nullptr) {
            return true;
        } else if (dynamic_cast<const RamIndexOperation*>(node) != nullptr) {
            return true;
        }
        return false;
    }

    /**
     *
     */
    const RamRelation& getRelationRefForView(const RamNode* node) {
        if (const RamAbstractExistenceCheck* exist = dynamic_cast<const RamAbstractExistenceCheck*>(node)) {
            return exist->getRelation();
        } else if (const RamIndexOperation* index = dynamic_cast<const RamIndexOperation*>(node)) {
            return index->getRelation();
        }
        assert(false);
    }

    /**
     * Same as toConjunctionList defined in RamOperation. But does not clone new node,
     * only holds a list of raw pointers to the original nodes.
     */
    inline std::vector<const RamCondition*> toConjunctionList(const RamCondition* condition) {
        std::vector<const RamCondition*> list;
        std::stack<const RamCondition*> stack;
        if (condition != nullptr) {
            stack.push(condition);
            while (!stack.empty()) {
                condition = stack.top();
                stack.pop();
                if (const auto* ramConj = dynamic_cast<const RamConjunction*>(condition)) {
                    stack.push(&ramConj->getLHS());
                    stack.push(&ramConj->getRHS());
                } else {
                    list.emplace_back(condition);
                }
            }
        }
        return list;
    }
};
}  // namespace souffle
