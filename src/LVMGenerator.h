/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2019, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file LVMGenerator.h
 *
 * Declares the generator class for transforming RAM into Bytecode representation.
 *
 ***********************************************************************/
#pragma once

#include "LVMCode.h"
#include "LVMRecords.h"
#include "LVMRelation.h"
#include "RamIndexAnalysis.h"
#include "RamTranslationUnit.h"
#include "RamVisitor.h"
#include <omp.h>

namespace souffle {

/** LVMStaticEnvironment package all LVM static information together. It is responsible for creating and
 * holding all static environment variables necessary for the execution of LVM. This includes LVMRelation
 * creation and encoding, LVMIndex encoding, String literal encoding and IOStreams.
 */
class LVMStaticEnvironment {
public:
    LVMStaticEnvironment(RamTranslationUnit& tUnit)
            : isa(tUnit.getAnalysis<RamIndexAnalysis>()), symbolTable(tUnit.getSymbolTable()) {
        for (const auto& pair : tUnit.getProgram()->getAllRelations()) {
            encodeRelation(*pair.second);
        }
    }

    /** Encode a relation into a index Id and return the encoding result.  */
    size_t encodeRelation(const RamRelation& rel) {
        const std::string& relationName = rel.getName();
        auto iter = relNameToIndex.find(relationName);
        // Create and give relation a new index if it is not in the environment yet
        if (iter == relNameToIndex.end()) {
            relNameToIndex.insert(std::make_pair(relationName, relNameToIndex.size()));
            relationMap.push_back(createRelation(rel));
            return relNameToIndex.size() - 1;
        } else {
            return iter->second;
        }
    }

    /** Decode relationId, return relationName */
    LVMRelation* decodeRelation(size_t relId) const {
        return relationMap[relId].get();
    }

    /** Get total number of relations */
    size_t getRelationNumber() const {
        return relationMap.size();
    }

    /** Return ptr to relation based on index position */
    std::unique_ptr<LVMRelation>& getRelation(size_t idx) {
        return relationMap[idx];
    }

    /** Encode and return index position based on the operation */
    template <class RamNode>
    size_t getIndexPos(RamNode* node) {
        const MinIndexSelection& orderSet = this->isa->getIndexes(node->getRelation());
        SearchSignature signature = this->isa->getSearchSignature(node);
        // A zero signature is equivalent as a full order signature.
        if (signature == 0) {
            signature = (1 << node->getRelation().getArity()) - 1;
        }
        return orderSet.getLexOrderNum(signature);
    };

    /** Encode string */
    size_t encodeString(const std::string& str) {
        return symbolTable.lookup(str);
    }

    /** Decode string */
    std::string decodeString(size_t id) const {
        return symbolTable.resolve(id);
    }

    /** Encode and store IODirctives. Return its id. */
    size_t encodeIODirectives(std::vector<IODirectives> IOs, const RamAbstractLoadStore* op) {
        auto iter = IOOperationToIndex.find(op);
        // Create IODirectives if it is not in the environment yet.
        if (iter == IOOperationToIndex.end()) {
            IOOperationToIndex.insert(std::make_pair(op, IOOperationToIndex.size()));
            IODirectivesMap.push_back(IOs);
            return IODirectivesMap.size() - 1;
        } else {
            return iter->second;
        }
    }

    /** Return IODirectives based on id */
    std::vector<IODirectives>& getIODirectives(size_t id) {
        return IODirectivesMap[id];
    }

    /** Return symbol table */
    SymbolTable& getSymbolTable() {
        return symbolTable;
    }

    /** Return relation map */
    std::vector<std::unique_ptr<LVMRelation>>& getRelationMap() {
        return relationMap;
    }

private:
    constexpr static size_t MAX_DIRECT_INDEX_SIZE = 12;

    RamIndexAnalysis* isa;

    SymbolTable& symbolTable;

    /** IODirectives to index mapping */
    std::unordered_map<const RamAbstractLoadStore*, size_t> IOOperationToIndex;

    /** Index to IODriectives mapping */
    std::vector<std::vector<IODirectives>> IODirectivesMap;

    /** RelName to index mapping */
    std::unordered_map<std::string, size_t> relNameToIndex;

    /** Index to concrete relation mapping */
    std::vector<std::unique_ptr<LVMRelation>> relationMap;

    /** Create relation with corresponding index type */
    std::unique_ptr<LVMRelation> createRelation(const RamRelation& rel) {
        const MinIndexSelection& orderSet = isa->getIndexes(rel);

        if (rel.getArity() > MAX_DIRECT_INDEX_SIZE) {
            return std::make_unique<LVMIndirectRelation>(
                    rel.getArity(), rel.getName(), rel.getAttributeTypeQualifiers(), orderSet);
        }

        switch (rel.getRepresentation()) {
            case RelationRepresentation::BTREE:
                return std::make_unique<LVMRelation>(
                        rel.getArity(), rel.getName(), rel.getAttributeTypeQualifiers(), orderSet);
            case RelationRepresentation::BRIE:
                return std::make_unique<LVMRelation>(rel.getArity(), rel.getName(),
                        rel.getAttributeTypeQualifiers(), orderSet, createBrieIndex);
            case RelationRepresentation::EQREL:
                return std::make_unique<LVMEqRelation>(
                        rel.getArity(), rel.getName(), rel.getAttributeTypeQualifiers(), orderSet);
            case RelationRepresentation::DEFAULT:
                return std::make_unique<LVMRelation>(
                        rel.getArity(), rel.getName(), rel.getAttributeTypeQualifiers(), orderSet);
            default:
                break;
        }
        return nullptr;
    }
};

/**
 * This class contains functions for views (Hints) analysis and creation.
 */
class LVMPreamble {
public:
    /**
     * Add outer-most filter operation which require a view.
     */
    void addOuterFilterOperation(const RamCondition* node) {
        outerOperations.push_back(node);
    }

    /**
     * Add existence check in outer-most filter operation.
     */
    void addViewForOuterFilter(const RamNode* node) {
        assert(dynamic_cast<const RamExistenceCheck*>(node) ||
                dynamic_cast<const RamProvenanceExistenceCheck*>(node));
        viewsForFilterOperation.push_back(node);
    }

    /**
     * Add nested operation which require a view (Hints).
     * This node cannot come from the outer-most filter operation.
     */
    void addViewForNestedOperation(const RamNode* op) {
        assert(dynamic_cast<const RamIndexOperation*>(op) || dynamic_cast<const RamExistenceCheck*>(op) ||
                dynamic_cast<const RamProvenanceExistenceCheck*>(op));
        viewsForNestedOperation.push_back(op);
    }

    /**
     * Add a list of nested operations which require a view.
     */
    void addNestedOperationList(const std::vector<const RamNode*>& nodes) {
        for (auto& node : nodes) {
            addViewForNestedOperation(node);
        }
    }

    /**
     * Return outer-most filter operations.
     */
    const std::vector<const RamCondition*>& getOuterFilterOperation() {
        return outerOperations;
    }

    /**
     * Return views for outer-most filter operations.
     */
    const std::vector<const RamNode*>& getViewsInOuterOperation() {
        return viewsForFilterOperation;
    }

    /**
     * Return nested operations
     */
    std::vector<const RamNode*> getViewsInNestedOperation() {
        return viewsForNestedOperation;
    }

    /**
     * Find all operations under the root node that require a view.
     *
     * In particular, find all index operation and existence check operation.
     */
    std::vector<const RamNode*> findAllViews(const RamNode& node) {
        std::vector<const RamNode*> res;
        visitDepthFirst(node, [&](const RamNode& node) {
            if (requireView(&node) == true) {
                res.push_back(&node);
            };
        });
        return res;
    }

    /**
     * Return true if the given operation requires a view.
     */
    bool requireView(const RamNode* node) {
        if (auto exists = dynamic_cast<const RamExistenceCheck*>(node)) {
            for (auto& v : exists->getValues()) {
                if (!isRamUndefValue(v)) {
                    return true;
                }
            }
            return false;
        } else if (auto provExists = dynamic_cast<const RamProvenanceExistenceCheck*>(node)) {
            for (auto& v : provExists->getValues()) {
                if (!isRamUndefValue(v)) {
                    return true;
                }
            }
            return false;
        } else if (auto indexOp = dynamic_cast<const RamIndexOperation*>(node)) {
            for (auto& v : indexOp->getRangePattern()) {
                if (!isRamUndefValue(v)) {
                    return true;
                }
            }
            return false;
        }
        return false;
    }

    void reset() {
        outerOperations.clear();
        viewsForFilterOperation.clear();
        viewsForNestedOperation.clear();
    }

    /**
     * Same as toConjunctionList defined in RamOperation. But does not clone new node,
     * only holds a list of raw pointers to the original node.
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

private:
    std::vector<const RamCondition*> outerOperations;
    std::vector<const RamNode*> viewsForFilterOperation;
    std::vector<const RamNode*> viewsForNestedOperation;
};

/**
 * LVMGenerator takes an RAM program and transfer it into an equivalent Bytecode representation.
 */
class LVMGenerator : protected RamVisitor<void, size_t> {
public:
    /**
     * The transformation is done in the constructor.
     * This is done by traversing the tree twice, in order to find the necessary information (Jump
     * destination) for LVM branch operations.
     */
    LVMGenerator(const RamStatement* entry, LVMStaticEnvironment& staticEnv) : staticEnv(staticEnv) {
#ifdef _OPENMP
        if (std::stoi(Global::config().get("jobs")) > 1) {
            omp_set_num_threads(std::stoi(Global::config().get("jobs")));
        }
#endif
        (*this)(entry, 0);
        (*this).cleanUp();
        (*this)(entry, 0);
        code.push_back(LVM_STOP);
    }

    virtual std::unique_ptr<LVMCode> getCodeStream() {
        return std::make_unique<LVMCode>(std::move(code));
    }

    virtual std::unique_ptr<LVMCode> parseProgram() {
        return std::make_unique<LVMCode>(std::move(code));
    }

protected:
    // Visit RAM Expressions

    void visitNumber(const RamNumber& num, size_t exitAddress) override {
        code.push_back(LVM_Number);
        code.push_back(num.getConstant());
    }

    void visitTupleElement(const RamTupleElement& access, size_t exitAddress) override {
        code.push_back(LVM_TupleElement);
        code.push_back(access.getTupleId());
        code.push_back(access.getElement());
    }

    void visitAutoIncrement(const RamAutoIncrement& inc, size_t exitAddress) override {
        code.push_back(LVM_AutoIncrement);
    }

    void visitIntrinsicOperator(const RamIntrinsicOperator& op, size_t exitAddress) override {
        const auto& args = op.getArguments();
        switch (op.getOperator()) {
            // Unary Functor Operator
            case FunctorOp::ORD:
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_ORD);
                break;
            case FunctorOp::STRLEN:
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_STRLEN);
                break;
            case FunctorOp::NEG:
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_NEG);
                break;
            case FunctorOp::BNOT:
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_BNOT);
                break;
            case FunctorOp::LNOT:
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_LNOT);
                break;
            case FunctorOp::TONUMBER:
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_TONUMBER);
                break;
            case FunctorOp::TOSTRING:
                visit(args[0], exitAddress);
                code.push_back(LVM_OP_TOSTRING);
                break;

            // Binary Functor Operators
            case FunctorOp::ADD:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_ADD);
                break;
            case FunctorOp::SUB:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_SUB);
                break;
            case FunctorOp::MUL:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_MUL);
                break;
            case FunctorOp::DIV:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_DIV);
                break;
            case FunctorOp::EXP:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_EXP);
                break;
            case FunctorOp::MOD:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_MOD);
                break;
            case FunctorOp::BAND:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_BAND);
                break;
            case FunctorOp::BOR:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_BOR);
                break;
            case FunctorOp::BXOR:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_BXOR);
                break;
            case FunctorOp::LAND:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_LAND);
                break;
            case FunctorOp::LOR:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                code.push_back(LVM_OP_LOR);
                break;
            case FunctorOp::MAX:
                for (auto& arg : args) {
                    visit(arg, exitAddress);
                }
                code.push_back(LVM_OP_MAX);
                code.push_back(args.size());
                break;
            case FunctorOp::MIN:
                for (auto& arg : args) {
                    visit(arg, exitAddress);
                }
                code.push_back(LVM_OP_MIN);
                code.push_back(args.size());
                break;
            case FunctorOp::CAT:
                for (auto iter = args.rbegin(); iter != args.rend(); iter++) {
                    visit(*iter, exitAddress);
                }
                code.push_back(LVM_OP_CAT);
                code.push_back(args.size());
                break;

            // Ternary Functor Operators
            case FunctorOp::SUBSTR:
                visit(args[0], exitAddress);
                visit(args[1], exitAddress);
                visit(args[2], exitAddress);
                code.push_back(LVM_OP_SUBSTR);
                break;

            // Undefined
            default:
                assert(false && "unsupported operator");
                return;
        }
    }

    void visitUserDefinedOperator(const RamUserDefinedOperator& op, size_t exitAddress) override {
        for (size_t i = op.getArgCount(); i-- > 0;) {
            visit(op.getArgument(i), exitAddress);
        }
        code.push_back(LVM_UserDefinedOperator);
        code.push_back(staticEnv.encodeString(op.getName()));
        code.push_back(staticEnv.encodeString(op.getType()));
        code.push_back(op.getArgCount());
    }

    void visitPackRecord(const RamPackRecord& pack, size_t exitAddress) override {
        auto values = pack.getArguments();
        for (auto& value : values) {
            visit(value, exitAddress);
        }
        code.push_back(LVM_PackRecord);
        code.push_back(values.size());
        createRecordMap(values.size());
    }

    void visitSubroutineArgument(const RamSubroutineArgument& arg, size_t exitAddress) override {
        code.push_back(LVM_Argument);
        code.push_back(arg.getArgument());
    }

    // Visit RAM Conditions

    void visitTrue(const RamTrue& ltrue, size_t exitAddress) override {
        code.push_back(LVM_True);
    }

    void visitFalse(const RamFalse& lfalse, size_t exitAddress) override {
        code.push_back(LVM_False);
    }

    void visitConjunction(const RamConjunction& conj, size_t exitAddress) override {
        visit(conj.getLHS(), exitAddress);
        visit(conj.getRHS(), exitAddress);
        code.push_back(LVM_Conjunction);
    }

    void visitNegation(const RamNegation& neg, size_t exitAddress) override {
        visit(neg.getOperand(), exitAddress);
        code.push_back(LVM_Negation);
    }

    void visitEmptinessCheck(const RamEmptinessCheck& emptiness, size_t exitAddress) override {
        code.push_back(LVM_EmptinessCheck);
        code.push_back(staticEnv.encodeRelation(emptiness.getRelation()));
    }

    void visitExistenceCheck(const RamExistenceCheck& exists, size_t exitAddress) override {
        auto values = exists.getValues();
        auto arity = exists.getRelation().getArity();
        auto relId = staticEnv.encodeRelation(exists.getRelation());
        std::vector<int> typeMask(arity);
        bool emptinessCheck = true;
        bool fullExistenceCheck = true;
        for (size_t i = arity; i-- > 0;) {
            if (!isRamUndefValue(values[i])) {
                visit(values[i], exitAddress);
                emptinessCheck = false;
                typeMask[i] = 1;
            } else {
                fullExistenceCheck = false;
            }
        }
        // Empty type mask is equivalent to a non-emptiness check
        if (emptinessCheck == true) {
            code.push_back(LVM_EmptinessCheck);
            code.push_back(relId);
            code.push_back(LVM_Negation);
        } else if (fullExistenceCheck == true) {
            // Full type mask is equivalent to a full order existence check
            size_t viewId = operationToIndexView[&exists];
            code.push_back(LVM_ContainCheck);
            code.push_back(viewId);
        } else {  // Otherwise we do a partial existence check.
            size_t viewId = operationToIndexView[&exists];
            this->emitExistenceCheckInst(arity, viewId, typeMask);
        }
    }

    void visitProvenanceExistenceCheck(
            const RamProvenanceExistenceCheck& provExists, size_t exitAddress) override {
        // By leaving the last two pattern mask empty (0), we can transfer a provenance existence into an
        // equivalent Ram existence check.
        // Unlike RamExistence, a ProvenanceExistence can never be a full order existence check.
        auto values = provExists.getValues();
        auto arity = provExists.getRelation().getArity();
        auto relId = staticEnv.encodeRelation(provExists.getRelation());
        std::vector<int> typeMask(arity);
        bool emptinessCheck = true;
        for (size_t i = arity - 2; i-- > 0;) {
            if (!isRamUndefValue(values[i])) {
                visit(values[i], exitAddress);
                emptinessCheck = false;
                typeMask[i] = 1;
            }
        }

        // Empty type mask is equivalent to a non-emptiness check
        if (emptinessCheck == true) {
            code.push_back(LVM_EmptinessCheck);
            code.push_back(relId);
            code.push_back(LVM_Negation);
        } else {  // Otherwise we do a partial existence check.
            size_t viewId = operationToIndexView[&provExists];
            this->emitExistenceCheckInst(arity, viewId, typeMask);
        }
    }

    void visitConstraint(const RamConstraint& relOp, size_t exitAddress) override {
        code.push_back(LVM_Constraint);
        visit(relOp.getLHS(), exitAddress);
        visit(relOp.getRHS(), exitAddress);
        switch (relOp.getOperator()) {
            case BinaryConstraintOp::EQ:
                code.push_back(LVM_OP_EQ);
                break;
            case BinaryConstraintOp::NE:
                code.push_back(LVM_OP_NE);
                break;
            case BinaryConstraintOp::LT:
                code.push_back(LVM_OP_LT);
                break;
            case BinaryConstraintOp::LE:
                code.push_back(LVM_OP_LE);
                break;
            case BinaryConstraintOp::GT:
                code.push_back(LVM_OP_GT);
                break;
            case BinaryConstraintOp::GE:
                code.push_back(LVM_OP_GE);
                break;
            case BinaryConstraintOp::MATCH:
                code.push_back(LVM_OP_MATCH);
                break;
            case BinaryConstraintOp::NOT_MATCH:
                code.push_back(LVM_OP_NOT_MATCH);
                break;
            case BinaryConstraintOp::CONTAINS:
                code.push_back(LVM_OP_CONTAINS);
                break;
            case BinaryConstraintOp::NOT_CONTAINS:
                code.push_back(LVM_OP_NOT_CONTAINS);
                break;
            default:
                assert(false && "unsupported operator");
        }
    }

    // Visit RAM Operations

    void visitNestedOperation(const RamNestedOperation& nested, size_t exitAddress) override {
        visit(nested.getOperation(), exitAddress);
    }

    void visitTupleOperation(const RamTupleOperation& search, size_t exitAddress) override {
        code.push_back(LVM_Search);
        if (search.getProfileText().empty()) {
            code.push_back(0);
        } else {
            code.push_back(1);
        }
        code.push_back(staticEnv.encodeString(search.getProfileText()));
        visitNestedOperation(search, exitAddress);
    }

    void visitParallelScan(const RamParallelScan& pscan, size_t exitAddress) override {
        const auto& rel = pscan.getRelation();

        assert(pscan.getTupleId() == 0 && "not outer-most loop");
        assert(rel.getArity() > 0 && "AstTranslator failed/no parallel scans for nullaries");

        size_t L1 = getNewAddressLabel();
        emitViewsCreationInst(preamble.getViewsInOuterOperation());

        // Issue filter operation
        for (auto node : preamble.getOuterFilterOperation()) {
            visit(*node, exitAddress);
            code.push_back(LVM_Jmpez);
            code.push_back(lookupAddress(L1) + 1);
        }

        // Enter new scope.
        enterParallelBlock();
        size_t counterLabel = getNewIterator();

        code.push_back(LVM_ITER_InitFullIndexParallel);
        code.push_back(counterLabel);
        code.push_back(staticEnv.encodeRelation(pscan.getRelation()));
        // Setup dest for joining
        code.push_back(lookupAddress(L1) + 1);

        emitViewsCreationInst(preamble.getViewsInNestedOperation());

        // While iterator is not at end
        size_t address_L0 = code.size();
        code.push_back(LVM_ITER_NotAtEnd);
        code.push_back(counterLabel);
        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L1));

        // Select the tuple pointed by iter
        code.push_back(LVM_ITER_Select);
        code.push_back(counterLabel);
        code.push_back(pscan.getTupleId());

        // Perform nested operation
        visitTupleOperation(pscan, lookupAddress(L1));

        // Increment the Iter and jump to the start of the while loop
        code.push_back(LVM_ITER_Inc);
        code.push_back(counterLabel);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);

        setAddress(L1, code.size());

        // Stop the child process
        code.push_back(LVM_ParallelStop);

        exitParallelBlock();
    }

    void visitScan(const RamScan& scan, size_t exitAddress) override {
        code.push_back(LVM_Scan);
        size_t counterLabel = getNewIterator();
        size_t L1 = getNewAddressLabel();

        // Init the Iterator
        code.push_back(LVM_ITER_InitFullIndex);
        code.push_back(counterLabel);
        code.push_back(staticEnv.encodeRelation(scan.getRelation()));

        // While iterator is not at end
        size_t address_L0 = code.size();

        code.push_back(LVM_ITER_NotAtEnd);
        code.push_back(counterLabel);
        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L1));

        // Select the tuple pointed by iter
        code.push_back(LVM_ITER_Select);
        code.push_back(counterLabel);
        code.push_back(scan.getTupleId());

        // Perform nested operation
        visitTupleOperation(scan, lookupAddress(L1));

        // Increment the Iter and jump to the start of the while loop
        code.push_back(LVM_ITER_Inc);
        code.push_back(counterLabel);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);

        setAddress(L1, code.size());
    }

    void visitParallelChoice(const RamParallelChoice& pchoice, size_t exitAddress) override {
        const auto& rel = pchoice.getRelation();

        assert(pchoice.getTupleId() == 0 && "not outer-most loop");
        assert(rel.getArity() > 0 && "AstTranslator failed/no parallel scans for nullaries");

        emitViewsCreationInst(preamble.getViewsInOuterOperation());

        size_t L1 = getNewAddressLabel();
        size_t L2 = getNewAddressLabel();

        // Issue filter operation
        for (auto node : preamble.getOuterFilterOperation()) {
            visit(*node, exitAddress);
            code.push_back(LVM_Jmpez);
            code.push_back(lookupAddress(L2) + 1);
        }

        // Enter new scope.
        enterParallelBlock();
        // Init the Iterator
        size_t counterLabel = getNewIterator();
        code.push_back(LVM_ITER_InitFullIndexParallel);
        code.push_back(counterLabel);
        code.push_back(staticEnv.encodeRelation(pchoice.getRelation()));
        // Setup dest for joining
        code.push_back(lookupAddress(L2) + 1);

        emitViewsCreationInst(preamble.getViewsInNestedOperation());

        // While iterator is not at end
        size_t address_L0 = code.size();
        code.push_back(LVM_ITER_NotAtEnd);
        code.push_back(counterLabel);
        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L2));

        // Select the tuple pointed by iter
        code.push_back(LVM_ITER_Select);
        code.push_back(counterLabel);
        code.push_back(pchoice.getTupleId());

        // If condition is met, perform nested operation and exit.
        visit(pchoice.getCondition(), exitAddress);
        code.push_back(LVM_Jmpnz);
        code.push_back(lookupAddress(L1));

        // Else increment the iter and jump to the start of the while loop.
        code.push_back(LVM_ITER_Inc);
        code.push_back(counterLabel);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);

        setAddress(L1, code.size());
        visitTupleOperation(pchoice, lookupAddress(L2));
        setAddress(L2, code.size());

        // Stop the child process
        code.push_back(LVM_ParallelStop);

        exitParallelBlock();
    }

    void visitChoice(const RamChoice& choice, size_t exitAddress) override {
        code.push_back(LVM_Choice);
        size_t counterLabel = getNewIterator();
        size_t L1 = getNewAddressLabel();
        size_t L2 = getNewAddressLabel();

        // Init the Iterator
        code.push_back(LVM_ITER_InitFullIndex);
        code.push_back(counterLabel);
        code.push_back(staticEnv.encodeRelation(choice.getRelation()));

        // While iterator is not at end
        size_t address_L0 = code.size();
        code.push_back(LVM_ITER_NotAtEnd);
        code.push_back(counterLabel);
        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L2));

        // Select the tuple pointed by iter
        code.push_back(LVM_ITER_Select);
        code.push_back(counterLabel);
        code.push_back(choice.getTupleId());

        // If condition is met, perform nested operation and exit.
        visit(choice.getCondition(), exitAddress);
        code.push_back(LVM_Jmpnz);
        code.push_back(lookupAddress(L1));

        // Else increment the iter and jump to the start of the while loop.
        code.push_back(LVM_ITER_Inc);
        code.push_back(counterLabel);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);

        setAddress(L1, code.size());
        visitTupleOperation(choice, lookupAddress(L2));
        setAddress(L2, code.size());
    }

    void visitParallelIndexScan(const RamParallelIndexScan& piscan, size_t exitAddress) override {
        const auto& rel = piscan.getRelation();

        assert(piscan.getTupleId() == 0 && "not outer-most loop");
        assert(rel.getArity() > 0 && "AstTranslator failed/no parallel scans for nullaries");

        // Obtain the pattern for index
        auto patterns = piscan.getRangePattern();
        auto arity = piscan.getRelation().getArity();
        auto relId = staticEnv.encodeRelation(piscan.getRelation());
        std::vector<int> typeMask(arity);
        bool fullIndexSearch = true;
        for (size_t i = arity; i-- > 0;) {
            if (!isRamUndefValue(patterns[i])) {
                visit(patterns[i], exitAddress);
                fullIndexSearch = false;
                typeMask[i] = 1;
            }
        }

        size_t L1 = getNewAddressLabel();

        emitViewsCreationInst(preamble.getViewsInOuterOperation());

        // Issue filter operation
        for (auto node : preamble.getOuterFilterOperation()) {
            visit(*node, exitAddress);
            code.push_back(LVM_Jmpez);
            code.push_back(lookupAddress(L1) + 1);
        }

        // Enter new scope.
        enterParallelBlock();
        // Init range index based on pattern
        size_t counterLabel = getNewIterator();
        if (fullIndexSearch == true) {
            code.push_back(LVM_ITER_InitFullIndexParallel);
            code.push_back(counterLabel);
            code.push_back(relId);
            code.push_back(lookupAddress(L1) + 1);
        } else {
            auto indexPos = staticEnv.getIndexPos(&piscan);
            this->emitParallelRangeIndexInst(
                    arity, relId, indexPos, counterLabel, typeMask, lookupAddress(L1) + 1);
        }

        emitViewsCreationInst(preamble.getViewsInNestedOperation());

        // While iter is not at end
        size_t address_L0 = code.size();
        code.push_back(LVM_ITER_NotAtEnd);
        code.push_back(counterLabel);
        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L1));

        // Select the tuple pointed by the iter
        code.push_back(LVM_ITER_Select);
        code.push_back(counterLabel);
        code.push_back(piscan.getTupleId());

        // Increment the iter and jump to the start of while loop.
        visitTupleOperation(piscan, lookupAddress(L1));

        code.push_back(LVM_ITER_Inc);
        code.push_back(counterLabel);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);
        setAddress(L1, code.size());

        code.push_back(LVM_ParallelStop);

        exitParallelBlock();
    }

    void visitIndexScan(const RamIndexScan& scan, size_t exitAddress) override {
        code.push_back(LVM_IndexScan);
        size_t counterLabel = getNewIterator();
        size_t L1 = getNewAddressLabel();

        // Obtain the pattern for index
        auto patterns = scan.getRangePattern();
        auto arity = scan.getRelation().getArity();
        auto relId = staticEnv.encodeRelation(scan.getRelation());
        std::vector<int> typeMask(arity);
        bool fullIndexSearch = true;
        for (size_t i = arity; i-- > 0;) {
            if (!isRamUndefValue(patterns[i])) {
                visit(patterns[i], exitAddress);
                fullIndexSearch = false;
                typeMask[i] = 1;
            }
        }

        // Init range index based on pattern
        if (fullIndexSearch == true) {
            code.push_back(LVM_ITER_InitFullIndex);
            code.push_back(counterLabel);
            code.push_back(relId);
        } else {
            assert(operationToIndexView.find(&scan) != operationToIndexView.end());
            size_t viewId = operationToIndexView[&scan];
            this->emitRangeIndexInst(arity, viewId, counterLabel, typeMask);
        }

        // While iter is not at end
        size_t address_L0 = code.size();
        code.push_back(LVM_ITER_NotAtEnd);
        code.push_back(counterLabel);
        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L1));

        // Select the tuple pointed by the iter
        code.push_back(LVM_ITER_Select);
        code.push_back(counterLabel);
        code.push_back(scan.getTupleId());

        // Increment the iter and jump to the start of while loop.
        visitTupleOperation(scan, lookupAddress(L1));

        code.push_back(LVM_ITER_Inc);
        code.push_back(counterLabel);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);
        setAddress(L1, code.size());
    }

    void visitParallelIndexChoice(const RamParallelIndexChoice& ichoice, size_t exitAddress) override {
        const auto& rel = ichoice.getRelation();

        assert(ichoice.getTupleId() == 0 && "not outer-most loop");
        assert(rel.getArity() > 0 && "AstTranslator failed/no parallel scans for nullaries");

        // Obtain the pattern for index
        auto patterns = ichoice.getRangePattern();
        auto arity = ichoice.getRelation().getArity();
        auto relId = staticEnv.encodeRelation(ichoice.getRelation());
        std::vector<int> typeMask(arity);
        bool fullIndexSearch = true;
        for (size_t i = arity; i-- > 0;) {
            if (!isRamUndefValue(patterns[i])) {
                visit(patterns[i], exitAddress);
                fullIndexSearch = false;
                typeMask[i] = 1;
            }
        }

        size_t L1 = getNewAddressLabel();
        size_t L2 = getNewAddressLabel();

        emitViewsCreationInst(preamble.getViewsInOuterOperation());

        // Issue filter operation
        for (auto node : preamble.getOuterFilterOperation()) {
            visit(*node, exitAddress);
            code.push_back(LVM_Jmpez);
            code.push_back(lookupAddress(L2) + 1);
        }

        // Enter new scope.
        enterParallelBlock();
        // Init range index based on pattern
        size_t counterLabel = getNewIterator();
        if (fullIndexSearch == true) {
            code.push_back(LVM_ITER_InitFullIndexParallel);
            code.push_back(counterLabel);
            code.push_back(relId);
            code.push_back(lookupAddress(L2) + 1);
        } else {
            auto indexPos = staticEnv.getIndexPos(&ichoice);
            this->emitParallelRangeIndexInst(
                    arity, relId, indexPos, counterLabel, typeMask, lookupAddress(L2) + 1);
        }

        emitViewsCreationInst(preamble.getViewsInNestedOperation());

        // While iter is not at end.
        size_t address_L0 = code.size();
        code.push_back(LVM_ITER_NotAtEnd);
        code.push_back(counterLabel);
        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L2));

        // Select the tuple pointed by iter
        code.push_back(LVM_ITER_Select);
        code.push_back(counterLabel);
        code.push_back(ichoice.getTupleId());

        visit(ichoice.getCondition(), lookupAddress(L2));
        // If condition is true, perform nested operation and return.
        code.push_back(LVM_Jmpnz);
        code.push_back(lookupAddress(L1));

        // Else increment the iter and continue
        code.push_back(LVM_ITER_Inc);
        code.push_back(counterLabel);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);
        setAddress(L1, code.size());
        visitTupleOperation(ichoice, exitAddress);
        setAddress(L2, code.size());
        code.push_back(LVM_ParallelStop);

        exitParallelBlock();
    }

    void visitIndexChoice(const RamIndexChoice& indexChoice, size_t exitAddress) override {
        code.push_back(LVM_IndexChoice);
        size_t counterLabel = getNewIterator();
        size_t L1 = getNewAddressLabel();
        size_t L2 = getNewAddressLabel();

        // Obtain the pattern for index
        auto patterns = indexChoice.getRangePattern();
        auto arity = indexChoice.getRelation().getArity();
        auto relId = staticEnv.encodeRelation(indexChoice.getRelation());
        std::vector<int> typeMask(arity);
        bool fullIndexSearch = true;
        for (size_t i = arity; i-- > 0;) {
            if (!isRamUndefValue(patterns[i])) {
                visit(patterns[i], exitAddress);
                fullIndexSearch = false;
                typeMask[i] = 1;
            }
        }

        // Init range index based on pattern
        if (fullIndexSearch == true) {
            code.push_back(LVM_ITER_InitFullIndex);
            code.push_back(counterLabel);
            code.push_back(relId);
        } else {
            size_t viewId = operationToIndexView[&indexChoice];
            this->emitRangeIndexInst(arity, viewId, counterLabel, typeMask);
        }

        // While iter is not at end.
        size_t address_L0 = code.size();
        code.push_back(LVM_ITER_NotAtEnd);
        code.push_back(counterLabel);
        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L2));

        // Select the tuple pointed by iter
        code.push_back(LVM_ITER_Select);
        code.push_back(counterLabel);
        code.push_back(indexChoice.getTupleId());

        visit(indexChoice.getCondition(), exitAddress);
        // If condition is true, perform nested operation and return.
        code.push_back(LVM_Jmpnz);
        code.push_back(lookupAddress(L1));

        // Else increment the iter and continue
        code.push_back(LVM_ITER_Inc);
        code.push_back(counterLabel);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);
        setAddress(L1, code.size());
        visitTupleOperation(indexChoice, exitAddress);
        setAddress(L2, code.size());
    }

    void visitUnpackRecord(const RamUnpackRecord& lookup, size_t exitAddress) override {
        // (xiaowen): In the case where reference we want to look up is null, we should return.
        // This can be expressed by the LVM instructions or delegate to CPP code.
        // For now, it is done by passing the next IP (L0) and let CPP to handle the case.
        visit(lookup.getExpression(), exitAddress);
        code.push_back(LVM_UnpackRecord);
        size_t L0 = getNewAddressLabel();
        code.push_back(lookup.getArity());
        code.push_back(lookup.getTupleId());
        code.push_back(lookupAddress(L0));
        visitTupleOperation(lookup, exitAddress);
        setAddress(L0, code.size());

        createRecordMap(lookup.getArity());
    }

    void visitAggregate(const RamAggregate& aggregate, size_t exitAddress) override {
        code.push_back(LVM_Aggregate);
        size_t counterLabel = getNewIterator();
        size_t L1 = getNewAddressLabel();
        size_t L2 = getNewAddressLabel();

        // Init the Iterator
        code.push_back(LVM_ITER_InitFullIndex);
        code.push_back(counterLabel);
        code.push_back(staticEnv.encodeRelation(aggregate.getRelation()));

        // TODO (xiaowen/#992): Count -> Size for optimization
        if (aggregate.getFunction() == souffle::COUNT &&
                dynamic_cast<const RamTrue*>(&aggregate.getCondition()) != nullptr) {
            code.push_back(LVM_Aggregate_COUNT);
            code.push_back(counterLabel);
        } else {
            // Init value
            switch (aggregate.getFunction()) {
                case souffle::MIN:
                    code.push_back(LVM_Number);
                    code.push_back(MAX_RAM_DOMAIN);
                    break;
                case souffle::MAX:
                    code.push_back(LVM_Number);
                    code.push_back(MIN_RAM_DOMAIN);
                    break;
                case souffle::COUNT:
                    code.push_back(LVM_Number);
                    code.push_back(0);
                    break;
                case souffle::SUM:
                    code.push_back(LVM_Number);
                    code.push_back(0);
                    break;
            }

            size_t address_L0 = code.size();

            // Start the aggregate for loop
            code.push_back(LVM_ITER_NotAtEnd);
            code.push_back(counterLabel);
            code.push_back(LVM_Jmpez);
            code.push_back(lookupAddress(L1));

            // Select the element pointed by iter
            code.push_back(LVM_ITER_Select);
            code.push_back(counterLabel);
            code.push_back(aggregate.getTupleId());

            // Produce condition inside the loop
            size_t endOfLoop = getNewAddressLabel();
            if (dynamic_cast<const RamTrue*>(&aggregate.getCondition()) == nullptr) {
                visit(aggregate.getCondition(), exitAddress);
                code.push_back(LVM_Jmpez);  // Continue; if condition is not met
                code.push_back(lookupAddress(endOfLoop));
            }

            if (aggregate.getFunction() != souffle::COUNT) {
                visit(aggregate.getExpression(), exitAddress);
            }

            switch (aggregate.getFunction()) {
                case souffle::MIN:
                    code.push_back(LVM_OP_MIN);
                    code.push_back(2);
                    break;
                case souffle::MAX:
                    code.push_back(LVM_OP_MAX);
                    code.push_back(2);
                    break;
                case souffle::COUNT:
                    code.push_back(LVM_Number);
                    code.push_back(1);
                    code.push_back(LVM_OP_ADD);
                    break;
                case souffle::SUM:
                    code.push_back(LVM_OP_ADD);
                    break;
            }
            setAddress(endOfLoop, code.size());
            code.push_back(LVM_ITER_Inc);
            code.push_back(counterLabel);
            code.push_back(LVM_Goto);
            code.push_back(address_L0);
        }

        setAddress(L1, code.size());

        // write result into environment tuple
        code.push_back(LVM_Aggregate_Return);
        code.push_back(aggregate.getTupleId());

        if (aggregate.getFunction() == souffle::MIN || aggregate.getFunction() == souffle::MAX) {
            // check whether there exists a min/max first before next loop

            // Retrieve the result we just saved.
            code.push_back(LVM_TupleElement);
            code.push_back(aggregate.getTupleId());
            code.push_back(0);
            code.push_back(LVM_Number);

            code.push_back(aggregate.getFunction() == souffle::MIN ? MAX_RAM_DOMAIN : MIN_RAM_DOMAIN);
            code.push_back(LVM_OP_EQ);
            code.push_back(LVM_Jmpnz);  // If init == result, does not visit nested search
            code.push_back(lookupAddress(L2));
        }
        visitTupleOperation(aggregate, exitAddress);
        setAddress(L2, code.size());
    }

    void visitIndexAggregate(const RamIndexAggregate& aggregate, size_t exitAddress) override {
        code.push_back(LVM_IndexAggregate);
        size_t counterLabel = getNewIterator();
        size_t L1 = getNewAddressLabel();
        size_t L2 = getNewAddressLabel();

        // Obtain the pattern for index
        auto patterns = aggregate.getRangePattern();
        auto arity = aggregate.getRelation().getArity();
        auto relId = staticEnv.encodeRelation(aggregate.getRelation());
        std::vector<int> typeMask(arity);
        bool fullIndexSearch = true;
        for (size_t i = arity; i-- > 0;) {
            if (!isRamUndefValue(patterns[i])) {
                visit(patterns[i], exitAddress);
                fullIndexSearch = false;
                typeMask[i] = 1;
            }
        }

        // Init range index based on pattern
        if (fullIndexSearch == true) {
            code.push_back(LVM_ITER_InitFullIndex);
            code.push_back(counterLabel);
            code.push_back(relId);
        } else {
            size_t viewId = operationToIndexView[&aggregate];
            this->emitRangeIndexInst(arity, viewId, counterLabel, typeMask);
        }

        if (aggregate.getFunction() == souffle::COUNT &&
                dynamic_cast<const RamTrue*>(&aggregate.getCondition()) != nullptr) {
            code.push_back(LVM_Aggregate_COUNT);
            code.push_back(counterLabel);
        } else {
            // Init value
            switch (aggregate.getFunction()) {
                case souffle::MIN:
                    code.push_back(LVM_Number);
                    code.push_back(MAX_RAM_DOMAIN);
                    break;
                case souffle::MAX:
                    code.push_back(LVM_Number);
                    code.push_back(MIN_RAM_DOMAIN);
                    break;
                case souffle::COUNT:
                    code.push_back(LVM_Number);
                    code.push_back(0);
                    break;
                case souffle::SUM:
                    code.push_back(LVM_Number);
                    code.push_back(0);
                    break;
            }

            size_t address_L0 = code.size();

            // Start the aggregate for loop
            code.push_back(LVM_ITER_NotAtEnd);
            code.push_back(counterLabel);
            code.push_back(LVM_Jmpez);
            code.push_back(lookupAddress(L1));

            code.push_back(LVM_ITER_Select);
            code.push_back(counterLabel);
            code.push_back(aggregate.getTupleId());

            // Produce condition inside the loop
            size_t endOfLoop = getNewAddressLabel();
            if (dynamic_cast<const RamTrue*>(&aggregate.getCondition()) == nullptr) {
                visit(aggregate.getCondition(), exitAddress);
                code.push_back(LVM_Jmpez);  // Continue; if condition is not met
                code.push_back(lookupAddress(endOfLoop));
            }

            if (aggregate.getFunction() != souffle::COUNT) {
                visit(aggregate.getExpression(), exitAddress);
            }

            switch (aggregate.getFunction()) {
                case souffle::MIN:
                    code.push_back(LVM_OP_MIN);
                    code.push_back(2);
                    break;
                case souffle::MAX:
                    code.push_back(LVM_OP_MAX);
                    code.push_back(2);
                    break;
                case souffle::COUNT:
                    code.push_back(LVM_Number);
                    code.push_back(1);
                    code.push_back(LVM_OP_ADD);
                    break;
                case souffle::SUM:
                    code.push_back(LVM_OP_ADD);
                    break;
            }
            setAddress(endOfLoop, code.size());
            code.push_back(LVM_ITER_Inc);
            code.push_back(counterLabel);
            code.push_back(LVM_Goto);
            code.push_back(address_L0);
        }

        setAddress(L1, code.size());

        // write result into environment tuple
        code.push_back(LVM_Aggregate_Return);
        code.push_back(aggregate.getTupleId());

        if (aggregate.getFunction() == souffle::MIN || aggregate.getFunction() == souffle::MAX) {
            // check whether there exists a min/max first before next loop

            // Retrieve the result we just saved.
            code.push_back(LVM_TupleElement);
            code.push_back(aggregate.getTupleId());
            code.push_back(0);
            code.push_back(LVM_Number);

            code.push_back(aggregate.getFunction() == souffle::MIN ? MAX_RAM_DOMAIN : MIN_RAM_DOMAIN);
            code.push_back(LVM_OP_EQ);
            code.push_back(LVM_Jmpnz);  // If init == result, does not visit nested search
            code.push_back(lookupAddress(L2));
        }
        visitTupleOperation(aggregate, exitAddress);
        setAddress(L2, code.size());
    }

    void visitBreak(const RamBreak& breakOp, size_t exitAddress) override {
        visit(breakOp.getCondition(), exitAddress);
        code.push_back(LVM_Jmpnz);
        code.push_back(exitAddress);
        visitNestedOperation(breakOp, exitAddress);
    }

    void visitFilter(const RamFilter& filter, size_t exitAddress) override {
        code.push_back(LVM_Filter);

        // Profile Action
        code.push_back(staticEnv.encodeString(filter.getProfileText()));

        size_t L0 = getNewAddressLabel();

        visit(filter.getCondition(), exitAddress);

        code.push_back(LVM_Jmpez);
        code.push_back(lookupAddress(L0));

        visitNestedOperation(filter, exitAddress);

        setAddress(L0, code.size());
    }

    void visitProject(const RamProject& project, size_t exitAddress) override {
        size_t arity = project.getRelation().getArity();
        std::string relationName = project.getRelation().getName();
        auto values = project.getValues();
        for (size_t i = values.size(); i-- > 0;) {
            assert(values[i]);
            visit(values[i], exitAddress);
        }
        code.push_back(LVM_Project);
        code.push_back(arity);
        code.push_back(staticEnv.encodeRelation(project.getRelation()));
    }
    void visitSubroutineReturnValue(const RamSubroutineReturnValue& ret, size_t exitAddress) override {
        std::string types;
        auto expressions = ret.getValues();
        size_t size = expressions.size();
        for (size_t i = size; i-- > 0;) {
            if (isRamUndefValue(expressions[i])) {
                types += '_';
            } else {
                types += 'V';
                visit(expressions[i], exitAddress);
            }
        }
        code.push_back(LVM_ReturnValue);
        code.push_back(ret.getValues().size());
        code.push_back(staticEnv.encodeString(types));
    }

    /** Visit RAM stmt*/

    void visitSequence(const RamSequence& seq, size_t exitAddress) override {
        code.push_back(LVM_Sequence);
        for (const auto& cur : seq.getStatements()) {
            visit(cur, exitAddress);
        }
    }

    void visitParallel(const RamParallel& parallel, size_t exitAddress) override {
        // TODO(xiaowen/#998): Currently parallel execution is suppressed.
        // All parallel execution will be executed in sequence.

        auto stmts = parallel.getStatements();
        size_t size = stmts.size();
        // Special case when size = 1: run in sequence instead.
        // Currently all parallel is executed in sequence.
        if (size == 1 || true) {
            for (const auto& cur : parallel.getStatements()) {
                visit(cur, exitAddress);
            }
            return;
        }

        code.push_back(LVM_Parallel);
        code.push_back(size);
        size_t endAddress = getNewAddressLabel();
        code.push_back(lookupAddress(endAddress));
        size_t startAddresses[size];

        for (size_t i = 0; i < size; ++i) {
            startAddresses[i] = getNewAddressLabel();
            code.push_back(lookupAddress(startAddresses[i]));
        }

        for (size_t i = 0; i < size; ++i) {
            setAddress(startAddresses[i], code.size());
            visit(parallel.getStatements()[i], exitAddress);
            code.push_back(LVM_Stop_Parallel);
            code.push_back(LVM_NOP);
        }
        setAddress(endAddress, code.size());
    }

    void visitLoop(const RamLoop& loop, size_t exitAddress) override {
        size_t address_L0 = code.size();
        code.push_back(LVM_Loop);

        size_t L1 = getNewAddressLabel();
        size_t address_L1 = lookupAddress(L1);

        // Address_L1 is the destination for LVM_Exit
        visit(loop.getBody(), address_L1);

        code.push_back(LVM_IncIterationNumber);
        code.push_back(LVM_Goto);
        code.push_back(address_L0);
        code.push_back(LVM_ResetIterationNumber);
        setAddress(L1, code.size());
    }

    void visitExit(const RamExit& exit, size_t exitAddress) override {
        visit(exit.getCondition(), exitAddress);
        code.push_back(LVM_Jmpnz);
        code.push_back(exitAddress);
    }

    void visitLogRelationTimer(const RamLogRelationTimer& timer, size_t exitAddress) override {
        code.push_back(LVM_LogRelationTimer);
        size_t timerIndex = getNewTimer();
        code.push_back(staticEnv.encodeString(timer.getMessage()));
        code.push_back(timerIndex);
        code.push_back(staticEnv.encodeRelation(timer.getRelation()));
        visit(timer.getStatement(), exitAddress);
        code.push_back(LVM_StopLogTimer);
        code.push_back(timerIndex);
    }

    void visitLogTimer(const RamLogTimer& timer, size_t exitAddress) override {
        code.push_back(LVM_LogTimer);
        size_t timerIndex = getNewTimer();
        code.push_back(staticEnv.encodeString(timer.getMessage()));
        code.push_back(timerIndex);
        visit(timer.getStatement(), exitAddress);
        code.push_back(LVM_StopLogTimer);
        code.push_back(timerIndex);
    }

    void visitDebugInfo(const RamDebugInfo& dbg, size_t exitAddress) override {
        code.push_back(LVM_DebugInfo);
        code.push_back(staticEnv.encodeString(dbg.getMessage()));
        visit(dbg.getStatement(), exitAddress);
    }

    void visitStratum(const RamStratum& stratum, size_t exitAddress) override {
        code.push_back(LVM_Stratum);
        visit(stratum.getBody(), exitAddress);
    }

    void visitCreate(const RamCreate& create, size_t exitAddress) override {
        code.push_back(LVM_Create);
        code.push_back(staticEnv.encodeRelation(create.getRelation()));
    }

    void visitClear(const RamClear& clear, size_t exitAddress) override {
        code.push_back(LVM_Clear);
        code.push_back(staticEnv.encodeRelation(clear.getRelation()));
    }

    void visitDrop(const RamDrop& drop, size_t exitAddress) override {
        code.push_back(LVM_Drop);
        code.push_back(staticEnv.encodeRelation(drop.getRelation()));
    }

    void visitLogSize(const RamLogSize& size, size_t exitAddress) override {
        code.push_back(LVM_LogSize);
        code.push_back(staticEnv.encodeRelation(size.getRelation()));
        code.push_back(staticEnv.encodeString(size.getMessage()));
    }

    void visitLoad(const RamLoad& load, size_t exitAddress) override {
        code.push_back(LVM_Load);
        code.push_back(staticEnv.encodeRelation(load.getRelation()));

        size_t id = staticEnv.encodeIODirectives(load.getIODirectives(), &load);
        code.push_back(id);
    }

    void visitStore(const RamStore& store, size_t exitAddress) override {
        code.push_back(LVM_Store);
        code.push_back(staticEnv.encodeRelation(store.getRelation()));

        size_t id = staticEnv.encodeIODirectives(store.getIODirectives(), &store);
        code.push_back(id);
    }

    void visitFact(const RamFact& fact, size_t exitAddress) override {
        size_t arity = fact.getRelation().getArity();
        auto values = fact.getValues();
        for (size_t i = arity; i-- > 0;) {
            visit(values[i], exitAddress);  // Values cannot be null here
        }
        std::string targertRelation = fact.getRelation().getName();
        code.push_back(LVM_Fact);
        code.push_back(staticEnv.encodeRelation(fact.getRelation()));
        code.push_back(arity);
    }

    void visitQuery(const RamQuery& query, size_t exitAddress) override {
        // Views analysis is done here. By visiting all operation nodes under the query, we extract those
        // operations that need a view. Then if the query does not involve parallel operation, we can issue
        // the LVM instructions to create views right away. Otherwise, we hold the preamble and issue the
        // instructions in the parallel statement (Since each thread needs its own view).
        // Extra optimization is done for the case when first operation is a filter operation.
        // We then issue the view creations after view-free filter operation to avoid some extra overhead.

        code.push_back(LVM_Query);
        preamble.reset();
        size_t L0 = getNewAddressLabel();

        // split terms of conditions of outer-most filter operation
        // into terms that require a context and terms that
        // do not require a view
        const RamOperation* next = &query.getOperation();
        std::vector<const RamCondition*> freeOfView;
        if (const auto* filter = dynamic_cast<const RamFilter*>(&query.getOperation())) {
            next = &filter->getOperation();
            // Check terms of outer filter operation whether they can be pushed before
            // the view-generation for speed imrovements
            auto conditions = preamble.toConjunctionList(&filter->getCondition());
            for (auto const& cur : conditions) {
                bool needView = false;
                visitDepthFirst(*cur, [&](const RamNode& node) {
                    if (preamble.requireView(&node)) {
                        needView = true;
                        preamble.addViewForOuterFilter(&node);
                    }
                });
                if (needView) {
                    preamble.addOuterFilterOperation(cur);
                } else {
                    freeOfView.push_back(cur);
                }
            }
            // discharge conditions that do not require a context
            if (freeOfView.size() > 0) {
                for (auto& cond : freeOfView) {
                    visit(*cond, exitAddress);
                    code.push_back(LVM_Jmpez);
                    code.push_back(lookupAddress(L0));
                }
            }
        }

        preamble.addNestedOperationList(preamble.findAllViews(*next));

        // check whether loop nest can be parallelized
        bool isParallel = false;
        visitDepthFirst(*next, [&](const RamAbstractParallel& node) { isParallel = true; });

        // discharge conditions that require a view
        if (isParallel) {
            // holds preamble. Process it until the parallel statement.
        } else {
            emitViewsCreationInst(preamble.getViewsInNestedOperation());
            emitViewsCreationInst(preamble.getViewsInOuterOperation());

            // Issue filter operation
            for (auto& node : preamble.getOuterFilterOperation()) {
                visit(*node, exitAddress);
                code.push_back(LVM_Jmpez);
                code.push_back(lookupAddress(L0));
            }
        }

        visit(*next, exitAddress);
        setAddress(L0, code.size());
    }

    void visitMerge(const RamMerge& merge, size_t exitAddress) override {
        std::string source = merge.getSourceRelation().getName();
        std::string target = merge.getTargetRelation().getName();
        code.push_back(LVM_Merge);
        code.push_back(staticEnv.encodeRelation(merge.getSourceRelation()));
        code.push_back(staticEnv.encodeRelation(merge.getTargetRelation()));
    }

    void visitSwap(const RamSwap& swap, size_t exitAddress) override {
        std::string first = swap.getFirstRelation().getName();
        std::string second = swap.getSecondRelation().getName();
        code.push_back(LVM_Swap);
        code.push_back(staticEnv.encodeRelation(swap.getFirstRelation()));
        code.push_back(staticEnv.encodeRelation(swap.getSecondRelation()));
    }

    void visitUndefValue(const RamUndefValue& undef, size_t exitAddress) override {
        assert(false && "Compilation error");
    }

    void visitNode(const RamNode& node, size_t exitAddress) override {
        assert(false && "Unknown Node type");
        /** Unknown Node */
    }

private:
    /** code stream */
    LVMCode code;

    /** Current address label */
    size_t currentAddressLabel = 0;

    /** Address map */
    std::vector<size_t> addressMap;

    /** Current iterator index */
    size_t iteratorIndex = 0;

    /** Current timer index for logger */
    size_t timerIndex = 0;

    /** Current index view id */
    size_t indexViewId = 0;

    /** Relation Encoder */
    LVMStaticEnvironment& staticEnv;

    /** Clean up all the content except for addressMap
     *  This is for the double traverse when transforming from RAM -> LVM Bytecode.
     * */
    void cleanUp() {
        code.clear();
        currentAddressLabel = 0;
        iteratorIndex = 0;
        timerIndex = 0;
        indexViewId = 0;
    }

    /** Get new Address Label */
    size_t getNewAddressLabel() {
        return currentAddressLabel++;
    }

    /** Get new iterator */
    size_t getNewIterator() {
        return iteratorIndex++;
    }

    /** Get new Timer */
    size_t getNewTimer() {
        return timerIndex++;
    }

    /** Get new indexView */
    size_t getNewIndexView() {
        return indexViewId++;
    }

    /** Store arguments used to create views. */
    std::unordered_map<const RamNode*, std::set<std::array<size_t, 3>>> operationViewsTable;

    std::unordered_map<const RamNode*, size_t> operationToIndexView;

    /* Return the value of the addressLabel.
     * Return 0 if label doesn't exits.
     */
    size_t lookupAddress(size_t addressLabel) {
        if (addressLabel < addressMap.size()) {
            return addressMap[addressLabel];
        }
        return 0;
    }

    /** Set the value of address label */
    void setAddress(size_t addressLabel, size_t value) {
        if (addressLabel >= addressMap.size()) {
            addressMap.resize((addressLabel + 1));
        }
        addressMap[addressLabel] = value;
    }

    LVMPreamble preamble;

    /** Temporary saved variables for scoping */
    size_t savedIterId = 0;
    size_t savedViewId = 0;

    /** Enter new parallel block, save current state */
    void enterParallelBlock() {
        savedIterId = this->iteratorIndex;
        savedViewId = this->indexViewId;
        this->iteratorIndex = 0;
        this->indexViewId = 0;
    }

    /** Exite parallel block, revert to previous state */
    void exitParallelBlock() {
        this->iteratorIndex = savedIterId;
        this->indexViewId = savedViewId;
    }

    /** Emit existence check instructions */
    void emitExistenceCheckInst(const size_t& arity, const size_t& viewId, const std::vector<int>& typeMask) {
        size_t numOfTypeMasks = arity / RAM_DOMAIN_SIZE + (arity % RAM_DOMAIN_SIZE != 0);
        // Emit special instruction for relation with arity < RAM_DOMAIN_SIZE
        // to avoid overhead of checking argument size --- as it is the most common case
        // TODO (xiaowen): benchmark suggest no noticeable difference whether we add
        // this optimization or not.
        if (numOfTypeMasks == 1) {
            code.push_back(LVM_ExistenceCheckOneArg);
        } else {
            code.push_back(LVM_ExistenceCheck);
        }
        code.push_back(viewId);
        for (size_t i = 0; i < numOfTypeMasks; ++i) {
            RamDomain types = 0;
            for (size_t j = 0; j < RAM_DOMAIN_SIZE; ++j) {
                auto projectedIndex = i * RAM_DOMAIN_SIZE + j;
                if (projectedIndex >= arity) {
                    break;
                }
                types |= (typeMask[projectedIndex] << j);
            }
            code.push_back(types);
        }
    }

    /** Emit range index instructions */
    void emitRangeIndexInst(const size_t& arity, const size_t& viewId, const size_t& counterLabel,
            const std::vector<int>& typeMask) {
        size_t numOfTypeMasks = arity / RAM_DOMAIN_SIZE + (arity % RAM_DOMAIN_SIZE != 0);
        // Emit special instruction for relation with arity < RAM_DOMAIN_SIZE
        // to avoid overhead of checking argumnet size --- as it is the most common case
        // TODO (xiaowen): benchmark suggest no noticeable difference whether we add
        // this optimization or not.
        if (numOfTypeMasks == 1) {
            code.push_back(LVM_ITER_InitRangeIndexOneArg);
        } else {
            code.push_back(LVM_ITER_InitRangeIndex);
        }
        code.push_back(counterLabel);
        code.push_back(viewId);
        for (size_t i = 0; i < numOfTypeMasks; ++i) {
            RamDomain types = 0;
            for (size_t j = 0; j < RAM_DOMAIN_SIZE; ++j) {
                auto projectedIndex = i * RAM_DOMAIN_SIZE + j;
                if (projectedIndex >= arity) {
                    break;
                }
                types |= (typeMask[projectedIndex] << j);
            }
            code.push_back(types);
        }
    }

    /** Emit parallel range index instruction */
    void emitParallelRangeIndexInst(const size_t& arity, const size_t& relId, const size_t& indexPos,
            const size_t& counterLabel, const std::vector<int>& typeMask, const size_t& joinLocation) {
        size_t numOfTypeMasks = arity / RAM_DOMAIN_SIZE + (arity % RAM_DOMAIN_SIZE != 0);
        if (numOfTypeMasks == 1) {
            code.push_back(LVM_ITER_InitRangeIndexOneArgParallel);
        } else {
            code.push_back(LVM_ITER_InitRangeIndexParallel);
        }
        code.push_back(counterLabel);
        code.push_back(relId);
        code.push_back(indexPos);
        code.push_back(joinLocation);
        for (size_t i = 0; i < numOfTypeMasks; ++i) {
            RamDomain types = 0;
            for (size_t j = 0; j < RAM_DOMAIN_SIZE; ++j) {
                auto projectedIndex = i * RAM_DOMAIN_SIZE + j;
                if (projectedIndex >= arity) {
                    break;
                }
                types |= (typeMask[projectedIndex] << j);
            }
            code.push_back(types);
        }
    }

    void emitViewsCreationInst(const std::vector<const RamNode*>& nodes) {
        code.push_back(LVM_CreateViews);
        code.push_back(nodes.size());
        for (auto node : nodes) {
            if (auto exists = dynamic_cast<const RamExistenceCheck*>(node)) {
                auto relId = staticEnv.encodeRelation(exists->getRelation());
                auto indexPos = staticEnv.getIndexPos(exists);
                auto viewId = getNewIndexView();
                operationToIndexView[exists] = viewId;
                code.push_back(relId);
                code.push_back(indexPos);
                code.push_back(viewId);
            } else if (auto provExists = dynamic_cast<const RamProvenanceExistenceCheck*>(node)) {
                auto relId = staticEnv.encodeRelation(provExists->getRelation());
                auto indexPos = staticEnv.getIndexPos(provExists);
                auto viewId = getNewIndexView();
                operationToIndexView[provExists] = viewId;
                code.push_back(relId);
                code.push_back(indexPos);
                code.push_back(viewId);
            } else if (auto index = dynamic_cast<const RamIndexOperation*>(node)) {
                auto relId = staticEnv.encodeRelation(index->getRelation());
                auto indexPos = staticEnv.getIndexPos(index);
                auto viewId = getNewIndexView();
                operationToIndexView[index] = viewId;
                code.push_back(relId);
                code.push_back(indexPos);
                code.push_back(viewId);
            } else {
                assert(false && "Operation does not need a view!");
            }
        }
    }

};  // namespace souffle

}  // end of namespace souffle
