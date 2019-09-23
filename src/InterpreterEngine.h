#include "InterpreterContext.h"
#include "InterpreterNode.h"
#include "InterpreterPreamble.h"
#include "InterpreterRelation.h"
#include "RamTranslationUnit.h"
#include "InterpreterGenerator.h"
#include "RamVisitor.h"
#include <dlfcn.h>

#pragma once

namespace souffle {

class InterpreterProgInterface;

class InterpreterEngine {
    using RelationHandle = std::unique_ptr<InterpreterRelation>;

public:
    InterpreterEngine(RamTranslationUnit& tUnit) : tUnit(tUnit), isa(tUnit.getAnalysis<RamIndexAnalysis>()), generator(isa) {
        numOfThreads = std::stoi(Global::config().get("jobs"));
#ifdef _OPENMP
        if (numOfThreads > 1) {
            omp_set_num_threads(numOfThreads);
        }
#endif
        isProvenance = Global::config().has("provenance");

        profileEnabled = Global::config().has("profile");
    }
    RamDomain executeMain();
    RamDomain execute(const InterpreterNode*, InterpreterContext&);

protected:
    void createRelation(const RamRelation& id, const MinIndexSelection& orderSet, size_t idx);
    void dropRelation(size_t relId);
    void swapRelation(size_t ramRel1, size_t ramRel2);
    InterpreterRelation& getRelation(size_t idx);
    RelationHandle& getRelationHandle(size_t idx);

private:
    friend InterpreterProgInterface;
    SymbolTable& getSymbolTable();
    RamTranslationUnit& getTranslationUnit();
    std::map<std::string, RelationHandle> relationMap;
    std::vector<RelationHandle>& getRelationMap();
    std::vector<RelationHandle> relations;
    void executeSubroutine(const std::string& name, const std::vector<RamDomain>& args,
            std::vector<RamDomain>& ret, std::vector<bool>& err);
    void* getMethodHandle(const std::string& method);
    InterpreterPreamble preamble;
    int incCounter();
    bool isProvenance = false;
    bool profileEnabled = false;
    std::atomic<RamDomain> counter{0};
    size_t iteration = 0;
    std::map<std::string, std::map<size_t, size_t>> frequencies;
    RamTranslationUnit& tUnit;
    size_t getIterationNumber() const;
    void incIterationNumber();
    void resetIterationNumber();
    std::vector<void*> dll;
    const std::vector<void*>& loadDLL();
    RamIndexAnalysis* isa;
    std::map<std::string, std::atomic<size_t>> reads;
    std::vector<std::unique_ptr<IndexView>> views;
    size_t numOfThreads = 0;
    NodeGenerator generator;
};

}  // namespace souffle
