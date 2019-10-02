#include "InterpreterContext.h"
#include "InterpreterGenerator.h"
#include "InterpreterNode.h"
#include "InterpreterPreamble.h"
#include "InterpreterRelation.h"
#include "RamTranslationUnit.h"
#include "RamVisitor.h"
#include <dlfcn.h>

#pragma once

namespace souffle {

class InterpreterProgInterface;

/**
 * @class InterpreterEngine
 * @brief This class translate the RAM Program into executable format and interpreter it.
 */
class InterpreterEngine {
    using RelationHandle = std::unique_ptr<InterpreterRelation>;
    friend InterpreterProgInterface;

public:
    InterpreterEngine(RamTranslationUnit& tUnit)
            : tUnit(tUnit), isa(tUnit.getAnalysis<RamIndexAnalysis>()), generator(isa),
              isProvenance(Global::config().has("provenance")),
              profileEnabled(Global::config().has("profile")),
              numOfThreads(std::stoi(Global::config().get("jobs"))) {
#ifdef _OPENMP
        if (numOfThreads > 1) {
            omp_set_num_threads(numOfThreads);
        }
#endif
    }
    /** @brief Execute the main program */
    void executeMain();
    /** @brief Execute the subroutine program */
    void executeSubroutine(const std::string& name, const std::vector<RamDomain>& args,
            std::vector<RamDomain>& ret, std::vector<bool>& err);

private:
    /** @brief Create a relation */
    void createRelation(const RamRelation& id, const MinIndexSelection& orderSet, const size_t& idx);
    /** @brief Remove a relation from the environment */
    void dropRelation(const size_t& relId);
    /** @brief Swap the content of two relations */
    void swapRelation(const size_t& ramRel1, const size_t& ramRel2);
    /** @brief Return a relation on the given index */
    InterpreterRelation& getRelation(const size_t& idx);
    /** @brief Return a reference to the relation on the given index */
    RelationHandle& getRelationHandle(const size_t& idx);
    /** @brief Return the string symbol table */
    SymbolTable& getSymbolTable();
    /** @brief Return the RamTranslationUnit */
    RamTranslationUnit& getTranslationUnit();
    /** @brief Execute the program */
    RamDomain execute(const InterpreterNode*, InterpreterContext&);
    /** @brief Return method handler */
    void* getMethodHandle(const std::string& method);
    /** @brief Load DLL */
    const std::vector<void*>& loadDLL();
    /** @brief Return current iteration number for loop operation */
    size_t getIterationNumber() const;
    /** @brief Increase iteration number by one */
    void incIterationNumber();
    /** @brief Reset iteration number */
    void resetIterationNumber();
    /** @brief Increment the counter */
    int incCounter();
    /** @brief Return the relation maps
     *  There can be nullptrs in the relation map (Dropped relation).
     * */
    std::vector<RelationHandle>& getRelationMap();

    /** @brief Symbol table for relations */
    std::vector<RelationHandle> relations;
    /** @brief If executing a provenance program */
    const bool isProvenance;
    /** @brief If profile is enable in this program */
    const bool profileEnabled;
    /** @brief Number of threads enabled for this program */
    size_t numOfThreads;
    /** @brief Profile counter */
    std::atomic<RamDomain> counter{0};
    /** @brief Loop iteration counter */
    size_t iteration = 0;
    /** @brief Profile for rule frequencies */
    std::map<std::string, std::map<size_t, size_t>> frequencies;
    /** @brief Profile for relation reads */
    std::map<std::string, std::atomic<size_t>> reads;
    /** @brief DLL */
    std::vector<void*> dll;
    /** @brief Program */
    RamTranslationUnit& tUnit;
    /** @brief RamIndexAnalysis */
    RamIndexAnalysis* isa;
    /** @brief Interpreter program generator */
    NodeGenerator generator;
};

}  // namespace souffle
