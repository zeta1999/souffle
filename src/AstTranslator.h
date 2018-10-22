/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstTranslator.h
 *
 * Defines utilities for translating AST structures into RAM constructs.
 *
 ***********************************************************************/

#pragma once

#include "AstArgument.h"
#include "AstClause.h"
#include "AstRelationIdentifier.h"
#include "IODirectives.h"
#include "RamRelation.h"
#include "RamStatement.h"
#include "RamValue.h"
#include "SymbolMask.h"

#include <map>
#include <memory>
#include <set>
#include <string>

namespace souffle {

// forward declarations
class AstClause;
class AstProgram;
class AstRelation;
class AstTranslationUnit;
class RamProgram;
class RamStatement;
class RamTranslationUnit;
class RecursiveClauses;
class TypeEnvironment;

/**
 * A utility class capable of conducting the conversion between AST
 * and RAM structures.
 */
class AstTranslator {
private:
    /** Map modified relation identifiers to original relation identifiers */
    std::map<std::string, std::string> modifiedIdMap;

    /** AST program */
    const AstProgram* program;

    /** Type environment */
    const TypeEnvironment* typeEnv;

private:
    /**
     * The location of some value in a loop nest.
     */
    struct Location {
        int level;         // < the loop level
        int component;     // < the component within the tuple created in the given level
        std::string name;  // < name of the variable

        bool operator==(const Location& loc) const {
            return level == loc.level && component == loc.component;
        }

        bool operator!=(const Location& loc) const {
            return !(*this == loc);
        }

        bool operator<(const Location& loc) const {
            return level < loc.level || (level == loc.level && component < loc.component);
        }

        void print(std::ostream& out) const {
            out << "(" << level << "," << component << ")";
        }

        friend std::ostream& operator<<(std::ostream& out, const Location& loc) {
            loc.print(out);
            return out;
        }
    };

    /**
     * A class indexing the location of variables and record
     * references within a loop nest resulting from the conversion
     * of a rule.
     */
    class ValueIndex {
        /**
         * The type mapping variables (referenced by their names) to the
         * locations where they are used.
         */
        using variable_reference_map = std::map<std::string, std::set<Location>>;

        /**
         * The type mapping record init expressions to their definition points,
         * hence the point where they get grounded/bound.
         */
        using record_definition_map = std::map<const AstRecordInit*, Location>;

        /**
         * The type mapping record init expressions to the loop level where
         * they get unpacked.
         */
        using record_unpack_map = std::map<const AstRecordInit*, int>;

        /**
         * A map from AstAggregators to storage locations. Note, since in this case
         * AstAggregators are indexed by their values (not their address) no standard
         * map can be utilized.
         */
        using aggregator_location_map = std::vector<std::pair<const AstAggregator*, Location>>;

        /** The index of variable accesses */
        variable_reference_map var_references;

        /** The index of record definition points */
        record_definition_map record_definitions;

        /** The index of record-unpack levels */
        record_unpack_map record_unpacks;

        /** The level of a nested ram operation that is handling a given aggregator operation */
        aggregator_location_map aggregator_locations;

    public:
        // -- variables --

        void addVarReference(const AstVariable& var, const Location& l) {
            std::set<Location>& locs = var_references[var.getName()];
            locs.insert(l);
        }

        void addVarReference(const AstVariable& var, int level, int pos, const std::string& name = "") {
            addVarReference(var, Location({level, pos, name}));
        }

        bool isDefined(const AstVariable& var) const {
            return var_references.find(var.getName()) != var_references.end();
        }

        const Location& getDefinitionPoint(const AstVariable& var) const {
            auto pos = var_references.find(var.getName());
            assert(pos != var_references.end() && "Undefined variable referenced!");
            return *pos->second.begin();
        }

        const variable_reference_map& getVariableReferences() const {
            return var_references;
        }

        // -- records --

        // - definition -

        void setRecordDefinition(const AstRecordInit& init, const Location& l) {
            record_definitions[&init] = l;
        }

        void setRecordDefinition(const AstRecordInit& init, int level, int pos, std::string name = "") {
            setRecordDefinition(init, Location({level, pos, std::move(name)}));
        }

        const Location& getDefinitionPoint(const AstRecordInit& init) const {
            auto pos = record_definitions.find(&init);
            if (pos != record_definitions.end()) {
                return pos->second;
            }
            assert(false && "Requested location for undefined record!");

            static Location fail;
            return fail;
        }

        // - unpacking -

        void setRecordUnpackLevel(const AstRecordInit& init, int level) {
            record_unpacks[&init] = level;
        }

        int getRecordUnpackLevel(const AstRecordInit& init) const {
            auto pos = record_unpacks.find(&init);
            if (pos != record_unpacks.end()) {
                return pos->second;
            }
            assert(false && "Requested record is not unpacked properly!");
            return 0;
        }

        // -- aggregates --

        void setAggregatorLocation(const AstAggregator& agg, const Location& loc) {
            aggregator_locations.push_back(std::make_pair(&agg, loc));
        }

        const Location& getAggregatorLocation(const AstAggregator& agg) const {
            // search list
            for (const auto& cur : aggregator_locations) {
                if (*cur.first == agg) {
                    return cur.second;
                }
            }

            // fail
            std::cout << "Lookup of " << &agg << " = " << agg << " failed\n";
            assert(false && "Requested aggregation operation is not processed!");

            const static Location fail = Location();
            return fail;
        }

        // -- others --

        bool isSomethingDefinedOn(int level) const {
            // check for variable definitions
            for (const auto& cur : var_references) {
                if (cur.second.begin()->level == level) {
                    return true;
                }
            }
            // check for record definitions
            for (const auto& cur : record_definitions) {
                if (cur.second.level == level) {
                    return true;
                }
            }
            // nothing defined on this level
            return false;
        }

        void print(std::ostream& out) const {
            out << "Variables:\n\t";
            out << join(var_references, "\n\t");
        }

        friend std::ostream& operator<<(std::ostream& out, const ValueIndex& index) __attribute__((unused)) {
            index.print(out);
            return out;
        }
    };

    /**
     * A utility function assigning names to unnamed variables such that enclosing
     * constructs may be cloned without losing the variable-identity.
     */
    void nameUnnamedVariables(AstClause* clause);

    void appendStmt(std::unique_ptr<RamStatement>& stmtList, std::unique_ptr<RamStatement> stmt);

    SymbolMask getSymbolMask(const AstRelation& rel);

    /* Converts the given relation identifier into a relation name.  */
    std::string getRelationName(const AstRelationIdentifier& id);

    void makeIODirective(IODirectives& ioDirective, const AstRelation* rel, const std::string& filePath,
            const std::string& fileExt, const bool isIntermediate);

    std::vector<IODirectives> getInputIODirectives(const AstRelation* rel,
            std::string filePath = std::string(), const std::string& fileExt = std::string());

    std::vector<IODirectives> getOutputIODirectives(const AstRelation* rel,
            std::string filePath = std::string(), const std::string& fileExt = std::string());

    std::unique_ptr<RamRelation> getRamRelation(const AstRelation* rel, std::string name, size_t arity,
            const bool istemp = false, const bool hashset = false);

    std::string translateRelationName(const AstRelationIdentifier& id);

    std::unique_ptr<RamValue> translateValue(const AstArgument* arg, const ValueIndex& index = ValueIndex());

    std::unique_ptr<RamValue> translateValue(const AstArgument& arg, const ValueIndex& index = ValueIndex()) {
        return translateValue(&arg, index);
    }

    /** generate RAM code for a clause */
    std::unique_ptr<RamStatement> translateClause(const AstClause& clause, const AstClause& originalClause,
            int version = 0, bool ret = false, bool hashset = false);

    /**
     * Generates RAM code for the non-recursive clauses of the given relation.
     *
     * @return a corresponding statement or null if there are no non-recursive clauses.
     */
    std::unique_ptr<RamStatement> translateNonRecursiveRelation(
            const AstRelation& rel, const RecursiveClauses* recursiveClauses);

    /** generate RAM code for recursive relations in a strongly-connected component */
    std::unique_ptr<RamStatement> translateRecursiveRelation(
            const std::set<const AstRelation*>& scc, const RecursiveClauses* recursiveClauses);

    /** generate RAM code for subroutine to get subproofs */
    std::unique_ptr<RamStatement> makeSubproofSubroutine(const AstClause& clause);

    /** Translate AST to RamProgram */
    std::unique_ptr<RamProgram> translateProgram(const AstTranslationUnit& translationUnit);

public:
    AstTranslator() : program(nullptr){};

    /** translates AST to translation unit  */
    std::unique_ptr<RamTranslationUnit> translateUnit(AstTranslationUnit& tu);
};

}  // end of namespace souffle
