/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstProgram.h
 *
 * Define a class that represents a Datalog program consisting of types,
 * relations, and clauses.
 *
 ***********************************************************************/

#pragma once

#include "AstComponent.h"
#include "AstFunctorDeclaration.h"
#include "AstIO.h"
#include "AstNode.h"
#include "AstPragma.h"
#include "AstRelation.h"
#include "AstRelationIdentifier.h"
#include "AstType.h"
#include "ErrorReport.h"
#include "Util.h"
#include <cassert>
#include <cstddef>
#include <iosfwd>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

namespace souffle {

class AstClause;
class AstLoad;
class AstPrintSize;
class AstStore;
class AstRelation;

/**
 *  Intermediate representation of a datalog program
 *          that consists of relations, clauses and types
 *  TODO (b-scholz): there are a lot of dependencies (pareser etc);
 *       we need to simplify the interface / class
 */
class AstProgram : public AstNode {
public:
    AstProgram() = default;

    void print(std::ostream& os) const override {
        /* Print types */
        os << "// ----- Types -----\n";
        for (const auto& cur : types) {
            os << *cur.second << "\n";
        }

        /* Print components */
        if (!components.empty()) {
            os << "\n// ----- Components -----\n";
            for (const auto& cur : components) {
                os << *cur << "\n";
            }
        }

        /* Print instantiations */
        if (!instantiations.empty()) {
            os << "\n";
            for (const auto& cur : instantiations) {
                os << *cur << "\n";
            }
        }

        /* Print functors */
        os << "\n// ----- Functors -----\n";
        for (const auto& cur : functors) {
            const std::unique_ptr<AstFunctorDeclaration>& f = cur.second;
            os << "\n\n// -- " << f->getName() << " --\n";
            f->print(os);
            os << "\n";
        }

        /* Print relations */
        os << "\n// ----- Relations -----\n";
        for (const auto& cur : relations) {
            const std::unique_ptr<AstRelation>& rel = cur.second;
            os << "\n\n// -- " << rel->getName() << " --\n";
            os << *rel << "\n\n";
            for (const auto clause : rel->getClauses()) {
                os << *clause << "\n\n";
            }
            for (const auto ioDirective : rel->getLoads()) {
                os << *ioDirective << "\n\n";
            }
            for (const auto ioDirective : rel->getStores()) {
                os << *ioDirective << "\n\n";
            }
        }

        if (!clauses.empty()) {
            os << "\n// ----- Orphan Clauses -----\n";
            os << join(clauses, "\n\n", print_deref<std::unique_ptr<AstClause>>()) << "\n";
        }

        if (!loads.empty()) {
            os << "\n// ----- Orphan Load directives -----\n";
            os << join(loads, "\n\n", print_deref<std::unique_ptr<AstLoad>>()) << "\n";
        }
        if (!stores.empty()) {
            os << "\n// ----- Orphan Store directives -----\n";
            os << join(stores, "\n\n", print_deref<std::unique_ptr<AstStore>>()) << "\n";
        }

        if (!pragmaDirectives.empty()) {
            os << "\n// ----- Pragma -----\n";
            for (const auto& cur : pragmaDirectives) {
                os << *cur << "\n";
            }
        }
    }

    /** Obtains the type with the given name */
    const AstType* getType(const AstTypeIdentifier& name) const {
        auto pos = types.find(name);
        return (pos == types.end()) ? nullptr : pos->second.get();
    }

    /** Gets a list of all types in this program */
    std::vector<const AstType*> getTypes() const {
        std::vector<const AstType*> res;
        for (const auto& cur : types) {
            res.push_back(cur.second.get());
        }
        return res;
    }

    /** Find and return the relation in the program given its name */
    AstRelation* getRelation(const AstRelationIdentifier& name) const {
        auto pos = relations.find(name);
        return (pos == relations.end()) ? nullptr : pos->second.get();
    }

    /** Get functor declaration */
    AstFunctorDeclaration* getFunctorDeclaration(const std::string& name) const {
        auto pos = functors.find(name);
        return (pos == functors.end()) ? nullptr : pos->second.get();
    }

    /** Get all relations in the program */
    std::vector<AstRelation*> getRelations() const {
        std::vector<AstRelation*> res;
        for (const auto& rel : relations) {
            res.push_back(rel.second.get());
        }
        return res;
    }

    /** Get all io directives in the program */
    const std::vector<std::unique_ptr<AstLoad>>& getLoads() const {
        return loads;
    }

    const std::vector<std::unique_ptr<AstPrintSize>>& getPrintSizes() const {
        return printSizes;
    }

    const std::vector<std::unique_ptr<AstStore>>& getStores() const {
        return stores;
    }

    /** Get all pragma directives in the program */
    const std::vector<std::unique_ptr<AstPragma>>& getPragmaDirectives() const {
        return pragmaDirectives;
    }

    /** Return the number of relations in the program */
    size_t relationSize() const {
        return relations.size();
    }

    /** appends a new relation to this program -- after parsing */
    void appendRelation(std::unique_ptr<AstRelation> r) {
        // get relation
        std::unique_ptr<AstRelation>& rel = relations[r->getName()];
        assert(!rel && "Adding pre-existing relation!");

        // add relation
        rel = std::move(r);
    }

    /** Remove a relation from the program. */
    void removeRelation(const AstRelationIdentifier& name) {
        /* Remove relation from map */
        relations.erase(relations.find(name));
    }

    /** append a new clause to this program -- after parsing */
    void appendClause(std::unique_ptr<AstClause> clause) {
        // get relation
        std::unique_ptr<AstRelation>& r = relations[clause->getHead()->getName()];
        assert(r && "Trying to append to unknown relation!");

        // delegate call
        r->addClause(std::move(clause));
    }

    /** Removes a clause from this program */
    void removeClause(const AstClause* clause) {
        // get relation
        auto pos = relations.find(clause->getHead()->getName());
        if (pos == relations.end()) {
            return;
        }

        // delegate call
        pos->second->removeClause(clause);
    }

    /**
     * Obtains a list of clauses not associated to any relations. In
     * a valid program this list is always empty
     */
    std::vector<AstClause*> getOrphanClauses() const {
        return toPtrVector(clauses);
    }

    // -- Components -----------------------------------------------------------

    /** Obtains a list of all comprised components */
    std::vector<AstComponent*> getComponents() const {
        return toPtrVector(components);
    }

    /** Obtains a list of all component instantiations */
    std::vector<AstComponentInit*> getComponentInstantiations() const {
        return toPtrVector(instantiations);
    }

    AstProgram* clone() const override {
        auto res = new AstProgram();

        // move types
        for (const auto& cur : types) {
            res->types.insert(std::make_pair(cur.first, std::unique_ptr<AstType>(cur.second->clone())));
        }

        for (const auto& cur : relations) {
            res->relations.insert(
                    std::make_pair(cur.first, std::unique_ptr<AstRelation>(cur.second->clone())));
        }

        for (const auto& cur : functors) {
            res->functors.insert(
                    std::make_pair(cur.first, std::unique_ptr<AstFunctorDeclaration>(cur.second->clone())));
        }

        for (const auto& cur : clauses) {
            res->clauses.emplace_back(cur->clone());
        }
        for (const auto& cur : loads) {
            res->loads.emplace_back(cur->clone());
        }
        for (const auto& cur : printSizes) {
            res->printSizes.emplace_back(cur->clone());
        }
        for (const auto& cur : stores) {
            res->stores.emplace_back(cur->clone());
        }

        for (const auto& cur : components) {
            res->components.emplace_back(cur->clone());
        }

        for (const auto& cur : instantiations) {
            res->instantiations.emplace_back(cur->clone());
        }

        for (const auto& cur : pragmaDirectives) {
            res->pragmaDirectives.emplace_back(cur->clone());
        }

        ErrorReport errors;

        res->finishParsing();

        // done
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& cur : types) {
            cur.second = map(std::move(cur.second));
        }
        for (auto& cur : relations) {
            cur.second = map(std::move(cur.second));
        }
        for (auto& cur : components) {
            cur = map(std::move(cur));
        }
        for (auto& cur : instantiations) {
            cur = map(std::move(cur));
        }
        for (auto& cur : pragmaDirectives) {
            cur = map(std::move(cur));
        }
        for (auto& cur : loads) {
            cur = map(std::move(cur));
        }
        for (auto& cur : stores) {
            cur = map(std::move(cur));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> res;
        for (const auto& cur : types) {
            res.push_back(cur.second.get());
        }
        for (const auto& cur : relations) {
            res.push_back(cur.second.get());
        }
        for (const auto& cur : components) {
            res.push_back(cur.get());
        }
        for (const auto& cur : instantiations) {
            res.push_back(cur.get());
        }
        for (const auto& cur : clauses) {
            res.push_back(cur.get());
        }
        for (const auto& cur : pragmaDirectives) {
            res.push_back(cur.get());
        }
        for (const auto& cur : loads) {
            res.push_back(cur.get());
        }
        for (const auto& cur : printSizes) {
            res.push_back(cur.get());
        }
        for (const auto& cur : stores) {
            res.push_back(cur.get());
        }
        return res;
    }

protected:
    bool equal(const AstNode& node) const override {
        assert(nullptr != dynamic_cast<const AstProgram*>(&node));
        const auto& other = static_cast<const AstProgram&>(node);

        // check list sizes
        if (types.size() != other.types.size()) {
            return false;
        }
        if (relations.size() != other.relations.size()) {
            return false;
        }

        // check types
        for (const auto& cur : types) {
            auto pos = other.types.find(cur.first);
            if (pos == other.types.end()) {
                return false;
            }
            if (*cur.second != *pos->second) {
                return false;
            }
        }

        // check relations
        for (const auto& cur : relations) {
            auto pos = other.relations.find(cur.first);
            if (pos == other.relations.end()) {
                return false;
            }
            if (*cur.second != *pos->second) {
                return false;
            }
        }

        // check components
        if (!equal_targets(components, other.components)) {
            return false;
        }
        if (!equal_targets(instantiations, other.instantiations)) {
            return false;
        }
        if (!equal_targets(clauses, other.clauses)) {
            return false;
        }
        if (!equal_targets(loads, other.loads)) {
            return false;
        }
        if (!equal_targets(printSizes, other.printSizes)) {
            return false;
        }
        if (!equal_targets(stores, other.stores)) {
            return false;
        }

        // no different found => programs are equal
        return true;
    }

private:
    friend class ComponentInstantiationTransformer;
    friend class ParserDriver;
    friend class ProvenanceTransformer;

    /** Program types  */
    std::map<AstTypeIdentifier, std::unique_ptr<AstType>> types;

    /** Program relations */
    std::map<AstRelationIdentifier, std::unique_ptr<AstRelation>> relations;

    /** External Functors */
    std::map<std::string, std::unique_ptr<AstFunctorDeclaration>> functors;

    /** The list of clauses provided by the user */
    std::vector<std::unique_ptr<AstClause>> clauses;

    /** The list of IO directives provided by the user */
    std::vector<std::unique_ptr<AstLoad>> loads;
    std::vector<std::unique_ptr<AstPrintSize>> printSizes;
    std::vector<std::unique_ptr<AstStore>> stores;

    /** Program components */
    std::vector<std::unique_ptr<AstComponent>> components;

    /** Component instantiations */
    std::vector<std::unique_ptr<AstComponentInit>> instantiations;

    /** Pragmas */
    std::vector<std::unique_ptr<AstPragma>> pragmaDirectives;

    // -- Types ----------------------------------------------------------------

    /** Add the given type to the program. Asserts if a type with the
      same name has already been added.  */
    void addType(std::unique_ptr<AstType> type) {
        auto& cur = types[type->getName()];
        assert(!cur && "Redefinition of type!");
        cur = std::move(type);
    }

    // -- Relations ------------------------------------------------------------

    /** Add the given relation to the program. Asserts if a relation with the
     * same name has already been added. */
    void addRelation(std::unique_ptr<AstRelation> r) {
        const auto& name = r->getName();
        assert(relations.find(name) == relations.end() && "Redefinition of relation!");
        relations[name] = std::move(r);
    }

    /** Add a clause to the program */
    void addClause(std::unique_ptr<AstClause> clause) {
        assert(clause && "NULL clause");
        clauses.push_back(std::move(clause));
    }

    /** Add an IO directive to the program */
    void addLoad(std::unique_ptr<AstLoad> directive) {
        assert(directive && "NULL IO directive");
        loads.push_back(std::move(directive));
    }

    void addPrintSize(std::unique_ptr<AstPrintSize> directive) {}

    void addStore(std::unique_ptr<AstStore> directive) {
        assert(directive && "NULL IO directive");
        stores.push_back(std::move(directive));
    }

    /** Add a pragma to the program */
    void addPragma(std::unique_ptr<AstPragma> pragma) {
        assert(pragma && "NULL IO directive");
        pragmaDirectives.push_back(std::move(pragma));
    }

    /** Add a functor to the program */
    void addFunctorDeclaration(std::unique_ptr<souffle::AstFunctorDeclaration> f) {
        const auto& name = f->getName();
        assert(functors.find(name) == functors.end() && "Redefinition of relation!");
        functors[name] = std::move(f);
    }

    // -- Components -----------------------------------------------------------

    /** Adds the given component to this program */
    void addComponent(std::unique_ptr<AstComponent> c) {
        components.push_back(std::move(c));
    }

    /** Adds a component instantiation */
    void addInstantiation(std::unique_ptr<AstComponentInit> i) {
        instantiations.push_back(std::move(i));
    }

    void finishParsing() {
        // unbound clauses with no relation defined
        std::vector<std::unique_ptr<AstClause>> unbound;

        // add clauses
        for (auto& cur : clauses) {
            auto pos = relations.find(cur->getHead()->getName());
            if (pos != relations.end()) {
                pos->second->addClause(std::move(cur));
            } else {
                unbound.push_back(std::move(cur));
            }
        }
        // remember the remaining orphan clauses
        clauses.clear();
        clauses.swap(unbound);

        // unbound directives with no relation defined
        std::vector<std::unique_ptr<AstLoad>> unboundLoads;
        std::vector<std::unique_ptr<AstStore>> unboundStores;

        // add IO directives
        for (auto& cur : loads) {
            auto pos = relations.find(cur->getName());
            if (pos != relations.end()) {
                pos->second->addLoad(std::move(cur));
            } else {
                unboundLoads.push_back(std::move(cur));
            }
        }
        // remember the remaining orphan directives
        loads.clear();
        loads.swap(unboundLoads);

        for (auto& cur : stores) {
            auto pos = relations.find(cur->getName());
            if (pos != relations.end()) {
                pos->second->addStore(std::move(cur));
            } else {
                unboundStores.push_back(std::move(cur));
            }
        }
        // remember the remaining orphan directives
        stores.clear();
        stores.swap(unboundStores);
    }
};

}  // namespace souffle
