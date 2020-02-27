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
#include "AstQualifiedName.h"
#include "AstRelation.h"
#include "AstType.h"
#include "AstUtils.h"
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
 */
class AstProgram : public AstNode {
public:
    void print(std::ostream& os) const override {
        /* Print types */
        os << "// ----- Types -----\n";
        for (const auto& cur : types) {
            os << *cur << "\n";
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
        for (const auto& f : functors) {
            os << "\n\n// -- " << f->getName() << " --\n";
            f->print(os);
            os << "\n";
        }

        /* Print relations */
        os << "\n// ----- Relations -----\n";
        for (const auto& rel : relations) {
            os << "\n\n// -- " << rel->getQualifiedName() << " --\n";
            os << *rel << "\n\n";
            for (const auto& clause : clauses) {
                if (clause->getHead()->getQualifiedName() == rel->getQualifiedName()) {
                    os << *clause << "\n\n";
                }
            }
        }

        const auto& orphanClauses = getOrphanClauses(*this);
        if (!orphanClauses.empty()) {
            os << "\n// ----- Orphan Clauses -----\n";
            os << join(orphanClauses, "\n\n", print_deref<AstClause*>()) << "\n";
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

    /** get types */
    std::vector<AstType*> getTypes() const {
        return toPtrVector(types);
    }

    /** get relations */
    std::vector<AstRelation*> getRelations() const {
        return toPtrVector(relations);
    }

    /** get clauses */
    std::vector<AstClause*> getClauses() const {
        return toPtrVector(clauses);
    }

    /** get functor declarations */
    std::vector<AstFunctorDeclaration*> getFunctorDeclarations() const {
        return toPtrVector(functors);
    }

    /** get load directives */
    // TODO (b-scholz): unify load/store/printsizes
    const std::vector<std::unique_ptr<AstLoad>>& getLoads() const {
        return loads;
    }

    /** get print-size directives */
    // TODO (b-scholz): unify load/store/printsizes
    const std::vector<std::unique_ptr<AstPrintSize>>& getPrintSizes() const {
        return printSizes;
    }

    /** get store directives */
    // TODO (b-scholz): unify load/store/printsizes
    const std::vector<std::unique_ptr<AstStore>>& getStores() const {
        return stores;
    }

    /** get pragma directives */
    const std::vector<std::unique_ptr<AstPragma>>& getPragmaDirectives() const {
        return pragmaDirectives;
    }

    /* add relation */
    void addRelation(std::unique_ptr<AstRelation> r) {
        assert(getRelation(*this, r->getQualifiedName()) == nullptr && "Redefinition of relation!");
        relations.push_back(std::move(r));
    }

    /** remove relation */
    bool removeRelation(const AstQualifiedName& name) {
        for (auto it = relations.begin(); it != relations.end(); it++) {
            const auto& rel = *it;
            if (rel->getQualifiedName() == name) {
                removeRelationClauses(*this, name);
                relations.erase(it);
                return true;
            }
        }
        return false;
    }

    /** add a clause */
    void addClause(std::unique_ptr<AstClause> clause) {
        assert(clause != nullptr && "Undefined clause");
        assert(clause->getHead() != nullptr && "Undefined head of the clause");
        clauses.push_back(std::move(clause));
    }

    /** remove a clause */
    bool removeClause(const AstClause* clause) {
        for (auto it = clauses.begin(); it != clauses.end(); it++) {
            if (**it == *clause) {
                clauses.erase(it);
                return true;
            }
        }
        return false;
    }

    /** get components */
    std::vector<AstComponent*> getComponents() const {
        return toPtrVector(components);
    }

    /** get component instantiation */
    std::vector<AstComponentInit*> getComponentInstantiations() const {
        return toPtrVector(instantiations);
    }

    AstProgram* clone() const override {
        auto res = new AstProgram();

        for (const auto& cur : pragmaDirectives) {
            res->pragmaDirectives.emplace_back(cur->clone());
        }
        for (const auto& cur : components) {
            res->components.emplace_back(cur->clone());
        }
        for (const auto& cur : instantiations) {
            res->instantiations.emplace_back(cur->clone());
        }
        for (const auto& cur : types) {
            res->types.emplace_back(cur->clone());
        }
        for (const auto& cur : functors) {
            res->functors.emplace_back(cur->clone());
        }
        for (const auto& cur : relations) {
            res->relations.emplace_back(cur->clone());
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

        // done
        return res;
    }

    void apply(const AstNodeMapper& map) override {
        for (auto& cur : pragmaDirectives) {
            cur = map(std::move(cur));
        }
        for (auto& cur : components) {
            cur = map(std::move(cur));
        }
        for (auto& cur : instantiations) {
            cur = map(std::move(cur));
        }
        for (auto& cur : functors) {
            cur = map(std::move(cur));
        }
        for (auto& cur : types) {
            cur = map(std::move(cur));
        }
        for (auto& cur : relations) {
            cur = map(std::move(cur));
        }
        for (auto& cur : clauses) {
            cur = map(std::move(cur));
        }
        for (auto& cur : loads) {
            cur = map(std::move(cur));
        }
        for (auto& cur : stores) {
            cur = map(std::move(cur));
        }
        for (auto& cur : printSizes) {
            cur = map(std::move(cur));
        }
    }

    std::vector<const AstNode*> getChildNodes() const override {
        std::vector<const AstNode*> res;
        for (const auto& cur : pragmaDirectives) {
            res.push_back(cur.get());
        }
        for (const auto& cur : components) {
            res.push_back(cur.get());
        }
        for (const auto& cur : instantiations) {
            res.push_back(cur.get());
        }
        for (const auto& cur : functors) {
            res.push_back(cur.get());
        }
        for (const auto& cur : types) {
            res.push_back(cur.get());
        }
        for (const auto& cur : relations) {
            res.push_back(cur.get());
        }
        for (const auto& cur : clauses) {
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

        if (!equal_targets(pragmaDirectives, other.pragmaDirectives)) {
            return false;
        }
        if (!equal_targets(components, other.components)) {
            return false;
        }
        if (!equal_targets(instantiations, other.instantiations)) {
            return false;
        }
        if (!equal_targets(functors, other.functors)) {
            return false;
        }
        if (!equal_targets(types, other.types)) {
            return false;
        }
        if (!equal_targets(relations, other.relations)) {
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
        return true;
    }

protected:
    friend class ComponentInstantiationTransformer;
    friend class ParserDriver;
    friend class ProvenanceTransformer;
    friend class MagicSetTransformer;

    /* add type */
    void addType(std::unique_ptr<AstType> type) {
        assert(getType(*this, type->getQualifiedName()) == nullptr && "Redefinition of type!");
        types.push_back(std::move(type));
    }

    /** add load directive */
    void addLoad(std::unique_ptr<AstLoad> directive) {
        assert(directive && "NULL IO directive");
        loads.push_back(std::move(directive));
    }

    /** add printsize directive */
    void addPrintSize(std::unique_ptr<AstPrintSize> directive) {
        assert(directive && "NULL IO directive");
        printSizes.push_back(std::move(directive));
    }

    /** add store directive */
    void addStore(std::unique_ptr<AstStore> directive) {
        assert(directive && "NULL IO directive");
        stores.push_back(std::move(directive));
    }

    /** add a pragma */
    void addPragma(std::unique_ptr<AstPragma> pragma) {
        assert(pragma && "NULL IO directive");
        pragmaDirectives.push_back(std::move(pragma));
    }

    /** add functor */
    void addFunctorDeclaration(std::unique_ptr<souffle::AstFunctorDeclaration> f) {
        assert(getFunctorDeclaration(*this, f->getName()) == nullptr && "Redefinition of functor!");
        functors.push_back(std::move(f));
    }

    /** add component */
    void addComponent(std::unique_ptr<AstComponent> c) {
        components.push_back(std::move(c));
    }

    /** add component instantiation */
    void addInstantiation(std::unique_ptr<AstComponentInit> i) {
        instantiations.push_back(std::move(i));
    }

    /** Program types  */
    std::vector<std::unique_ptr<AstType>> types;

    /** Program relations */
    std::vector<std::unique_ptr<AstRelation>> relations;

    /** External Functors */
    std::vector<std::unique_ptr<AstFunctorDeclaration>> functors;

    /** Program clauses */
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
};

}  // namespace souffle
