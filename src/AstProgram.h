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
class AstRelation;
class AstIO;

/**
 *  Intermediate representation of a datalog program
 *          that consists of relations, clauses and types
 *  TODO (b-scholz): there are a lot of dependencies (pareser etc);
 *       we need to simplify the interface / class
 */
class AstProgram : public AstNode {
public:
    void print(std::ostream& os) const override {
        for (const auto& cur : types) {
            os << *cur.second << "\n";
        }
        if (!components.empty()) {
            for (const auto& cur : components) {
                os << *cur << "\n";
            }
        }
        if (!instantiations.empty()) {
            os << "\n";
            for (const auto& cur : instantiations) {
                os << *cur << "\n";
            }
        }
        for (const auto& cur : functors) {
            const std::unique_ptr<AstFunctorDeclaration>& f = cur.second;
            os << "\n\n// -- " << f->getName() << " --\n";
            f->print(os);
            os << "\n";
        }
        for (const auto& cur : relations) {
            const std::unique_ptr<AstRelation>& rel = cur.second;
            os << "\n\n// -- " << rel->getQualifiedName() << " --\n";
            os << *rel << "\n\n";
            for (const auto clause : rel->getClauses()) {
                os << *clause << "\n\n";
            }
        }
        if (!clauses.empty()) {
            os << join(clauses, "\n\n", print_deref<std::unique_ptr<AstClause>>()) << "\n";
        }
        if (!ios.empty()) {
            os << join(ios, "\n\n", print_deref<std::unique_ptr<AstIO>>()) << "\n";
        }
        if (!pragmaDirectives.empty()) {
            for (const auto& cur : pragmaDirectives) {
                os << *cur << "\n";
            }
        }
    }

    /** get type */
    // TODO (b-scholz): remove this method
    const AstType* getType(const AstQualifiedName& name) const {
        auto pos = types.find(name);
        return (pos == types.end()) ? nullptr : pos->second.get();
    }

    /** get types */
    std::vector<const AstType*> getTypes() const {
        std::vector<const AstType*> res;
        for (const auto& cur : types) {
            res.push_back(cur.second.get());
        }
        return res;
    }

    /** get relations */
    std::vector<AstRelation*> getRelations() const {
        std::vector<AstRelation*> res;
        for (const auto& rel : relations) {
            res.push_back(rel.second.get());
        }
        return res;
    }

    /** get relation */
    // TODO (b-scholz): remove this method
    AstRelation* getRelation(const AstQualifiedName& name) const {
        auto pos = relations.find(name);
        return (pos == relations.end()) ? nullptr : pos->second.get();
    }

    /** get number of relations */
    // TODO (b-scholz): remove this method
    size_t relationSize() const {
        return relations.size();
    }

    /** get functor declaration */
    // TODO (b-scholz): replace by list of functors
    AstFunctorDeclaration* getFunctorDeclaration(const std::string& name) const {
        auto pos = functors.find(name);
        return (pos == functors.end()) ? nullptr : pos->second.get();
    }

    /** get io directives */
    const std::vector<std::unique_ptr<AstIO>>& getIOs() const {
        return ios;
    }

    /** get pragma directives */
    const std::vector<std::unique_ptr<AstPragma>>& getPragmaDirectives() const {
        return pragmaDirectives;
    }

    /** append new relation */
    void appendRelation(std::unique_ptr<AstRelation> r) {
        // get relation
        std::unique_ptr<AstRelation>& rel = relations[r->getQualifiedName()];
        assert(!rel && "Adding pre-existing relation!");

        // add relation
        rel = std::move(r);
    }

    /** remove relation */
    void removeRelation(const AstQualifiedName& name) {
        /* Remove relation from map */
        relations.erase(relations.find(name));
    }

    /** append clause */
    void appendClause(std::unique_ptr<AstClause> clause) {
        // get relation
        std::unique_ptr<AstRelation>& r = relations[clause->getHead()->getQualifiedName()];
        assert(r && "Trying to append to unknown relation!");

        // delegate call
        r->addClause(std::move(clause));
    }

    /** remove clause */
    void removeClause(const AstClause* clause) {
        // get relation
        auto pos = relations.find(clause->getHead()->getQualifiedName());
        if (pos == relations.end()) {
            return;
        }

        // delegate call
        pos->second->removeClause(clause);
    }

    /** get orphan clauses (clauses without relation declarations) */
    std::vector<AstClause*> getOrphanClauses() const {
        return toPtrVector(clauses);
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
            res->types.insert(std::make_pair(cur.first, std::unique_ptr<AstType>(cur.second->clone())));
        }
        for (const auto& cur : functors) {
            res->functors.insert(
                    std::make_pair(cur.first, std::unique_ptr<AstFunctorDeclaration>(cur.second->clone())));
        }
        for (const auto& cur : relations) {
            res->relations.insert(
                    std::make_pair(cur.first, std::unique_ptr<AstRelation>(cur.second->clone())));
        }
        for (const auto& cur : clauses) {
            res->clauses.emplace_back(cur->clone());
        }
        for (const auto& cur : ios) {
            res->ios.emplace_back(cur->clone());
        }

        // TODO (b-scholz): that is odd - revisit!
        res->finishParsing();

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
            cur.second = map(std::move(cur.second));
        }
        for (auto& cur : types) {
            cur.second = map(std::move(cur.second));
        }
        for (auto& cur : relations) {
            cur.second = map(std::move(cur.second));
        }
        for (auto& cur : clauses) {
            cur = map(std::move(cur));
        }
        for (auto& cur : ios) {
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
            res.push_back(cur.second.get());
        }
        for (const auto& cur : types) {
            res.push_back(cur.second.get());
        }
        for (const auto& cur : relations) {
            res.push_back(cur.second.get());
        }
        for (const auto& cur : clauses) {
            res.push_back(cur.get());
        }
        for (const auto& cur : ios) {
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
        if (!equal_targets(ios, other.ios)) {
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
        auto& cur = types[type->getQualifiedName()];
        assert(!cur && "Redefinition of type!");
        cur = std::move(type);
    }

    /* add relation */
    void addRelation(std::unique_ptr<AstRelation> r) {
        const auto& name = r->getQualifiedName();
        assert(relations.find(name) == relations.end() && "Redefinition of relation!");
        relations[name] = std::move(r);
    }

    /** add a clause */
    void addClause(std::unique_ptr<AstClause> clause) {
        assert(clause && "NULL clause");
        clauses.push_back(std::move(clause));
    }

    /** add IO directive */
    void addIO(std::unique_ptr<AstIO> directive) {
        assert(directive && "NULL IO directive");
        ios.push_back(std::move(directive));
    }

    /** add a pragma */
    void addPragma(std::unique_ptr<AstPragma> pragma) {
        assert(pragma && "NULL IO directive");
        pragmaDirectives.push_back(std::move(pragma));
    }

    /** add functor */
    void addFunctorDeclaration(std::unique_ptr<souffle::AstFunctorDeclaration> f) {
        const auto& name = f->getName();
        assert(functors.find(name) == functors.end() && "Redefinition of relation!");
        functors[name] = std::move(f);
    }

    /** add component */
    void addComponent(std::unique_ptr<AstComponent> c) {
        components.push_back(std::move(c));
    }

    /** add component instantiation */
    void addInstantiation(std::unique_ptr<AstComponentInit> i) {
        instantiations.push_back(std::move(i));
    }

    /** finishing parsing */
    void finishParsing() {
        // unbound clauses with no relation defined
        std::vector<std::unique_ptr<AstClause>> unbound;

        // add clauses
        for (auto& cur : clauses) {
            auto pos = relations.find(cur->getHead()->getQualifiedName());
            if (pos != relations.end()) {
                pos->second->addClause(std::move(cur));
            } else {
                unbound.push_back(std::move(cur));
            }
        }
        // remember the remaining orphan clauses
        clauses.clear();
        clauses.swap(unbound);
    }

    /** Program types  */
    // TODO(b-scholz): change to vector
    std::map<AstQualifiedName, std::unique_ptr<AstType>> types;

    /** Program relations */
    // TODO(b-scholz): change to vector
    std::map<AstQualifiedName, std::unique_ptr<AstRelation>> relations;

    /** External Functors */
    // TODO(b-scholz): change to vector
    std::map<std::string, std::unique_ptr<AstFunctorDeclaration>> functors;

    /** The list of clauses provided by the user */
    std::vector<std::unique_ptr<AstClause>> clauses;

    /** IO statements */
    std::vector<std::unique_ptr<AstIO>> ios;

    /** Program components */
    std::vector<std::unique_ptr<AstComponent>> components;

    /** Component instantiations */
    std::vector<std::unique_ptr<AstComponentInit>> instantiations;

    /** Pragmas */
    std::vector<std::unique_ptr<AstPragma>> pragmaDirectives;
};

}  // namespace souffle
