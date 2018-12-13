/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2018, The Souffle Developers. All rights reserved.
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file ResolveAliasesTransformer.cpp
 *
 * Define classes and functionality related to the ResolveAliases
 * transformer.
 *
 ***********************************************************************/

#include "AstTransforms.h"
#include "AstVisitor.h"

namespace souffle {

std::unique_ptr<AstClause> ResolveAliasesTransformer::resolveAliases(const AstClause& clause) {
    
}

bool ResolveAliasesTransformer::transform(AstTranslationUnit& translationUnit) {
    AstProgram& program = *translationUnit.getProgram();

    // get all clauses
    std::vector<const AstClause*> clauses;
    visitDepthFirst(program, [&](const AstRelation& rel) {
        for (const auto& cur : rel.getClauses()) {
            clauses.push_back(cur);
        }
    });

    // clean all clauses
    for (const AstClause* cur : clauses) {
        // -- Step 1 --
        // get rid of aliases
        std::unique_ptr<AstClause> noAlias = resolveAliases(*cur);

        // clean up equalities
        std::unique_ptr<AstClause> cleaned = removeTrivialEquality(*noAlias);

        // -- Step 2 --
        // restore simple terms in atoms
        removeComplexTermsInAtoms(*cleaned);

        // exchange the rules
        program.removeClause(cur);
        program.appendClause(std::move(cleaned));
    }

    // TODO: change from return true to actually return whether any changes were made
    return true;
}

}

// XXX XXX XXX ::: CLEAR UP FROM HERE ::: XXX XXX XXX

namespace souffle {

namespace {

/**
 * A utility class for the unification process required to eliminate
 * aliases. A substitution maps variables to terms and can be applied
 * as a transformation to AstArguments.
 */
class Substitution {
    // the type of map for storing mappings internally
    //   - variables are identified by their name (!)
    using map_t = std::map<std::string, std::unique_ptr<AstArgument>>;

    /** The mapping of variables to terms (see type def above) */
    map_t map;

public:
    // -- Ctors / Dtors --

    Substitution() = default;
    ;

    Substitution(const std::string& var, const AstArgument* arg) {
        map.insert(std::make_pair(var, std::unique_ptr<AstArgument>(arg->clone())));
    }

    virtual ~Substitution() = default;

    /**
     * Applies this substitution to the given argument and
     * returns a pointer to the modified argument.
     *
     * @param node the node to be transformed
     * @return a pointer to the modified or replaced node
     */
    virtual std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const {
        // create a substitution mapper
        struct M : public AstNodeMapper {
            const map_t& map;

            M(const map_t& map) : map(map) {}

            using AstNodeMapper::operator();

            std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
                // see whether it is a variable to be substituted
                if (auto var = dynamic_cast<AstVariable*>(node.get())) {
                    auto pos = map.find(var->getName());
                    if (pos != map.end()) {
                        return std::unique_ptr<AstNode>(pos->second->clone());
                    }
                }

                // otherwise - apply mapper recursively
                node->apply(*this);
                return node;
            }
        };

        // apply mapper
        return M(map)(std::move(node));
    }

    /**
     * A generic, type consistent wrapper of the transformation
     * operation above.
     */
    template <typename T>
    std::unique_ptr<T> operator()(std::unique_ptr<T> node) const {
        std::unique_ptr<AstNode> resPtr =
                (*this)(std::unique_ptr<AstNode>(static_cast<AstNode*>(node.release())));
        assert(nullptr != dynamic_cast<T*>(resPtr.get()) && "Invalid node type mapping.");
        return std::unique_ptr<T>(dynamic_cast<T*>(resPtr.release()));
    }

    /**
     * Appends the given substitution to this substitution such that
     * this substitution has the same effect as applying this following
     * the given substitution in sequence.
     */
    void append(const Substitution& s) {
        // apply substitution on all current mappings
        for (auto& cur : map) {
            cur.second = s(std::move(cur.second));
        }

        // append uncovered variables to the end
        for (const auto& cur : s.map) {
            auto pos = map.find(cur.first);
            if (pos != map.end()) {
                continue;
            }
            map.insert(std::make_pair(cur.first, std::unique_ptr<AstArgument>(cur.second->clone())));
        }
    }

    /** A print function (for debugging) */
    void print(std::ostream& out) const {
        out << "{"
            << join(map, ",",
                       [](std::ostream& out,
                               const std::pair<const std::string, std::unique_ptr<AstArgument>>& cur) {
                           out << cur.first << " -> " << *cur.second;
                       })
            << "}";
    }

    friend std::ostream& operator<<(std::ostream& out, const Substitution& s) __attribute__((unused)) {
        s.print(out);
        return out;
    }
};

/**
 * An equality constraint between to AstArguments utilized by the
 * unification algorithm required by the alias resolution.
 */
struct Equation {
    /** The two terms to be equivalent */
    std::unique_ptr<AstArgument> lhs;
    std::unique_ptr<AstArgument> rhs;

    Equation(const AstArgument& lhs, const AstArgument& rhs)
            : lhs(std::unique_ptr<AstArgument>(lhs.clone())), rhs(std::unique_ptr<AstArgument>(rhs.clone())) {
    }

    Equation(const AstArgument* lhs, const AstArgument* rhs)
            : lhs(std::unique_ptr<AstArgument>(lhs->clone())),
              rhs(std::unique_ptr<AstArgument>(rhs->clone())) {}

    Equation(const Equation& other)
            : lhs(std::unique_ptr<AstArgument>(other.lhs->clone())),
              rhs(std::unique_ptr<AstArgument>(other.rhs->clone())) {}

    Equation(Equation&& other) : lhs(std::move(other.lhs)), rhs(std::move(other.rhs)) {}

    ~Equation() = default;

    /**
     * Applies the given substitution to both sides of the equation.
     */
    void apply(const Substitution& s) {
        lhs = s(std::move(lhs));
        rhs = s(std::move(rhs));
    }

    /** Enables equations to be printed (for debugging) */
    void print(std::ostream& out) const {
        out << *lhs << " = " << *rhs;
    }

    friend std::ostream& operator<<(std::ostream& out, const Equation& e) __attribute__((unused)) {
        e.print(out);
        return out;
    }
};
}  // namespace

std::unique_ptr<AstClause> ResolveAliasesTransformer::resolveAliases(const AstClause& clause) {
    /**
     * This alias analysis utilizes unification over the equality
     * constraints in clauses.
     */

    // -- utilities --

    // tests whether something is a variable
    auto isVar = [&](const AstArgument& arg) { return dynamic_cast<const AstVariable*>(&arg); };

    // tests whether something is a record
    auto isRec = [&](const AstArgument& arg) { return dynamic_cast<const AstRecordInit*>(&arg); };

    // tests whether a value a occurs in a term b
    auto occurs = [](const AstArgument& a, const AstArgument& b) {
        bool res = false;
        visitDepthFirst(b, [&](const AstArgument& cur) { res = res || cur == a; });
        return res;
    };

    // find all variables appearing as functorless arguments in grounding atoms
    std::set<std::string> baseGroundedVariables;
    for (const AstAtom* atom : clause.getAtoms()) {
        for (const AstArgument* arg : atom->getArguments()) {
            if (const AstVariable* var = dynamic_cast<const AstVariable*>(arg)) {
                baseGroundedVariables.insert(var->getName());
            }
        }
    }

    // I) extract equations
    std::vector<Equation> equations;
    visitDepthFirst(clause, [&](const AstBinaryConstraint& rel) {
        if (rel.getOperator() == BinaryConstraintOp::EQ) {
            equations.push_back(Equation(rel.getLHS(), rel.getRHS()));
        }
    });

    // II) compute unifying substitution
    Substitution substitution;

    // a utility for processing newly identified mappings
    auto newMapping = [&](const std::string& var, const AstArgument* term) {
        // found a new substitution
        Substitution newMapping(var, term);

        // apply substitution to all remaining equations
        for (auto& cur : equations) {
            cur.apply(newMapping);
        }

        // add mapping v -> t to substitution
        substitution.append(newMapping);
    };

    while (!equations.empty()) {
        // get next equation to compute
        Equation cur = equations.back();
        equations.pop_back();

        // shortcuts for left/right
        const AstArgument& a = *cur.lhs;
        const AstArgument& b = *cur.rhs;

        // #1:   t = t   => skip
        if (a == b) {
            continue;
        }

        // #2:   [..] = [..]   => decompose
        if (isRec(a) && isRec(b)) {
            // get arguments
            const auto& args_a = static_cast<const AstRecordInit&>(a).getArguments();
            const auto& args_b = static_cast<const AstRecordInit&>(b).getArguments();

            // make sure sizes are identical
            assert(args_a.size() == args_b.size());

            // create new equalities
            for (size_t i = 0; i < args_a.size(); ++i) {
                equations.push_back(Equation(args_a[i], args_b[i]));
            }
            continue;
        }

        // neither is a variable
        if (!isVar(a) && !isVar(b)) {
            continue;  // => nothing to do
        }

        // both are variables
        if (isVar(a) && isVar(b)) {
            // a new mapping is found
            auto& var = static_cast<const AstVariable&>(a);
            newMapping(var.getName(), &b);
            continue;
        }

        // #3:   t = v   => swap
        if (!isVar(a)) {
            equations.push_back(Equation(b, a));
            continue;
        }

        // now we know a is a variable
        assert(isVar(a));

        // we have   v = t
        const auto& v = static_cast<const AstVariable&>(a);
        const AstArgument& t = b;

        // #4:   v occurs in t
        if (occurs(v, t)) {
            continue;
        }

        assert(!occurs(v, t));

        // #5:   v is already grounded
        if (baseGroundedVariables.find(v.getName()) != baseGroundedVariables.end()) {
            // v = t, where v is already intrinsically grounded
            // should not resolve this constraint here, unless t is a record type
            if (!dynamic_cast<const AstRecordInit*>(&t)) {
                continue;
            }
        }

        // add new maplet
        newMapping(v.getName(), &t);
    }

    // III) compute resulting clause
    return substitution(std::unique_ptr<AstClause>(clause.clone()));
}

std::unique_ptr<AstClause> ResolveAliasesTransformer::removeTrivialEquality(const AstClause& clause) {
    // finally: remove t = t constraints
    std::unique_ptr<AstClause> res(clause.cloneHead());
    for (AstLiteral* cur : clause.getBodyLiterals()) {
        // filter out t = t
        if (auto* rel = dynamic_cast<AstBinaryConstraint*>(cur)) {
            if (rel->getOperator() == BinaryConstraintOp::EQ) {
                if (*rel->getLHS() == *rel->getRHS()) {
                    continue;  // skip this one
                }
            }
        }
        res->addToBody(std::unique_ptr<AstLiteral>(cur->clone()));
    }

    // done
    return res;
}

void ResolveAliasesTransformer::removeComplexTermsInAtoms(AstClause& clause) {
    // restore temporary variables for expressions in atoms

    // get list of atoms
    std::vector<AstAtom*> atoms;
    for (AstLiteral* cur : clause.getBodyLiterals()) {
        if (auto* arg = dynamic_cast<AstAtom*>(cur)) {
            atoms.push_back(arg);
        }
    }

    // find all binary operations in atoms
    std::vector<const AstArgument*> terms;
    for (const AstAtom* cur : atoms) {
        for (const AstArgument* arg : cur->getArguments()) {
            // only interested in functions
            if (!(dynamic_cast<const AstFunctor*>(arg))) {
                continue;
            }
            // add this one if not yet registered
            if (!any_of(terms, [&](const AstArgument* cur) { return *cur == *arg; })) {
                terms.push_back(arg);
            }
        }
    }

    // substitute them with variables (a real map would compare pointers)
    using substitution_map =
            std::vector<std::pair<std::unique_ptr<AstArgument>, std::unique_ptr<AstVariable>>>;
    substitution_map map;

    int var_counter = 0;
    for (const AstArgument* arg : terms) {
        map.push_back(std::make_pair(std::unique_ptr<AstArgument>(arg->clone()),
                std::make_unique<AstVariable>(" _tmp_" + toString(var_counter++))));
    }

    // apply mapping to replace terms with variables
    struct Update : public AstNodeMapper {
        const substitution_map& map;
        Update(const substitution_map& map) : map(map) {}
        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            // check whether node needs to be replaced
            for (const auto& cur : map) {
                if (*cur.first == *node) {
                    return std::unique_ptr<AstNode>(cur.second->clone());
                }
            }
            // continue recursively
            node->apply(*this);
            return node;
        }
    };

    Update update(map);

    // update atoms
    for (AstAtom* atom : atoms) {
        atom->apply(update);
    }

    // add variable constraints to clause
    for (const auto& cur : map) {
        clause.addToBody(std::make_unique<AstBinaryConstraint>(BinaryConstraintOp::EQ,
                std::unique_ptr<AstArgument>(cur.second->clone()),
                std::unique_ptr<AstArgument>(cur.first->clone())));
    }
}

}  // namespace souffle
