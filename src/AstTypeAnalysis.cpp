/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file AstTypeAnalysis.cpp
 *
 * Implements a collection of type analyses operating on AST constructs.
 *
 ***********************************************************************/

#include "AstTypeAnalysis.h"
#include "AstArgument.h"
#include "AstAttribute.h"
#include "AstClause.h"
#include "AstConstraintAnalysis.h"
#include "AstFunctorDeclaration.h"
#include "AstLiteral.h"
#include "AstNode.h"
#include "AstProgram.h"
#include "AstRelation.h"
#include "AstTranslationUnit.h"
#include "AstType.h"
#include "AstTypeEnvironmentAnalysis.h"
#include "AstUtils.h"
#include "AstVisitor.h"
#include "Constraints.h"
#include "Global.h"
#include "TypeSystem.h"
#include "Util.h"
#include <cassert>
#include <map>
#include <memory>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <utility>

namespace souffle {

namespace {

// -----------------------------------------------------------------------------
//                          Type Deduction Lattice
// -----------------------------------------------------------------------------

/**
 * An implementation of a meet operation between sets of types computing
 * the set of pair-wise greatest common subtypes.
 */
struct sub_type {
    bool operator()(TypeSet& a, const TypeSet& b) const {
        // compute result set
        TypeSet res = getGreatestCommonSubtypes(a, b);

        // check whether a should change
        if (res == a) {
            return false;
        }

        // update a
        a = res;
        return true;
    }
};

/**
 * A factory for computing sets of types covering all potential types.
 */
struct all_type_factory {
    TypeSet operator()() const {
        return TypeSet::getAllTypes();
    }
};

/**
 * The type lattice forming the property space for the Type analysis. The
 * value set is given by sets of types and the meet operator is based on the
 * pair-wise computation of greatest common subtypes. Correspondingly, the
 * bottom element has to be the set of all types.
 */
struct type_lattice : public property_space<TypeSet, sub_type, all_type_factory> {};

/** The definition of the type of variable to be utilized in the type analysis */
using TypeVar = AstConstraintAnalysisVar<type_lattice>;

/** The definition of the type of constraint to be utilized in the type analysis */
using TypeConstraint = std::shared_ptr<Constraint<TypeVar>>;

/**
 * A constraint factory ensuring that all the types associated to the variable
 * a are subtypes of the variable b.
 */
TypeConstraint isSubtypeOf(const TypeVar& a, const TypeVar& b) {
    return sub(a, b, "<:");
}

/**
 * A constraint factory ensuring that all the types associated to the variable
 * a are subtypes of type b.
 */
TypeConstraint isSubtypeOf(const TypeVar& variable, const Type& type) {
    struct C : public Constraint<TypeVar> {
        TypeVar variable;
        const Type& type;

        C(TypeVar variable, const Type& type) : variable(std::move(variable)), type(type) {}

        bool update(Assignment<TypeVar>& assignments) const override {
            // get current value of variable a
            TypeSet& assignment = assignments[variable];

            // remove all types that are not sub-types of b
            if (assignment.isAll()) {
                assignment = TypeSet(type);
                return true;
            }

            TypeSet newAssignment;
            for (const Type& t : assignment) {
                newAssignment.insert(getGreatestCommonSubtypes(t, type));
            }

            // check whether there was a change
            if (assignment == newAssignment) {
                return false;
            }
            assignment = newAssignment;
            return true;
        }

        void print(std::ostream& out) const override {
            out << variable << " <: " << type.getName();
        }
    };

    return std::make_shared<C>(variable, type);
}

/**
 * A constraint factory ensuring that all the types associated to the variable
 * are subtypes of some type in the provided set (values)
 *
 * Values can't be all.
 */
TypeConstraint hasSuperTypeInSet(const TypeVar& var, TypeSet values) {
    struct C : public Constraint<TypeVar> {
        TypeVar var;
        TypeSet values;

        C(TypeVar var, TypeSet values) : var(std::move(var)), values(std::move(values)) {}

        bool update(Assignment<TypeVar>& assigment) const override {
            // get current value of variable a
            TypeSet& assigments = assigment[var];

            // remove all types that are not sub-types of b
            if (assigments.isAll()) {
                assigments = values;
                return true;
            }

            TypeSet newAssigments;
            for (const Type& type : assigments) {
                bool existsSuperTypeInValues = any_of(values.begin(), values.end(),
                        [&type](const Type& value) { return isSubtypeOf(type, value); });
                if (existsSuperTypeInValues) {
                    newAssigments.insert(type);
                }
            }

            // check whether there was a change
            if (newAssigments == assigments) {
                return false;
            }
            assigments = newAssigments;
            return true;
        }

        void print(std::ostream& out) const override {
            out << "∃ t ∈ " << values << ": " << var << " <: t";
        }
    };

    return std::make_shared<C>(var, values);
}

/**
 * Ensure that types of left and right have the same base types.
 */
TypeConstraint subtypesOfTheSameBaseType(const TypeVar& left, const TypeVar& right) {
    struct C : public Constraint<TypeVar> {
        TypeVar left;
        TypeVar right;

        C(TypeVar left, TypeVar right) : left(std::move(left)), right(std::move(right)) {}

        bool update(Assignment<TypeVar>& assigment) const override {
            // get current value of variable a
            TypeSet& assigmentsLeft = assigment[left];
            TypeSet& assigmentsRight = assigment[right];

            // Base types common to left and right variables.
            TypeSet baseTypes;

            // Base types present in left/right variable.
            TypeSet baseTypesLeft;
            TypeSet baseTypesRight;

            // Iterate over possible types extracting base types.
            // Left
            if (!assigmentsLeft.isAll()) {
                for (const auto& type : assigmentsLeft) {
                    // Predefined type is always a base type.
                    if (dynamic_cast<const PrimitiveType*>(&type) != nullptr) {
                        baseTypesLeft.insert(type);
                    } else if (auto* primitive = dynamic_cast<const SubsetType*>(&type)) {
                        baseTypesLeft.insert(primitive->getBaseType());
                    }
                }
            }
            // Right
            if (!assigmentsRight.isAll()) {
                for (const auto& type : assigmentsRight) {
                    // Predefined type is always a base type.
                    if (dynamic_cast<const PrimitiveType*>(&type) != nullptr) {
                        baseTypesRight.insert(type);
                    } else if (auto* primitive = dynamic_cast<const SubsetType*>(&type)) {
                        baseTypesRight.insert(primitive->getBaseType());
                    }
                }
            }

            baseTypes = TypeSet::intersection(baseTypesLeft, baseTypesRight);

            TypeSet resultLeft;
            TypeSet resultRight;

            // Handle all
            if (assigmentsLeft.isAll() && assigmentsRight.isAll()) {
                return false;
            }

            // If left xor right is all, assign base types of the other side as possible values.
            if (assigmentsLeft.isAll()) {
                assigmentsLeft = baseTypesRight;
                return true;
            }
            if (assigmentsRight.isAll()) {
                assigmentsRight = baseTypesLeft;
                return true;
            }

            // Allow types if they are subtypes of any of the common base types.
            for (const Type& type : assigmentsLeft) {
                bool isSubtypeOfCommonBaseType = any_of(baseTypes.begin(), baseTypes.end(),
                        [&type](const Type& baseType) { return isSubtypeOf(type, baseType); });
                if (isSubtypeOfCommonBaseType) {
                    resultLeft.insert(type);
                }
            }

            for (const Type& type : assigmentsRight) {
                bool isSubtypeOfCommonBaseType = any_of(baseTypes.begin(), baseTypes.end(),
                        [&type](const Type& baseType) { return isSubtypeOf(type, baseType); });
                if (isSubtypeOfCommonBaseType) {
                    resultRight.insert(type);
                }
            }

            // check whether there was a change
            if (resultLeft == assigmentsLeft && resultRight == assigmentsRight) {
                return false;
            }
            assigmentsLeft = resultLeft;
            assigmentsRight = resultRight;
            return true;
        }
        //
        void print(std::ostream& out) const override {
            out << "∃ t : (" << left << " <: t)"
                << " ∧ "
                << "(" << right << " <: t)"
                << " where t is a base type";
        }
    };

    return std::make_shared<C>(left, right);
}

/**
 * A constraint factory ensuring that all the types associated to the variable
 * a are subtypes of type b.
 */
TypeConstraint isSupertypeOf(const TypeVar& variable, const Type& type) {
    struct C : public Constraint<TypeVar> {
        TypeVar variable;
        const Type& type;
        mutable bool repeat;

        C(TypeVar variable, const Type& type) : variable(std::move(variable)), type(type), repeat(true) {}

        bool update(Assignment<TypeVar>& assignments) const override {
            // don't continually update super type constraints
            if (!repeat) {
                return false;
            }
            repeat = false;

            // get current value of variable a
            TypeSet& assignment = assignments[variable];

            // remove all types that are not super-types of b
            if (assignment.isAll()) {
                assignment = TypeSet(type);
                return true;
            }

            TypeSet newAssignment;
            for (const Type& t : assignment) {
                newAssignment.insert(getLeastCommonSupertypes(t, type));
            }

            // check whether there was a change
            if (assignment == newAssignment) {
                return false;
            }
            assignment = newAssignment;
            return true;
        }

        void print(std::ostream& out) const override {
            out << variable << " >: " << type.getName();
        }
    };

    return std::make_shared<C>(variable, type);
}

TypeConstraint isSubtypeOfComponent(
        const TypeVar& elementVariable, const TypeVar& recordVariable, size_t index) {
    struct C : public Constraint<TypeVar> {
        TypeVar elementVariable;
        TypeVar recordVariable;
        unsigned index;

        C(TypeVar elementVariable, TypeVar recordVariable, int index)
                : elementVariable(std::move(elementVariable)), recordVariable(std::move(recordVariable)),
                  index(index) {}

        bool update(Assignment<TypeVar>& assignment) const override {
            // get list of types for b
            const TypeSet& recordTypes = assignment[recordVariable];

            // if it is (not yet) constrainted => skip
            if (recordTypes.isAll()) {
                return false;
            }

            // compute new types for element and record
            TypeSet newElementTypes;
            TypeSet newRecordTypes;

            for (const Type& type : recordTypes) {
                // only retain records
                if (!isRecordType(type)) {
                    continue;
                }
                const auto& typeAsRecord = static_cast<const RecordType&>(type);

                // Wrong size => skip.
                if (typeAsRecord.getFields().size() <= index) {
                    continue;
                }

                // Valid record type
                newRecordTypes.insert(typeAsRecord);

                // and its corresponding field for a
                newElementTypes.insert(typeAsRecord.getFields()[index].type);
            }

            // combine with current types assigned to element
            newElementTypes = getGreatestCommonSubtypes(assignment[elementVariable], newElementTypes);

            // update values
            bool changed = false;
            if (newRecordTypes != recordTypes) {
                assignment[recordVariable] = newRecordTypes;
                changed = true;
            }

            if (assignment[elementVariable] != newElementTypes) {
                assignment[elementVariable] = newElementTypes;
                changed = true;
            }

            return changed;
        }

        void print(std::ostream& out) const override {
            out << elementVariable << " <: " << recordVariable << "::" << index;
        }
    };

    return std::make_shared<C>(elementVariable, recordVariable, index);
}
}  // namespace

/* Return a new clause with type-annotated variables */
AstClause* createAnnotatedClause(
        const AstClause* clause, const std::map<const AstArgument*, TypeSet> argumentTypes) {
    // Annotates each variable with its type based on a given type analysis result
    struct TypeAnnotator : public AstNodeMapper {
        const std::map<const AstArgument*, TypeSet>& types;

        TypeAnnotator(const std::map<const AstArgument*, TypeSet>& types) : types(types) {}

        std::unique_ptr<AstNode> operator()(std::unique_ptr<AstNode> node) const override {
            if (auto* var = dynamic_cast<AstVariable*>(node.get())) {
                std::stringstream newVarName;
                newVarName << var->getName() << "&isin;" << types.find(var)->second;
                return std::make_unique<AstVariable>(newVarName.str());
            } else if (auto* var = dynamic_cast<AstUnnamedVariable*>(node.get())) {
                std::stringstream newVarName;
                newVarName << "_"
                           << "&isin;" << types.find(var)->second;
                return std::make_unique<AstVariable>(newVarName.str());
            }
            node->apply(*this);
            return node;
        }
    };

    /* Note:
     * Because the type of each argument is stored in the form [address -> type-set],
     * the type-analysis result does not immediately apply to the clone due to differing
     * addresses.
     * Two ways around this:
     *  (1) Perform the type-analysis again for the cloned clause
     *  (2) Keep track of the addresses of equivalent arguments in the cloned clause
     * Method (2) was chosen to avoid having to recompute the analysis each time.
     */
    AstClause* annotatedClause = clause->clone();

    // Maps x -> y, where x is the address of an argument in the original clause, and y
    // is the address of the equivalent argument in the clone.
    std::map<const AstArgument*, const AstArgument*> memoryMap;

    std::vector<const AstArgument*> originalAddresses;
    visitDepthFirst(*clause, [&](const AstArgument& arg) { originalAddresses.push_back(&arg); });

    std::vector<const AstArgument*> cloneAddresses;
    visitDepthFirst(*annotatedClause, [&](const AstArgument& arg) { cloneAddresses.push_back(&arg); });

    assert(cloneAddresses.size() == originalAddresses.size());

    for (size_t i = 0; i < originalAddresses.size(); i++) {
        memoryMap[originalAddresses[i]] = cloneAddresses[i];
    }

    // Map the types to the clause clone
    std::map<const AstArgument*, TypeSet> cloneArgumentTypes;
    for (auto& pair : argumentTypes) {
        cloneArgumentTypes[memoryMap[pair.first]] = pair.second;
    }

    // Create the type-annotated clause
    TypeAnnotator annotator(cloneArgumentTypes);
    annotatedClause->apply(annotator);
    return annotatedClause;
}

void TypeAnalysis::run(const AstTranslationUnit& translationUnit) {
    // Check if debugging information is being generated and note where logs should be sent
    std::ostream* debugStream = nullptr;
    if (Global::config().has("debug-report") || Global::config().get("show") == "type-analysis") {
        debugStream = &analysisLogs;
    }
    const auto& program = *translationUnit.getProgram();
    auto* typeEnvAnalysis = translationUnit.getAnalysis<TypeEnvironmentAnalysis>();
    for (const AstRelation* rel : translationUnit.getProgram()->getRelations()) {
        for (const AstClause* clause : getClauses(program, *rel)) {
            // Perform the type analysis
            std::map<const AstArgument*, TypeSet> clauseArgumentTypes =
                    analyseTypes(typeEnvAnalysis->getTypeEnvironment(), *clause, translationUnit.getProgram(),
                            debugStream);
            argumentTypes.insert(clauseArgumentTypes.begin(), clauseArgumentTypes.end());

            if (debugStream != nullptr) {
                // Store an annotated clause for printing purposes
                AstClause* annotatedClause = createAnnotatedClause(clause, clauseArgumentTypes);
                annotatedClauses.emplace_back(annotatedClause);
            }
        }
    }
}

void TypeAnalysis::print(std::ostream& os) const {
    os << "-- Analysis logs --" << std::endl;
    os << analysisLogs.str() << std::endl;
    os << "-- Result --" << std::endl;
    for (const auto& cur : annotatedClauses) {
        os << *cur << std::endl;
    }
}

class TypeConstraintsAnalysis : public AstConstraintAnalysis<TypeVar> {
public:
    TypeConstraintsAnalysis(const TypeEnvironment& typeEnv, const AstProgram* program)
            : typeEnv(typeEnv), program(program) {}

private:
    // /** Collect head variables. It will allow to distinguish between head and body variables later. */
    void preProcessing(const AstClause& clause) override {
        head = clause.getHead();
        // visitDepthFirst(
        //         *clause.getHead(), [&](const AstVariable& variable) { headVariables.insert(&variable); });
    }

    // // Remove arg.
    // void solveConstraints(const AstClause& clause) override {
    //     //        std::cerr << clause << std::endl;
    //     assignment = constraints.solve();

    //     for (auto iteratorToHeadVariable : unifyHeadVariables) {
    //         auto name = iteratorToHeadVariable.first;
    //         auto headVar = iteratorToHeadVariable.second;

    //         auto iteratorToBodyVariable = unifyBodyVariables.find(name);

    //         // No entry -> head solution is correct.
    //         if (iteratorToBodyVariable == end(unifyBodyVariables)) {
    //             continue;
    //         }

    //         auto bodyVar = iteratorToBodyVariable->second;
    //         // std::cerr << "name: " << name << " head: " << assignment[headVar] << " body: " <<
    //         // assignment[bodyVar] << std::endl;

    //         // We need to combine head and body results.
    //         auto newAssignment = TypeSet();

    //         for (auto itHead = assignment[headVar].begin(); itHead != assignment[headVar].end(); ++itHead)
    //         {
    //             for (auto itBody = assignment[bodyVar].begin(); itBody != assignment[bodyVar].end();
    //                     ++itBody) {
    //                 if (isSubtypeOf(*itBody, *itHead)) {
    //                     newAssignment.insert(*itBody);
    //                 }
    //             }
    //         }
    //         if (newAssignment.empty()) {
    //             assignment[headVar] = newAssignment;
    //             assignment[bodyVar] = newAssignment;
    //         }
    //         // std::cerr << "New: " << assignment[headVar] << " " << assignment[bodyVar] << std::endl;
    //         addConstraint(isSubtypeOf(bodyVar, headVar));
    //     }

    //     constraints.solve(assignment);
    // }

    // virtual TypeVar getVar(const AstArgument& argument) override {
    //     auto variable = dynamic_cast<const AstVariable*>(&argument);
    //     if (variable == nullptr) {
    //         // no mapping required
    //         return TypeVar(argument);
    //     }

    //     std::map<std::string, TypeVar>::iterator iteratorToVariable;
    //     // The variable is either head variable of body variable.
    //     if (isHeadVariable(variable)) {
    //         iteratorToVariable = unifyHeadVariables.insert({variable->getName(), TypeVar(variable)}).first;
    //     } else {
    //         iteratorToVariable = unifyBodyVariables.insert({variable->getName(), TypeVar(variable)}).first;
    //     }

    //     return iteratorToVariable->second;
    // }

    // TypeVar getVar(const AstArgument* arg) {
    //     return getVar(*arg);
    // }

    const TypeEnvironment& typeEnv;
    const AstProgram* program;
    std::set<const AstAtom*> negated;
    AstAtom* head;

    // std::set<const AstVariable*> headVariables;
    // std::map<std::string, TypeVar> unifyHeadVariables;
    // std::map<std::string, TypeVar> unifyBodyVariables;

    // bool isHeadVariable(const AstVariable* variable) {
    //     return headVariables.find(variable) != headVariables.end();
    // }

    // predicate
    void visitAtom(const AstAtom& atom) override {
        // if (&atom == head) {
        //     return;
        // }

        // get relation
        auto rel = getAtomRelation(&atom, program);
        if (rel == nullptr) {
            return;  // error in input program
        }

        auto atts = rel->getAttributes();
        auto args = atom.getArguments();
        if (atts.size() != args.size()) {
            return;  // error in input program
        }

        // set upper boundary of argument types
        bool notNegated = negated.find(&atom) == negated.end();
        for (size_t i = 0; i < atts.size(); i++) {
            const auto& typeName = atts[i]->getTypeName();
            if (typeEnv.isType(typeName)) {
                if (notNegated) {
                    addConstraint(isSubtypeOf(getVar(args[i]), typeEnv.getType(typeName)));
                } else {
                    addConstraint(isSupertypeOf(getVar(args[i]), typeEnv.getType(typeName)));
                }
            }
        }
    }

    // negations need to be skipped
    void visitNegation(const AstNegation& cur) override {
        // add nested atom to black-list
        negated.insert(cur.getAtom());
    }

    // symbol
    void visitStringConstant(const AstStringConstant& cnst) override {
        addConstraint(isSubtypeOf(getVar(cnst), typeEnv.getSymbolType()));
    }

    // Numeric constant
    void visitNumericConstant(const AstNumericConstant& constant) override {
        TypeSet possibleTypes;

        // Check if the type is given.
        if (constant.getType().has_value()) {
            switch (*constant.getType()) {
                // Insert a type, but only after checking that parsing is possible.
                case AstNumericConstant::Type::Int:
                    if (canBeParsedAsRamSigned(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getNumberType());
                    }
                    break;
                case AstNumericConstant::Type::Uint:
                    if (canBeParsedAsRamUnsigned(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getUnsignedType());
                    }
                    break;
                case AstNumericConstant::Type::Float:
                    if (canBeParsedAsRamFloat(constant.getConstant())) {
                        possibleTypes.insert(typeEnv.getFloatType());
                    }
                    break;
            }
            // Else: all numeric types that can be parsed are valid.
        } else {
            if (canBeParsedAsRamSigned(constant.getConstant())) {
                possibleTypes.insert(typeEnv.getNumberType());
            }

            if (canBeParsedAsRamUnsigned(constant.getConstant())) {
                possibleTypes.insert(typeEnv.getUnsignedType());
            }

            if (canBeParsedAsRamFloat(constant.getConstant())) {
                possibleTypes.insert(typeEnv.getFloatType());
            }
        }

        addConstraint(hasSuperTypeInSet(getVar(constant), possibleTypes));
    }

    // binary constraint
    void visitBinaryConstraint(const AstBinaryConstraint& rel) override {
        auto lhs = getVar(rel.getLHS());
        auto rhs = getVar(rel.getRHS());
        addConstraint(isSubtypeOf(lhs, rhs));
        addConstraint(isSubtypeOf(rhs, lhs));
    }

    // intrinsic functor
    void visitFunctor(const AstFunctor& fun) override {
        auto functorVar = getVar(fun);

        // In polymorphic case
        // We only require arguments to share a base type with a return type.
        // (instead of, for example, requiring them to be of the same type)
        // This approach is related to old type semantics
        // See #1296 and tests/semantic/type_system4
        if (auto intrinsicFunctor = dynamic_cast<const AstIntrinsicFunctor*>(&fun)) {
            if (isOverloadedFunctor(intrinsicFunctor->getFunction())) {
                for (auto* argument : intrinsicFunctor->getArguments()) {
                    auto argumentVar = getVar(argument);
                    addConstraint(subtypesOfTheSameBaseType(argumentVar, functorVar));
                }

                return;
            }
        }

        try {
            fun.getReturnType();
        } catch (std::bad_optional_access& e) {
            return;
        }

        // add a constraint for the return type of the functor
        switch (fun.getReturnType()) {
            case TypeAttribute::Signed:
                addConstraint(isSubtypeOf(functorVar, typeEnv.getNumberType()));
                break;
            case TypeAttribute::Float:
                addConstraint(isSubtypeOf(functorVar, typeEnv.getFloatType()));
                break;
            case TypeAttribute::Unsigned:
                addConstraint(isSubtypeOf(functorVar, typeEnv.getUnsignedType()));
                break;
            case TypeAttribute::Symbol:
                addConstraint(isSubtypeOf(functorVar, typeEnv.getSymbolType()));
                break;
            default:
                assert(false && "Invalid return type");
        }

        // Special case
        if (auto intrFun = dynamic_cast<const AstIntrinsicFunctor*>(&fun)) {
            if (intrFun->getFunction() == FunctorOp::ORD) {
                return;
            }
        }
        size_t i = 0;
        for (auto arg : fun.getArguments()) {
            auto argumentVar = getVar(arg);
            switch (fun.getArgType(i)) {
                case TypeAttribute::Signed:
                    addConstraint(isSubtypeOf(argumentVar, typeEnv.getNumberType()));
                    break;
                case TypeAttribute::Float:
                    addConstraint(isSubtypeOf(argumentVar, typeEnv.getFloatType()));
                    break;
                case TypeAttribute::Unsigned:
                    addConstraint(isSubtypeOf(argumentVar, typeEnv.getUnsignedType()));
                    break;
                case TypeAttribute::Symbol:
                    addConstraint(isSubtypeOf(argumentVar, typeEnv.getSymbolType()));
                    break;
                default:
                    assert(false && "Invalid argument type");
            }
            ++i;
        }
    }
    // counter
    void visitCounter(const AstCounter& counter) override {
        // this value must be a number value
        addConstraint(isSubtypeOf(getVar(counter), typeEnv.getNumberType()));
    }

    // components of records
    void visitRecordInit(const AstRecordInit& init) override {
        // link element types with sub-values
        auto rec = getVar(init);
        int i = 0;

        for (const AstArgument* value : init.getArguments()) {
            addConstraint(isSubtypeOfComponent(getVar(value), rec, i++));
        }
    }

    // visit aggregates
    void visitAggregator(const AstAggregator& agg) override {
        if (agg.getOperator() == AggregateOp::COUNT) {
            addConstraint(isSubtypeOf(getVar(agg), typeEnv.getNumberType()));
        } else if (agg.getOperator() == AggregateOp::MEAN) {
            addConstraint(isSubtypeOf(getVar(agg), typeEnv.getFloatType()));
        } else {
            addConstraint(hasSuperTypeInSet(getVar(agg), typeEnv.getNumericTypes()));
        }

        // If there is a target expression - it should be of the same type as the aggregator.
        if (auto expr = agg.getTargetExpression()) {
            addConstraint(isSubtypeOf(getVar(expr), getVar(agg)));
            addConstraint(isSubtypeOf(getVar(agg), getVar(expr)));
        }
    }
};

/**
 * Generic type analysis framework for clauses
 */
std::map<const AstArgument*, TypeSet> TypeAnalysis::analyseTypes(const TypeEnvironment& typeEnv,
        const AstClause& clause, const AstProgram* program, std::ostream* logs) {
    return TypeConstraintsAnalysis(typeEnv, program).analyse(clause, logs);
}

}  // end of namespace souffle
