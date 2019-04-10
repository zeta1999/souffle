/*
 * Souffle - A Datalog Compiler
 * Copyright (c) 2013, 2015, Oracle and/or its affiliates. All rights reserved
 * Licensed under the Universal Permissive License v 1.0 as shown at:
 * - https://opensource.org/licenses/UPL
 * - <souffle root>/licenses/SOUFFLE-UPL.txt
 */

/************************************************************************
 *
 * @file parser.yy
 *
 * @brief Parser for Datalog
 *
 ***********************************************************************/
%skeleton "lalr1.cc"
%require "3.0.2"

%defines
%define parser_class_name {parser}
%define api.token.constructor
%define api.value.type variant
%define parse.assert
%define api.location.type {SrcLocation}

%locations

%define parse.trace
%define parse.error verbose

/* -- Dependencies -- */
%code requires {
    #include "AstArgument.h"
    #include "AstClause.h"
    #include "AstComponent.h"
    #include "AstFunctorDeclaration.h"
    #include "AstIO.h"
    #include "AstNode.h"
    #include "AstPragma.h"
    #include "AstProgram.h"
    #include "AstTypes.h"
    #include "BinaryConstraintOps.h"
    #include "FunctorOps.h"
    #include "AstParserUtils.h"

    using namespace souffle;

    namespace souffle {
        class ParserDriver;
    }

    using yyscan_t = void*;

    #define YY_NULLPTR nullptr

    /* Macro to update locations as parsing proceeds */
    #define YYLLOC_DEFAULT(Cur, Rhs, N)                         \
    do {                                                        \
        if (N) {                                                \
            (Cur).start         = YYRHSLOC(Rhs, 1).start;       \
            (Cur).end           = YYRHSLOC(Rhs, N).end;         \
            (Cur).filename      = YYRHSLOC(Rhs, N).filename;    \
        } else {                                                \
            (Cur).start         = YYRHSLOC(Rhs, 0).end;         \
            (Cur).end           = YYRHSLOC(Rhs, 0).end;         \
            (Cur).filename      = YYRHSLOC(Rhs, 0).filename;    \
        }                                                       \
    } while (0)
}

%code {
    #include "ParserDriver.h"
}

%param { ParserDriver &driver }
%param { yyscan_t yyscanner }

/* -- Tokens -- */
%token END 0                     "end of file"
%token <std::string> STRING      "symbol"
%token <std::string> IDENT       "identifier"
%token <AstDomain> NUMBER        "number"
%token <std::string> RELOP       "relational operator"
%token PRAGMA                    "pragma directive"
%token OUTPUT_QUALIFIER          "relation qualifier output"
%token INPUT_QUALIFIER           "relation qualifier input"
%token PRINTSIZE_QUALIFIER       "relation qualifier printsize"
%token BRIE_QUALIFIER            "BRIE datastructure qualifier"
%token BTREE_QUALIFIER           "BTREE datastructure qualifier"
%token EQREL_QUALIFIER           "equivalence relation qualifier"
%token OVERRIDABLE_QUALIFIER     "relation qualifier overidable"
%token INLINE_QUALIFIER          "relation qualifier inline"
%token TMATCH                    "match predicate"
%token TCONTAINS                 "checks whether substring is contained in a string"
%token CAT                       "concatenation of two strings"
%token ORD                       "ordinal number of a string"
%token STRLEN                    "length of a string"
%token SUBSTR                    "sub-string of a string"
%token MIN                       "min aggregator"
%token MAX                       "max aggregator"
%token COUNT                     "count aggregator"
%token SUM                       "sum aggregator"
%token TRUE                      "true literal constraint"
%token FALSE                     "false literal constraint"
%token STRICT                    "strict marker"
%token PLAN                      "plan keyword"
%token IF                        ":-"
%token DECL                      "relation declaration"
%token FUNCTOR                   "functor declaration"
%token INPUT_DECL                "input directives declaration"
%token OUTPUT_DECL               "output directives declaration"
%token PRINTSIZE_DECL            "printsize directives declaration"
%token OVERRIDE                  "override rules of super-component"
%token TYPE                      "type declaration"
%token COMPONENT                 "component declaration"
%token INSTANTIATE               "component instantiation"
%token NUMBER_TYPE               "numeric type declaration"
%token SYMBOL_TYPE               "symbolic type declaration"
%token TONUMBER                  "convert string to number"
%token TOSTRING                  "convert number to string"
%token AS                        "type cast"
%token AT                        "@"
%token NIL                       "nil reference"
%token PIPE                      "|"
%token LBRACKET                  "["
%token RBRACKET                  "]"
%token UNDERSCORE                "_"
%token DOLLAR                    "$"
%token PLUS                      "+"
%token MINUS                     "-"
%token EXCLAMATION               "!"
%token LPAREN                    "("
%token RPAREN                    ")"
%token COMMA                     ","
%token COLON                     ":"
%token DOUBLECOLON               "::"
%token SEMICOLON                 ";"
%token DOT                       "."
%token EQUALS                    "="
%token STAR                      "*"
%token SLASH                     "/"
%token CARET                     "^"
%token PERCENT                   "%"
%token LBRACE                    "{"
%token RBRACE                    "}"
%token LT                        "<"
%token GT                        ">"
%token BW_AND                    "band"
%token BW_OR                     "bor"
%token BW_XOR                    "bxor"
%token BW_NOT                    "bnot"
%token L_AND                     "land"
%token L_OR                      "lor"
%token L_NOT                     "lnot"

/* -- Non-Terminal Types -- */
%type <uint32_t>                    qualifiers
%type <std::vector<std::string>>    identifier
%type <AstType *>                   type
%type <AstFunctorDeclaration *>     functor_decl
%type <AstArgument *>               arg
%type <std::vector<AstArgument *>>  arg_list non_empty_arg_list
%type <AstAtom *>                   atom
%type <std::vector<AstAtom *>>      head
%type <AstComponent *>              component component_body component_head
%type <AstComponentType *>          comp_type
%type <AstComponentInit *>          comp_init
%type <std::vector<AstTypeIdentifier>>  type_params type_param_list
%type <std::string>                 functor_arg_type_list non_empty_functor_arg_type_list functor_type
%type <AstConstraint *>             constraint
%type <RuleBody *>                  body conjunction disjunction term
%type <std::vector<AstAttribute *>> attributes non_empty_attributes
%type <std::vector<AstRelation *>>  relation_decl relation_list
%type <std::vector<AstClause *>>    rule rule_def
%type <AstExecutionPlan *>          exec_plan exec_plan_list
%type <AstExecutionOrder *>         exec_order_list non_empty_exec_order_list
%type <AstPragma *>                 pragma
%type <std::vector<AstLoad *>>      load_head
%type <std::vector<AstStore *>>     store_head
%type <std::vector<AstIO *>>        io_directive_list
%type <AstRecordType *>             record_type_list non_empty_record_type_list
%type <AstUnionType *>              union_type_list
%type <std::string>                 kvp_value
%type <AstIO *>                     key_value_pairs non_empty_key_value_pairs
%type <std::vector<std::vector<std::string>>> io_relation_list

/* TODO: think about these ones */
%type <AstClause *>                 fact

/* Operator precedence */
/* TODO: ORDERING??? */
%left L_OR
%left L_AND
%left BW_OR
%left BW_XOR
%left BW_AND
%left PLUS MINUS
%left STAR SLASH PERCENT
%right CARET
%precedence BW_NOT L_NOT
%precedence NEG
%precedence AS

/* TODO: CHANGE AS (TYPECAST) SYNTAX?? */

%%

%start program;

/* Program */
program
  : unit
  ;

/* Top-level statement */
unit
  : unit type {
        driver.addType(std::unique_ptr<AstType>($type));
    }
  | unit functor_decl {
        driver.addFunctorDeclaration(std::unique_ptr<AstFunctorDeclaration>($functor_decl));
    }
  | unit relation_decl {
        for (auto* cur : $relation_decl) {
            driver.addRelation(std::unique_ptr<AstRelation>(cur));
        }
    }
  | unit load_head {
        for (auto* cur : $load_head) {
            driver.addLoad(std::unique_ptr<AstLoad>(cur));
        }
    }
  | unit store_head {
        for (auto* cur : $store_head) {
            driver.addStore(std::unique_ptr<AstStore>(cur));
        }
    }
  | unit fact {
        driver.addClause(std::unique_ptr<AstClause>($fact));
    }
  | unit rule {
        for (auto* cur : $rule) {
            driver.addClause(std::unique_ptr<AstClause>(cur));
        }
    }
  | unit component {
        driver.addComponent(std::unique_ptr<AstComponent>($component));
    }
  | unit comp_init {
        driver.addInstantiation(std::unique_ptr<AstComponentInit>($comp_init));
    }
  | unit pragma {
        driver.addPragma(std::unique_ptr<AstPragma>($pragma));
    }
  | %empty {
    }
  ;

/**
 * Identifiers
 */

identifier
  : IDENT {
        $$.push_back($IDENT);
    }
  | identifier[curr_identifier] DOUBLECOLON IDENT {
        $$ = $curr_identifier;
        $$.push_back($IDENT);
    }
  ;

/**
 * Types
 */

/* Type declarations */
type
  : NUMBER_TYPE IDENT {
        $$ = new AstPrimitiveType($IDENT, true);
    }
  | SYMBOL_TYPE IDENT {
        $$ = new AstPrimitiveType($IDENT, false);
    }
  | TYPE IDENT {
        $$ = new AstPrimitiveType($IDENT);
    }
  | TYPE IDENT EQUALS union_type_list {
        $$ = $union_type_list;
        $$->setName($IDENT);
    }
  | TYPE IDENT EQUALS LBRACKET record_type_list RBRACKET {
        $$ = $record_type_list;
        $$->setName($IDENT);
    }
  ;

/* Record type argument declarations */
record_type_list
  : non_empty_record_type_list {
        $$ = $non_empty_record_type_list;
    }
  | %empty {
        $$ = new AstRecordType();
    }
  ;
non_empty_record_type_list
  : IDENT COLON identifier {
        $$ = new AstRecordType();
        $$->add($IDENT, $identifier);
    }
  | non_empty_record_type_list[curr_record] COMMA IDENT COLON identifier {
        $$ = $curr_record;
        $$->add($IDENT, $identifier);
    }
  ;

/* Union type argument declarations */
union_type_list
  : IDENT {
        $$ = new AstUnionType();
        $$->add($IDENT);
    }
  | union_type_list[curr_union] PIPE IDENT {
        $$ = $curr_union;
        $$->add($IDENT);
    }
  ;

/**
 * Relations
 */

/* Relation declaration */
relation_decl
  : DECL relation_list LPAREN attributes RPAREN qualifiers {
        for (auto* rel : $relation_list) {
            for (auto* attr : $attributes) {
                rel->addAttribute(std::unique_ptr<AstAttribute>(attr->clone()));
            }
        }

        for (auto* attr : $attributes) {
            delete attr;
        }

        $$ = $relation_list;
    }
  ;

/* List of relation names to declare */
relation_list
  : IDENT {
        auto* rel = new AstRelation();
        rel->setName($IDENT);

        $$.push_back(rel);
    }
  | relation_list[curr_list] COMMA IDENT {
        auto* rel = new AstRelation();
        rel->setName($IDENT);

        $$ = $curr_list;
        $$.push_back(rel);
    }
  ;

/* Attribute definition of a relation */
attributes
  : non_empty_attributes {
        $$ = $non_empty_attributes;
    }
  | %empty {
        $$ = std::vector<AstAttribute*>();
    }
  ;
non_empty_attributes
  : IDENT COLON identifier {
        $$.push_back(new AstAttribute($IDENT, $identifier));
    }
  | non_empty_attributes[curr_list] COMMA IDENT COLON identifier {
        $$ = $curr_list;
        $$.push_back(new AstAttribute($IDENT, $identifier));
    }
  ;

/* Relation qualifiers */
qualifiers
  : qualifiers OUTPUT_QUALIFIER {
        if($1 & OUTPUT_RELATION)
            driver.error(@2, "output qualifier already set");
        $$ = $1 | OUTPUT_RELATION;
    }
  | qualifiers INPUT_QUALIFIER {
        if($1 & INPUT_RELATION)
            driver.error(@2, "input qualifier already set");
        $$ = $1 | INPUT_RELATION;
    }
  | qualifiers PRINTSIZE_QUALIFIER {
        if($1 & PRINTSIZE_RELATION)
            driver.error(@2, "printsize qualifier already set");
        $$ = $1 | PRINTSIZE_RELATION;
    }
  | qualifiers OVERRIDABLE_QUALIFIER {
        if($1 & OVERRIDABLE_RELATION)
            driver.error(@2, "overridable qualifier already set");
        $$ = $1 | OVERRIDABLE_RELATION;
    }
  | qualifiers INLINE_QUALIFIER {
        if($1 & INLINE_RELATION)
            driver.error(@2, "inline qualifier already set");
        $$ = $1 | INLINE_RELATION;
    }
  | qualifiers BRIE_QUALIFIER {
        if($1 & (BRIE_RELATION|BTREE_RELATION|EQREL_RELATION))
            driver.error(@2, "btree/brie/eqrel qualifier already set");
        $$ = $1 | BRIE_RELATION;
    }
  | qualifiers BTREE_QUALIFIER {
        if($1 & (BRIE_RELATION|BTREE_RELATION|EQREL_RELATION))
            driver.error(@2, "btree/brie/eqrel qualifier already set");
        $$ = $1 | BTREE_RELATION;
    }
  | qualifiers EQREL_QUALIFIER {
        if($1 & (BRIE_RELATION|BTREE_RELATION|EQREL_RELATION))
            driver.error(@2, "btree/brie/eqrel qualifier already set");
        $$ = $1 | EQREL_RELATION;
    }
  | %empty {
        $$ = 0;
    }
  ;

/**
 * Datalog Rule Structure
 */

/* Fact */
fact
  : atom DOT {
        $$ = new AstClause();
        $$->setHead(std::unique_ptr<AstAtom>($atom));
    }
  ;

/* Rule */
rule
  : rule_def {
        $$ = $rule_def;
    }
  | rule[nested_rule] STRICT {
        $$ = $nested_rule;
        for (auto* rule : $$) {
            rule->setFixedExecutionPlan();
        }
    }
  | rule[nested_rule] exec_plan {
        $$ = $nested_rule;
        for (auto* rule : $$) {
            rule->setExecutionPlan(std::unique_ptr<AstExecutionPlan>($exec_plan->clone()));
        }
        delete $exec_plan;
    }
  ;

/* Rule definition */
rule_def
  : head IF body DOT {
        auto heads = $head;
        auto bodies = $body->toClauseBodies();

        bool generated = heads.size() != 1 || bodies.size() != 1;

        for (const auto* head : heads) {
            for (const auto* body : bodies) {
                AstClause* cur = body->clone();
                cur->setHead(std::unique_ptr<AstAtom>(head->clone()));
                cur->setGenerated(generated);
                $$.push_back(cur);
            }
        }

        for (auto* head : heads) {
            delete head;
        }

        for (auto* body : bodies) {
            delete body;
        }
    }
  ;

/* Rule head */
head
  : atom {
        $$.push_back($atom);
    }
  | head[curr_head] COMMA atom {
        $$ = $curr_head;
        $$.push_back($atom);
    }
  ;

/* Rule body */
body
  : disjunction {
        $$ = $disjunction;
    }
  ;

/* Rule body disjunction */
disjunction
  : conjunction {
        $$ = $conjunction;
    }
  | disjunction[curr_disjunction] SEMICOLON conjunction {
        $$ = $curr_disjunction;
        $$->disjunct(std::move(*$conjunction));
    }
  ;

/* Rule body conjunction */
conjunction
  : term {
        $$ = $term;
    }
  | conjunction[curr_conjunction] COMMA term {
        $$ = $curr_conjunction;
        $$->conjunct(std::move(*$term));
    }
  ;

/* Rule execution plan */
exec_plan
  : PLAN exec_plan_list {
        $$ = $exec_plan_list;
    }
  ;

/* Rule execution plan list */
exec_plan_list
  : NUMBER COLON LPAREN exec_order_list RPAREN {
        $$ = new AstExecutionPlan();
        $$->setOrderFor($NUMBER, std::unique_ptr<AstExecutionOrder>($exec_order_list));
    }
  | exec_plan_list[curr_list] COMMA NUMBER COLON LPAREN exec_order_list RPAREN {
        $$ = $curr_list;
        $$->setOrderFor($NUMBER, std::unique_ptr<AstExecutionOrder>($exec_order_list));
    }
  ;

/* Rule execution order */
exec_order_list
  : non_empty_exec_order_list {
        $$ = $non_empty_exec_order_list;
    }
  | %empty {
        $$ = new AstExecutionOrder();
    }
  ;
non_empty_exec_order_list
  : NUMBER {
        $$ = new AstExecutionOrder();
        $$->appendAtomIndex($NUMBER);
    }
  | non_empty_exec_order_list[curr_list] COMMA NUMBER {
        $$ = $curr_list;
        $$->appendAtomIndex($NUMBER);
    }
  ;

/**
 * Terms in Rule Bodies
 */

/* Rule body term */
term
  : atom {
        $$ = new RuleBody(RuleBody::atom($atom));
    }
  | constraint {
        $$ = new RuleBody(RuleBody::constraint($constraint));
    }
  | EXCLAMATION term[nested_term] {
        $$ = $nested_term;
        $$->negate();
    }
  | LPAREN disjunction RPAREN {
        $$ = $disjunction;
    }
  ;

/* Rule body atom */
atom
  : identifier LPAREN arg_list RPAREN {
        $$ = new AstAtom();
        for (auto* arg : $arg_list) {
            $$->addArgument(std::unique_ptr<AstArgument>(arg));
        }
        $$->setName($identifier);
    }
  ;

/* Rule literal constraints */
constraint
    /* binary infix constraints */
  : arg[left] RELOP arg[right] {
        $$ = new AstBinaryConstraint($RELOP,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] LT arg[right] {
        $$ = new AstBinaryConstraint(BinaryConstraintOp::LT,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] GT arg[right] {
        $$ = new AstBinaryConstraint(BinaryConstraintOp::GT,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] EQUALS arg[right] {
        $$ = new AstBinaryConstraint(BinaryConstraintOp::EQ,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }

    /* binary prefix constraints */
  | TMATCH LPAREN arg[left] COMMA arg[right] RPAREN {
        $$ = new AstBinaryConstraint(BinaryConstraintOp::MATCH,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | TCONTAINS LPAREN arg[left] COMMA arg[right] RPAREN {
        $$ = new AstBinaryConstraint(BinaryConstraintOp::CONTAINS,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }

    /* zero-arity constraints */
  | TRUE {
        $$ = new AstBooleanConstraint(true);
    }
  | FALSE {
        $$ = new AstBooleanConstraint(false);
    }
  ;

/* Argument list */
arg_list
  : non_empty_arg_list {
        $$ = $non_empty_arg_list;
    }
  | %empty {
        $$ = std::vector<AstArgument*>();
    }
  ;
non_empty_arg_list
  : arg {
        $$.push_back($arg);
    }
  | non_empty_arg_list[curr_arg_list] COMMA arg {
        $$ = $curr_arg_list;
        $$.push_back($arg);
    }
  ;

/* Atom argument */
arg
  : STRING {
        $$ = new AstStringConstant(driver.getSymbolTable(), $STRING);
    }
  | NUMBER {
        $$ = new AstNumberConstant($NUMBER);
    }
  | UNDERSCORE {
        $$ = new AstUnnamedVariable();
    }
  | DOLLAR {
        $$ = new AstCounter();
    }
  | IDENT {
        $$ = new AstVariable($IDENT);
    }
  | LPAREN arg[nested_arg] RPAREN {
        $$ = $nested_arg;
    }

    /* type-cast */
  | arg[nested_arg] AS IDENT {
        $$ = new AstTypeCast(std::unique_ptr<AstArgument>($nested_arg), $IDENT);
    }

    /* record constructor */
  | NIL {
        $$ = new AstNullConstant();
    }
  | identifier LBRACKET arg_list RBRACKET {
        auto record = new AstRecordInit();
        for (auto* arg : $arg_list) {
            record->add(std::unique_ptr<AstArgument>(arg));
        }
        $$ = record;
    }

    /* user-defined functor */
  | AT IDENT LPAREN arg_list RPAREN {
        auto functor = new AstUserDefinedFunctor();
        for (auto* arg : $arg_list) {
            functor->add(std::unique_ptr<AstArgument>(arg));
        }
        $$ = functor;
    }

    /* -- intrinsic functor -- */
    /* unary functors */
  | MINUS arg[nested_arg] %prec NEG {
        if (const AstNumberConstant* original = dynamic_cast<const AstNumberConstant*>($nested_arg)) {
            $$ = new AstNumberConstant(-1 * original->getIndex());
        } else {
            $$ = new AstIntrinsicFunctor(FunctorOp::NEG,
                std::unique_ptr<AstArgument>($nested_arg));
        }
    }
  | BW_NOT arg[nested_arg] {
        $$ = new AstIntrinsicFunctor(FunctorOp::BNOT,
                std::unique_ptr<AstArgument>($nested_arg));
    }
  | L_NOT arg [nested_arg] {
        $$ = new AstIntrinsicFunctor(FunctorOp::LNOT,
                std::unique_ptr<AstArgument>($nested_arg));
    }
  | ORD LPAREN arg[nested_arg] RPAREN {
        $$ = new AstIntrinsicFunctor(FunctorOp::ORD,
                std::unique_ptr<AstArgument>($nested_arg));
    }
  | STRLEN LPAREN arg[nested_arg] RPAREN {
        $$ = new AstIntrinsicFunctor(FunctorOp::STRLEN,
                std::unique_ptr<AstArgument>($nested_arg));
    }
  | TONUMBER LPAREN arg[nested_arg] RPAREN {
        $$ = new AstIntrinsicFunctor(FunctorOp::TONUMBER,
                std::unique_ptr<AstArgument>($nested_arg));
    }
  | TOSTRING LPAREN arg[nested_arg] RPAREN {
        $$ = new AstIntrinsicFunctor(FunctorOp::TOSTRING,
                std::unique_ptr<AstArgument>($nested_arg));
    }

    /* binary infix functors */
  | arg[left] PLUS arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::ADD,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] MINUS arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::SUB,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] STAR arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::MUL,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] SLASH arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::DIV,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] PERCENT arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::MOD,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] CARET arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::EXP,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] BW_OR arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::BOR,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] BW_XOR arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::BXOR,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] BW_AND arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::BAND,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] L_OR arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::LOR,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] L_AND arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::LAND,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }

    /* binary prefix functors */
  | MIN LPAREN arg[left] COMMA arg[right] RPAREN {
        $$ = new AstIntrinsicFunctor(FunctorOp::MIN,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | MAX LPAREN arg[left] COMMA arg[right] RPAREN {
        $$ = new AstIntrinsicFunctor(FunctorOp::MAX,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | CAT LPAREN arg[left] COMMA arg[right] RPAREN {
        $$ = new AstIntrinsicFunctor(FunctorOp::CAT,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }

    /* ternary functors */
  | SUBSTR LPAREN arg[first] COMMA arg[second] COMMA arg[third] RPAREN {
        $$ = new AstIntrinsicFunctor(FunctorOp::SUBSTR,
                std::unique_ptr<AstArgument>($first),
                std::unique_ptr<AstArgument>($second),
                std::unique_ptr<AstArgument>($third));
    }

    /* -- aggregators -- */
  | COUNT COLON atom {
        auto aggr = new AstAggregator(AstAggregator::count);
        aggr->addBodyLiteral(std::unique_ptr<AstLiteral>($atom));
        $$ = aggr;
    }
  | COUNT COLON LBRACE body RBRACE {
        auto aggr = new AstAggregator(AstAggregator::count);

        auto bodies = $body->toClauseBodies();
        if (bodies.size() != 1) {
            std::cerr << "ERROR: currently not supporting non-conjunctive aggregation clauses!";
            exit(1);
        }

        for (auto& cur : bodies[0]->getBodyLiterals()) {
            aggr->addBodyLiteral(std::unique_ptr<AstLiteral>(cur));
        }

        $$ = aggr;
    }

  | SUM arg[target_expr] COLON atom {
        auto aggr = new AstAggregator(AstAggregator::sum);
        aggr->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));
        aggr->addBodyLiteral(std::unique_ptr<AstLiteral>($atom));
        $$ = aggr;
    }
  | SUM arg[target_expr] COLON LBRACE body RBRACE {
        auto aggr = new AstAggregator(AstAggregator::sum);
        aggr->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));

        auto bodies = $body->toClauseBodies();
        if (bodies.size() != 1) {
            std::cerr << "ERROR: currently not supporting non-conjunctive aggregation clauses!";
            exit(1);
        }

        for (auto& cur : bodies[0]->getBodyLiterals()) {
            aggr->addBodyLiteral(std::unique_ptr<AstLiteral>(cur));
        }

        $$ = aggr;
    }

  | MIN arg[target_expr] COLON atom {
        auto aggr = new AstAggregator(AstAggregator::min);
        aggr->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));
        aggr->addBodyLiteral(std::unique_ptr<AstLiteral>($atom));
        $$ = aggr;
    }
  | MIN arg[target_expr] COLON LBRACE body RBRACE {
        auto aggr = new AstAggregator(AstAggregator::min);
        aggr->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));

        auto bodies = $body->toClauseBodies();
        if (bodies.size() != 1) {
            std::cerr << "ERROR: currently not supporting non-conjunctive aggregation clauses!";
            exit(1);
        }

        for (auto& cur : bodies[0]->getBodyLiterals()) {
            aggr->addBodyLiteral(std::unique_ptr<AstLiteral>(cur));
        }

        $$ = aggr;
    }

  | MAX arg[target_expr] COLON atom {
        auto aggr = new AstAggregator(AstAggregator::max);
        aggr->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));
        aggr->addBodyLiteral(std::unique_ptr<AstLiteral>($atom));
        $$ = aggr;
    }
  | MAX arg[target_expr] COLON LBRACE body RBRACE {
        auto aggr = new AstAggregator(AstAggregator::max);
        aggr->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));

        auto bodies = $body->toClauseBodies();
        if (bodies.size() != 1) {
            std::cerr << "ERROR: currently not supporting non-conjunctive aggregation clauses!";
            exit(1);
        }

        for (auto& cur : bodies[0]->getBodyLiterals()) {
            aggr->addBodyLiteral(std::unique_ptr<AstLiteral>(cur));
        }

        $$ = aggr;
    }
  ;

/**
 * Components
 */

/* Component */
component
  : component_head LBRACE component_body RBRACE {
        $$ = $component_body;
        auto* type = $component_head->getComponentType()->clone();
        $$->setComponentType(std::unique_ptr<AstComponentType>(type));
        $$->copyBaseComponents($component_head);
        delete $component_head;
    }
  ;

/* Component head */
component_head
  : COMPONENT comp_type {
        $$ = new AstComponent();
        $$->setComponentType(std::unique_ptr<AstComponentType>($comp_type));
    }
  | component_head[comp] COLON comp_type {
        $$ = $comp;
        $$->addBaseComponent(std::unique_ptr<AstComponentType>($comp_type));
    }
  | component_head[comp] COMMA comp_type {
        $$ = $comp;
        $$->addBaseComponent(std::unique_ptr<AstComponentType>($comp_type));
    }
  ;

/* Component type */
comp_type
  : IDENT type_params {
        $$ = new AstComponentType($IDENT, $type_params);
    }
  ;

/* Component type parameters */
type_params
  : LT type_param_list GT {
        $$ = $type_param_list;
    }
  | %empty {
        $$ = std::vector<AstTypeIdentifier>();
    }
  ;

/* Component type parameter list */
type_param_list
  : IDENT {
        $$.push_back($IDENT);
    }
  | type_param_list[curr_list] COMMA IDENT {
        $$ = $curr_list;
        $$.push_back($IDENT);
    }
  ;

/* Component body */
component_body
  : component_body[comp] type {
        $$ = $comp;
        $$->addType(std::unique_ptr<AstType>($type));
    }
  | component_body[comp] relation_decl {
        $$ = $comp;
        for (auto* rel : $relation_decl) {
            $$->addRelation(std::unique_ptr<AstRelation>(rel));
        }
    }
  | component_body[comp] load_head {
        $$ = $comp;
        for (auto* io : $load_head) {
            $$->addLoad(std::unique_ptr<AstLoad>(io));
        }
    }
  | component_body[comp] store_head {
        $$ = $comp;
        for (auto* io : $store_head) {
            $$->addStore(std::unique_ptr<AstStore>(io));
        }
    }
  | component_body[comp] fact {
        $$ = $comp;
        $$->addClause(std::unique_ptr<AstClause>($fact));
    }
  | component_body[comp] rule {
        $$ = $comp;
        for (auto* rule : $rule) {
            $$->addClause(std::unique_ptr<AstClause>(rule));
        }
    }
  | component_body[comp] OVERRIDE IDENT {
        $$ = $comp;
        $$->addOverride($IDENT);
    }
  | component_body[comp] comp_init {
        $$ = $comp;
        $$->addInstantiation(std::unique_ptr<AstComponentInit>($comp_init));
    }
  | component_body[comp] component {
        $$ = $comp;
        $$->addComponent(std::unique_ptr<AstComponent>($component));
    }
  | %empty {
        $$ = new AstComponent();
    }
  ;

/* Component initialisation */
comp_init
  : INSTANTIATE IDENT EQUALS comp_type {
        $$ = new AstComponentInit();
        $$->setInstanceName($IDENT);
        $$->setComponentType(std::unique_ptr<AstComponentType>($comp_type));
    }
  ;

/**
 * User-Defined Functors
 */

/* Functor declaration */
functor_decl
  : FUNCTOR IDENT LPAREN functor_arg_type_list RPAREN COLON functor_type {
        auto typesig = $functor_arg_type_list + $functor_type;
        $$ = new AstFunctorDeclaration($IDENT, typesig);
    }
  ;

/* Functor argument list types */
functor_arg_type_list
  : non_empty_functor_arg_type_list {
        $$ = $non_empty_functor_arg_type_list;
    }
  | %empty {
        $$ = "";
    }
  ;
non_empty_functor_arg_type_list
  : functor_type {
        $$ = $functor_type;
    }
  | non_empty_functor_arg_type_list[curr_list] COMMA functor_type {
        $$ = $curr_list + $functor_type;
    }
  ;

/* Functor type */
functor_type
  : IDENT {
        if ($IDENT == "number") {
            $$ = "N";
        } else if ($IDENT == "symbol") {
            $$ = "S";
        } else {
            driver.error(@IDENT, "number or symbol identifier expected");
        }
    }
  ;

/**
 * Other Directives
 */

/* Pragma directives */
pragma
  : PRAGMA STRING[key] STRING[value] {
        $$ = new AstPragma($key, $value);
    }
  | PRAGMA STRING[option] {
        $$ = new AstPragma($option, "");
    }
  ;

/* Load directives */
load_head
  : INPUT_DECL io_directive_list {
        for (const auto* io : $io_directive_list) {
            $$.push_back(new AstLoad(*io));
            delete io;
        }
    }
  ;

/* Store directives */
store_head
  : OUTPUT_DECL io_directive_list {
        for (const auto* io : $io_directive_list) {
            $$.push_back(new AstStore(*io));
            delete io;
        }
    }
  | PRINTSIZE_DECL io_directive_list {
        for (const auto* io : $io_directive_list) {
            $$.push_back(new AstPrintSize(*io));
            delete io;
        }
    }
  ;

/* IO directive list */
io_directive_list
  : io_relation_list {
        for (const auto& rel : $io_relation_list) {
            auto* io = new AstIO();
            io->setName(rel);
            $$.push_back(io);
        }
    }
  | io_relation_list LPAREN key_value_pairs RPAREN {
        for (const auto& rel : $io_relation_list) {
            auto* io = $key_value_pairs->clone();
            io->setName(rel);
        }

        delete $key_value_pairs;
    }
  ;

/* IO relation list */
io_relation_list
  : identifier {
        $$.push_back($identifier);
    }
  | io_relation_list[curr_list] COMMA identifier {
        $$ = $curr_list;
        $$.push_back($identifier);
    }
  ;

/* Key-value pairs */
key_value_pairs
  : non_empty_key_value_pairs {
        $$ = $non_empty_key_value_pairs;
    }
  | %empty {
        $$ = new AstIO();
    }
  ;
non_empty_key_value_pairs
  : IDENT EQUALS kvp_value {
        $$ = new AstIO();
        $$->addKVP($IDENT, $kvp_value);
    }
  | non_empty_key_value_pairs[curr_io] COMMA IDENT EQUALS kvp_value {
        $$ = $curr_io;
        $$->addKVP($IDENT, $kvp_value);
    }
  ;
kvp_value
  : STRING {
        $$ = $STRING;
    }
  | IDENT {
        $$ = $IDENT;
    }
  | TRUE {
        $$ = "true";
    }
  | FALSE {
        $$ = "false";
    }
  ;

%%

void yy::parser::error(const location_type &l, const std::string &m) {
    driver.error(l, m);
}
