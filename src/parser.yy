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
/* %define api.location.type {SrcLocation} */

%code requires {}

%locations

%define parse.trace
%define parse.error verbose

/* -- Tokens -- */
%token <std::string> RESERVED    "reserved keyword"
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
%type <std::vector<std::string>>    identifier
%type <AstType *>                   type
%type <AstArgument *>               arg
%type <std::vector<AstArgument *>>  arg_list non_empty_arg_list
%type <AstAtom *>                   atom
%type <std::vector<AstAtom *>>      head
%type <AstConstraint *>             constraint
%type <RuleBody *>                  body conjunction disjunction term
%type <std::vector<AstClause *>>    rule rule_def
%type <AstExecutionPlan *>          exec_plan exec_plan_list
%type <AstExecutionOrder *>         exec_order_list non_empty_exec_order_list
%type <AstRecordType *>             record_type_list non_empty_record_type_list
%type <AstUnionType *>              union_type_list

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
  | unit functor_decl
  | unit relation_decl
  | unit load_head
  | unit store_head
  | unit fact {
        driver.addClause(std::unique_ptr<AstClause>($fact));
    }
  | unit rule {
        for (auto* cur : $rule) {
            driver.addClause(std::unique_ptr<AstClause>(cur));
        }
    }
  | unit component
  | unit comp_init
  | unit pragma
  | %empty
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
        $$->add($IDENT, *$identifier);
    }
  | non_empty_record_type_list[curr_record] COMMA IDENT COLON identifier {
        $$ = $curr_record;
        $$->add($IDENT, *$identifier);
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
  : DECL relation_list LPAREN attributes RPAREN qualifiers
  ;

/* List of relation names to declare */
relation_list
  : IDENT
  | relation_list COMMA IDENT
  ;

/* Attribute definition of a relation */
attributes
  : non_empty_attributes
  | %empty
  ;
non_empty_attributes
  : IDENT COLON identifier
  | non_empty_attributes COMMA IDENT COLON identifier
  ;

/* Relation qualifiers */
qualifiers
  : qualifiers OUTPUT_QUALIFIER
  | qualifiers INPUT_QUALIFIER
  | qualifiers PRINTSIZE_QUALIFIER
  | qualifiers OVERRIDABLE_QUALIFIER
  | qualifiers INLINE_QUALIFIER
  | qualifiers BRIE_QUALIFIER
  | qualifiers BTREE_QUALIFIER
  | qualifiers EQREL_QUALIFIER
  | %empty
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
            rule->setExecutionPlan(std::unique_ptr<AstExecutionPlan>($exec_plan)->clone());
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
        $$->disjunct($conjunction);
    }
  ;

/* Rule body conjunction */
conjunction
  : term {
        $$ = $term;
    }
  | conjunction[curr_conjunction] COMMA term {
        $$ = $curr_conjunction;
        $$->conjunct($term);
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
        $$ = $arg_list;
        $$->setName(*$identifier);
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
        $$ = new AstAtom();
    }
  ;
non_empty_arg_list
  : arg {
        $$ = new AstAtom();
        $$->addArgument(std::unique_ptr<AstArgument>($arg));
    }
  | non_empty_arg_list[curr_arg_list] COMMA arg {
        $$ = $curr_arg_list;
        $$->addArgument(std::unique_ptr<AstArgument>($arg));
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
        $$ = new AstRecordInit();
        for (const auto* arg : $arg_list) {
            $$->add(std::unique_ptr<AstArgument>(arg));
        }
    }

    /* user-defined functor */
  | IDENT LPAREN arg_list RPAREN {
        $$ = new AstUserDefinedFunctor();
        for (const auto* arg : $arg_list) {
            $$->add(std::unique_ptr<AstArgument>(arg));
        }
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
        $$ = new AstIntrinsicFunctor(FunctorOp::TOSTIRNG,
                std::unique_ptr<AstArgument>($nested_arg));
    }

    /* binary infix functors */
  | arg[left] PLUS arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::PLUS,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] MINUS arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::MINUS,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] STAR arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::STAR,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] SLASH arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::SLASH,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] PERCENT arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::PERCENT,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] CARET arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::CARET,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] BW_OR arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::BW_OR,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] BW_XOR arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::BW_XOR,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] BW_AND arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::BW_AND,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] L_OR arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::L_OR,
                std::unique_ptr<AstArgument>($left),
                std::unique_ptr<AstArgument>($right));
    }
  | arg[left] L_AND arg[right] {
        $$ = new AstIntrinsicFunctor(FunctorOp::L_AND,
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
        $$ = new AstAggregator(AstAggregator::count);
        $$->addBodyLiteral(std::unique_ptr<AstLiteral>($atom));
    }
  | COUNT COLON LBRACE body RBRACE {
        $$ = new AstAggregator(AstAggregator::count);

        auto bodies = $body->toClauseBodies();
        if (bodies.size() != 1) {
            std::cerr << "ERROR: currently not supporting non-conjunctive aggregation clauses!";
            exit(1);
        }

        for (auto& cur : bodies[0]->getBodyLiterals()) {
            $$->addBodyLiteral(std::unique_ptr<AstLiteral>(cur));
        }
    }

  | SUM arg[target_expr] COLON atom {
        $$ = new AstAggregator(AstAggregator::sum);
        $$->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));
        $$->addBodyLiteral(std::unique_ptr<AstLiteral>($atom));
    }
  | SUM arg[target_expr] COLON LBRACE body RBRACE {
        $$ = new AstAggregator(AstAggregator::sum);
        $$->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));

        auto bodies = $body->toClauseBodies();
        if (bodies.size() != 1) {
            std::cerr << "ERROR: currently not supporting non-conjunctive aggregation clauses!";
            exit(1);
        }

        for (auto& cur : bodies[0]->getBodyLiterals()) {
            $$->addBodyLiteral(std::unique_ptr<AstLiteral>(cur));
        }
    }

  | MIN arg[target_expr] COLON atom {
        $$ = new AstAggregator(AstAggregator::min);
        $$->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));
        $$->addBodyLiteral(std::unique_ptr<AstLiteral>($atom));
    }
  | MIN arg[target_expr] COLON LBRACE body RBRACE {
        $$ = new AstAggregator(AstAggregator::min);
        $$->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));

        auto bodies = $body->toClauseBodies();
        if (bodies.size() != 1) {
            std::cerr << "ERROR: currently not supporting non-conjunctive aggregation clauses!";
            exit(1);
        }

        for (auto& cur : bodies[0]->getBodyLiterals()) {
            $$->addBodyLiteral(std::unique_ptr<AstLiteral>(cur));
        }
    }

  | MAX arg[target_expr] COLON atom {
        $$ = new AstAggregator(AstAggregator::max);
        $$->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));
        $$->addBodyLiteral(std::unique_ptr<AstLiteral>($atom));
    }
  | MAX arg[target_expr] COLON LBRACE body RBRACE {
        $$ = new AstAggregator(AstAggregator::max);
        $$->setTargetExpression(std::unique_ptr<AstArgument>($target_expr));

        auto bodies = $body->toClauseBodies();
        if (bodies.size() != 1) {
            std::cerr << "ERROR: currently not supporting non-conjunctive aggregation clauses!";
            exit(1);
        }

        for (auto& cur : bodies[0]->getBodyLiterals()) {
            $$->addBodyLiteral(std::unique_ptr<AstLiteral>(cur));
        }
    }
  ;

/**
 * Components
 */

/* Component */
component
  : component_head LBRACE component_body RBRACE
  ;

/* Component head */
component_head
  : COMPONENT comp_type
  | component_head COLON comp_type
  | component_head COMMA comp_type
  ;

/* Component type */
comp_type
  : IDENT type_params
  ;

/* Component type parameters */
type_params
  : LT type_param_list GT
  | %empty
  ;

/* Component type parameter list */
type_param_list
  : IDENT
  | type_param_list COMMA IDENT
  ;

/* Component body */
component_body
  : component_body type
  | component_body relation_decl
  | component_body load_head
  | component_body store_head
  | component_body fact
  | component_body rule
  | component_body comp_override
  | component_body comp_init
  | component_body component
  | %empty
  ;

/* Component initialisation */
comp_init
  : INSTANTIATE IDENT EQUALS comp_type
  ;

/* Component overriding rules of a relation */
comp_override
  : OVERRIDE IDENT
  ;

/**
 * User-Defined Functors
 */

/* Functor declaration */
functor_decl
  : FUNCTOR IDENT LPAREN functor_arg_type_list RPAREN COLON functor_type
  ;

/* Functor argument list types */
functor_arg_type_list
  : non_empty_functor_arg_type_list
  | %empty
  ;
non_empty_functor_arg_type_list
  : functor_type
  | non_empty_functor_arg_type_list COMMA functor_type
  ;

/* Functor type */
functor_type
  : IDENT
  ;

/**
 * Other Directives
 */

/* Pragma directives */
pragma
  : PRAGMA STRING STRING
  | PRAGMA STRING
  ;

/* Load directives */
load_head
  : INPUT_DECL io_directive_list
  ;

/* Store directives */
store_head
  : OUTPUT_DECL io_directive_list
  | PRINTSIZE_DECL io_directive_list
  ;

/* IO directive list */
io_directive_list
  : relation_list
  | relation_list LPAREN key_value_pairs RPAREN
  ;

/* Key-value pairs */
key_value_pairs
  : non_empty_key_value_pairs
  | %empty
  ;
non_empty_key_value_pairs
  : kvp
  | non_empty_key_value_pairs COMMA kvp
  ;
kvp
  : IDENT EQUALS STRING
  | IDENT EQUALS IDENT
  | IDENT EQUALS TRUE
  | IDENT EQUALS FALSE
  ;

%%
