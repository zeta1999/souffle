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
  : unit type
  | unit functor_decl
  | unit relation_decl
  | unit load_head
  | unit store_head
  | unit fact
  | unit rule
  | unit component
  | unit comp_init
  | unit pragma
  | %empty
  ;

/**
 * Identifiers
 */

identifier
  : IDENT
  | identifier DOUBLECOLON IDENT
  ;

/**
 * Types
 */

/* Type declarations */
type
  : NUMBER_TYPE IDENT
  | SYMBOL_TYPE IDENT
  | TYPE IDENT
  | TYPE IDENT EQUALS union_type_list
  | TYPE IDENT EQUALS LBRACKET record_type_list RBRACKET
  ;

/* Record type argument declarations */
record_type_list
  : non_empty_record_type_list
  | %empty
  ;
non_empty_record_type_list
  : IDENT COLON identifier
  | non_empty_record_type_list COMMA IDENT COLON identifier
  ;

/* Union type argument declarations */
union_type_list
  : IDENT
  | union_type_list PIPE IDENT
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
  : atom DOT
  ;

/* Rule */
rule
  : rule_def
  | rule STRICT
  | rule exec_plan
  ;

/* Rule definition */
rule_def
  : head IF body DOT
  ;

/* Rule head */
head
  : atom
  | head COMMA atom
  ;

/* Rule body */
body
  : disjunction
  ;

/* Rule body disjunction */
disjunction
  : conjunction
  | disjunction SEMICOLON conjunction
  ;

/* Rule body conjunction */
conjunction
  : term
  | conjunction COMMA term
  ;

/* Rule execution plan */
exec_plan
  : PLAN exec_plan_list
  ;

/* Rule execution plan list */
exec_plan_list
  : NUMBER COLON LPAREN exec_order_list RPAREN
  | exec_plan_list COMMA NUMBER COLON LPAREN exec_order_list RPAREN
  ;

/* Rule execution order */
exec_order_list
  : non_empty_exec_order_list
  | %empty
  ;
non_empty_exec_order_list
  : NUMBER
  | non_empty_exec_order_list COMMA NUMBER
  ;

/**
 * Terms in Rule Bodies
 */

/* Rule body term */
term
  : literal
  | EXCLAMATION term
  ;

/* Rule body literal */
literal
  : atom
  | constraint
  ;

/* Rule body atom */
atom
  : identifier LPAREN arg_list RPAREN
  ;

/* Rule literal constraints */
constraint
    /* binary infix constraints */
  : arg RELOP arg
  | arg LT arg
  | arg GT arg
  | arg EQUALS arg

    /* binary prefix constraints */
  | TMATCH LPAREN arg COMMA arg RPAREN
  | TCONTAINS LPAREN arg COMMA arg RPAREN

    /* zero-arity constraints */
  | TRUE
  | FALSE
  ;

/* Argument list */
arg_list
  : non_empty_arg_list
  | %empty
  ;
non_empty_arg_list
  : arg
  | arg_list COMMA arg
  ;

/* Atom argument */
arg
  : STRING
  | NUMBER
  | UNDERSCORE
  | DOLLAR
  | IDENT
  | LPAREN arg RPAREN

    /* type-cast */
  | arg AS IDENT

    /* record constructor */
  | NIL
  | identifier LBRACKET arg_list RBRACKET

    /* user-defined functor */
  | identifier LPAREN arg_list RPAREN

    /* -- intrinsic functor -- */
    /* unary functors */
  | MINUS arg %prec NEG
  | BW_NOT arg
  | L_NOT arg
  | ORD LPAREN arg RPAREN
  | STRLEN LPAREN arg RPAREN
  | TONUMBER LPAREN arg RPAREN
  | TOSTRING LPAREN arg RPAREN

    /* binary infix functors */
  | arg PLUS arg
  | arg MINUS arg
  | arg STAR arg
  | arg SLASH arg
  | arg PERCENT arg
  | arg CARET arg
  | arg BW_OR arg
  | arg BW_XOR arg
  | arg BW_AND arg
  | arg L_OR arg
  | arg L_AND arg

    /* binary prefix functors */
  | MIN LPAREN arg COMMA arg RPAREN
  | MAX LPAREN arg COMMA arg RPAREN
  | CAT LPAREN arg COMMA arg RPAREN

    /* ternary functors */
  | SUBSTR LPAREN arg COMMA arg COMMA arg RPAREN

    /* -- aggregators -- */
  | COUNT COLON atom
  | COUNT COLON LBRACE body RBRACE

  | SUM arg COLON RESERVED
  | SUM arg COLON LBRACE body RBRACE

  | MIN arg COLON atom
  | MIN arg COLON LBRACE body RBRACE

  | MAX arg COLON atom
  | MAX arg COLON LBRACE body RBRACE
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
