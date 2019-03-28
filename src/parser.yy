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
%token SEMICOLON                 ";"
%token DOT                       "."
%token EQUALS                    "="
%token STAR                      "*"
%token AT                        "@"
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
/* %left SEMICOLON */
/* %left COMMA */
/* %left EXCLAMATION */
/* %left L_OR */
/* %left L_AND */
/* %left BW_OR */
/* %left BW_XOR */
/* %left BW_AND */
%left PLUS MINUS
%left STAR SLASH PERCENT
%right CARET
/* %precedence BW_NOT L_NOT */
/* %precedence NEG --- WHERE'S THIS? re: caret */

%%
%start program;

/* Program */
program
    : unit
    ;

/* Top-level statement */
unit
    : unit rule
    | %empty
    ;

/**
 * Identifiers
 */

identifier
    : IDENT
    | identifier COLON COLON IDENT
    ;

/**
 * Datalog Rule Structure
 */

/* Rule */
rule
    : rule_def
    ;

/* Rule definition */
rule_def
    : head IF body DOT
    ;

/* Rule head */
head
    : atom
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

/**
 * Terms in Rule Bodies
 */

/* Rule body term */
term
    : literal
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
    | identifier
    | identifier LBRACKET arg_list RBRACKET

    /* -- user-defined functor -- */
    | identifier LPAREN arg_list RPAREN

    /* -- intrinsic functor -- */
    /* unary functors */
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

    /* binary prefix functors */
    | MIN LPAREN arg COMMA arg RPAREN
    | MAX LPAREN arg COMMA arg RPAREN
    | CAT LPAREN arg COMMA arg RPAREN

    /* ternary functors */
    | SUBSTR LPAREN arg COMMA arg COMMA arg RPAREN
    ;

%%
