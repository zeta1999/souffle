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
// Defined in version 3.2. This would solve a lot of the verbose `move`s.
// %define api.value.automove

%locations

%define parse.trace
%define parse.error verbose

/* -- Dependencies -- */
%code requires {
    #include "AggregateOp.h"
    #include "AstArgument.h"
    #include "AstClause.h"
    #include "AstComponent.h"
    #include "AstFunctorDeclaration.h"
    #include "AstIO.h"
    #include "AstNode.h"
    #include "AstParserUtils.h"
    #include "AstPragma.h"
    #include "AstProgram.h"
    #include "BinaryConstraintOps.h"
    #include "FunctorOps.h"
    #include "RamTypes.h"
    #include "Util.h"

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
            (Cur).filenames     = YYRHSLOC(Rhs, N).filenames;   \
        } else {                                                \
            (Cur).start         = YYRHSLOC(Rhs, 0).end;         \
            (Cur).end           = YYRHSLOC(Rhs, 0).end;         \
            (Cur).filenames     = YYRHSLOC(Rhs, 0).filenames;   \
        }                                                       \
    } while (0)
}

%code {
    #include "ParserDriver.h"
    using std::move;
}

%param { ParserDriver &driver }
%param { yyscan_t yyscanner }

/* -- Tokens -- */
%token END 0                     "end of file"
%token <std::string> STRING      "symbol"
%token <std::string> IDENT       "identifier"
%token <std::string> NUMBER      "number"
%token <std::string> FLOAT       "float"
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
%token CAT                       "concatenation of strings"
%token ORD                       "ordinal number of a string"
%token STRLEN                    "length of a string"
%token SUBSTR                    "sub-string of a string"
%token MEAN                      "mean aggregator"
%token MIN                       "min aggregator"
%token MAX                       "max aggregator"
%token COUNT                     "count aggregator"
%token SUM                       "sum aggregator"
%token TRUE                      "true literal constraint"
%token FALSE                     "false literal constraint"
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
%token TONUMBER                  "convert string to (signed) number"
%token TOSTRING                  "convert number to string"
%token ITOU                      "convert int to unsigned"
%token ITOF                      "convert int to float"
%token UTOI                      "convert unsigned to int"
%token UTOF                      "convert unsigned to float"
%token FTOI                      "convert float to int"
%token FTOU                      "convert float to unsigned"
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
%token SUBTYPE                   "<:"
%token LT                        "<"
%token GT                        ">"
%token LE                        "<="
%token GE                        ">="
%token NE                        "!="
%token BW_AND                    "band"
%token BW_OR                     "bor"
%token BW_XOR                    "bxor"
%token BW_SHIFT_L                "bshl"
%token BW_SHIFT_R                "bshr"
%token BW_SHIFT_R_UNSIGNED       "bshru"
%token BW_NOT                    "bnot"
%token L_AND                     "land"
%token L_OR                      "lor"
%token L_NOT                     "lnot"

/* -- Non-Terminal Types -- */
%type <RuleBody>                            aggregate_body
%type <AggregateOp>                         aggregate_func
%type <Own<AstArgument>>                    arg
%type <VecOwn<AstArgument>>                 arg_list
%type <Own<AstAtom>>                        atom
%type <VecOwn<AstAttribute>>                attributes_list
%type <RuleBody>                            body
%type <Own<AstComponentType>>               comp_type
%type <Own<AstComponentInit>>               comp_init
%type <Own<AstComponent>>                   component
%type <Own<AstComponent>>                   component_body
%type <Own<AstComponent>>                   component_head
%type <RuleBody>                            conjunction
%type <Own<AstConstraint>>                  constraint
%type <RuleBody>                            disjunction
%type <Own<AstExecutionOrder>>              exec_order
%type <Own<AstExecutionPlan>>               exec_plan
%type <Own<AstExecutionPlan>>               exec_plan_list
%type <Own<AstClause>>                      fact
%type <std::vector<TypeAttribute>>          functor_arg_type_list
%type <FunctorOp>                           functor_built_in
%type <Own<AstFunctorDeclaration>>          functor_decl
%type <VecOwn<AstAtom>>                     head
%type <std::vector<std::string>>            identifier
%type <VecOwn<AstIO>>                       io_directive_list
%type <VecOwn<AstIO>>                       io_head
%type <AstIoType>                           io_head_decl
%type <VecOwn<AstIO>>                       io_relation_list
%type <std::string>                         kvp_value
%type <VecOwn<AstArgument>>                 non_empty_arg_list
%type <VecOwn<AstAttribute>>                non_empty_attributes
%type <AstExecutionOrder::ExecOrder>        non_empty_exec_order_list
%type <std::vector<TypeAttribute>>          non_empty_functor_arg_type_list
%type <std::vector<std::pair
            <std::string, std::string>>>    non_empty_key_value_pairs
%type <VecOwn<AstRelation>>                 non_empty_relation_list
%type <Own<AstPragma>>                      pragma
%type <TypeAttribute>                       predefined_type
%type <VecOwn<AstAttribute>>                record_type_list
%type <VecOwn<AstRelation>>                 relation_decl
%type <std::set<RelationTag>>               relation_tags
%type <VecOwn<AstClause>>                   rule
%type <VecOwn<AstClause>>                   rule_def
%type <RuleBody>                            term
%type <Own<AstType>>                        type
%type <std::vector<AstQualifiedName>>       type_params
%type <std::vector<AstQualifiedName>>       type_param_list
%type <std::vector<AstQualifiedName>>       union_type_list

/* -- Operator precedence -- */
%left L_OR
%left L_AND
%left BW_OR
%left BW_XOR
%left BW_AND
%left BW_SHIFT_L BW_SHIFT_R BW_SHIFT_R_UNSIGNED
%left PLUS MINUS
%left STAR SLASH PERCENT
%precedence NEG BW_NOT L_NOT
%right CARET

/* -- Grammar -- */
%%

%start program;

/* Program */
program
  : unit
  ;

/* Top-level statement */
unit
  : %empty              { }
  | unit io_head        { for (auto&& cur : $io_head) driver.addIO(move(cur)); }
  | unit rule           { for (auto&& cur : $rule   ) driver.addClause(move(cur)); }
  | unit fact           { driver.addClause            (move($fact)); }
  | unit component      { driver.addComponent         (move($component)); }
  | unit comp_init      { driver.addInstantiation     (move($comp_init)); }
  | unit pragma         { driver.addPragma            (move($pragma)); }
  | unit type           { driver.addType              (move($type)); }
  | unit functor_decl   { driver.addFunctorDeclaration(move($functor_decl)); }
  | unit relation_decl  {
        for (auto&& rel : $relation_decl) {
            driver.addDeprecatedIoModifiers(*rel);
            driver.addRelation(move(rel));
        }
    }
  ;

/**
 * Identifiers
 */

identifier
  : IDENT                 { $$.push_back(move($IDENT)); }
    /* TODO (azreika): in next version: DOT -> DOUBLECOLON */
  | identifier DOT IDENT  { $1.push_back(move($IDENT)); $$ = move($1); }
  ;

/**
 * Types
 */

/* Type declarations */
type
  : TYPE IDENT SUBTYPE  predefined_type   { $$ = mk<AstSubsetType>(move($2), move($4), @$); }
  | TYPE IDENT EQUALS   union_type_list   { $$ = mk<AstUnionType >(move($2), move($4), @$); }
  | TYPE IDENT EQUALS   record_type_list  { $$ = mk<AstRecordType>(move($2), move($4), @$); }
    /* deprecated subset type forms */
  | NUMBER_TYPE IDENT { $$ = driver.mkDeprecatedSubType(move($IDENT), TypeAttribute::Signed, @$); }
  | SYMBOL_TYPE IDENT { $$ = driver.mkDeprecatedSubType(move($IDENT), TypeAttribute::Symbol, @$); }
  | TYPE        IDENT { $$ = driver.mkDeprecatedSubType(move($IDENT), TypeAttribute::Symbol, @$); }
  ;

/* Union type argument declarations */
union_type_list
  :                       identifier { $$.push_back(move($identifier)); }
  | union_type_list PIPE  identifier { $1.push_back(move($identifier)); $$ = move($1); }
  ;

/**
 * Relations
 */

/* Relation declaration */
relation_decl
  : DECL non_empty_relation_list attributes_list relation_tags {
        for (auto&& rel : $non_empty_relation_list) {
            for (auto tag : $relation_tags) {
                if (isRelationQualifierTag(tag)) {
                    rel->addQualifier(getRelationQualifierFromTag(tag));
                } else if (isRelationRepresentationTag(tag)) {
                    rel->setRepresentation(getRelationRepresentationFromTag(tag));
                } else {
                    assert(false && "unhandled tag");
                }
            }

            rel->setAttributes(clone($attributes_list));
        }

        $$ = move($non_empty_relation_list);
    }
  ;

/* List of relation names to declare */
non_empty_relation_list
  :                               IDENT { $$.push_back(mk<AstRelation>(move($1), @1)); }
  | non_empty_relation_list COMMA IDENT { $1.push_back(mk<AstRelation>(move($3), @3)); $$ = move($1); }
  ;

/* Attribute definition of a relation */
/* specific wrapper to ensure the err msg says "expected ',' or ')'" */
record_type_list
  : LBRACKET RBRACKET                       { }
  | LBRACKET non_empty_attributes RBRACKET  { $$ = move($2); }
  ;
attributes_list
  : LPAREN RPAREN                       { }
  | LPAREN non_empty_attributes RPAREN  { $$ = move($2); }
  ;
non_empty_attributes
  :                            IDENT COLON identifier
    { $$.push_back(mk<AstAttribute>(move($IDENT), move($identifier), @identifier)); }
  | non_empty_attributes COMMA IDENT COLON identifier
    { $1.push_back(mk<AstAttribute>(move($IDENT), move($identifier), @identifier)); $$ = move($1); }
  ;

/* Relation tags */
relation_tags
  : %empty { }
  | relation_tags OUTPUT_QUALIFIER {
        driver.warning(@2, "Deprecated output qualifier used");
        if ($1.find(RelationTag::OUTPUT) != $1.end())
            driver.error(@2, "output qualifier already set");
        $1.insert(RelationTag::OUTPUT);
        $$ = move($1);
    }
  | relation_tags INPUT_QUALIFIER {
        driver.warning(@2, "Deprecated input qualifier was used");
        if ($1.find(RelationTag::INPUT) != $1.end())
            driver.error(@2, "input qualifier already set");
        $1.insert(RelationTag::INPUT);
        $$ = move($1);
    }
  | relation_tags PRINTSIZE_QUALIFIER {
        driver.warning(@2, "Deprecated printsize qualifier was used");
        if ($1.find(RelationTag::PRINTSIZE) != $1.end())
            driver.error(@2, "printsize qualifier already set");
        $1.insert(RelationTag::PRINTSIZE);
        $$ = move($1);
    }
  | relation_tags OVERRIDABLE_QUALIFIER {
        if ($1.find(RelationTag::OVERRIDABLE) != $1.end())
            driver.error(@2, "overridable qualifier already set");
        $1.insert(RelationTag::OVERRIDABLE);
        $$ = move($1);
    }
  | relation_tags INLINE_QUALIFIER {
        if ($1.find(RelationTag::INLINE) != $1.end())
            driver.error(@2, "inline qualifier already set");
        $1.insert(RelationTag::INLINE);
        $$ = move($1);
    }
  | relation_tags BRIE_QUALIFIER {
        if ($1.find(RelationTag::BRIE) != $1.end() ||
            $1.find(RelationTag::BTREE) != $1.end() ||
            $1.find(RelationTag::EQREL) != $1.end())
                driver.error(@2, "btree/brie/eqrel qualifier already set");
        $1.insert(RelationTag::BRIE);
        $$ = move($1);
    }
  | relation_tags BTREE_QUALIFIER {
        if ($1.find(RelationTag::BRIE) != $1.end() ||
            $1.find(RelationTag::BTREE) != $1.end() ||
            $1.find(RelationTag::EQREL) != $1.end())
                driver.error(@2, "btree/brie/eqrel qualifier already set");
        $1.insert(RelationTag::BTREE);
        $$ = move($1);
    }
  | relation_tags EQREL_QUALIFIER {
        if ($1.find(RelationTag::BRIE) != $1.end() ||
            $1.find(RelationTag::BTREE) != $1.end() ||
            $1.find(RelationTag::EQREL) != $1.end())
                driver.error(@2, "btree/brie/eqrel qualifier already set");
        $1.insert(RelationTag::EQREL);
        $$ = move($1);
    }
  ;

/**
 * Datalog Rule Structure
 */

/* Fact */
fact : atom DOT { $$ = mk<AstClause>(move($atom), VecOwn<AstLiteral> {}, nullptr, @$); };

/* Rule */
rule
  : rule_def {
        $$ = move($rule_def);
    }
  | rule_def exec_plan {
        $$ = move($rule_def);
        for (auto&& rule : $$) {
            rule->setExecutionPlan(clone($exec_plan));
        }
    }
  ;

/* Rule definition */
rule_def
  : head[heads] IF body DOT {
        auto bodies = $body.toClauseBodies();

        for (auto&& head : $heads) {
            for (auto&& body : bodies) {
                auto cur = clone(body);
                cur->setHead(clone(head));
                cur->setSrcLoc(@$);
                $$.push_back(move(cur));
            }
        }
    }
  ;

/* Rule head */
head
  :            atom { $$.push_back(move($atom)); }
  | head COMMA atom { $1.push_back(move($atom)); $$ = move($1); }
  ;

/* Rule body */
body : disjunction { $$ = move($disjunction); };

disjunction
  :                       conjunction { $$ = move($conjunction); }
  | disjunction SEMICOLON conjunction { $1.disjunct(move($conjunction)); $$ = move($1); }
  ;

conjunction
  :                   term { $$ = move($term); }
  | conjunction COMMA term { $1.conjunct(move($term)); $$ = move($1); }
  ;

/* Rule execution plan */
exec_plan : PLAN exec_plan_list { $$ = move($exec_plan_list); };

/* Rule execution plan list */
exec_plan_list
  : NUMBER COLON exec_order {
        $$ = mk<AstExecutionPlan>();
        $$->setOrderFor(RamSignedFromString($NUMBER), move($exec_order));
    }
  | exec_plan_list[curr_list] COMMA NUMBER COLON exec_order {
        $$ = move($curr_list);
        $$->setOrderFor(RamSignedFromString($NUMBER), move($exec_order));
    }
  ;

/* Rule execution order */
exec_order
  : LPAREN RPAREN                           { $$ = mk<AstExecutionOrder>(AstExecutionOrder::ExecOrder(), @$); }
  | LPAREN non_empty_exec_order_list RPAREN { $$ = mk<AstExecutionOrder>(move($2), @$); }
  ;
non_empty_exec_order_list
  :                                 NUMBER { $$.push_back(RamUnsignedFromString($NUMBER)); }
  | non_empty_exec_order_list COMMA NUMBER { $1.push_back(RamUnsignedFromString($NUMBER)); $$ = move($1); }
  ;

/**
 * Terms in Rule Bodies
 */

/* Rule body term */
term
  : atom                      { $$ = RuleBody::atom(move($atom)); }
  | constraint                { $$ = RuleBody::constraint(move($constraint)); }
  | LPAREN disjunction RPAREN { $$ = move($disjunction); }
  | EXCLAMATION term          { $$ = $2.negated(); }
  ;

/* Rule body atom */
atom : identifier LPAREN arg_list RPAREN { $$ = mk<AstAtom>(move($identifier), move($arg_list), @$); };

/* Rule literal constraints */
constraint
    /* binary infix constraints */
  : arg LT      arg { $$ = mk<AstBinaryConstraint>(BinaryConstraintOp::LT, move($1), move($3), @$); }
  | arg GT      arg { $$ = mk<AstBinaryConstraint>(BinaryConstraintOp::GT, move($1), move($3), @$); }
  | arg LE      arg { $$ = mk<AstBinaryConstraint>(BinaryConstraintOp::LE, move($1), move($3), @$); }
  | arg GE      arg { $$ = mk<AstBinaryConstraint>(BinaryConstraintOp::GE, move($1), move($3), @$); }
  | arg EQUALS  arg { $$ = mk<AstBinaryConstraint>(BinaryConstraintOp::EQ, move($1), move($3), @$); }
  | arg NE      arg { $$ = mk<AstBinaryConstraint>(BinaryConstraintOp::NE, move($1), move($3), @$); }

    /* binary prefix constraints */
  | TMATCH    LPAREN arg[a0] COMMA arg[a1] RPAREN
    { $$ = mk<AstBinaryConstraint>(BinaryConstraintOp::MATCH   , move($a0), move($a1), @$); }
  | TCONTAINS LPAREN arg[a0] COMMA arg[a1] RPAREN
    { $$ = mk<AstBinaryConstraint>(BinaryConstraintOp::CONTAINS, move($a0), move($a1), @$); }

    /* zero-arity constraints */
  | TRUE  { $$ = mk<AstBooleanConstraint>(true , @$); }
  | FALSE { $$ = mk<AstBooleanConstraint>(false, @$); }
  ;

/* Argument list */
arg_list : %empty { } | non_empty_arg_list { $$ = move($1); } ;
non_empty_arg_list
  :                           arg { $$.push_back(move($arg)); }
  | non_empty_arg_list COMMA  arg { $1.push_back(move($arg)); $$ = move($1); }
  ;

/* Atom argument */
arg
  : STRING      { $$ = mk<AstStringConstant >(move($STRING), @$); }
  | FLOAT       { $$ = mk<AstNumericConstant>(move($FLOAT), AstNumericConstant::Type::Float, @$); }
  | NUMBER      { $$ = mk<AstNumericConstant>(move($NUMBER), @$); }
  | UNDERSCORE  { $$ = mk<AstUnnamedVariable>(@$); }
  | DOLLAR      { $$ = mk<AstCounter        >(@$); }
  | IDENT       { $$ = mk<AstVariable       >(move($IDENT), @$); }
  | NIL         { $$ = mk<AstNilConstant    >(@$); }

  /* TODO (azreika): in next version: prepend records with identifiers */
  | LBRACKET arg_list RBRACKET { $$ = mk<AstRecordInit>(move($arg_list), @$); }

  |     LPAREN arg                  RPAREN { $$ = move($2); }
  | AS  LPAREN arg COMMA identifier RPAREN { $$ = mk<AstTypeCast>(move($3), move($identifier), @$); }

  | AT IDENT         LPAREN arg_list RPAREN { $$ = mk<AstUserDefinedFunctor>(move($IDENT), move($arg_list), @$); }
  | functor_built_in LPAREN arg_list RPAREN { $$ = mk<AstIntrinsicFunctor>($functor_built_in, move($arg_list), @$); }

    /* some aggregates have the same name as functors */
  | aggregate_func LPAREN arg[first] COMMA non_empty_arg_list[rest] RPAREN {
        auto arg_list = move($rest);
        arg_list.insert(arg_list.begin(), move($first));

        auto agg_2_func = [](AggregateOp op) -> std::optional<FunctorOp> {
          switch (op) {
            case AggregateOp::COUNT : return {};
            case AggregateOp::MAX   : return FunctorOp::MAX;
            case AggregateOp::MEAN  : return {};
            case AggregateOp::MIN   : return FunctorOp::MIN;
            case AggregateOp::SUM   : return {};
            default                 :
              assert(false && "overloads found?");
              abort();
          }
        };

        if (auto func_op = agg_2_func($aggregate_func)) {
          $$ = mk<AstIntrinsicFunctor>(*func_op, move(arg_list), @$);
        } else {
          driver.error(@$, "aggregate operation has no functor equivalent");
          $$ = mk<AstUnnamedVariable>(@$);
        }
    }

    /* -- intrinsic functor -- */
    /* unary functors */
  | MINUS arg[nested_arg] %prec NEG {
        // If we have a constant, that is not already negated we create a mk<constant>.
        const auto* asNumeric = dynamic_cast<const AstNumericConstant*>(&*$nested_arg);
        if (asNumeric && !isPrefix("-", asNumeric->getConstant())) {
            $$ = mk<AstNumericConstant>("-" + asNumeric->getConstant(), asNumeric->getType(), @nested_arg);
        } else { // Otherwise, create a functor.
            $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::NEG, move($nested_arg));
        }
    }
  | BW_NOT  arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::BNOT, move($2)); }
  | L_NOT   arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::LNOT, move($2)); }

    /* binary infix functors */
  | arg PLUS                arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::ADD     , move($1), move($3)); }
  | arg MINUS               arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::SUB     , move($1), move($3)); }
  | arg STAR                arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::MUL     , move($1), move($3)); }
  | arg SLASH               arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::DIV     , move($1), move($3)); }
  | arg PERCENT             arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::MOD     , move($1), move($3)); }
  | arg CARET               arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::EXP     , move($1), move($3)); }
  | arg BW_OR               arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::BOR     , move($1), move($3)); }
  | arg BW_XOR              arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::BXOR    , move($1), move($3)); }
  | arg BW_AND              arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::BAND    , move($1), move($3)); }
  | arg BW_SHIFT_L          arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::BSHIFT_L, move($1), move($3)); }
  | arg BW_SHIFT_R          arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::BSHIFT_R, move($1), move($3)); }
  | arg BW_SHIFT_R_UNSIGNED arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::BSHIFT_R_UNSIGNED , move($1), move($3)); }
  | arg L_OR                arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::LOR     , move($1), move($3)); }
  | arg L_AND               arg { $$ = mk<AstIntrinsicFunctor>(@$, FunctorOp::LAND    , move($1), move($3)); }

    /* -- aggregators -- */
  | aggregate_func arg_list COLON aggregate_body {
        auto bodies = $aggregate_body.toClauseBodies();
        if (bodies.size() != 1) {
            driver.error("ERROR: disjunctions in aggregation clauses are currently not supported");
        }

        // TODO: move this to a semantic check when aggs are extended to multiple exprs
        auto given    = $arg_list.size();
        auto required = aggregateArity($aggregate_func);
        if (given < required.first || required.second < given) {
            driver.error("ERROR: incorrect expression arity for given aggregate mode");
        }

        auto expr = $arg_list.empty() ? nullptr : std::move($arg_list[0]);
        auto body = (bodies.size() == 1) ? clone(bodies[0]->getBodyLiterals()) : VecOwn<AstLiteral> {};
        $$ = mk<AstAggregator>($aggregate_func, move(expr), move(body), @$);
    }
  ;

functor_built_in
  : CAT       { $$ = FunctorOp::CAT;      }
  | FTOI      { $$ = FunctorOp::FTOI;     }
  | FTOU      { $$ = FunctorOp::FTOU;     }
  | ITOF      { $$ = FunctorOp::ITOF;     }
  | ITOU      { $$ = FunctorOp::ITOU;     }
  | ORD       { $$ = FunctorOp::ORD;      }
  | STRLEN    { $$ = FunctorOp::STRLEN;   }
  | SUBSTR    { $$ = FunctorOp::SUBSTR;   }
  | TONUMBER  { $$ = FunctorOp::TONUMBER; }
  | TOSTRING  { $$ = FunctorOp::TOSTRING; }
  | UTOF      { $$ = FunctorOp::UTOF;     }
  | UTOI      { $$ = FunctorOp::UTOI;     }
  ;

aggregate_func
  : COUNT { $$ = AggregateOp::COUNT;  }
  | MAX   { $$ = AggregateOp::MAX;    }
  | MEAN  { $$ = AggregateOp::MEAN;   }
  | MIN   { $$ = AggregateOp::MIN;    }
  | SUM   { $$ = AggregateOp::SUM;    }
  ;

aggregate_body
  : LBRACE body RBRACE  { $$ = move($body); }
  | atom                { $$ = RuleBody::atom(move($atom)); }
  ;

/**
 * Components
 */

/* Component */
component
  : component_head LBRACE component_body RBRACE {
        $$ = move($component_body);
        $$->setComponentType(clone($component_head->getComponentType()));
        $$->copyBaseComponents(*$component_head);
        $$->setSrcLoc(@$);
    }
  ;

/* Component head */
component_head
  : COMPONENT             comp_type { $$ = mk<AstComponent>(); $$->setComponentType(move($comp_type)); }
  | component_head COLON  comp_type { $$ = move($1);           $$->addBaseComponent(move($comp_type)); }
  | component_head COMMA  comp_type { $$ = move($1);           $$->addBaseComponent(move($comp_type)); }
  ;

/* Component type */
comp_type : IDENT type_params { $$ = mk<AstComponentType>(move($IDENT), move($type_params), @$); };

/* Component type parameters */
type_params
  : %empty                { }
  | LT type_param_list GT { $$ = move($type_param_list); }
  ;

/* Component type parameter list */
type_param_list
  :                       IDENT { $$.push_back(move($IDENT)); }
  | type_param_list COMMA IDENT { $1.push_back(move($IDENT)); $$ = move($1); }
  ;

/* Component body */
component_body
  : %empty                        { $$ = mk<AstComponent>(); }
  | component_body io_head        { $$ = move($1); for (auto&& x : $2) $$->addIO    (move(x)); }
  | component_body rule           { $$ = move($1); for (auto&& x : $2) $$->addClause(move(x)); }
  | component_body fact           { $$ = move($1); $$->addClause       (move($2)); }
  | component_body OVERRIDE IDENT { $$ = move($1); $$->addOverride     (move($3)); }
  | component_body comp_init      { $$ = move($1); $$->addInstantiation(move($2)); }
  | component_body component      { $$ = move($1); $$->addComponent    (move($2)); }
  | component_body type           { $$ = move($1); $$->addType         (move($2)); }
  | component_body relation_decl  {
        $$ = move($1);
        for (auto&& rel : $relation_decl) {
            driver.addDeprecatedIoModifiers(*rel);
            $$->addRelation(move(rel));
        }
    }
  ;

/* Component initialisation */
comp_init
  : INSTANTIATE IDENT EQUALS comp_type { $$ = mk<AstComponentInit>(move($IDENT), move($comp_type), @$); }
  ;

/**
 * User-Defined Functors
 */

/* Functor declaration */
functor_decl
  : FUNCTOR IDENT LPAREN functor_arg_type_list[args] RPAREN COLON predefined_type
    { $$ = mk<AstFunctorDeclaration>(move($IDENT), move($args), $predefined_type, @$); }
  ;

/* Functor argument list type */
functor_arg_type_list : %empty { } | non_empty_functor_arg_type_list { $$ = move($1); };
non_empty_functor_arg_type_list
  :                                        predefined_type { $$.push_back($predefined_type); }
  | non_empty_functor_arg_type_list COMMA  predefined_type { $1.push_back($predefined_type); $$ = move($1); }
  ;

/* Predefined type */
predefined_type
  : IDENT {
        if ($IDENT == "number") {
            $$ = TypeAttribute::Signed;
        } else if ($IDENT == "symbol") {
            $$ = TypeAttribute::Symbol;
        } else if ($IDENT == "float") {
            $$ = TypeAttribute::Float;
        } else if ($IDENT == "unsigned") {
            $$ = TypeAttribute::Unsigned;
        } else {
            driver.error(@IDENT, "[number | symbol | float | unsigned] identifier expected");
        }
    }
  ;

/**
 * Other Directives
 */

/* Pragma directives */
pragma
  : PRAGMA STRING[key   ] STRING[value] { $$ = mk<AstPragma>(move($key   ), move($value), @$); }
  | PRAGMA STRING[option]               { $$ = mk<AstPragma>(move($option), ""          , @$); }
  ;

/* io directives */
io_head
  : io_head_decl io_directive_list {
        for (auto&& io : $io_directive_list) {
            io->setType($io_head_decl);
            $$.push_back(move(io));
        }
    }
  ;

io_head_decl
  : INPUT_DECL      { $$ = AstIoType::input;      }
  | OUTPUT_DECL     { $$ = AstIoType::output;     }
  | PRINTSIZE_DECL  { $$ = AstIoType::printsize;  }
  ;

/* IO directive list */
io_directive_list
  : io_relation_list                { $$ = move($io_relation_list); }
  | io_relation_list LPAREN RPAREN  { $$ = move($io_relation_list); }
  | io_relation_list LPAREN non_empty_key_value_pairs RPAREN {
        $$ = move($io_relation_list);
        for (auto&& io : $$) {
            for (const auto& kvp : $non_empty_key_value_pairs) {
                io->addDirective(kvp.first, kvp.second);
            }
        }
    }
  ;

/* IO relation list */
/* use a dummy `AstIoType` for now. `io_head` will replace it */
io_relation_list
  :                         identifier { $$.push_back(mk<AstIO>(AstIoType::input, move($1), @1)); }
  | io_relation_list COMMA  identifier { $1.push_back(mk<AstIO>(AstIoType::input, move($3), @3)); $$ = move($1); }
  ;

/* Key-value pairs */
non_empty_key_value_pairs
  :                                 IDENT EQUALS kvp_value { $$.emplace_back(move($1), move($3)); }
  | non_empty_key_value_pairs COMMA IDENT EQUALS kvp_value { $1.emplace_back(move($3), move($5)); $$ = move($1); }
  ;
kvp_value
  : STRING  { $$ = move($STRING); }
  | IDENT   { $$ = move($IDENT); }
  | TRUE    { $$ = "true"; }
  | FALSE   { $$ = "false"; }
  ;

%%

void yy::parser::error(const location_type &l, const std::string &m) {
    driver.error(l, m);
}
