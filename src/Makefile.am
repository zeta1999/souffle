# Souffle - A Datalog Compiler
# Copyright (c) 2013, Oracle and/or its affiliates. All rights reserved.
# Licensed under the Universal Permissive License v 1.0 as shown at:
# - https://opensource.org/licenses/UPL
# - <souffle root>/licenses/SOUFFLE-UPL.txt

SUFFIXES = .cpp .h .yy .ll .cc .hh .h

bin_PROGRAMS = souffle souffle-profile

nodist_souffle_profile_SOURCES = $(BUILT_SOURCES)

souffle_profile_sources = \
        ProfileDatabase.h                                  \
        json11.h                                           \
        profile/Cell.h                                     \
        profile/CellInterface.h                            \
        profile/Cli.h                                      \
        profile/DataComparator.h                           \
        profile/HtmlGenerator.h                            \
        profile/Iteration.h                                \
        profile/OutputProcessor.h                          \
        profile/ProgramRun.h                               \
        profile/Reader.h                                   \
        profile/Relation.h                                 \
        profile/Row.h                                      \
        profile/Rule.h                                     \
        profile/StringUtils.h                              \
        profile/Table.h                                    \
        profile/Tui.h                                      \
        profile/UserInputReader.h                          \
        profile/htmlCssChartist.h                          \
        profile/htmlCssStyle.h                             \
        profile/htmlJsChartistMin.h                        \
        profile/htmlJsChartistPlugin.h                     \
        profile/htmlJsMain.h                               \
        profile/htmlJsTableSort.h                          \
        profile/htmlJsUtil.h                               \
        profile/htmlMain.h

souffle_swig_sources = \
        swig/SwigInterface.h                               \
        swig/SwigInterface.i

souffle_utility_sources = \
        utility/CacheUtil.h                                \
        utility/ContainerUtil.h                            \
        utility/FileUtil.h                                 \
        utility/EvaluatorUtil.h                            \
        utility/FunctionalUtil.h                           \
        utility/MiscUtil.h                                 \
        utility/ParallelUtil.h                             \
        utility/StreamUtil.h                               \
        utility/StringUtil.h                               \
        utility/tinyformat.h

DIR := ${CURDIR}

# ... which should no go to distribution
nodist_souffle_SOURCES = $(BUILT_SOURCES)

if LIBZ
libz_sources = gzfstream.h
endif

if SQLITE
sqlite_sources = ReadStreamSQLite.h WriteStreamSQLite.h
endif


souffle_sources = \
        AggregateOp.h                                      \
        AstAbstract.h                                      \
        AstAnalysis.h                                      \
        AstArgument.h                                      \
        AstAttribute.h                                     \
        AstClause.h                                        \
        AstComponent.h                                     \
        AstComponentChecker.cpp                            \
        AstComponentChecker.h                              \
        AstConstraintAnalysis.h                            \
        AstFunctorDeclaration.h                            \
        AstGroundAnalysis.cpp                              \
        AstGroundAnalysis.h                                \
        AstIO.h                                            \
        AstIOTypeAnalysis.cpp                              \
        AstIOTypeAnalysis.h                                \
        AstLiteral.h                                       \
        AstNode.h                                          \
        AstParserUtils.cpp                                 \
        AstParserUtils.h                                   \
        AstPragma.h                                        \
        AstPragmaChecker.cpp                               \
        AstPragmaChecker.h                                 \
        AstProfileUse.cpp                                  \
        AstProfileUse.h                                    \
        AstProgram.h                                       \
        AstQualifiedName.h                                 \
        AstRelation.h                                      \
        AstSemanticChecker.cpp                             \
        AstSemanticChecker.h                               \
        AstTransformer.cpp                                 \
        AstTransformer.h                                   \
        AstTransforms.cpp                                  \
        AstTransforms.h                                    \
        AstTranslationUnit.h                               \
        AstTranslator.cpp                                  \
        AstTranslator.h                                    \
        AstType.h                                          \
        AstTypeAnalysis.cpp                                \
        AstTypeAnalysis.h                                  \
        AstTypeEnvironmentAnalysis.cpp                     \
        AstTypeEnvironmentAnalysis.h                       \
        AstUtils.cpp                                       \
        AstUtils.h                                         \
        AstVisitor.h                                       \
        AuxArityAnalysis.cpp                               \
        AuxArityAnalysis.h                                 \
        BinaryConstraintOps.h                              \
        ComponentInstantiationTransformer.cpp              \
        ComponentInstantiationTransformer.h                \
        ComponentLookupAnalysis.cpp                        \
        ComponentLookupAnalysis.h                          \
        Constraints.h                                      \
        DebugReport.cpp                                    \
        DebugReport.h                                      \
        DebugReporter.cpp                                  \
        DebugReporter.h                                    \
        ErrorReport.h                                      \
        EventProcessor.h                                   \
        FunctorOps.h                                       \
        Global.cpp                                         \
        Global.h                                           \
        GraphUtils.h                                       \
        IOSystem.h                                         \
        InlineRelationsTransformer.cpp                     \
        InterpreterContext.h                               \
        InterpreterEngine.cpp                              \
        InterpreterEngine.h                                \
        InterpreterGenerator.h                             \
        InterpreterIndex.cpp                               \
        InterpreterIndex.h                                 \
        InterpreterNode.h                                  \
        InterpreterPreamble.h                              \
        InterpreterProgInterface.h                         \
        InterpreterRelation.cpp                            \
        InterpreterRelation.h                              \
        LogStatement.h                                     \
        MagicSet.cpp                                       \
        MagicSet.h                                         \
        MinimiseProgramTransformer.cpp                     \
        ParserDriver.cpp                                   \
        ParserDriver.h                                     \
        PrecedenceGraph.cpp                                \
        PrecedenceGraph.h                                  \
        ProfileEvent.h                                     \
        ProvenanceTransformer.cpp                          \
        RamAnalysis.h                                      \
        RamComplexityAnalysis.cpp                          \
        RamComplexityAnalysis.h                            \
        RamCondition.h                                     \
        RamExpression.h                                    \
        RamIndexAnalysis.cpp                               \
        RamIndexAnalysis.h                                 \
        RamLevelAnalysis.cpp                               \
        RamLevelAnalysis.h                                 \
        RamNode.h                                          \
        RamOperation.h                                     \
        RamProgram.h                                       \
        RamRelation.h                                      \
        RamStatement.h                                     \
        RamTransformer.cpp                                 \
        RamTransformer.h                                   \
        RamTransforms.cpp                                  \
        RamTransforms.h                                    \
        RamTranslationUnit.h                               \
        RamTypes.h                                         \
        RamUtils.h                                         \
        RamVisitor.h                                       \
        ReadStream.h                                       \
        ReadStreamCSV.h                                    \
        RecordTable.h                                      \
        RelationTag.h                                      \
        ReorderLiteralsTransformer.cpp                     \
        ResolveAliasesTransformer.cpp                      \
        SerialisationStream.h                              \
        SignalHandler.h                                    \
        SrcLocation.cpp                                    \
        SrcLocation.h                                      \
        Synthesiser.cpp                                    \
        Synthesiser.h                                      \
        SynthesiserRelation.cpp                            \
        SynthesiserRelation.h                              \
        TypeSystem.cpp                                     \
        TypeSystem.h                                       \
        Util.cpp                                           \
        WriteStream.h                                      \
        WriteStreamCSV.h                                   \
        parser.cc                                          \
        scanner.cc                                         \
        parser.hh                                          \
        stack.hh                                           \
        $(sqlite_sources)                                  \
        $(libz_sources)                                    \
        $(souffle_utility_sources)                         \
        $(souffle_profile_sources)

# -- build souffle as a library so it can be reused in testing
noinst_LTLIBRARIES = libsouffle.la
libsouffle_la_SOURCES  = $(souffle_sources)
libsouffle_la_CXXFLAGS = $(souffle_CPPFLAGS) $(FFI_CFLAGS)
libsouffle_la_LDFLAGS = --static --dlopen --pic -ldl -lffi

souffle_SOURCES = main.cpp
souffle_LDADD = libsouffle.la

souffle_profile_SOURCES = souffle_prof.cpp

dist_bin_SCRIPTS = souffle-compile souffle-config

EXTRA_DIST = parser.yy scanner.ll  test/test.h

soufflepublicdir = $(includedir)/souffle

soufflepublic_HEADERS = \
        BTree.h                                            \
        BinaryConstraintOps.h                              \
        Brie.h                                             \
        CompiledIndexUtils.h                               \
        CompiledOptions.h                                  \
        CompiledSouffle.h                                  \
        CompiledTuple.h                                    \
        EquivalenceRelation.h                              \
        EventProcessor.h                                   \
        Explain.h                                          \
        ExplainProvenance.h                                \
        ExplainProvenanceImpl.h                            \
        ExplainTree.h                                      \
        IOSystem.h                                         \
        IterUtils.h                                        \
        LambdaBTree.h                                      \
        Logger.h                                           \
        PiggyList.h                                        \
        ProfileDatabase.h                                  \
        ProfileEvent.h                                     \
        RamTypes.h                                         \
        ReadStream.h                                       \
        ReadStreamCSV.h                                    \
        RecordTable.h                                      \
        SerialisationStream.h                              \
        SignalHandler.h                                    \
        SouffleInterface.h                                 \
        SymbolTable.h                                      \
        Table.h                                            \
        UnionFind.h                                        \
        WriteStream.h                                      \
        WriteStreamCSV.h                                   \
        json11.h                                           \
        $(souffle_utility_sources)                         \
        $(libz_sources)                                    \
        $(sqlite_sources)

souffleprofiledir = $(soufflepublicdir)/profile

souffleprofile_HEADERS = $(souffle_profile_sources)

souffleswigdir = $(soufflepublicdir)/swig

souffleswig_HEADERS = $(souffle_swig_sources)

souffleutilitydir = $(soufflepublicdir)/utility

souffleutility_HEADERS = $(souffle_utility_sources)

# files to clean
CLEANFILES = $(BUILT_SOURCES)  parser.cc scanner.cc parser.hh stack.hh

# run Bison
$(builddir)/parser.hh: $(srcdir)/parser.yy
	$(BISON) -Wall -Werror -Wno-error=deprecated -Wno-error=other -v -d -o parser.cc $(srcdir)/parser.yy

# and FLEX
$(builddir)/scanner.cc: $(srcdir)/scanner.ll
	$(FLEX) -o scanner.cc $(srcdir)/scanner.ll

# driver depends on the generated header
$(builddir)/parser.cc $(builddir)/stack.hh $(builddir)/scanner.cc \
    $(builddir)/main.cpp $(builddir)/ParserDriver.cpp: $(builddir)/parser.hh

########## Unit Tests

AM_COLOR_TESTS=always

# -------------------------

# init check programs
check_PROGRAMS =

# test description
# check_PROGRAMS += test/testname
# test_testname_SOURCES = test/cppfile
# if needed
# test_testname_LDADD = libsouffle.la
# test_testname_CXXFLAGS = test-specific flags

# profile utilities test
check_PROGRAMS += test/profile_util_test
test_profile_util_test_SOURCES = test/profile_util_test.cpp

# utils test
check_PROGRAMS += test/util_test
test_util_test_SOURCES = test/util_test.cpp

# matching test
check_PROGRAMS += test/matching_test
test_matching_test_SOURCES = test/matching_test.cpp
test_matching_test_LDADD = libsouffle.la

# table test
check_PROGRAMS += test/table_test
test_table_test_SOURCES = test/table_test.cpp

# b-tree set test
check_PROGRAMS += test/btree_set_test
test_btree_set_test_SOURCES = test/btree_set_test.cpp

# b-tree multi-set test
check_PROGRAMS += test/btree_multiset_test
test_btree_multiset_test_SOURCES = test/btree_multiset_test.cpp

# binary relation tests
check_PROGRAMS += test/binary_relation_test
test_binary_relation_test_SOURCES = test/binary_relation_test.cpp

# pnappa's good ol fashioned data structure tests (auxilliary structures to binrel, and potentially useful outside)
check_PROGRAMS += test/eqrel_datastructure_test
test_eqrel_datastructure_test_SOURCES = test/eqrel_datastructure_test.cpp

# compiled ram tuple test
check_PROGRAMS += test/compiled_tuple_test
test_compiled_tuple_test_SOURCES = test/compiled_tuple_test.cpp

# compiled ram index utils test
check_PROGRAMS += test/compiled_index_utils_test
test_compiled_index_utils_test_SOURCES = test/compiled_index_utils_test.cpp

# interpreter relation test
check_PROGRAMS += test/interpreter_relation_test
test_interpreter_relation_test_SOURCES = test/interpreter_relation_test.cpp
test_interpreter_relation_test_LDADD = libsouffle.la

# type system test
check_PROGRAMS += test/type_system_test
test_type_system_test_SOURCES = test/type_system_test.cpp
test_type_system_test_LDADD = libsouffle.la

# constraints test
check_PROGRAMS += test/constraints_test
test_constraints_test_SOURCES = test/constraints_test.cpp
test_constraints_test_LDADD = libsouffle.la

# ast print test
check_PROGRAMS += test/ast_print_test
test_ast_print_test_SOURCES = test/ast_print_test.cpp
test_ast_print_test_LDADD = libsouffle.la

# ast program test
check_PROGRAMS += test/ast_program_test
test_ast_program_test_SOURCES = test/ast_program_test.cpp
test_ast_program_test_LDADD = libsouffle.la

# ast utils test
check_PROGRAMS += test/ast_utils_test
test_ast_utils_test_SOURCES = test/ast_utils_test.cpp
test_ast_utils_test_LDADD = libsouffle.la

# ast parser utils test
check_PROGRAMS += test/ast_parser_utils_test
test_ast_parser_utils_test_SOURCES = test/ast_parser_utils_test.cpp
test_ast_parser_utils_test_LDADD = libsouffle.la

# symbol table
check_PROGRAMS += test/symbol_table_test
test_symbol_table_test_SOURCES = test/symbol_table_test.cpp

# graph utils
check_PROGRAMS += test/graph_utils_test
test_graph_utils_test_SOURCES = test/graph_utils_test.cpp

# trie implementation
check_PROGRAMS += test/brie_test
test_brie_test_SOURCES = test/brie_test.cpp

# parallel utils implementation
check_PROGRAMS += test/parallel_utils_test
test_parallel_utils_test_SOURCES = test/parallel_utils_test.cpp

# interpreter relation test
check_PROGRAMS += test/ram_condition_equal_clone_test
test_ram_condition_equal_clone_test_SOURCES = test/ram_condition_equal_clone_test.cpp
test_ram_condition_equal_clone_test_LDADD = libsouffle.la

check_PROGRAMS += test/ram_statement_equal_clone_test
test_ram_statement_equal_clone_test_SOURCES = test/ram_statement_equal_clone_test.cpp
test_ram_statement_equal_clone_test_LDADD = libsouffle.la

check_PROGRAMS += test/ram_expression_equal_clone_test
test_ram_expression_equal_clone_test_SOURCES = test/ram_expression_equal_clone_test.cpp
test_ram_expression_equal_clone_test_LDADD = libsouffle.la

check_PROGRAMS += test/ram_operation_equal_clone_test
test_ram_operation_equal_clone_test_SOURCES = test/ram_operation_equal_clone_test.cpp
test_ram_operation_equal_clone_test_LDADD = libsouffle.la

check_PROGRAMS += test/ram_relation_equal_clone_test
test_ram_relation_equal_clone_test_SOURCES = test/ram_relation_equal_clone_test.cpp
test_ram_relation_equal_clone_test_LDADD = libsouffle.la

# relation test
check_PROGRAMS += test/ram_relation_test
test_ram_relation_test_SOURCES = test/ram_relation_test.cpp
test_ram_relation_test_LDADD = libsouffle.la

# arithmetic test
check_PROGRAMS += test/ram_arithmetic_test
test_ram_arithmetic_test_SOURCES = test/ram_arithmetic_test.cpp
test_ram_arithmetic_test_LDADD = libsouffle.la

check_PROGRAMS += test/ram_type_conversion_test
test_ram_type_conversion_test_SOURCES = test/ram_type_conversion_test.cpp
test_ram_type_conversion_test_LDADD = libsouffle.la

check_PROGRAMS += test/record_table_test
test_record_table_test_SOURCES = test/record_table_test.cpp

# make all check-programs tests
TESTS = $(check_PROGRAMS)
