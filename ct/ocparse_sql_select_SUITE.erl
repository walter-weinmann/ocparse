%%%-------------------------------------------------------------------
%%% File    : ocparse_sql_select_SUITE.erl
%%% Description : Test Suite related to module ocparse_sql_select.
%%%
%%% Created : 04.2016
%%%-------------------------------------------------------------------
-module(ocparse_sql_select_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%--------------------------------------------------------------------

suite() ->
  [
    {timetrap, {minutes, 10}}
  ].

init_per_suite(Config) ->
  Config.

end_per_suite(_Config) ->
  ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - GROUPS - STRUCTURE
%%--------------------------------------------------------------------

groups() ->
  [
    {atom, [], [
      {group, atom_number_literal},
      {group, atom_number_literal_sign},
      {group, atom_misc}
    ]},
    {atom_misc, [], [
      test_atom_all_single,
      test_atom_case_1,
      test_atom_case_2,
      test_atom_constant,
      test_atom_count,
      test_atom_expression_list,
      test_atom_function_invocation,
      test_atom_list_comprehension,
      test_atom_map_literal,
      test_atom_parameter,
      test_atom_parenthesized_expression,
      test_atom_reduce,
      test_atom_relationships_pattern,
      test_atom_shortest_path_pattern,
      test_atom_string_literal,
      test_atom_variable
    ]},
    {atom_number_literal, [], [
      test_atom_number_literal_decimal_integer,
      test_atom_number_literal_hex_integer,
      test_atom_number_literal_octal_integer,
      test_atom_number_literal_exponential_decimal,
      test_atom_number_literal_regular_decimal
    ]},
    {atom_number_literal_sign, [], [
      test_atom_number_literal_decimal_integer_sign,
      test_atom_number_literal_hex_integer_sign,
      test_atom_number_literal_octal_integer_sign,
      test_atom_number_literal_exponential_decimal_sign,
      test_atom_number_literal_regular_decimal_sign
    ]},
    {command_index, [], [
      test_command_index_create,
      test_command_index_drop
    ]},
    {command_constraint, [], [
      test_command_constraint_create,
      test_command_constraint_drop,
      test_command_constraint_node_create,
      test_command_constraint_node_drop,
      test_command_constraint_relationship_create,
      test_command_constraint_relationship_drop
    ]},
    {expression, [], [
      test_expression_2,
      test_expression_3,
      test_expression_4,
      test_expression_5,
      test_expression_6,
      test_expression_7,
      test_expression_8,
      test_expression_9,
      test_expression_10,
      test_expression_11,
      test_expression_12
    ]},
    {query_options, [], [
      test_command_query_options
    ]}
  ].

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - GROUPS - INIT
%%--------------------------------------------------------------------

init_per_group(_, Config) ->
  Config.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - GROUPS - END
%%--------------------------------------------------------------------

end_per_group(_, _Config) ->
  ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
  [
    {group, atom},
    {group, command_constraint},
    {group, command_index},
    {group, expression},
    {group, query_options}
  ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

%%--------------------------------------------------------------------
%% Atom.
%%--------------------------------------------------------------------

test_atom_all_single(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT fIlter ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT eXtract ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT eXtract ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLl ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aNy ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT nOne ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sIngle ( variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT fIlter(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT eXtract(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT eXtract(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLl(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aNy(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT nOne(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sIngle(variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique").

test_atom_constant(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT TRUE is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT FALSE is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT NULL is unique").

test_atom_case_1(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT cAse wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique").

test_atom_case_2(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT cAse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null tHen nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eLse nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null eNd is unique").

test_atom_count(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT count(*) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT count ( * ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT COUNT(*) is unique").

test_atom_expression_list(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [ nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [ nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique").

test_atom_function_invocation(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( dIstinct ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1 ( dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1() is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1(dIstinct) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1(dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1(nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT function_1(dIstinct nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique").

test_atom_list_comprehension(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [ variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT [variable_1 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null wHere nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null] is unique").

test_atom_map_literal(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT { } is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT { property_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT { property_1 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , property_2 : nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null } is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {} is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,property_2:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} is unique").

test_atom_number_literal_decimal_integer(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 4711 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 8 is unique").

test_atom_number_literal_decimal_integer_sign(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -4711 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -8 is unique").

test_atom_number_literal_exponential_decimal(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0E0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0E789 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 9E0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 9E789 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0E-0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0E-789 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 9E-0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 9E-789 is unique").

test_atom_number_literal_exponential_decimal_sign(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0E0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0E789 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -9E0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -9E789 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0E-0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0E-789 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -9E-0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -9E-789 is unique").

test_atom_number_literal_hex_integer(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0X0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0XA is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0X0123456789ABCDEF is unique").

test_atom_number_literal_hex_integer_sign(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0X0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0XA is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0X0123456789ABCDEF is unique").

test_atom_number_literal_octal_integer(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 00 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 001234567 is unique").

test_atom_number_literal_octal_integer_sign(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -00 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -001234567 is unique").

test_atom_number_literal_regular_decimal(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT .0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT .00 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT .01 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT .08 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT .1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT .12345 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT .8 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0.1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 0.12345 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 1.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 8.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 00.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 01.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 08.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 10.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 11.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 18.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 3210.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 3210.1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 3210.12345 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 80.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 81.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 88.0 is unique").

test_atom_number_literal_regular_decimal_sign(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -.00 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -.01 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -.08 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -.1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -.12345 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -.8 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0.1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -0.12345 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -1.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -8.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -00.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -01.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -08.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -10.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -11.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -18.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -3210.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -3210.1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -3210.12345 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -80.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -81.0 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT -88.0 is unique").

test_atom_parameter(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {test} is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {Test} is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {0} is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {1} is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {10} is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT {19} is unique").

test_atom_parenthesized_expression(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT (nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT (nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique").

test_atom_reduce(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT rEduce ( variable_1 = nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null , variable_2 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null | nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT rEduce(variable_1=nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null,variable_2 iN nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null|nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null) is unique").

test_atom_relationships_pattern(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? * 1 ..99 ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 { name_1 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 { 4711 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 { name_1 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 { 4711 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 { name_1 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 { 4711 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 : node_2 { name_1 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 : node_2 { 4711 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 { name_1 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 { 4711 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( { name_1 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( { 4711 } ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 : node_2 ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( : node_1 ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT (  ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) -- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -- (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) -- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] -- ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) -- --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- -- (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) -- -- ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 * 1 ..] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * ..99]--> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 *] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 * 1 ..99] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 | :rel_2 * ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * 1 ..] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 | :rel_2 * ..99]--> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * 1 ..99] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * ..] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? * ..] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ * ..] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ * .. {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 | :rel_2 * 1 ..99 { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ variable_1 ? :rel_1 | :rel_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( ) <-- [ ? { name_1 }] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 { name_1 } ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT ( variable_1 : node_1 : node_2 { name_1 } ) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) <-- [ variable_1 ? :rel_1 | :rel_2 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ] --> (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) is unique").

test_atom_shortest_path_pattern(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( (variable_1 :node_1 :node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 : node_1 : node_2 { name_1 } ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 : node_1 { name_1 } ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 : node_1 : node_2 ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 { name_1 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( variable_1 ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( : node_1 : node_2 { name_1 } ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( (  ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( { 4711 } ) <--  --> ( ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( { name_1 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT sHortestpath ( ( ( { name_1 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( : node_1 { 4711 } ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( { name_1 } ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( { name_1 } ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( { 4711 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( : node_1 ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( : node_1 { name_1 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( : node_1 : node_2 { 4711 } ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 { 4711 } ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 : node_1 ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 : node_1 { 4711 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 : node_1 : node_2 { 4711 } ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT aLlsHortestpaths ( ( ( variable_1 : node_1 : node_2 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null} ) <-- [ variable_1 ? :rel_1 * 1 ..99 {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}] --> ( ) ) ) is unique").

test_atom_string_literal(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT \"Dies ist ein String\" is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT \"Dies' ist 'ein String\" is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 'Dies ist ein String' is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT 'Dies\" ist \"ein String' is unique").

test_atom_variable(_Config) ->
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT Name1 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT name2 is unique"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT NAME3 is unique").

%%--------------------------------------------------------------------
%% Command Constraint.
%%--------------------------------------------------------------------

test_command_constraint_create(_Config) ->
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) IS UNIQUE"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) IS UNIQUE"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE"),
  test_cypher("CREATE CONSTRAINT ON (movie:Movie) ASSERT 4711 IS UNIQUE"),
  test_cypher("CREATE CONSTRAINT ON (movie:Movie) ASSERT movie.title IS UNIQUE"),
  test_cypher("CREATE CONSTRAINT ON ( n : Person ) ASSERT n . name IS UNIQUE;"),
  test_cypher("create constraint on (n:Person) assert n.role is unique;").

test_command_constraint_drop(_Config) ->
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) IS UNIQUE"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) IS UNIQUE"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE"),
  test_cypher("DROP CONSTRAINT ON (movie:Movie) ASSERT 4711 IS UNIQUE"),
  test_cypher("DROP CONSTRAINT ON (movie:Movie) ASSERT movie.title IS UNIQUE"),
  test_cypher("DROP CONSTRAINT ON ( n : Person ) ASSERT n . name IS UNIQUE;"),
  test_cypher("drop constraint on (n:Person) assert n.role is unique;").

test_command_constraint_node_create(_Config) ->
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON (movie:Movie) ASSERT exists ( 4711 IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON (movie:Movie) ASSERT exists ( movie.title IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON ( n : Person ) ASSERT exists ( n . name IS UNIQUE);"),
  test_cypher("create constraint on (n:Person) assert EXISTS (n.role is unique);"),
  test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)"),
  test_cypher("CREATE CONSTRAINT ON ()-[like:LIKED]-() ASSERT exists(like.day)").

test_command_constraint_node_drop(_Config) ->
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON ( variable_1 : node_1) ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON (movie:Movie) ASSERT exists ( 4711 IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON (movie:Movie) ASSERT exists ( movie.title IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON ( n : Person ) ASSERT exists ( n . name IS UNIQUE);"),
  test_cypher("drop constraint on (n:Person) assert EXISTS (n.role is unique);"),
  test_cypher("DROP CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)"),
  test_cypher("DROP CONSTRAINT ON ()-[like:LIKED]-() ASSERT exists(like.day)").

test_command_constraint_relationship_create(_Config) ->
  test_cypher("CREATE CONSTRAINT ON () -- variable_1 : relation_1 -- () ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON () <-- variable_1 : relation_1 -- () ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON () -- variable_1 : relation_1 --> () ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON () -- variable_1 : relation_1 -- () ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON () <-- variable_1 : relation_1 -- () ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON () -- variable_1 : relation_1 --> () ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON () -- variable_1 : relation_1 -- () ASSERT exists ( 4711 IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON () <-- variable_1 : relation_1 -- () ASSERT exists ( movie.title IS UNIQUE)"),
  test_cypher("CREATE CONSTRAINT ON () -- variable_1 : relation_1 --> () ASSERT exists ( n . name IS UNIQUE);)"),
  test_cypher("create constraint on () -- variable_1 : relation_1 -- () assert EXISTS (n.role is unique);)"),
  test_cypher("CREATE CONSTRAINT ON () <-- variable_1 : relation_1 -- () ASSERT exists(book.isbn)"),
  test_cypher("CREATE CONSTRAINT ON () -- variable_1 : relation_1 --> () ASSERT exists(like.day)").

test_command_constraint_relationship_drop(_Config) ->
  test_cypher("DROP CONSTRAINT ON () -- variable_1 : relation_1 -- () ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON () <-- variable_1 : relation_1 -- () ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON () -- variable_1 : relation_1 --> () ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON () -- variable_1 : relation_1 -- () ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON () <-- variable_1 : relation_1 -- () ASSERT exists ( ( {property_1:nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null}) .property_1 IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON () -- variable_1 : relation_1 --> () ASSERT exists ( sHortestpath ( ( ( : node_1 : node_2 ) ) ) .property_1 .property2! .property3? IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON () -- variable_1 : relation_1 -- () ASSERT exists ( 4711 IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON () <-- variable_1 : relation_1 -- () ASSERT exists ( movie.title IS UNIQUE)"),
  test_cypher("DROP CONSTRAINT ON () -- variable_1 : relation_1 --> () ASSERT exists ( n . name IS UNIQUE);)"),
  test_cypher("drop constraint on () -- variable_1 : relation_1 -- () assert EXISTS (n.role is unique);)"),
  test_cypher("DROP CONSTRAINT ON () <-- variable_1 : relation_1 -- () ASSERT exists(book.isbn)"),
  test_cypher("DROP CONSTRAINT ON () -- variable_1 : relation_1 --> () ASSERT exists(like.day)").

%%--------------------------------------------------------------------
%% Command Index.
%%--------------------------------------------------------------------

test_command_index_create(_Config) ->
  test_cypher("CREATE INDEX ON :Actor(name)"),
  test_cypher("create index on :Actor (name)"),
  test_cypher("create index on :Actor ( name )").

test_command_index_drop(_Config) ->
  test_cypher("DROP INDEX ON :Actor(name)"),
  test_cypher("drop index on :Actor (name)"),
  test_cypher("drop index on :Actor ( name )").

%%--------------------------------------------------------------------
%% Expression.
%%--------------------------------------------------------------------

test_expression_2(_Config) ->
  test_cypher("4711 :label_1"),
  test_cypher("4711 :label_1 :label_2 :label_3"),
  test_cypher("4711 .property_1"),
  test_cypher("4711 .property_1!"),
  test_cypher("4711 .property_1?"),
  test_cypher("4711 .property_1 .property2! .property3?"),
  test_cypher("4711 :label_1 .property_1"),
  test_cypher("4711 :label_1 .property_1 :label_2"),
  test_cypher("4711 .property_1 :label_1"),
  test_cypher("4711 :label_1 :label_2 .property_1 .property2? :label_3 :label_4"),
  test_cypher("4711 .property_1 .property2? :label_1 :label_2 .property_3 .property4?"),
  test_cypher("'test' :label_1"),
  test_cypher("'test' :label_1 :label_2 :label_3"),
  test_cypher("'test' .property_1"),
  test_cypher("'test' .property_1!"),
  test_cypher("'test' .property_1?"),
  test_cypher("'test' .property_1 .property2! .property3?"),
  test_cypher("'test' :label_1 .property_1"),
  test_cypher("'test' :label_1 .property_1 :label_2"),
  test_cypher("'test' .property_1 :label_1"),
  test_cypher("'test' :label_1 :label_2 .property_1 .property2? :label_3 :label_4"),
  test_cypher("'test' .property_1 .property2? :label_1 :label_2 .property_3 .property4?").

test_expression_3(_Config) ->
  test_cypher("'test_1' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is NOT null"),
  test_cypher("'test_1' .property_1 :label_1 =~ 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 iN 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 STARTS with 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 ends WITH 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]"),
  test_cypher("'test_1' .property_1 :label_1 [nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1]"),
  test_cypher("'test_1' .property_1 :label_1 [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null .. nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]"),
  test_cypher("'test_1' .property_1 :label_1 [nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null .. nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1]"),
  test_cypher("'test_1' .property_1 :label_1 [nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 .. nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null]"),
  test_cypher("'test_1' .property_1 :label_1 [nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 .. nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1]").

test_expression_4(_Config) ->
  test_cypher("'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 is null"),
  test_cypher("- 'test_1' .property_1 :label_1 is null"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("- 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

test_expression_5(_Config) ->
  test_cypher("'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 is null ^'test_1' .property_1 :label_1 is null"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

test_expression_6(_Config) ->
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 is null * 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null / 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null % 'test_1' .property_1 :label_1 is null"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 * + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 / + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

test_expression_7(_Config) ->
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 is null + 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null - 'test_1' .property_1 :label_1 is null"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 + + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

test_expression_8(_Config) ->
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("'test_1' .property_1 :label_1 is null = 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null <> 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null != 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null < 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null > 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null <= 'test_1' .property_1 :label_1 is null"),
  test_cypher("'test_1' .property_1 :label_1 is null >= 'test_1' .property_1 :label_1 is null"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 <> + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 != + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 < + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 > + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 <= + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 >= + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

test_expression_9(_Config) ->
  test_cypher("+ 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 >= + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("nOt 'test_1' .property_1 :label_1 is null >= 'test_1' .property_1 :label_1 is null"),
  test_cypher("nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

test_expression_10(_Config) ->
  test_cypher("nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("nOt 'test_1' .property_1 :label_1 is null aNd 'test_1' .property_1 :label_1 is null"),
  test_cypher("nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

test_expression_11(_Config) ->
  test_cypher("nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("nOt 'test_1' .property_1 :label_1 is null xOr 'test_1' .property_1 :label_1 is null"),
  test_cypher("nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

test_expression_12(_Config) ->
  test_cypher("nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1"),
  test_cypher("nOt 'test_1' .property_1 :label_1 is null oR 'test_1' .property_1 :label_1 is null"),
  test_cypher("nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 oR nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 xOr nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 aNd nOt + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 = + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 - + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 % + 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1 ^ - 'test_1' .property_1 :label_1 conTains 'test_2' .property_1 :label_1").

%%--------------------------------------------------------------------
%% Query Options.
%%--------------------------------------------------------------------

test_command_query_options(_Config) ->
  test_cypher("cypher create index on :Actor(name)"),
  test_cypher("explain create index on :Actor(name)"),
  test_cypher("profile create index on :Actor(name)"),
  test_cypher("explain profile create index on :Actor(name)"),
  test_cypher("EXPLAIN PROFILE create index on :Actor(name)"),
  test_cypher("cypher 2.2 create index on :Actor(name)"),
  test_cypher("cypher planner=cost create index on :Actor(name)"),
  test_cypher("cypher planner=rule create index on :Actor(name)"),
  test_cypher("cypher planner=cost planner=rule create index on :Actor(name)"),
  test_cypher("explain cypher planner=cost create index on :Actor(name)"),
  test_cypher("profile cypher planner=rule create index on :Actor(name)"),
  test_cypher("explain profile cypher planner=cost planner=rule create index on :Actor(name)"),
  test_cypher("cypher 2.2 planner=cost create index on :Actor(name)"),
  test_cypher("explain cypher 2.2 planner=cost create index on :Actor(name)"),
  test_cypher("profile cypher 2.2 planner=cost create index on :Actor(name)"),
  test_cypher("explain profile cypher 2.2 planner=cost create index on :Actor(name)"),
  test_cypher("explain profile cypher 2.2 planner=cost planner=rule create index on :Actor(name)"),
  test_cypher("explain cypher 2.2 cypher planner=cost create index on :Actor(name)"),
  test_cypher("explain profile cypher 2.2 cypher planner=cost create index on :Actor(name)"),
  test_cypher("explain profile cypher 2.2 cypher planner=cost cypher planner=rule create index on :Actor(name)"),
  test_cypher("explain profile cypher cypher 2.2 cypher planner=a cypher planner=b planner=c cypher 2.3 planner=d cypher 2.4 planner=e planner=f create index on :Actor(name)").

%%--------------------------------------------------------------------
%% HELPER FUNCTIONS
%%--------------------------------------------------------------------

test_cypher(Test) ->
  % ?debugFmt("wwe debugging test_cypher/1 ===> ~n Test: ~p~n", [Test]),
  case ocparse:parsetree_with_tokens(Test) of
    {ok, {ParseTree, Tokens}} ->
      % ?debugFmt("wwe debugging test_cypher/1 ===> ~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, Tokens]),
      NCypher = case ocparse:pt_to_string(ParseTree) of
                  {error, Error} ->
                    throw({error, Error});
                  NS ->
                    % ?debugFmt("wwe debugging test_cypher/1 ===> ~n NS: ~p~n", [NS]),
                    NS
                end,
      {ok, {NPTree, NToks}}
        = try
            {ok, {NPT, NT}} = ocparse:parsetree_with_tokens(NCypher),
            {ok, {NPT, NT}}
          catch _:_ ->
        ?debugFmt("wwe debugging test_cypher/1 ===> ~n Test: ~p~n NCypher: ~p~n", [Test, NCypher])
          end,
      try
        ParseTree = NPTree
      catch
        _:_ ->
          ?debugFmt("wwe debugging test_cypher/1 ===> ~n Test: ~p~n ParseTree: ~p~n NPTree: ~p~n Tokens: ~p~n NToks: ~p~n", [Test, ParseTree, NPTree, Tokens, NToks])
      end,
      ?assertEqual(ParseTree, NPTree);
    {lex_error, Error} ->
      ?debugFmt("wwe debugging test_cypher/1 ===> Failed lexer~n Test: ~p~n Error: ~p~n", [Test, Error]),
      ?assertEqual(ok, Error);
    {parse_error, {Error, Tokens}} ->
      ?debugFmt("wwe debugging test_cypher/1 ===> Failed parser~n Test: ~p~n Error: ~p~n Tokens: ~p~n", [Test, Error, Tokens]),
      ?assertEqual(ok, Error)
  end.
