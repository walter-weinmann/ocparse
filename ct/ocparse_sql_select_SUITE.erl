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
            test_atom_constant,
            test_atom_count,
            test_atom_parameter,
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
            test_command_constraint_drop
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

test_atom_constant(_Config) ->
    test_cypher("TRUE"),
    test_cypher("FALSE"),
    test_cypher("NULL").

test_atom_count(_Config) ->
    test_cypher("count(*)"),
    test_cypher("count ( * )"),
    test_cypher("COUNT(*)").

test_atom_number_literal_decimal_integer(_Config) ->
    test_cypher("1"),
    test_cypher("4711"),
    test_cypher("8").

test_atom_number_literal_decimal_integer_sign(_Config) ->
    test_cypher("-1"),
    test_cypher("-4711"),
    test_cypher("-8").

test_atom_number_literal_exponential_decimal(_Config) ->
    test_cypher("0E0"),
    test_cypher("0E789"),
    test_cypher("9E0"),
    test_cypher("9E789"),
    test_cypher("0E-0"),
    test_cypher("0E-789"),
    test_cypher("9E-0"),
    test_cypher("9E-789").

test_atom_number_literal_exponential_decimal_sign(_Config) ->
    test_cypher("-0E0"),
    test_cypher("-0E789"),
    test_cypher("-9E0"),
    test_cypher("-9E789"),
    test_cypher("-0E-0"),
    test_cypher("-0E-789"),
    test_cypher("-9E-0"),
    test_cypher("-9E-789").

test_atom_number_literal_hex_integer(_Config) ->
    test_cypher("0X0"),
    test_cypher("0XA"),
    test_cypher("0X0123456789ABCDEF").

test_atom_number_literal_hex_integer_sign(_Config) ->
    test_cypher("-0X0"),
    test_cypher("-0XA"),
    test_cypher("-0X0123456789ABCDEF").

test_atom_number_literal_octal_integer(_Config) ->
    test_cypher("00"),
    test_cypher("001234567").

test_atom_number_literal_octal_integer_sign(_Config) ->
    test_cypher("-00"),
    test_cypher("-001234567").

test_atom_number_literal_regular_decimal(_Config) ->
    test_cypher(".0"),
    test_cypher(".00"),
    test_cypher(".01"),
    test_cypher(".08"),
    test_cypher(".1"),
    test_cypher(".12345"),
    test_cypher(".8"),
    test_cypher("0.0"),
    test_cypher("0.1"),
    test_cypher("0.12345"),
    test_cypher("1.0"),
    test_cypher("8.0"),
    test_cypher("00.0"),
    test_cypher("01.0"),
    test_cypher("08.0"),
    test_cypher("10.0"),
    test_cypher("11.0"),
    test_cypher("18.0"),
    test_cypher("3210.0"),
    test_cypher("3210.1"),
    test_cypher("3210.12345"),
    test_cypher("80.0"),
    test_cypher("81.0"),
    test_cypher("88.0").

test_atom_number_literal_regular_decimal_sign(_Config) ->
    test_cypher("-.0"),
    test_cypher("-.00"),
    test_cypher("-.01"),
    test_cypher("-.08"),
    test_cypher("-.1"),
    test_cypher("-.12345"),
    test_cypher("-.8"),
    test_cypher("-0.0"),
    test_cypher("-0.1"),
    test_cypher("-0.12345"),
    test_cypher("-1.0"),
    test_cypher("-8.0"),
    test_cypher("-00.0"),
    test_cypher("-01.0"),
    test_cypher("-08.0"),
    test_cypher("-10.0"),
    test_cypher("-11.0"),
    test_cypher("-18.0"),
    test_cypher("-3210.0"),
    test_cypher("-3210.1"),
    test_cypher("-3210.12345"),
    test_cypher("-80.0"),
    test_cypher("-81.0"),
    test_cypher("-88.0").

test_atom_parameter(_Config) ->
    test_cypher("{test}"),
    test_cypher("{Test}"),
    test_cypher("{0}"),
    test_cypher("{1}"),
    test_cypher("{10}"),
    test_cypher("{19}").


test_atom_string_literal(_Config) ->
    test_cypher("\"Dies ist ein String\""),
    test_cypher("\"Dies' ist 'ein String\""),
    test_cypher("'Dies ist ein String'"),
    test_cypher("'Dies\" ist \"ein String'").

test_atom_variable(_Config) ->
    test_cypher("Name1"),
    test_cypher("name2"),
    test_cypher("NAME3").

%%--------------------------------------------------------------------
%% Command Constraint.
%%--------------------------------------------------------------------

test_command_constraint_create(_Config) ->
    test_cypher("CREATE CONSTRAINT ON (movie:Movie) ASSERT 4711 IS UNIQUE"),
    test_cypher("CREATE CONSTRAINT ON (movie:Movie) ASSERT movie.title IS UNIQUE"),
    test_cypher("CREATE CONSTRAINT ON ( n : Person ) ASSERT n . name IS UNIQUE;"),
    test_cypher("create constraint on (n:Person) assert n.role is unique;"),
    test_cypher("CREATE CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)"),
    test_cypher("CREATE CONSTRAINT ON ()-[like:LIKED]-() ASSERT exists(like.day)").

test_command_constraint_drop(_Config) ->
    test_cypher("DROP CONSTRAINT ON (person:Person) ASSERT person.id IS UNIQUE"),
    test_cypher("DROP CONSTRAINT ON ( movie : Movie ) ASSERT movie . id IS UNIQUE"),
    test_cypher("drop constraint on (person:Person) assert person.id is unique"),
    test_cypher("DROP CONSTRAINT ON (book:Book) ASSERT exists(book.isbn)"),
    test_cypher("DROP CONSTRAINT ON ()-[like:LIKED]-() ASSERT exists(like.day)").

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
                ?debugFmt("wwe debugging test_cypher/1 ===> ~n NCypher: ~p~n", [NCypher])
                  end,
            try
                ParseTree = NPTree
            catch
                _:_ ->
                    ?debugFmt("wwe debugging test_cypher/1 ===> ~n NPTree: ~p~n Tokens: ~p~n NToks: ~p~n", [NPTree, Tokens, NToks])
            end,
            ?assertEqual(ParseTree, NPTree);
        {lex_error, Error} ->
            ?debugFmt("wwe debugging test_cypher/1 ===> Failed lexer~n Error: ~p~n", [Error]),
            ?assertEqual(ok, Error);
        {parse_error, {Error, Tokens}} ->
            ?debugFmt("wwe debugging test_cypher/1 ===> Failed~n Test: ~p~n Error: ~p~n Tokens: ~p~n", [Test, Error, Tokens]),
            ?assertEqual(ok, Error)
    end.

