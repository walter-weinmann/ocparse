%% -----------------------------------------------------------------------------
%%
%% ocparse_generator.erl: opencypher - test data generator.
%%
%% Copyright (c) 2017 Walter Weinmann.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -----------------------------------------------------------------------------

-module(ocparse_generator).

-export([generate/0]).

-define(ALL_CLAUSE_CT_PERFORMANCE, [
    cypher,
    query,
    statement
]).
-define(ALL_CLAUSE_CT_RELIABILITY, [
    unaryAddOrSubtractExpression,
    create,
    cypher,
    delete,
    match,
    merge,
    query,
    remove,
    return,
    set,
    statement,
    unwind,
    with
]).
-define(ALL_CLAUSE_EUNIT, []).
-define(CODE_TEMPLATES, code_templates).
-define(DASH, "-").
-define(LEFT_ARROW_HEAD, "<").
-define(MAX_CLAUSE, 2000).
-define(MAX_CYPHER, 2000).
-define(MAX_QUERY, 2000).
-define(MAX_RULE_ATOM, 500).
-define(MAX_RULE_EXPRESSION, 500).
-define(MAX_STATEMENT, 2000).
-define(PATH_CT, "test").
-define(PATH_EUNIT, "test").
-define(PRIME, 37).
-define(RIGHT_ARROW_HEAD, ">").
-define(SP, " ").
-define(SP_OPT, []).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate Test Data.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    create_code(),

    % performance common tests
    ok = file_create_ct_all(performance, ?ALL_CLAUSE_CT_PERFORMANCE),

    % reliability common tests
    ok = file_create_ct_all(reliability, ?ALL_CLAUSE_CT_RELIABILITY),

    % reliability eunit tests
    ok = file_create_eunit_all(?ALL_CLAUSE_EUNIT),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating code base.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code() ->
    try
        ets:new(?CODE_TEMPLATES, [set, named_table])
    catch
        error:badarg ->
            ets:delete(?CODE_TEMPLATES),
            ets:new(?CODE_TEMPLATES, [set, named_table])
    end,

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Atom = ...
    %      | (N,U,L,L)
    %      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
    %      | ... ;
    % --------------------------------------------------------------------------
    AtomCount = ["Count(*)", "Count ( * )"],
    insert_table(atom_count, AtomCount),
    AtomNull = ["Null"],
    insert_table(atom_null, AtomNull),

    % --------------------------------------------------------------------------
    % BooleanLiteral = (T,R,U,E)
    %                | (F,A,L,S,E)
    %                ;
    % --------------------------------------------------------------------------
    BooleanLiteral = sort_list_random([
        "true",
        "false"
    ]),
    BooleanLiteral_Length = length(BooleanLiteral),
    insert_table(boolean_literal, BooleanLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses boolean_literal ~n", [BooleanLiteral_Length])),
    % --------------------------------------------------------------------------
    % DecimalInteger = (('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'), [DigitString])
    %                | '0' ;
    % --------------------------------------------------------------------------
    DecimalInteger = sort_list_random([
        "0",
        "7",
        "12",
        "999",
        "1000",
        "12345",
        "654321",
        "1234567",
        "87654321",
        "123456789"
    ]),
    DecimalInteger_Length = length(DecimalInteger),
    insert_table(decimal_integer, DecimalInteger),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses decimal_integer ~n", [DecimalInteger_Length])),
    % --------------------------------------------------------------------------
    % (* Any character except "`", enclosed within `backticks`. Backticks are escaped with double backticks. *)EscapedSymbolicName = { '`', { ANY - ('`') }, '`' }- ;
    % --------------------------------------------------------------------------
    EscapedSymbolicName = sort_list_random([
        "`1esn`",
        "`2esn`",
        "`3esn`",
        "`4esn`",
        "`5esn`",
        "`6esn`",
        "`7esn`",
        "`8esn`",
        "`aesn`",
        "`Aesn`",
        "`besn`",
        "`Besn`",
        "`.esn`",
        "`,esn`",
        "`@esn`",
        "``"
    ]),
    EscapedSymbolicName_Length = length(EscapedSymbolicName),
    insert_table(escaped_symbolic_name, EscapedSymbolicName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses escaped_symbolic_name ~n", [EscapedSymbolicName_Length])),
    % --------------------------------------------------------------------------
    % ExponentDecimalReal = ({ Digit | '.' }- | DecimalInteger), ((E) | (E)), (DigitString | DecimalInteger) ;
    % --------------------------------------------------------------------------
    ExponentDecimalReal = sort_list_random([
        "9e0",
        "9e1",
        "9e-1",
        "9e12",
        "9e-12",
        "0e0",
        "0e-0",
        "1e1",
        "1e-1",
        "12e12",
        "12e-12",
        ".9e0",
        ".9e1",
        ".9e-1",
        ".9e12",
        ".9e-12",
        ".0e0",
        ".0e-0",
        ".1e1",
        ".1e-1",
        ".12e12",
        ".12e-12",
        "1.9e0",
        "2.9e1",
        "3.9e-1",
        "4.9e12",
        "5.9e-12",
        "6.0e0",
        "7.0e-0",
        "8.1e1",
        "9.1e-1",
        "10.12e12",
        "11.12e-12"
    ]),
    ExponentDecimalReal_Length = length(ExponentDecimalReal),
    insert_table(exponent_decimal_real, ExponentDecimalReal),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses exponent_decimal_real ~n", [ExponentDecimalReal_Length])),
    % --------------------------------------------------------------------------
    % HexInteger = ('0',X), HexString ;
    % --------------------------------------------------------------------------
    HexInteger = sort_list_random([
        "0x0",
        "0x7",
        "0xa",
        "0xabc",
        "0xabcdef",
        "0x12a",
        "0x3456abc",
        "0x7890abcdef",
        "0x0123456789ABCDEF"
    ]),
    HexInteger_Length = length(HexInteger),
    insert_table(hex_integer, HexInteger),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses hex_integer ~n", [HexInteger_Length])),
    % --------------------------------------------------------------------------
    % OctalInteger = '0', OctalString ;
    % --------------------------------------------------------------------------
    OctalInteger = sort_list_random([
        "00",
        "01",
        "07",
        "010",
        "0321"
        "01234"
        "054321"
        "0123456"
        "01234567"
    ]),
    OctalInteger_Length = length(OctalInteger),
    insert_table(octal_integer, OctalInteger),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses octal_integer ~n", [OctalInteger_Length])),
    % --------------------------------------------------------------------------
    % RegularDecimalReal = ({ Digit } | DecimalInteger), '.', (DigitString | DecimalInteger) ;
    % --------------------------------------------------------------------------
    RegularDecimalReal = sort_list_random([".0", ".12", "0.0", "0.12", "12.0", "123.654"]),
    RegularDecimalReal_Length = length(RegularDecimalReal),
    insert_table(regular_decimal_real, RegularDecimalReal),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses regular_decimal_real ~n", [RegularDecimalReal_Length])),
    % --------------------------------------------------------------------------
    % StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
    %               | ("'", { ANY - ("'" | '\') | EscapedChar }, "'") ;
    % --------------------------------------------------------------------------
    StringLiteral = sort_list_random([
        "\\\"d_str\\\"",
        "'s_str'"
    ]),
    StringLiteral_Length = length(StringLiteral),
    insert_table(string_literal, StringLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses string_literal ~n", [StringLiteral_Length])),
    % --------------------------------------------------------------------------
    % UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;
    % --------------------------------------------------------------------------
    UnescapedSymbolicName = sort_list_random([
        "usn1",
        "usn2",
        "_usn3",
        "_usn4",
        "@usn5",
        "@usn6",
        "#usn7",
        "#usn8",
        "usna",
        "usnb",
        "_usnc",
        "_usnd",
        "@usne",
        "@usnf",
        "#usng"
    ]),
    UnescapedSymbolicName_Length = length(UnescapedSymbolicName),
    insert_table(unescaped_symbolic_name, UnescapedSymbolicName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses unescaped_symbolic_name ~n", [UnescapedSymbolicName_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % DoubleLiteral = ExponentDecimalReal
    %               | RegularDecimalReal ;
    % --------------------------------------------------------------------------
    DoubleLiteral = sort_list_random(sets:to_list(sets:from_list(lists:append([
        ExponentDecimalReal,
        RegularDecimalReal
    ])))),
    DoubleLiteral_Length = length(DoubleLiteral),
    insert_table(double_literal, DoubleLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses double_literal ~n", [DoubleLiteral_Length])),
    % --------------------------------------------------------------------------
    % IntegerLiteral = HexInteger
    %                | OctalInteger
    %                | DecimalInteger ;
    % --------------------------------------------------------------------------
    IntegerLiteral = sort_list_random(sets:to_list(sets:from_list(lists:append([
        DecimalInteger,
        HexInteger,
        OctalInteger
    ])))),
    IntegerLiteral_Length = length(IntegerLiteral),
    insert_table(integer_literal, IntegerLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses integer_literal ~n", [IntegerLiteral_Length])),
    % --------------------------------------------------------------------------
    % SymbolicName = UnescapedSymbolicName
    %              | EscapedSymbolicName ;
    % --------------------------------------------------------------------------
    SymbolicName = sort_list_random(sets:to_list(sets:from_list(lists:append([
        EscapedSymbolicName,
        UnescapedSymbolicName
    ])))),
    SymbolicName_Length = length(SymbolicName),
    insert_table(symbolic_name, SymbolicName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses symbolic_name ~n", [SymbolicName_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % FunctionName = SymbolicName ;
    % --------------------------------------------------------------------------
    FunctionName = sort_list_random(sets:to_list(sets:from_list(lists:append([
        SymbolicName,
        ["count"]
    ])))),
    FunctionName_Length = length(FunctionName),
    insert_table(function_name, FunctionName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses function_name ~n", [FunctionName_Length])),
    %---------------------------------------------------------------------------
    % LiteralIds = IntegerLiteral, { [SP], ',', [SP], IntegerLiteral } ;
    %---------------------------------------------------------------------------
    LiteralIds = sort_list_random(
        [
                IL ++
                case rand:uniform(?PRIME) rem 4 of
                    1 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                        lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral);
                    2 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                        lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++
                        lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral);
                    3 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                        lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++
                        lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++
                        lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral);
                    _ -> []
                end
            || IL <- IntegerLiteral
        ]),
    LiteralIds_Length = length(LiteralIds),
    insert_table(literal_ids, LiteralIds),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses literal_ids ~n", [LiteralIds_Length])),
    % --------------------------------------------------------------------------
    % LabelName = SymbolicName ;
    % --------------------------------------------------------------------------
    LabelName = SymbolicName,
    LabelName_Length = length(LabelName),
    insert_table(label_name, LabelName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses label_name ~n", [LabelName_Length])),
    % --------------------------------------------------------------------------
    % NumberLiteral = DoubleLiteral
    %               | IntegerLiteral ;
    % --------------------------------------------------------------------------
    NumberLiteral = sort_list_random(sets:to_list(sets:from_list(lists:append([
        DoubleLiteral,
        IntegerLiteral
    ])))),
    NumberLiteral_Length = length(NumberLiteral),
    insert_table(number_literal, NumberLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses number_literal ~n", [NumberLiteral_Length])),
    % --------------------------------------------------------------------------
    % Parameter = '$', (SymbolicName | DecimalInteger) ;
    % --------------------------------------------------------------------------
    Parameter = sort_list_random(sets:to_list(sets:from_list(lists:append([
        ["$" ++ SN || SN <- SymbolicName],
        ["$" ++ SN || SN <- SymbolicName]
    ])))),
    Parameter_Length = length(Parameter),
    insert_table(parameter, Parameter),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses parameter ~n", [Parameter_Length])),
    % --------------------------------------------------------------------------
    % PropertyKeyName = SymbolicName ;
    % --------------------------------------------------------------------------
    PropertyKeyName = SymbolicName,
    PropertyKeyName_Length = length(PropertyKeyName),
    insert_table(property_key_name, PropertyKeyName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses property_key_name ~n", [PropertyKeyName_Length])),
    % --------------------------------------------------------------------------
    % RangeLiteral = '*', [SP], [IntegerLiteral, [SP]], ['..', [SP], [IntegerLiteral, [SP]]] ;
    % --------------------------------------------------------------------------
    RangeLiteral = sort_list_random(
        ["*", "*.."] ++
        [
                ?SP ++ case rand:uniform(?PRIME) rem 4 of
                           1 -> "*" ++ IL ++ ?SP_OPT ++ ".." ++ ?SP_OPT ++
                               lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral) ++ ?SP_OPT;
                           2 -> "*" ++ IL ++ ?SP_OPT ++ ".." ++ ?SP_OPT;
                           3 -> "*" ++ IL ++ ?SP_OPT;
                           _ -> "*" ++ ".." ++ ?SP_OPT ++ IL ++ ?SP_OPT
                       end
            || IL <- IntegerLiteral
        ]),
    RangeLiteral_Length = length(RangeLiteral),
    insert_table(range_literal, RangeLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses range_literal ~n", [RangeLiteral_Length])),
    % --------------------------------------------------------------------------
    % RelTypeName = SymbolicName ;
    % --------------------------------------------------------------------------
    RelTypeName = SymbolicName,
    RelTypeName_Length = length(RelTypeName),
    insert_table(rel_type_name, RelTypeName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses rel_type_name ~n", [RelTypeName_Length])),
    % --------------------------------------------------------------------------
    % Variable = SymbolicName ;
    % --------------------------------------------------------------------------
    Variable = SymbolicName,
    Variable_Length = length(Variable),
    insert_table(variable, Variable),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses variable ~n", [Variable_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Literal_Part_1 = NumberLiteral
    %      | StringLiteral
    %      | BooleanLiteral
    %      | (N,U,L,L)    
    %      | ...
    % --------------------------------------------------------------------------
    Literal_Part_1 = sort_list_random(sets:to_list(sets:from_list(lists:append([
        AtomNull,
        BooleanLiteral,
        NumberLiteral,
        StringLiteral
    ])))),
    Literal_Part_1_Length = length(Literal_Part_1),
    insert_table(literal_part_1, Literal_Part_1),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses literal (1) ~n", [Literal_Part_1_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Atom = Literal
    %      | Parameter
    %      | ...
    %      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
    %      | ...
    %      | Variable ;
    % --------------------------------------------------------------------------
    Atom_Part_1 = sort_list_random(sets:to_list(sets:from_list(lists:append([
        AtomCount,
        Literal_Part_1,
        Parameter,
        Variable
    ])))),
    Atom_Part_1_Length = length(Atom_Part_1),
    insert_table(atom_part_1, Atom_Part_1),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses atom (1) ~n", [Atom_Part_1_Length])),
    % --------------------------------------------------------------------------
    % NodeLabel = ':', [SP], LabelName ;
    % --------------------------------------------------------------------------
    NodeLabel = sort_list_random([
            ":" ++ ?SP_OPT ++ LN
        || LN <- LabelName
    ]),
    NodeLabel_Length = length(NodeLabel),
    insert_table(node_label, NodeLabel),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses node_label ~n", [NodeLabel_Length])),
    % --------------------------------------------------------------------------
    % PropertyLookup = '.', [SP], (PropertyKeyName) ;
    % --------------------------------------------------------------------------
    PropertyLookup =
        sort_list_random([
                ?SP_OPT ++ "." ++ ?SP_OPT ++ PK
            || PK <- PropertyKeyName
        ]),
    PropertyLookup_Length = length(PropertyLookup),
    insert_table(property_lookup, PropertyLookup),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses property_lookup ~n", [PropertyLookup_Length])),
    % --------------------------------------------------------------------------
    % RelationshipTypes = ':', [SP], RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;
    % --------------------------------------------------------------------------
    RelationshipTypes = sort_list_random([
            ":" ++ ?SP_OPT ++ RTN ++
            case rand:uniform(?PRIME) rem 3 of
                1 ->
                    ?SP_OPT ++ "|" ++ ":" ++ ?SP_OPT ++ lists:nth(rand:uniform(RelTypeName_Length), RelTypeName);
                2 ->
                    ?SP_OPT ++ "|" ++ ?SP_OPT ++ lists:nth(rand:uniform(RelTypeName_Length), RelTypeName);
                _ -> []
            end
        || RTN <- RelTypeName
    ]),
    RelationshipTypes_Length = length(RelationshipTypes),
    insert_table(relationship_types, RelationshipTypes),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses relationship_types ~n", [RelationshipTypes_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % NodeLabels = NodeLabel, { [SP], NodeLabel } ;
    % --------------------------------------------------------------------------
    NodeLabels = sort_list_random([
            NL ++
            case rand:uniform(?PRIME) rem 2 of
                1 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(NodeLabel_Length), NodeLabel);
                _ -> []
            end
        || NL <- NodeLabel]),
    NodeLabels_Length = length(NodeLabels),
    insert_table(node_labels, NodeLabels),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses node_labels ~n", [NodeLabels_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Expression = OrExpression ;
    % --------------------------------------------------------------------------
    Expression_Part_1 = sort_list_random(create_code_expression(?MAX_RULE_EXPRESSION, Atom_Part_1, NodeLabels, PropertyLookup)),
    Expression_Part_1_Length = length(Expression_Part_1),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (1) ~n", [Expression_Part_1_Length])),
    % --------------------------------------------------------------------------
    % Where = (W,H,E,R,E), SP, Expression ;
    % --------------------------------------------------------------------------
    Where = sort_list_random([
            "Where" ++ ?SP ++ E
        || E <- Expression_Part_1
    ]),
    Where_Length = length(Where),
    insert_table(where, Where),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses where ~n", [Where_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;
    % --------------------------------------------------------------------------
    FunctionInvocation = sort_list_random([
            lists:nth(rand:uniform(FunctionName_Length), FunctionName) ++ ?SP_OPT ++
            "(" ++ ?SP_OPT ++
            case rand:uniform(?PRIME) rem 6 of
                1 -> "Distinct" ++ ?SP ++
                    lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT ++
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT;
                2 -> "Distinct" ++ ?SP ++
                    lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                3 -> "Distinct";
                4 ->
                    lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT ++
                        "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT;
                5 ->
                    lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                _ -> []
            end ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    FunctionInvocation_Length = length(FunctionInvocation),
    insert_table(function_invocation, FunctionInvocation),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses function_invocation ~n", [FunctionInvocation_Length])),
    % --------------------------------------------------------------------------
    % IdInColl = Variable, SP, (I,N), SP, Expression ;
    % --------------------------------------------------------------------------
    IdInColl_Targ = max(Variable_Length, 20),
    IdInColl = sort_list_random([
            lists:nth(rand:uniform(Variable_Length), Variable) ++
            ?SP ++ "In" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1)
        || _ <- lists:seq(1, IdInColl_Targ)
    ]),
    IdInColl_Length = length(IdInColl),
    insert_table(id_in_coll, IdInColl),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses id_in_coll ~n", [IdInColl_Length])),
    % --------------------------------------------------------------------------
    % ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;
    % --------------------------------------------------------------------------
    ListLiteral = sort_list_random([
            "[" ++ ?SP_OPT ++
            lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT ++
            case rand:uniform(?PRIME) rem 3 of
                1 ->
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT ++
                        "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT;
                2 ->
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT;
                _ -> []
            end ++
            "]"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    ListLiteral_Length = length(ListLiteral),
    insert_table(list_literal, ListLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses list_literal ~n", [ListLiteral_Length])),
    % --------------------------------------------------------------------------
    % MapLiteral = '{', [SP], [PropertyKeyName, [SP], ':', [SP], Expression, [SP], { ',', [SP], PropertyKeyName, [SP], ':', [SP], Expression, [SP] }], '}' ;
    % --------------------------------------------------------------------------
    MapLiteral = sort_list_random([
            "{" ++ ?SP_OPT ++ "}"
    ] ++
    [
            "{" ++ ?SP_OPT ++ lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName) ++
            ?SP_OPT ++ ":" ++ ?SP_OPT ++
            lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT ++
            case rand:uniform(?PRIME) rem 2 of
                1 ->
                    "," ++ ?SP_OPT ++
                        lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName) ++
                        ?SP_OPT ++ ":" ++ ?SP_OPT ++
                        lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT;
                _ -> []
            end ++
            "}"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    MapLiteral_Length = length(MapLiteral),
    insert_table(map_literal, MapLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses map_literal ~n", [MapLiteral_Length])),
    % --------------------------------------------------------------------------
    % ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;
    % --------------------------------------------------------------------------
    ParenthesizedExpression = sort_list_random([
            "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
            ?SP_OPT ++ ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    ParenthesizedExpression_Length = length(ParenthesizedExpression),
    insert_table(parenthesized_expression, ParenthesizedExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses parenthesized_expression ~n", [ParenthesizedExpression_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % FilterExpression = IdInColl, [[SP], Where] ;
    % --------------------------------------------------------------------------
    % wwe ???
    % FilterExpression = IdInColl, [SP, Where] ;
    % --------------------------------------------------------------------------
    FilterExpression = sort_list_random([
            lists:nth(rand:uniform(IdInColl_Length), IdInColl) ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    FilterExpression_Length = length(FilterExpression),
    insert_table(filter_expression, FilterExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses filter_expression ~n", [FilterExpression_Length])),
    % --------------------------------------------------------------------------
    % Literal = ...
    %         | MapLiteral
    %         | ListLiteral
    %         ;
    % --------------------------------------------------------------------------
    Literal = sort_list_random(sets:to_list(sets:from_list(lists:append([
%% wwe    ListLiteral,
        Literal_Part_1,
        MapLiteral
    ])))),
    Literal_Length = length(Literal),
    insert_table(literal, Literal),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses literal ~n", [Literal_Length])),
    % --------------------------------------------------------------------------
    % Properties = MapLiteral
    %            | Parameter ;
    % --------------------------------------------------------------------------
    Properties = sort_list_random(sets:to_list(sets:from_list(lists:append([
        MapLiteral,
        Parameter
    ])))),
    Properties_Length = length(Properties),
    insert_table(properties, Properties),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses properties ~n", [Properties_Length])),
    % --------------------------------------------------------------------------
    % Reduce = (R,E,D,U,C,E), [SP], '(', Variable, '=', Expression, ',', IdInColl, '|', Expression, ')' ;
    % --------------------------------------------------------------------------
    Reduce = sort_list_random([
            "Reduce" ++ ?SP_OPT ++ "(" ++
            lists:nth(rand:uniform(Variable_Length), Variable) ++
            "=" ++
            lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
            "," ++
            lists:nth(rand:uniform(IdInColl_Length), IdInColl) ++
            "|" ++
            lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    Reduce_Length = length(Reduce),
    insert_table(reduce, Reduce),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses reduce ~n", [Reduce_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % ListComprehension = '[', FilterExpression, [[SP], '|', Expression], ']' ;
    % --------------------------------------------------------------------------
    ListComprehension = sort_list_random([
            "[" ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++
            case rand:uniform(?PRIME) rem 2 of
                1 ->
                    ?SP_OPT ++ "|" ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                _ -> []
            end ++
            "]"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    ListComprehension_Length = length(ListComprehension),
    insert_table(list_comprehension, ListComprehension),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses list_comprehension ~n", [ListComprehension_Length])),
    % --------------------------------------------------------------------------
    % NodePattern = '(', [SP], [Variable, [SP]], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
    % --------------------------------------------------------------------------
    % wwe ???
    % NodePattern = '(', [SP], [Variable, SP], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
    % --------------------------------------------------------------------------
    NodePattern = sort_list_random([
            "(" ++ ?SP_OPT ++
            case rand:uniform(?PRIME) rem 8 of
                1 ->
                    lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP ++
                        lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT ++
                        lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                2 ->
                    lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP ++
                        lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT;
                3 ->
                    lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP ++
                        lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                4 -> lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP;
                5 ->
                    lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT ++
                        lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                6 ->
                    lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT;
                7 ->
                    lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                _ -> []
            end ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    NodePattern_Length = length(NodePattern),
    insert_table(node_pattern, NodePattern),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses node_pattern ~n", [NodePattern_Length])),
    % --------------------------------------------------------------------------
    % RelationshipDetail = '[', [SP], [Variable, [SP]], [RelationshipTypes, [SP]], [RangeLiteral], [Properties, [SP]], ']' ;
    % --------------------------------------------------------------------------
    RelationshipDetail = sort_list_random(
        [
%% wwe            "[]" ++
%% wwe            "[ ]" ++
                "[" ++ ?SP_OPT ++
                case rand:uniform(?PRIME) rem 15 of
                    1 ->
                        lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                    2 ->
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++ ?SP_OPT;
                    3 ->
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                            lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                    4 ->
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT;
                    5 ->
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                    6 ->
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++ ?SP_OPT;
                    7 ->
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                            lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                    8 ->
                        lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT;
                    9 ->
                        lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                    10 ->
                        lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++ ?SP_OPT;
                    11 ->
                        lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                            lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                    12 ->
                        lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT;
                    13 ->
                        lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                    14 ->
                        lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++ ?SP_OPT;
                    _ ->
                        lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT ++
                            lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                            lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT
                end ++
                "]"
            || _ <- lists:seq(1, ?MAX_RULE_ATOM)
        ]),
    RelationshipDetail_Length = length(RelationshipDetail),
    insert_table(relationship_detail, RelationshipDetail),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses relationship_detail ~n", [RelationshipDetail_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % RelationshipPattern = (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
    %                     | (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash)
    %                     | (                     Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
    %                     | (                     Dash, [SP], [RelationshipDetail], [SP], Dash) ;
    % --------------------------------------------------------------------------
    RelationshipPattern = sort_list_random([
        case rand:uniform(?PRIME) rem 4 of
            1 -> ?LEFT_ARROW_HEAD ++ ?SP_OPT ++
                ?DASH ++ ?SP_OPT ++
                lists:nth(rand:uniform(RelationshipDetail_Length), RelationshipDetail) ++
                ?SP_OPT ++ ?DASH ++
                ?SP_OPT ++ ?RIGHT_ARROW_HEAD;
            2 -> ?LEFT_ARROW_HEAD ++ ?SP_OPT ++
                ?DASH ++ ?SP_OPT ++
                lists:nth(rand:uniform(RelationshipDetail_Length), RelationshipDetail) ++
                ?SP_OPT ++ ?DASH;
            3 -> ?DASH ++ ?SP_OPT ++
                lists:nth(rand:uniform(RelationshipDetail_Length), RelationshipDetail) ++
                ?SP_OPT ++ ?DASH ++
                ?SP_OPT ++ ?RIGHT_ARROW_HEAD;
            _ -> ?DASH ++ ?SP_OPT ++
                lists:nth(rand:uniform(RelationshipDetail_Length), RelationshipDetail) ++
                ?SP_OPT ++ ?DASH
        end
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    RelationshipPattern_Length = length(RelationshipPattern),
    insert_table(relationship_pattern, RelationshipPattern),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses relationship_pattern ~n", [RelationshipPattern_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 12
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % PatternElementChain = RelationshipPattern, [SP], NodePattern ;
    % --------------------------------------------------------------------------
    PatternElementChain = sort_list_random([
            lists:nth(rand:uniform(RelationshipPattern_Length), RelationshipPattern) ++
            ?SP_OPT ++
            lists:nth(rand:uniform(NodePattern_Length), NodePattern)
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    PatternElementChain_Length = length(PatternElementChain),
    insert_table(pattern_element_chain, PatternElementChain),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses pattern_element_chain ~n", [PatternElementChain_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 13
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % PatternElement = (NodePattern, { [SP], PatternElementChain })
    %                | ('(', PatternElement, ')') ;
    % --------------------------------------------------------------------------
    PatternElement = sort_list_random([
        case rand:uniform(?PRIME) rem 7 of
            1 -> "(" ++
                "(" ++
                lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                ")" ++
                ")";
            2 -> "(" ++
                lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                ")";
            3 -> "(" ++
                lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                ")";
            4 -> "(" ++
                lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
                ")";
            5 -> lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain);
            6 -> lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain);
            _ -> lists:nth(rand:uniform(NodePattern_Length), NodePattern)
        end
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    PatternElement_Length = length(PatternElement),
    insert_table(pattern_element, PatternElement),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses pattern_element ~n", [PatternElement_Length])),
    % --------------------------------------------------------------------------
    % RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;
    % --------------------------------------------------------------------------
    RelationshipsPattern = sort_list_random([
            lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
            case rand:uniform(?PRIME) rem 2 of
                1 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                        ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain);
                _ ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain)
            end
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    RelationshipsPattern_Length = length(RelationshipsPattern),
    insert_table(relationships_pattern, RelationshipsPattern),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses relationships_pattern ~n", [RelationshipsPattern_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 14
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [(W,H,E,R,E), [SP], Expression, [SP]], '|', [SP], Expression, [SP], ']' ;
    % --------------------------------------------------------------------------
    PatternComprehension = sort_list_random([
        case rand:uniform(?PRIME) rem 4 of
            1 -> "[" ++
                ?SP_OPT ++ lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern) ++
                ?SP_OPT ++ "|" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                ?SP_OPT ++ "]";
            2 -> "[" ++
                ?SP_OPT ++ lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern) ++
                ?SP_OPT ++ " where " ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                ?SP_OPT ++ "|" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                ?SP_OPT ++ "]";
            3 -> "[" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP_OPT ++ "=" ++
                ?SP_OPT ++ lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern) ++
                ?SP_OPT ++ "|" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                ?SP_OPT ++ "]";
            _ -> "[" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP_OPT ++ "=" ++
                ?SP_OPT ++ lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern) ++
                ?SP_OPT ++ " where " ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                ?SP_OPT ++ "|" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                ?SP_OPT ++ "]"
        end
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    PatternComprehension_Length = length(PatternComprehension),
    insert_table(pattern_element, PatternComprehension),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses pattern_comprehension ~n", [PatternComprehension_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 15
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % AnonymousPatternPart = PatternElement ;
    % --------------------------------------------------------------------------
    AnonymousPathPattern = sort_list_random(PatternElement),
    AnonymousPathPattern_Length = length(AnonymousPathPattern),
    insert_table(anonymous_path_pattern, AnonymousPathPattern),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses anonymous_path_pattern ~n", [AnonymousPathPattern_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 50
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Atom = ...
    %      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ((E,X,T,R,A,C,T), [SP], '(', [SP], FilterExpression, [SP], [[SP], '|', Expression], ')')
    %      | ...
    %      | ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ...
    % --------------------------------------------------------------------------
    AtomFilter = sort_list_random([
            "Filter" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    AtomFilter_Length = length(AtomFilter),
    insert_table(atom_filter, AtomFilter),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses atom (filter) ~n", [AtomFilter_Length])),

    AtomExtract = sort_list_random([
            "Extract" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> ?SP_OPT ++ "|" ++
                    lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                _ -> []
            end ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    AtomExtract_Length = length(AtomExtract),
    insert_table(atom_extract, AtomExtract),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses atom (extract) ~n", [AtomExtract_Length])),

    AtomAll = sort_list_random([
            "All" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    AtomAll_Length = length(AtomAll),
    insert_table(atom_all, AtomAll),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses atom (all) ~n", [AtomAll_Length])),

    AtomAny = sort_list_random([
            "Any" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    AtomAny_Length = length(AtomAny),
    insert_table(atom_any, AtomAny),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses atom (any) ~n", [AtomAny_Length])),

    AtomNone = sort_list_random([
            "None" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    AtomNone_Length = length(AtomNone),
    insert_table(atom_none, AtomNone),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses atom (none) ~n", [AtomNone_Length])),

    AtomSingle = sort_list_random([
            "Single" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    AtomSingle_Length = length(AtomSingle),
    insert_table(atom_single, AtomSingle),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses atom (single) ~n", [AtomSingle_Length])),
    % --------------------------------------------------------------------------
    % Atom = Literal
    %      | ...
    %      | CaseExpression
    %      | ...
    %      | ListComprehension
    %      | PatternComprehension
    %      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ((E,X,T,R,A,C,T), [SP], '(', [SP], FilterExpression, [SP], [[SP], '|', Expression], ')')
    %      | Reduce
    %      | ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
    %      | ShortestPathPattern
    %      | RelationshipsPattern
    %      | ParenthesizedExpression
    %      | FunctionInvocation
    %      | ...
    % --------------------------------------------------------------------------
    Atom = sort_list_random(sets:to_list(sets:from_list(lists:append([
        Atom_Part_1,
        Literal,
        ListComprehension,
        PatternComprehension,
        AtomFilter,
        AtomExtract,
        AtomAll,
        AtomAny,
        AtomNone,
        AtomSingle,
        RelationshipsPattern,
%%        ParenthesizedExpression,
        FunctionInvocation
    ])))),
    Atom_Length = length(Atom),
    insert_table(atom, Atom),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses atom ~n", [Atom_Length])),
    % --------------------------------------------------------------------------
    % Expression = OrExpression ;
    % --------------------------------------------------------------------------
    Expression = sort_list_random(create_code_expression(?MAX_RULE_EXPRESSION * 2, Atom, NodeLabels, PropertyLookup)),
    Expression_Length = length(Expression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression ~n", [Expression_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 51
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Limit = (L,I,M,I,T), SP, Expression ;
    % --------------------------------------------------------------------------
    Limit_Targ = Expression_Length,
    Limit = sort_list_random([
            "Limit" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression)
        || _ <- lists:seq(1, Limit_Targ)
    ]),
    Limit_Length = length(Limit),
    insert_table(limit, Limit),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses limit ~n", [Limit_Length])),
    % --------------------------------------------------------------------------
    % PatternPart = (Variable, [SP], '=', [SP], AnonymousPatternPart)
    %             | AnonymousPatternPart ;
    % --------------------------------------------------------------------------
    PatternPart_Targ = AnonymousPathPattern_Length,
    PatternPart = sort_list_random([
        case rand:uniform(?PRIME) rem 2 of
            1 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP_OPT ++ "=" ++ ?SP_OPT ++
                lists:nth(rand:uniform(AnonymousPathPattern_Length), AnonymousPathPattern);
            _ ->
                lists:nth(rand:uniform(AnonymousPathPattern_Length), AnonymousPathPattern)
        end
        || _ <- lists:seq(1, PatternPart_Targ)
    ]),
    PatternPart_Length = length(PatternPart),
    insert_table(pattern_part, PatternPart),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses pattern_part ~n", [PatternPart_Length])),
    % --------------------------------------------------------------------------
    % PropertyExpression = Atom, { [SP], PropertyLookup }- ;
    % --------------------------------------------------------------------------
    PropertyExpression_Targ = max(Atom_Length, PropertyLookup_Length),
    PropertyExpression = sort_list_random([
            lists:nth(rand:uniform(Atom_Length), Atom) ++
            case rand:uniform(?PRIME) rem 3 of
                3 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                        ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                        ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                2 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                        ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                _ ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup)
            end
        || _ <- lists:seq(1, PropertyExpression_Targ)
    ]),
    PropertyExpression_Length = length(PropertyExpression),
    insert_table(property_expression, PropertyExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses property_expression ~n", [PropertyExpression_Length])),
    % --------------------------------------------------------------------------
    % RemoveItem = (Variable, NodeLabels)
    %            | PropertyExpression ;
    % --------------------------------------------------------------------------
    RemoveItem_Targ = max(NodeLabels_Length, PropertyExpression_Length),
    RemoveItem = sort_list_random([
        case rand:uniform(?PRIME) rem 2 of
            1 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
            lists:nth(rand:uniform(NodeLabels_Length), NodeLabels);
            _ ->
                lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression)
        end
        || _ <- lists:seq(1, RemoveItem_Targ)
    ]),
    RemoveItem_Length = length(RemoveItem),
    insert_table(remove_item, RemoveItem),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses remove_item ~n", [RemoveItem_Length])),
    % --------------------------------------------------------------------------
    % ReturnItem = (Expression, SP, (A,S), SP, Variable)
    %            | Expression ;
    % --------------------------------------------------------------------------
    ReturnItem_Targ = max(Expression_Length, Variable_Length),
    ReturnItem = sort_list_random([
            lists:nth(rand:uniform(Expression_Length), Expression) ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> ?SP ++ "As" ++ ?SP ++
                    lists:nth(rand:uniform(Variable_Length), Variable);
                _ -> []
            end
        || _ <- lists:seq(1, ReturnItem_Targ)
    ]),
    ReturnItem_Length = length(ReturnItem),
    insert_table(return_item, ReturnItem),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses return_item ~n", [ReturnItem_Length])),
    % --------------------------------------------------------------------------
    % SetItem = (PropertyExpression, [SP], '=', [SP], Expression)
    %         | (Variable, [SP], '=', [SP], Expression)
    %         | (Variable, [SP], '+=', [SP], Expression)
    %         | (Variable, [SP], NodeLabels) ;
    % --------------------------------------------------------------------------
    SetItem_Targ = max(Expression_Length, max(NodeLabels_Length, max(PropertyExpression_Length, Variable_Length))),
    SetItem = sort_list_random([
        case rand:uniform(?PRIME) rem 4 of
            1 ->
                lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression) ++
                    ?SP_OPT ++ "=" ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression_Length), Expression);
            2 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP_OPT ++ "=" ++ ?SP_OPT ++
                lists:nth(rand:uniform(Expression_Length), Expression);
            3 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP_OPT ++ "+=" ++ ?SP_OPT ++
                lists:nth(rand:uniform(Expression_Length), Expression);
            _ ->
                lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                    lists:nth(rand:uniform(NodeLabels_Length), NodeLabels)
        end
        || _ <- lists:seq(1, SetItem_Targ)
    ]),
    SetItem_Length = length(SetItem),
    insert_table(set_item, SetItem),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses set_item ~n", [SetItem_Length])),
    % --------------------------------------------------------------------------
    % Skip = (S,K,I,P), SP, Expression ;
    % --------------------------------------------------------------------------
    Skip_Targ = Expression_Length, Variable_Length,
    Skip = sort_list_random([
            "Skip" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression)
        || _ <- lists:seq(1, Skip_Targ)
    ]),
    Skip_Length = length(Skip),
    insert_table(skip, Skip),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses skip ~n", [Skip_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 52
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Pattern = PatternPart, { [SP], ',', [SP], PatternPart } ;
    % --------------------------------------------------------------------------
    Pattern_Targ = PatternPart_Length,
    Pattern = sort_list_random([
            lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
            case rand:uniform(?PRIME) rem 4 of
                1 ->
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(PatternPart_Length), PatternPart);
                2 ->
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(PatternPart_Length), PatternPart);
                3 ->
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(PatternPart_Length), PatternPart);
                _ -> []
            end
        || _ <- lists:seq(1, Pattern_Targ)
    ]),
    Pattern_Length = length(Pattern),
    insert_table(pattern, Pattern),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses pattern ~n", [Pattern_Length])),
    % --------------------------------------------------------------------------
    % ReturnItems = ('*', { [SP], ',', [SP], ReturnItem })
    %             | (ReturnItem, { [SP], ',', [SP], ReturnItem }) ;
    % --------------------------------------------------------------------------
    ReturnItems_Targ = ReturnItem_Length,
    ReturnItems = sort_list_random([
        "*"
    ] ++
    [
        case rand:uniform(?PRIME) rem 5 of
            1 -> lists:nth(rand:uniform(ReturnItem_Length), ReturnItem) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(ReturnItem_Length), ReturnItem) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(ReturnItem_Length), ReturnItem);
            2 -> "*" ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(ReturnItem_Length), ReturnItem) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(ReturnItem_Length), ReturnItem);
            3 -> lists:nth(rand:uniform(ReturnItem_Length), ReturnItem) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(ReturnItem_Length), ReturnItem);
            4 -> "*" ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(ReturnItem_Length), ReturnItem);
            _ -> lists:nth(rand:uniform(ReturnItem_Length), ReturnItem)
        end
        || _ <- lists:seq(1, ReturnItems_Targ)
    ]),
    ReturnItems_Length = length(ReturnItems),
    insert_table(return_items, ReturnItems),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses return_items ~n", [ReturnItems_Length])),
    % --------------------------------------------------------------------------
    % SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;
    % --------------------------------------------------------------------------
    % wwe ???
    % SortItem = Expression, [SP, ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;
    % --------------------------------------------------------------------------
    SortItem_Targ = Expression_Length,
    SortItem = sort_list_random([
            lists:nth(rand:uniform(Expression_Length), Expression) ++
            case rand:uniform(?PRIME) rem 4 of
                1 -> ?SP ++ "Descending";
                2 -> ?SP ++ "Desc";
                3 -> ?SP ++ "Ascending";
                _ -> ?SP ++ "Asc"
            end
        || _ <- lists:seq(1, SortItem_Targ)
    ]),
    SortItem_Length = length(SortItem),
    insert_table(sort_item, SortItem),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses sort_item ~n", [SortItem_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 53
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Order = (O,R,D,E,R), SP, (B,Y), SP, SortItem, { ',', [SP], SortItem } ;
    % --------------------------------------------------------------------------
    Order_Targ = SortItem_Length,
    Order = sort_list_random([
            "Order" ++ ?SP ++ "By" ++ ?SP ++
            lists:nth(rand:uniform(SortItem_Length), SortItem) ++
            case rand:uniform(?PRIME) rem 3 of
                1 ->
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem) ++
                        "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem);
                2 ->
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem);
                _ -> []
            end
        || _ <- lists:seq(1, Order_Targ)
    ]),
    Order_Length = length(Order),
    insert_table(order, Order),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses order ~n", [Order_Length])),
    % --------------------------------------------------------------------------
    % ReturnBody = ReturnItems, [SP, Order], [SP, Skip], [SP, Limit] ;
    % --------------------------------------------------------------------------
    ReturnBody_Targ = max(Limit_Length, max(Order_Length, max(ReturnItems_Length, Skip_Length))),
    ReturnBody = sort_list_random([
            lists:nth(rand:uniform(ReturnItems_Length), ReturnItems) ++
            case rand:uniform(?PRIME) rem 8 of
                1 -> ?SP ++ lists:nth(rand:uniform(Order_Length), Order) ++
                    ?SP ++ lists:nth(rand:uniform(Skip_Length), Skip) ++
                    ?SP ++ lists:nth(rand:uniform(Limit_Length), Limit);
                2 -> ?SP ++ lists:nth(rand:uniform(Order_Length), Order) ++
                    ?SP ++ lists:nth(rand:uniform(Skip_Length), Skip);
                3 -> ?SP ++ lists:nth(rand:uniform(Order_Length), Order) ++
                    ?SP ++ lists:nth(rand:uniform(Limit_Length), Limit);
                4 -> ?SP ++ lists:nth(rand:uniform(Order_Length), Order);
                5 -> ?SP ++ lists:nth(rand:uniform(Skip_Length), Skip) ++
                    ?SP ++ lists:nth(rand:uniform(Limit_Length), Limit);
                6 -> ?SP ++ lists:nth(rand:uniform(Skip_Length), Skip);
                7 -> ?SP ++ lists:nth(rand:uniform(Limit_Length), Limit);
                _ -> []
            end
        || _ <- lists:seq(1, ReturnBody_Targ)
    ]),
    ReturnBody_Length = length(ReturnBody),
    insert_table(return_body, ReturnBody),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses return_body ~n", [ReturnBody_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 90
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Create = (C,R,E,A,T,E), [SP], Pattern ;
    % --------------------------------------------------------------------------
    % wwe ???
    % Create = (C,R,E,A,T,E), SP, Pattern ;
    % --------------------------------------------------------------------------
    Create = sort_list_random([
            "Create" ++ ?SP ++
            lists:nth(rand:uniform(Pattern_Length), Pattern)
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    Create_Length = length(Create),
    insert_table(create, Create),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses create ~n", [Create_Length])),
    % --------------------------------------------------------------------------
    % Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;
    % --------------------------------------------------------------------------
    Delete = sort_list_random([
        case rand:uniform(?PRIME) rem 6 of
            1 -> "Detach" ++ ?SP ++
                "Delete" ++ ?SP ++
                lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression);
            2 -> "Detach" ++ ?SP ++
                "Delete" ++ ?SP ++
                lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression);
            3 -> "Detach" ++ ?SP ++
                "Delete" ++ ?SP ++
                lists:nth(rand:uniform(Expression_Length), Expression);
            4 -> "Delete" ++ ?SP ++
                lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression);
            5 -> "Delete" ++ ?SP ++
                lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression);
            _ -> "Delete" ++ ?SP ++
                lists:nth(rand:uniform(Expression_Length), Expression)
        end
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    Delete_Length = length(Delete),
    insert_table(delete, Delete),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses delete ~n", [Delete_Length])),
    % --------------------------------------------------------------------------
    % Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), [SP], Pattern, [[SP], Where] ;
    % --------------------------------------------------------------------------
    % wwe ???
    % Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), SP, Pattern, [SP, Where] ;
    % --------------------------------------------------------------------------
    Match = sort_list_random([
        case rand:uniform(?PRIME) rem 12 of
            1 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            2 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern);
            3 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            4 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern);
            5 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            6 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern);
            7 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            8 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern);
            9 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            10 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern);
            11 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            _ -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern)
        end
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    Match_Length = length(Match),
    insert_table(match, Match),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses match ~n", [Match_Length])),
    % --------------------------------------------------------------------------
    % Remove = (R,E,M,O,V,E), SP, RemoveItem, { [SP], ',', [SP], RemoveItem } ;
    % --------------------------------------------------------------------------
    Remove = sort_list_random([
            "Remove" ++ ?SP ++
            lists:nth(rand:uniform(RemoveItem_Length), RemoveItem) ++
            case rand:uniform(?PRIME) rem 3 of
                1 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                    lists:nth(rand:uniform(RemoveItem_Length), RemoveItem) ++
                    ?SP_OPT ++ "," ++ ?SP_OPT ++
                    lists:nth(rand:uniform(RemoveItem_Length), RemoveItem);
                2 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                    lists:nth(rand:uniform(RemoveItem_Length), RemoveItem);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    Remove_Length = length(Remove),
    insert_table(remove, Remove),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses remove ~n", [Remove_Length])),
    % --------------------------------------------------------------------------
    % Return = (R,E,T,U,R,N), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody ;
    % --------------------------------------------------------------------------
    Return = sort_list_random([
            "Return" ++ ?SP ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> "Distinct" ++ ?SP;
                _ -> []
            end ++
            lists:nth(rand:uniform(ReturnBody_Length), ReturnBody)
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    Return_Length = length(Return),
    insert_table(return, Return),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses return ~n", [Return_Length])),
    % --------------------------------------------------------------------------
    % Set = (S,E,T), [SP], SetItem, { ',', SetItem } ;
    % --------------------------------------------------------------------------
    % wwe ???
    % Set = (S,E,T), SP, SetItem, { ',', SetItem } ;
    % --------------------------------------------------------------------------
    Set = sort_list_random([
            "Set" ++ ?SP ++
            lists:nth(rand:uniform(SetItem_Length), SetItem) ++
            case rand:uniform(?PRIME) rem 3 of
                1 -> "," ++ lists:nth(rand:uniform(SetItem_Length), SetItem) ++
                    "," ++ lists:nth(rand:uniform(SetItem_Length), SetItem);
                2 -> "," ++ lists:nth(rand:uniform(SetItem_Length), SetItem);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    Set_Length = length(Set),
    insert_table(set, Set),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses set ~n", [Set_Length])),
    % --------------------------------------------------------------------------
    % Unwind = (U,N,W,I,N,D), [SP], Expression, SP, (A,S), SP, Variable ;
    % --------------------------------------------------------------------------
    % wwe ???
    % Unwind = (U,N,W,I,N,D), SP, Expression, SP, (A,S), SP, Variable ;
    % --------------------------------------------------------------------------
    Unwind = sort_list_random([
            "Unwind" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression) ++
            ?SP ++ "As" ++ ?SP ++
            lists:nth(rand:uniform(Variable_Length), Variable)
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    Unwind_Length = length(Unwind),
    insert_table(unwind, Unwind),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses unwind ~n", [Unwind_Length])),
    % --------------------------------------------------------------------------
    % With = (W,I,T,H), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody, [[SP], Where] ;
    % --------------------------------------------------------------------------
    With = sort_list_random([
            "With" ++ ?SP ++
            case rand:uniform(?PRIME) rem 4 of
                1 -> "Distinct" ++ ?SP ++
                    lists:nth(rand:uniform(ReturnBody_Length), ReturnBody) ++
                    ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
                2 -> "Distinct" ++ ?SP ++
                    lists:nth(rand:uniform(ReturnBody_Length), ReturnBody);
                3 -> ?SP ++
                    lists:nth(rand:uniform(ReturnBody_Length), ReturnBody) ++
                    ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
                _ -> ?SP ++
                lists:nth(rand:uniform(ReturnBody_Length), ReturnBody)
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    With_Length = length(With),
    insert_table(with, With),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses with ~n", [With_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 91
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Clause = LoadCSV
    %        | Match
    %        | Unwind
    %        | ...
    %        | Create
    %        | CreateUnique
    %        | ...
    %        | Delete
    %        | Remove
    %        | ...
    %        | With
    %        | Return ;
    % --------------------------------------------------------------------------
    Clause_Part_1 = sort_list_random(sets:to_list(sets:from_list(lists:append([
        Match,
        Unwind,
        Create,
        Delete,
        Remove,
        With,
        Return
    ])))),
    Clause_Part_1_Length = length(Clause_Part_1),
    insert_table(clause_part_1, Clause_Part_1),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses clause (1) ~n", [Clause_Part_1_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 92
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % MergeAction = ((O,N), SP, (M,A,T,C,H), SP, Set)
    %             | ((O,N), SP, (C,R,E,A,T,E), SP, Set) ;
    % --------------------------------------------------------------------------
    MergeAction = sort_list_random([
            "On" ++ ?SP ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> "Match" ++ ?SP ++
                    lists:nth(rand:uniform(Set_Length), Set);
                _ -> "Create" ++ ?SP ++
                    lists:nth(rand:uniform(Set_Length), Set)
            end
        || _ <- lists:seq(1, Set_Length)
    ]),
    MergeAction_Length = length(MergeAction),
    insert_table(merge_action, MergeAction),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses merge_action ~n", [MergeAction_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 93
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Merge = (M,E,R,G,E), [SP], PatternPart, { SP, MergeAction } ;
    % --------------------------------------------------------------------------
    % wwe ???
    % Merge = (M,E,R,G,E), SP, PatternPart, { SP, MergeAction } ;
    % --------------------------------------------------------------------------
    Merge = sort_list_random([
            "Merge" ++ ?SP ++
            lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
            case rand:uniform(?PRIME) rem 3 of
                1 ->
                    ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction) ++
                        ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction);
                2 ->
                    ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    Merge_Length = length(Merge),
    insert_table(merge, Merge),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses merge ~n", [Merge_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 94
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Clause = ...
    %        | Merge
    %        | ...
    %        | Foreach
    %        | ...
    % --------------------------------------------------------------------------
    Clause = sort_list_random(sets:to_list(sets:from_list(lists:append([
        Clause_Part_1,
        Merge
    ])))),
    Clause_Length = length(Clause),
    insert_table(clause, Clause),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses clause ~n", [Clause_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 95
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % SingleQuery = Clause, { [SP], Clause } ;
    % --------------------------------------------------------------------------
    % wwe ???
    % SingleQuery = Clause, { SP, Clause } ;
    % --------------------------------------------------------------------------
    SingleQuery = sort_list_random([
            C ++
            case rand:uniform(?PRIME) rem 3 of
                1 -> ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause) ++
                    ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause);
                2 -> ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause);
                _ -> []
            end
        || C <- Clause
    ]),
    SingleQuery_Length = length(SingleQuery),
    insert_table(single_query, SingleQuery),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses single_query ~n", [SingleQuery_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 96
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Union = ((U,N,I,O,N), SP, (A,L,L), [SP], SingleQuery)
    %       | ((U,N,I,O,N), [SP], SingleQuery) ;
    % --------------------------------------------------------------------------
    % wwe ???
    % Union = ((U,N,I,O,N), SP, (A,L,L), SP, SingleQuery)
    %       | ((U,N,I,O,N), SP, SingleQuery) ;
    % --------------------------------------------------------------------------
    Union = sort_list_random([
            "Union" ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> ?SP ++ "All";
                _ -> []
            end ++
            ?SP ++ SQ
        || SQ <- SingleQuery
    ]),
    Union_Length = length(Union),
    insert_table(union, Union),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses union ~n", [Union_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 97
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % RegularQuery = SingleQuery, { [SP], Union } ;
    % --------------------------------------------------------------------------
    % wwe ???
    % RegularQuery = SingleQuery, { SP, Union } ;
    % --------------------------------------------------------------------------
    RegularQuery = sort_list_random([
            SQ ++
            case rand:uniform(?PRIME) rem 3 of
                1 -> ?SP ++ lists:nth(rand:uniform(Union_Length), Union) ++
                    ?SP ++ lists:nth(rand:uniform(Union_Length), Union);
                2 -> ?SP ++ lists:nth(rand:uniform(Union_Length), Union);
                _ -> []
            end
        || SQ <- SingleQuery
    ]),
    RegularQuery_Length = length(RegularQuery),
    insert_table(regular_query, RegularQuery),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses regular_query ~n", [RegularQuery_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 98
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Query = RegularQuery
    %       | BulkImportQuery ;
    % --------------------------------------------------------------------------
    Query_Curr = sort_list_random(RegularQuery),
    Query = case length(Query_Curr) > ?MAX_QUERY of
                true -> lists:sublist(Query_Curr, 1, ?MAX_QUERY);
                _ -> Query_Curr
            end,
    Query_Length = length(Query),
    insert_table(query, Query),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses query ~n", [Query_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 99
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Statement = Command
    %           | Query ;
    % --------------------------------------------------------------------------
    Statement_Curr = sort_list_random(Query),
    Statement = case length(Statement_Curr) > ?MAX_STATEMENT of
                    true -> lists:sublist(Statement_Curr, 1, ?MAX_STATEMENT);
                    _ -> Statement_Curr
                end,
    Statement_Length = length(Statement),
    insert_table(statement, Statement),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses statement ~n", [Statement_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 100
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % --------------------------------------------------------------------------
    % Cypher = [SP], QueryOptions, Statement, [[SP], ';'], [SP] ;
    % --------------------------------------------------------------------------
    Cypher_Curr = sort_list_random([
            ?SP_OPT ++
            S ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> ?SP_OPT ++ ";";
                _ -> []
            end
            ++ ?SP_OPT
        || S <- Statement
    ]),
    Cypher = case length(Cypher_Curr) > ?MAX_CYPHER of
                 true -> lists:sublist(Cypher_Curr, 1, ?MAX_CYPHER);
                 _ -> Cypher_Curr
             end,
    Cypher_Length = length(Cypher),
    insert_table(cypher, Cypher),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses cypher ~n", [Cypher_Length])),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating code of rules Expression, ... .
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code_expression(Max, Atom, NodeLabels, PropertyLookup) ->
    Atom_Length = length(Atom),
    NodeLabels_Length = length(NodeLabels),
    PropertyLookup_Length = length(PropertyLookup),
    % --------------------------------------------------------------------------
    % PropertyOrLabelsExpression = Atom, { PropertyLookup | NodeLabels } ;
    % --------------------------------------------------------------------------
    PropertyOrLabelsExpression_Prev = case ets:lookup(?CODE_TEMPLATES, propertyOrLabelsExpression) of
                                          [{_, PropertyOrLabelsExpression_Exist}] ->
                                              PropertyOrLabelsExpression_Exist;
                                          _ -> []
                                      end,
    PropertyOrLabelsExpression_Curr = sort_list_random(sets:to_list(sets:from_list(
        PropertyOrLabelsExpression_Prev ++
        [lists:nth(rand:uniform(Atom_Length), Atom) ++
            case rand:uniform(?PRIME) rem 7 of
                1 -> lists:nth(rand:uniform(NodeLabels_Length), NodeLabels);
                2 -> lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++
                lists:nth(rand:uniform(NodeLabels_Length), NodeLabels);
                3 -> lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++
                lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                4 ->
                    lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                5 ->
                    lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                    lists:nth(rand:uniform(NodeLabels_Length), NodeLabels);
                6 ->
                    lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                    lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                _ -> []
            end
            || _ <- lists:seq(1, Max)
        ]
    ))),
    PropertyOrLabelsExpression = case length(PropertyOrLabelsExpression_Curr) > Max of
                                     true ->
                                         lists:sublist(PropertyOrLabelsExpression_Curr, 1, Max);
                                     _ -> PropertyOrLabelsExpression_Curr
                                 end,
    PropertyOrLabelsExpression_Length = length(PropertyOrLabelsExpression),
    insert_table(propertyOrLabelsExpression, PropertyOrLabelsExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses property_or_labels_expression ~n", [PropertyOrLabelsExpression_Length])),
    % --------------------------------------------------------------------------
    % StringListNullOperatorExpression = PropertyOrLabelsExpression, { ([SP], '[', Expression, ']')
    %                                  | ([SP], '[', [Expression], '..', [Expression], ']')
    %                                  | ((([SP], '=~')
    %                                  | (SP, (I,N))
    %                                  | (SP, (S,T,A,R,T,S), SP, (W,I,T,H))
    %                                  | (SP, (E,N,D,S), SP, (W,I,T,H))
    %                                  | (SP, (C,O,N,T,A,I,N,S))), [SP], PropertyOrLabelsExpression)
    %                                  | (SP, (I,S), SP, (N,U,L,L))
    %                                  | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L)) } ;
    % --------------------------------------------------------------------------
    StringListNullOperatorExpression_Prev = case ets:lookup(?CODE_TEMPLATES, stringListNullOperatorExpression) of
                                                [{_, StringListNullOperatorExpression_Exist}] ->
                                                    StringListNullOperatorExpression_Exist;
                                                _ -> []
                                            end,
    StringListNullOperatorExpression = sort_list_random(sets:to_list(sets:from_list(
        StringListNullOperatorExpression_Prev ++
        [lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
            case rand:uniform(?PRIME) rem 23 of
                1 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]" ++
                    ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]";
                2 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ".." ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]" ++
                    ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ".." ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]";
                3 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ".." ++ "]" ++
                    ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ".." ++ "]";
                4 -> ?SP_OPT ++ "[" ++ ".." ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]" ++
                    ?SP_OPT ++ "[" ++ ".." ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]";
                5 -> ?SP ++ "=~" ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ?SP ++ "=~" ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                6 -> ?SP ++ "In" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ?SP ++ "In" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                7 -> ?SP ++ "Starts" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ?SP ++ "Starts" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                8 -> ?SP ++ "Ends" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ?SP ++ "Ends" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                9 -> ?SP ++ "Contains" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ?SP ++ "Contains" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                10 -> ?SP ++ "Is" ++ ?SP ++ "Null" ++
                    ?SP ++ "Is" ++ ?SP ++ "Null";
                11 -> ?SP ++ "Is" ++ ?SP ++ "Not" ++ ?SP ++ "Null" ++
                    ?SP ++ "Is" ++ ?SP ++ "Not" ++ ?SP ++ "Null";
                12 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]";
                13 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ".." ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]";
                14 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    ".." ++ "]";
                15 -> ?SP_OPT ++ "[" ++
                    ".." ++ lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                    "]";
                16 -> ?SP ++ "=~" ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                17 -> ?SP ++ "In" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                18 -> ?SP ++ "Starts" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                19 -> ?SP ++ "Ends" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                20 -> ?SP ++ "Contains" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression);
                21 -> ?SP ++ "Is" ++ ?SP ++ "Null";
                22 -> ?SP ++ "Is" ++ ?SP ++ "Not" ++ ?SP ++ "Null";
                _ -> []
            end
            || _ <- lists:seq(1, Max)
        ]
    ))),
    StringListNullOperatorExpression_Length = length(StringListNullOperatorExpression),
    insert_table(stringListNullOperatorExpression, StringListNullOperatorExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses string_list_null_operator_expression ~n", [StringListNullOperatorExpression_Length])),
    % --------------------------------------------------------------------------
    % UnaryAddOrSubtractExpression = { ('+' | '-'), SP }, StringListNullOperatorExpression ;
    % --------------------------------------------------------------------------
    UnaryAddOrSubtractExpression_Prev = case ets:lookup(?CODE_TEMPLATES, unaryAddOrSubtractExpression) of
                                            [{_, UnaryAddOrSubtractExpression_Exist}] ->
                                                UnaryAddOrSubtractExpression_Exist;
                                            _ -> []
                                        end,
    UnaryAddOrSubtractExpression = sort_list_random(sets:to_list(sets:from_list(
        UnaryAddOrSubtractExpression_Prev ++
        [
                case rand:uniform(?PRIME) rem 5 of
                    1 -> "+" ++ ?SP ++ "+" ++ ?SP;
                    2 -> "-" ++ ?SP ++ "-" ++ ?SP;
                    3 -> "+" ++ ?SP;
                    4 -> "-" ++ ?SP;
                    _ -> []
                end ++
                lists:nth(rand:uniform(StringListNullOperatorExpression_Length), StringListNullOperatorExpression)
            || _ <- lists:seq(1, Max)
        ]))),
    UnaryAddOrSubtractExpression_Length = length(UnaryAddOrSubtractExpression),
    insert_table(unaryAddOrSubtractExpression, UnaryAddOrSubtractExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses unary_add_or_subtract_expression ~n", [UnaryAddOrSubtractExpression_Length])),
    % --------------------------------------------------------------------------
    % PowerOfExpression = UnaryAddOrSubtractExpression, { [SP], '^', [SP], UnaryAddOrSubtractExpression } ;
    % --------------------------------------------------------------------------
    PowerOfExpression_Prev = case ets:lookup(?CODE_TEMPLATES, powerOfExpression) of
                                 [{_, PowerOfExpression_Exist}] ->
                                     PowerOfExpression_Exist;
                                 _ -> []
                             end,
    PowerOfExpression = sort_list_random(sets:to_list(sets:from_list(
        PowerOfExpression_Prev ++
        [
                lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression) ++
                case rand:uniform(?PRIME) rem 3 of
                    1 ->
                        ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression) ++
                            ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression);
                    2 ->
                        ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    PowerOfExpression_Length = length(PowerOfExpression),
    insert_table(powerOfExpression, PowerOfExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses power_of_expression ~n", [PowerOfExpression_Length])),
    % --------------------------------------------------------------------------
    % MultiplyDivideModuloExpression = PowerOfExpression, { ([SP], '*', [SP], PowerOfExpression) | ([SP], '/', [SP], PowerOfExpression) | ([SP], '%', [SP], PowerOfExpression) } ;
    % --------------------------------------------------------------------------
    MultiplyDivideModuloExpression_Prev = case ets:lookup(?CODE_TEMPLATES, multiplyDivideModuloExpression) of
                                              [{_, MultiplyDivideModuloExpression_Exist}] ->
                                                  MultiplyDivideModuloExpression_Exist;
                                              _ -> []
                                          end,
    MultiplyDivideModuloExpression = sort_list_random(sets:to_list(sets:from_list(
        MultiplyDivideModuloExpression_Prev ++
        [
                lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression) ++
                case rand:uniform(?PRIME) rem 7 of
                    1 ->
                        ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression) ++
                            ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression);
                    2 ->
                        ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression) ++
                            ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression);
                    3 ->
                        ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression) ++
                            ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression);
                    4 ->
                        ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression);
                    5 ->
                        ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression);
                    6 ->
                        ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    MultiplyDivideModuloExpression_Length = length(MultiplyDivideModuloExpression),
    insert_table(multiplyDivideModuloExpression, MultiplyDivideModuloExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses multiply_divide_modulo_expression ~n", [MultiplyDivideModuloExpression_Length])),
    % --------------------------------------------------------------------------
    % AddOrSubtractExpression = MultiplyDivideModuloExpression, { ([SP], '+', [SP], MultiplyDivideModuloExpression) | ([SP], '-', [SP], MultiplyDivideModuloExpression) } ;
    % --------------------------------------------------------------------------
    AddOrSubtractExpression_Prev = case ets:lookup(?CODE_TEMPLATES, addOrSubtractExpression) of
                                       [{_, AddOrSubtractExpression_Exist}] ->
                                           AddOrSubtractExpression_Exist;
                                       _ -> []
                                   end,
    AddOrSubtractExpression = sort_list_random(sets:to_list(sets:from_list(
        AddOrSubtractExpression_Prev ++
        [
                lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression) ++
                case rand:uniform(?PRIME) rem 5 of
                    1 ->
                        ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression) ++
                            ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression);
                    2 ->
                        ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression) ++
                            ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression);
                    3 ->
                        ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression);
                    4 ->
                        ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    AddOrSubtractExpression_Length = length(AddOrSubtractExpression),
    insert_table(addOrSubtractExpression, AddOrSubtractExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses add_or_subtract_expression ~n", [AddOrSubtractExpression_Length])),
    % --------------------------------------------------------------------------
    % PartialComparisonExpression = ('=', [SP], AddOrSubtractExpression)
    %                             | ('<>', [SP], AddOrSubtractExpression)
    %                             | ('!=', [SP], AddOrSubtractExpression)
    %                             | ('<', [SP], AddOrSubtractExpression)
    %                             | ('>', [SP], AddOrSubtractExpression)
    %                             | ('<=', [SP], AddOrSubtractExpression)
    %                             | ('>=', [SP], AddOrSubtractExpression) ;
    % --------------------------------------------------------------------------
    PartialComparisonExpression_Prev = case ets:lookup(?CODE_TEMPLATES, partial_comparison_expression) of
                                           [{_, PartialComparisonExpression_Exist}] ->
                                               PartialComparisonExpression_Exist;
                                           _ -> []
                                       end,
    PartialComparisonExpression = sort_list_random(sets:to_list(sets:from_list(
        PartialComparisonExpression_Prev ++
        [
            case rand:uniform(?PRIME) rem 7 of
                1 ->
                    "=" ++ ?SP_OPT ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
                2 ->
                    "<>" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
                3 ->
                    "!=" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
                4 ->
                    "<" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
                5 ->
                    ">" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
                6 ->
                    "<=" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
                _ ->
                    ">=" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression)
            end
            || _ <- lists:seq(1, Max)
        ]))),
    PartialComparisonExpression_Length = length(PartialComparisonExpression),
    insert_table(partial_comparison_expression, PartialComparisonExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses partial_comparison_expression ~n", [PartialComparisonExpression_Length])),
    % --------------------------------------------------------------------------
    % ComparisonExpression = AddOrSubtractExpression, { [SP], PartialComparisonExpression } ;
    % --------------------------------------------------------------------------
    ComparisonExpression_Prev = case ets:lookup(?CODE_TEMPLATES, comparisonExpression) of
                                    [{_, ComparisonExpression_Exist}] ->
                                        ComparisonExpression_Exist;
                                    _ -> []
                                end,
    ComparisonExpression = sort_list_random(sets:to_list(sets:from_list(
        ComparisonExpression_Prev ++
        [
                lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression) ++
                case rand:uniform(?PRIME) rem 3 of
                    1 -> ?SP_OPT ++
                        lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression) ++
                        ?SP_OPT ++
                        lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression);
                    2 -> ?SP_OPT ++
                    lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    ComparisonExpression_Length = length(ComparisonExpression),
    insert_table(comparisonExpression, ComparisonExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses comparison_expression ~n", [ComparisonExpression_Length])),
    % --------------------------------------------------------------------------
    % NotExpression = { (N,O,T), [SP] }, ComparisonExpression ;
    % --------------------------------------------------------------------------
    NotExpression_Prev = case ets:lookup(?CODE_TEMPLATES, notExpression) of
                             [{_, NotExpression_Exist}] -> NotExpression_Exist;
                             _ -> []
                         end,
    NotExpression = sort_list_random(sets:to_list(sets:from_list(
        NotExpression_Prev ++
        [
                case rand:uniform(?PRIME) rem 3 of
                    1 -> "Not" ++ ?SP_OPT ++ "Not" ++ ?SP_OPT;
                    2 -> "Not" ++ ?SP_OPT;
                    _ -> []
                end ++
                lists:nth(rand:uniform(ComparisonExpression_Length), ComparisonExpression)
            || _ <- lists:seq(1, Max)
        ]))),
    NotExpression_Length = length(NotExpression),
    insert_table(notExpression, NotExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses not_expression ~n", [NotExpression_Length])),
    % --------------------------------------------------------------------------
    % AndExpression = NotExpression, { SP, (A,N,D), SP, NotExpression } ;
    % --------------------------------------------------------------------------
    AndExpression_Prev = case ets:lookup(?CODE_TEMPLATES, andExpression) of
                             [{_, AndExpression_Exist}] -> AndExpression_Exist;
                             _ -> []
                         end,
    AndExpression = sort_list_random(sets:to_list(sets:from_list(
        AndExpression_Prev ++
        [
                lists:nth(rand:uniform(NotExpression_Length), NotExpression) ++
                case rand:uniform(?PRIME) rem 3 of
                    1 ->
                        ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(NotExpression_Length), NotExpression) ++
                            ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(NotExpression_Length), NotExpression);
                    2 ->
                        ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(NotExpression_Length), NotExpression);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    AndExpression_Length = length(AndExpression),
    insert_table(andExpression, AndExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses and_expression ~n", [AndExpression_Length])),
    % --------------------------------------------------------------------------
    % XorExpression = AndExpression, { SP, (X,O,R), SP, AndExpression } ;
    % --------------------------------------------------------------------------
    XorExpression_Prev = case ets:lookup(?CODE_TEMPLATES, xorExpression) of
                             [{_, XorExpression_Exist}] -> XorExpression_Exist;
                             _ -> []
                         end,
    XorExpression = sort_list_random(sets:to_list(sets:from_list(
        XorExpression_Prev ++
        [
                lists:nth(rand:uniform(AndExpression_Length), AndExpression) ++
                case rand:uniform(?PRIME) rem 3 of
                    1 ->
                        ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(AndExpression_Length), AndExpression) ++
                            ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(AndExpression_Length), AndExpression);
                    2 ->
                        ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(AndExpression_Length), AndExpression);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    XorExpression_Length = length(XorExpression),
    insert_table(xorExpression, XorExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses xor_expression ~n", [XorExpression_Length])),
    % --------------------------------------------------------------------------
    % OrExpression = XorExpression, { SP, (O,R), SP, XorExpression } ;
    % --------------------------------------------------------------------------
    OrExpression_Prev = case ets:lookup(?CODE_TEMPLATES, orExpression) of
                            [{_, OrExpression_Exist}] -> OrExpression_Exist;
                            _ -> []
                        end,
    OrExpression = sort_list_random(sets:to_list(sets:from_list(
        OrExpression_Prev ++
        [
                lists:nth(rand:uniform(XorExpression_Length), XorExpression) ++
                case rand:uniform(?PRIME) rem 3 of
                    1 ->
                        ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(XorExpression_Length), XorExpression) ++
                            ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(XorExpression_Length), XorExpression);
                    2 ->
                        ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(XorExpression_Length), XorExpression);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    OrExpression_Length = length(OrExpression),
    insert_table(orExpression, OrExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses or_expression ~n", [OrExpression_Length])),
    % --------------------------------------------------------------------------
    % Expression = OrExpression ;
    % --------------------------------------------------------------------------
    Expression = sort_list_random(sets:to_list(sets:from_list(lists:append([
        AddOrSubtractExpression,
        AndExpression,
        ComparisonExpression,
        MultiplyDivideModuloExpression,
        NotExpression,
        OrExpression,
        PowerOfExpression,
        PropertyOrLabelsExpression,
        StringListNullOperatorExpression,
        UnaryAddOrSubtractExpression,
        XorExpression
    ])))),
    insert_table(expression, Expression),
    Expression.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating EUnit data files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_eunit_all([]) ->
    ok;
file_create_eunit_all([Rule | Rules]) ->
    file_create_eunit(Rule),
    file_create_eunit_all(Rules).

file_create_eunit(Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),
    FileName = "reliability_" ++ atom_to_list(Rule) ++ ".tst",
    {ok, File, _} = file:path_open([?PATH_EUNIT], FileName, [write]),

    io:format(File, "~s~n", ["%%-*- mode: erlang -*-"]),
    io:format(File, "~s~n", ["%%-*- coding: utf-8 -*-"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["% Manual testing."]),
    io:format(File, "~s~n", ["[{tests, []}]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%"]),
    io:format(File, "~s~n", ["%% Tests for rule: " ++ atom_to_list(Rule)]),
    io:format(File, "~s~n", ["%%"]),
    io:format(File, "~s~n", [""]),

    file_write_eunit(File, Code).

file_write_eunit(File, []) ->
    file:close(File);
file_write_eunit(File, [H | T]) ->
    io:format(File, "~s~n", ["\"" ++ H ++ "\"."]),
    file_write_eunit(File, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating Common Test data files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_Type, []) ->
    ok;
file_create_ct_all(Type, [Rule | Rules]) ->
    file_create_ct(Type, Rule),
    file_create_ct_all(Type, Rules).

file_create_ct(Type, Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),
    erlang:display(io:format("~5.. B records ===> type: ~s rule: ~s ~n", [length(Code), atom_to_list(Type), atom_to_list(Rule)])),

    FileName = atom_to_list(Type) ++ "_" ++ atom_to_list(Rule) ++ "_SUITE",
    {ok, File, _} = file:path_open([?PATH_CT], FileName ++ ".erl", [write]),

    {{Current_Year, Current_Month, Current_Day}, _} = calendar:local_time(),

    io:format(File, "~s~n", ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%%% File        : " ++ FileName ++ ".erl"]),
    io:format(File, "~s~n", ["%%% Description : Test Suite for rule: " ++ atom_to_list(Rule) ++ "."]),
    io:format(File, "~s~n", ["%%%"]),
    io:format(File, "~s~n", ["%%% Created     : " ++ lists:flatten(io_lib:format("~2..0w.~2..0w.~4..0w", [Current_Day, Current_Month, Current_Year]))]),
    io:format(File, "~s~n", ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["-module(" ++ FileName ++ ")."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-compile(export_all)."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-include_lib(\"common_test/include/ct.hrl\")."]),
    io:format(File, "~s~n", ["-include_lib(\"eunit/include/eunit.hrl\")."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% COMMON TEST CALLBACK FUNCTIONS - SUITE"]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["suite() ->"]),
    io:format(File, "~s~n", ["    ["]),
    io:format(File, "~s~n", ["        {timetrap, {minutes, 10}}"]),
    io:format(File, "~s~n", ["    ]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["init_per_suite(Config) ->"]),
    io:format(File, "~s~n", ["    Config."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["end_per_suite(_Config) ->"]),
    io:format(File, "~s~n", ["    ok."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% COMMON TEST CALLBACK FUNCTIONS - ALL"]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["all() ->"]),
    io:format(File, "~s~n", ["    [test_" ++ atom_to_list(Rule) ++ "]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% TEST CASES"]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["test_" ++ atom_to_list(Rule) ++ "(_Config) ->"]),

    file_write_ct(Type, File, Code).

file_write_ct(_Type, File, []) ->
    file:close(File);
file_write_ct(Type, File, [H | T]) ->
    io:format(File, "~s~n", ["    " ++ case Type of
                                           performance ->
                                               "ocparse:source_to_pt";
                                           _ ->
                                               "ocparse_test:common_test_source"
                                       end ++ "(\"" ++ H ++ "\")" ++ case T of
                                                                         [] ->
                                                                             ".";
                                                                         _ ->
                                                                             ","
                                                                     end]),
    file_write_ct(Type, File, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Insert generated code into helper table.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_table(Rule, Code) ->
    ets:insert(?CODE_TEMPLATES, {Rule, Code}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Randomising unique lists.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_list_random(L) ->
    F = fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end,
    lists:sort(F, sets:to_list(sets:from_list(L))).
