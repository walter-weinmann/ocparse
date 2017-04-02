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

-define(ALL_ATOM, [literal,
    parameter,
    atom_count,
    list_comprehension,
    atom_square_bracket,
    atom_filter,
    atom_extract,
    atom_all,
    atom_any,
    atom_none,
    atom_single,
    relationships_pattern,
    parenthesized_expression,
    function_invocation,
    variable]).
-define(ALL_CLAUSE, [match,
    unwind,
    merge,
    create,
    set,
    delete,
    remove,
    with,
    return]).
-define(ALL_COMMAND, [create_index,
    drop_index,
    create_unique_constraint,
    drop_unique_constraint,
    create_node_property_existence_constraint,
    drop_node_property_existence_constraint,
    create_relationship_property_existence_constraint,
    drop_relationship_property_existence_constraint]).
-define(ALL_EXPRESSION, [expression,
    expression10,
    expression11,
    expression2,
    expression3,
    expression4,
    expression5,
    expression6,
    expression7,
    expression8,
    expression9]).
-define(CODE_TEMPLATES, code_templates).
-define(DASH, "-").
-define(LEFT_ARROW_HEAD, "<").
-define(MAX_BASE_VAR, 2).
-define(MAX_CLAUSE, 500).
-define(MAX_CYPHER, 1000).
-define(MAX_COMMAND, 1000).
-define(MAX_QUERY, 1000).
-define(MAX_RULE_ATOM, 1000).
-define(MAX_RULE_EXPRESSION, 2000).
-define(MAX_STATEMENT, 1000).
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
    % ----------------------------------------------------------------------------------------------
    % Standard version -----------------------------------------------------------------------------
    % ----------------------------------------------------------------------------------------------

    create_code(),

    % rekliability common tests
    ok = file_create_ct_all(?ALL_CLAUSE),

    % performance common tests
    ok = file_create_ct_all([
        cypher,
        query,
        statement
    ]),

    % reliability eunit tests
    ok = file_create_eunit_all(?ALL_CLAUSE),

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

% -----------------------------------------------------
% Atom = ...
%      | (N,U,L,L)
%      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
%      | ... ;
% -----------------------------------------------------
    AtomCount = ["Count(*)", "Count ( * )"],
    insert_table(atom_count, AtomCount),
    AtomNull = ["Null"],
    insert_table(atom_null, AtomNull),

% ---------------------------------------------------------------------------------------
% BooleanLiteral = (T,R,U,E)
%                | (F,A,L,S,E)
%                ;
% ---------------------------------------------------------------------------------------
    BooleanLiteral = sort_list_random([
        "true",
        "true",
        "true",
        "true",
        "true",
        "false",
        "false",
        "false",
        "false",
        "false"
    ]),
    insert_table(boolean_literal, BooleanLiteral),
% ---------------------------------------------------------------------------------------
% DecimalInteger = (('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'), [DigitString])
%                | '0' ;
% ---------------------------------------------------------------------------------------
    DecimalInteger = sort_list_random([
        "0",
        "7",
        "12",
        "999",
        "1000",
        "123456789"
    ]),
    DecimalInteger_Length = length(DecimalInteger),
    insert_table(decimal_integer, DecimalInteger),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses decimal_integer ~n", [DecimalInteger_Length])),
% ---------------------------------------------------------------------------------------------------------------------------------------------------------------
% (* Any character except "`", enclosed within `backticks`. Backticks are escaped with double backticks. *)EscapedSymbolicName = { '`', { ANY - ('`') }, '`' }- ;
% ---------------------------------------------------------------------------------------------------------------------------------------------------------------
    EscapedSymbolicName = sort_list_random([
        "`1esn`",
        "`2esn`",
        "`3esn`",
        "`4esn`",
        "`5esn`",
        "`6esn`",
        "`7esn`",
        "`8esn`",
        "``"
    ]),
    insert_table(escaped_symbolic_name, EscapedSymbolicName),
% --------------------------------------------------------------------------------------------------------
% ExponentDecimalReal = ({ Digit | '.' }- | DecimalInteger), ((E) | (E)), (DigitString | DecimalInteger) ;
% --------------------------------------------------------------------------------------------------------
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
    insert_table(exponent_decimal_real, ExponentDecimalReal),
% ---------------------------------
% HexInteger = ('0',X), HexString ;
% ---------------------------------
    HexInteger = sort_list_random([
        "0x0",
        "0X7",
        "0Xa",
        "0xabc",
        "0X0123456789ABCDEF"
    ]),
    insert_table(hex_integer, HexInteger),
% ---------------------------------
% OctalInteger = '0', OctalString ;
% ---------------------------------
    OctalInteger = sort_list_random([
        "00",
        "01",
        "07",
        "010",
        "01234567"
    ]),
    insert_table(octal_integer, OctalInteger),
% ----------------------------------------------------------------------------------------
% RegularDecimalReal = ({ Digit } | DecimalInteger), '.', (DigitString | DecimalInteger) ;
% ----------------------------------------------------------------------------------------
    RegularDecimalReal = sort_list_random([".0", ".12", "0.0", "0.12", "12.0", "123.654"]),
    insert_table(regular_decimal_real, RegularDecimalReal),
% -----------------------------------------------------------------
% StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
%               | ("'", { ANY - ("'" | '\') | EscapedChar }, "'") ;
% -----------------------------------------------------------------
    StringLiteral = sort_list_random([
        "\\\"d_str\\\"",
        "'s_str'"
    ]),
    StringLiteral_Length = length(StringLiteral),
    insert_table(string_literal, StringLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses string_literal ~n", [StringLiteral_Length])),
% -------------------------------------------------------------
% UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;
% -------------------------------------------------------------
    UnescapedSymbolicName = sort_list_random([
        "usn1",
        "usn2",
        "_usn3",
        "_usn4",
        "@usn5",
        "@usn6",
        "#usn7",
        "#usn8"
    ]),
    insert_table(unescaped_symbolic_name, UnescapedSymbolicName),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------
% DoubleLiteral = ExponentDecimalReal
%               | RegularDecimalReal ;
% ------------------------------------
    DoubleLiteral = sort_list_random(
        ExponentDecimalReal ++
        RegularDecimalReal),
    insert_table(double_literal, DoubleLiteral),
% ---------------------------------
% IntegerLiteral = HexInteger
%                | OctalInteger
%                | DecimalInteger ;
% ---------------------------------
    IntegerLiteral = sort_list_random(
        HexInteger ++
            OctalInteger ++
            DecimalInteger),
    IntegerLiteral_Length = length(IntegerLiteral),
    insert_table(integer_literal, IntegerLiteral),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses integer_literal ~n", [IntegerLiteral_Length])),
% ------------------------------------
% SymbolicName = UnescapedSymbolicName
%              | EscapedSymbolicName ;
% ------------------------------------
    SymbolicName = sort_list_random(
        UnescapedSymbolicName ++
        EscapedSymbolicName),
    SymbolicName_Length = length(SymbolicName),
    insert_table(symbolic_name, SymbolicName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses symbolic_name ~n", [SymbolicName_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------
% FunctionName = SymbolicName ;
% -----------------------------
    FunctionName = SymbolicName ++
        ["count"],
    FunctionName_Length = length(FunctionName),
    insert_table(function_name, FunctionName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses function_name ~n", [FunctionName_Length])),
%-------------------------------------------------------------------
% LiteralIds = IntegerLiteral, { [SP], ',', [SP], IntegerLiteral } ;
%-------------------------------------------------------------------
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
    insert_table(literal_ids, LiteralIds),
% --------------------------
% LabelName = SymbolicName ;
% --------------------------
    LabelName = SymbolicName,
    insert_table(label_name, LabelName),
% --------------------------------
% NumberLiteral = DoubleLiteral
%               | IntegerLiteral ;
% --------------------------------
    NumberLiteral = sort_list_random(
        DoubleLiteral ++
        IntegerLiteral),
    insert_table(number_literal, NumberLiteral),
% --------------------------------------------------
% Parameter = '$', (SymbolicName | DecimalInteger) ;
% --------------------------------------------------
    Parameter = sort_list_random([
            "$" ++ SN
        || SN <- SymbolicName
    ] ++
    [
            "$" ++ DI
        || DI <- DecimalInteger
    ]),
    insert_table(parameter, Parameter),
% --------------------------------
% PropertyKeyName = SymbolicName ;
% --------------------------------
    PropertyKeyName = SymbolicName,
    PropertyKeyName_Length = length(PropertyKeyName),
    insert_table(property_key_name, PropertyKeyName),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses property_key_name ~n", [PropertyKeyName_Length])),
% ----------------------------------------------------------------------------------------
% RangeLiteral = '*', [SP], [IntegerLiteral, [SP]], ['..', [SP], [IntegerLiteral, [SP]]] ;
% ----------------------------------------------------------------------------------------
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
% ----------------------------
% RelTypeName = SymbolicName ;
% ----------------------------
    RelTypeName = SymbolicName,
    RelTypeName_Length = length(RelTypeName),
    insert_table(rel_type_name, RelTypeName),
% -------------------------
% Variable = SymbolicName ;
% -------------------------
    Variable = SymbolicName,
    Variable_Length = length(Variable),
    insert_table(variable, Variable),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------------------
% Literal_Part_1 = NumberLiteral
%      | StringLiteral
%      | BooleanLiteral
%      | (N,U,L,L)    
%      | ...
% ------------------------------------------------------
    Literal_Part_1 = sort_list_random(
        NumberLiteral ++
            StringLiteral ++
            BooleanLiteral ++
            AtomNull ++
            AtomNull ++
            AtomNull ++
            AtomNull ++
            AtomNull),
    insert_table(literal_part_1, Literal_Part_1),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------------------
% Atom = Literal
%      | Parameter
%      | ...
%      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
%      | ...
%      | Variable ;
% ------------------------------------------------------
    Atom_Part_1 = sort_list_random(
        Literal_Part_1 ++
            Parameter ++
            AtomCount ++
            Variable),
    insert_table(atom_part_1, Atom_Part_1),
% ----------------------------
% NodeLabel = ':', LabelName ;
% ----------------------------
    NodeLabel = sort_list_random([
            ":" ++ LN
        || LN <- LabelName
    ]),
    NodeLabel_Length = length(NodeLabel),
    insert_table(node_label, NodeLabel),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses node_label ~n", [NodeLabel_Length])),
% --------------------------------------------------------------------------------------
% PropertyLookup = [SP], '.', [SP], ((PropertyKeyName, ('?' | '!')) | PropertyKeyName) ;
% --------------------------------------------------------------------------------------
    PropertyLookup =
        sort_list_random([
                ?SP_OPT ++ "." ++ ?SP_OPT ++ PK ++ "?"
            || PK <- PropertyKeyName
        ] ++
            [
                    ?SP_OPT ++ "." ++ ?SP_OPT ++ PK ++ "!"
                || PK <- PropertyKeyName
            ] ++
            [
                    ?SP_OPT ++ "." ++ ?SP_OPT ++ PK
                || PK <- PropertyKeyName
            ]),
    PropertyLookup_Length = length(PropertyLookup),
    insert_table(property_lookup, PropertyLookup),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses property_lookup ~n", [PropertyLookup_Length])),
% -------------------------------------------------------------------------------
% RelationshipTypes = ':', RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;
% -------------------------------------------------------------------------------
    RelationshipTypes = sort_list_random([
            ":" ++ RTN ++
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

% ---------------------------------------------
% NodeLabels = NodeLabel, { [SP], NodeLabel } ;
% ---------------------------------------------
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

% ---------------------------
% Expression = Expression12 ;
% ---------------------------
    Expression_Part_1 = sort_list_random(create_code_expression(?MAX_RULE_EXPRESSION, Atom_Part_1, NodeLabels, PropertyLookup)),
    Expression_Part_1_Length = length(Expression_Part_1),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (1) ~n", [Expression_Part_1_Length])),
% -------------------------------------
% Where = (W,H,E,R,E), SP, Expression ;
% -------------------------------------
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

% -----------------------------------------------------------------------------------------------------------------------------------------
% FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;
% -----------------------------------------------------------------------------------------------------------------------------------------
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
    insert_table(function_invocation, FunctionInvocation),
% ------------------------------------------------
% IdInColl = Variable, SP, (I,N), SP, Expression ;
% ------------------------------------------------
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
% -----------------------------------------------------------------------------------
% ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;
% -----------------------------------------------------------------------------------
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
    insert_table(list_literal, ListLiteral),
% ------------------------------------------------------------------------------------------------------------------------------------------------------
% MapLiteral = '{', [SP], [PropertyKeyName, [SP], ':', [SP], Expression, [SP], { ',', [SP], PropertyKeyName, [SP], ':', [SP], Expression, [SP] }], '}' ;
% ------------------------------------------------------------------------------------------------------------------------------------------------------
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
    insert_table(map_literal, MapLiteral),
% ------------------------------------------------------------
% ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;
% ------------------------------------------------------------
    ParenthesizedExpression = sort_list_random([
            "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
            ?SP_OPT ++ ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(parenthesized_expression, ParenthesizedExpression),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --------------------------------------------
% FilterExpression = IdInColl, [[SP], Where] ;
% --------------------------------------------
% wwe ???
% FilterExpression = IdInColl, [SP, Where] ;
% --------------------------------------------
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
% ------------------------------------------------------------------------------------------------
% Literal = ...
%         | MapLiteral
%         | ListLiteral
%         ;
% ------------------------------------------------------------------------------------------------
    Literal = sort_list_random(
        Literal_Part_1 ++
        MapLiteral),
%%            MapLiteral ++
%%            ListLiteral),
    insert_table(literal, Literal),
% ------------------------------
% Properties = MapLiteral
%            | Parameter ;
% ------------------------------
    Properties = sort_list_random(
        MapLiteral ++
        Parameter),
    Properties_Length = length(Properties),
    insert_table(properties, Properties),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses properties ~n", [Properties_Length])),
% ---------------------------------------------------------------------------------------------------
% Reduce = (R,E,D,U,C,E), [SP], '(', Variable, '=', Expression, ',', IdInColl, '|', Expression, ')' ;
% ---------------------------------------------------------------------------------------------------
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
    insert_table(reduce, Reduce),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------------
% ListComprehension = '[', FilterExpression, [[SP], '|', Expression], ']' ;
% -------------------------------------------------------------------------
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
    insert_table(list_comprehension, ListComprehension),
% ----------------------------------------------------------------------------------------
% NodePattern = '(', [SP], [Variable, [SP]], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
% --------------------------------------------------------------------------------
% wwe ???
% NodePattern = '(', [SP], [Variable, SP], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
% ----------------------------------------------------------------------------------------
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
% -----------------------------------------------------------------------------------------------------
% RelationshipDetail = '[', [Variable], ['?'], [RelationshipTypes], [RangeLiteral], [Properties], ']' ;
% -----------------------------------------------------------------------------------------------------
    RelationshipDetail = sort_list_random(
        [
            "[?]"
        ] ++
        [
                "[" ++
                case rand:uniform(?PRIME) rem 32 of
                    1 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        "?" ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    2 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        "?" ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    3 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        "?" ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    4 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        "?" ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes);
                    5 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        "?" ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    6 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        "?" ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    7 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        "?" ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    8 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                    "?";
                    9 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    10 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    11 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    12 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                    lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes);
                    13 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    14 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                    lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    15 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                    lists:nth(rand:uniform(Properties_Length), Properties);
                    16 -> lists:nth(rand:uniform(Variable_Length), Variable);
                    17 -> "?" ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    18 -> "?" ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    19 -> "?" ++
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    20 -> "?" ++
                    lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes);
                    21 -> "?" ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    22 -> "?" ++
                    lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    23 -> "?" ++
                    lists:nth(rand:uniform(Properties_Length), Properties);
                    24 -> "?";
                    25 ->
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                            lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                            lists:nth(rand:uniform(Properties_Length), Properties);
                    26 ->
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    27 ->
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    28 ->
                        lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes);
                    29 ->
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    30 ->
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    31 ->
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    _ -> []
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

% ---------------------------------------------------------------------------------------------------------------
% RelationshipPattern = (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
%                     | (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash)
%                     | (                     Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
%                     | (                     Dash, [SP], [RelationshipDetail], [SP], Dash) ;
% ----------------------------------------------------------------------------------------------------------------
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

% --------------------------------------------------------------
% PatternElementChain = RelationshipPattern, [SP], NodePattern ;
% --------------------------------------------------------------
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

% -------------------------------------------------------------
% PatternElement = (NodePattern, { [SP], PatternElementChain })
%                | ('(', PatternElement, ')') ;
% -------------------------------------------------------------
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
% --------------------------------------------------------------------
% RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;
% --------------------------------------------------------------------
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
    insert_table(relationships_pattern, RelationshipsPattern),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 14
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 15
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------
% AnonymousPatternPart = PatternElement ;
% ------------------------------------------
    AnonymousPathPattern = sort_list_random(PatternElement),
    AnonymousPathPattern_Length = length(AnonymousPathPattern),
    insert_table(anonymous_path_pattern, AnonymousPathPattern),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses anonymous_path_pattern ~n", [AnonymousPathPattern_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 50
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------------------------------------------------
% Atom = ...
%      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
%      | ((E,X,T,R,A,C,T), [SP], '(', [SP], FilterExpression, [SP], [[SP], '|', Expression], ')')
%      | ...
%      | ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
%      | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
%      | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%      | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%      | ...
% -------------------------------------------------------------------------------------------------
    AtomFilter = sort_list_random([
            "Filter" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(atom_filter, AtomFilter),

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
    insert_table(atom_extract, AtomExtract),

    AtomAll = sort_list_random([
            "All" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(atom_all, AtomAll),

    AtomAny = sort_list_random([
            "Any" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(atom_any, AtomAny),

    AtomNone = sort_list_random([
            "None" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(atom_none, AtomNone),

    AtomSingle = sort_list_random([
            "Single" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(atom_single, AtomSingle),
% ------------------------------------------------------------------------------------------------
% Atom = Literal
%      | ...
%      | CaseExpression
%      | ...
%      | ListComprehension
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
% ------------------------------------------------------------------------------------------------
    Atom = sort_list_random(
        Atom_Part_1 ++
            Literal ++
            ListComprehension ++
            AtomFilter ++
            AtomExtract ++
            AtomAll ++
            AtomAny ++
            AtomNone ++
            AtomSingle ++
            RelationshipsPattern ++
% wwe ???
% ParenthesizedExpression ++
            FunctionInvocation),
    insert_table(atom, Atom),
    Atom_Length = length(Atom),
% ---------------------------
% Expression = Expression12 ;
% ---------------------------
    Expression = sort_list_random(create_code_expression(?MAX_RULE_EXPRESSION * 2, Atom, NodeLabels, PropertyLookup)),
    Expression_Length = length(Expression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression ~n", [Expression_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 51
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------
% Limit = (L,I,M,I,T), SP, Expression ;
% -------------------------------------
    Limit_Targ = Expression_Length,
    Limit = sort_list_random([
            "Limit" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression)
        || _ <- lists:seq(1, Limit_Targ)
    ]),
    Limit_Length = length(Limit),
    insert_table(limit, Limit),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses limit ~n", [Limit_Length])),
% ---------------------------------------------------------------
% PatternPart = (Variable, [SP], '=', [SP], AnonymousPatternPart)
%             | AnonymousPatternPart ;
% ---------------------------------------------------------------
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
% ------------------------------------------------------
% PropertyExpression = Atom, { [SP], PropertyLookup }- ;
% ------------------------------------------------------
    PropertyExpression_Targ = max(Atom_Length, PropertyLookup_Length),
    PropertyExpression = sort_list_random([
            lists:nth(rand:uniform(Atom_Length), Atom) ++
            case rand:uniform(?PRIME) rem 5 of
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
% -----------------------------------
% RemoveItem = (Variable, NodeLabels)
%            | PropertyExpression ;
% -----------------------------------
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
% --------------------------------------------------
% ReturnItem = (Expression, SP, (A,S), SP, Variable)
%            | Expression ;
% --------------------------------------------------
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
% ---------------------------------------------------
% SetItem = (PropertyExpression, '=', Expression)
%         | (Variable, '=', Expression)
%         | (Variable, '+=', Expression)
%         | (Variable, NodeLabels) ;
% ---------------------------------------------------
% wwe ???
% SetItem = (PropertyExpression, SP, '=', Expression)
%         | (Variable, SP, '=', Expression)
%         | (Variable, '+=', Expression)
%         | (Variable, NodeLabels) ;
% ---------------------------------------------------
    SetItem_Targ = max(Expression_Length, max(NodeLabels_Length, max(PropertyExpression_Length, Variable_Length))),
    SetItem = sort_list_random([
        case rand:uniform(?PRIME) rem 4 of
            1 ->
                lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression) ++
                    ?SP ++ "=" ++
                    lists:nth(rand:uniform(Expression_Length), Expression);
            2 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP ++ "=" ++
                lists:nth(rand:uniform(Expression_Length), Expression);
            3 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                "+=" ++
                lists:nth(rand:uniform(Expression_Length), Expression);
            _ -> lists:nth(rand:uniform(Variable_Length), Variable) ++
            lists:nth(rand:uniform(NodeLabels_Length), NodeLabels)
        end
        || _ <- lists:seq(1, SetItem_Targ)
    ]),
    SetItem_Length = length(SetItem),
    insert_table(set_item, SetItem),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses set_item ~n", [SetItem_Length])),
% ----------------------------------
% Skip = (S,K,I,P), SP, Expression ;
% ----------------------------------
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

% ---------------------------------------------------------
% Pattern = PatternPart, { [SP], ',', [SP], PatternPart } ;
% ---------------------------------------------------------
    Pattern_Targ = PatternPart_Length,
    Pattern = sort_list_random([
            lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
            case rand:uniform(?PRIME) rem 2 of
                1 ->
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(PatternPart_Length), PatternPart);
                _ -> []
            end
        || _ <- lists:seq(1, Pattern_Targ)
    ]),
    Pattern_Length = length(Pattern),
    insert_table(pattern, Pattern),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses pattern ~n", [Pattern_Length])),
% -------------------------------------------------------------
% ReturnItems = ('*', { [SP], ',', [SP], ReturnItem })
%             | (ReturnItem, { [SP], ',', [SP], ReturnItem }) ;
% -------------------------------------------------------------
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
% ----------------------------------------------------------------------------------------------------
% SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;
% ----------------------------------------------------------------------------------------------------
% wwe ???
% SortItem = Expression, [SP, ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;
% ----------------------------------------------------------------------------------------------------
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

% -----------------------------------------------------------------------
% Order = (O,R,D,E,R), SP, (B,Y), SP, SortItem, { ',', [SP], SortItem } ;
% -----------------------------------------------------------------------
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
% ----------------------------------------------------------------
% ReturnBody = ReturnItems, [SP, Order], [SP, Skip], [SP, Limit] ;
% ----------------------------------------------------------------
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

% ---------------------------------------
% Create = (C,R,E,A,T,E), [SP], Pattern ;
% ---------------------------------------
% wwe ???
% Create = (C,R,E,A,T,E), SP, Pattern ;
% ---------------------------------------
    Create = sort_list_random([
            "Create" ++ ?SP ++
            lists:nth(rand:uniform(Pattern_Length), Pattern)
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    insert_table(create, Create),
% ------------------------------------------------------------------------------------------------
% Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;
% ------------------------------------------------------------------------------------------------
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
    insert_table(delete, Delete),
% ----------------------------------------------------------------------------
% Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), [SP], Pattern, [[SP], Where] ;
% ----------------------------------------------------------------------------
% wwe ???
% Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), SP, Pattern, [SP, Where] ;
% -----------------------------------------------------------------------------
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
    insert_table(match, Match),
% -------------------------------------------------------------------------
% Remove = (R,E,M,O,V,E), SP, RemoveItem, { [SP], ',', [SP], RemoveItem } ;
% -------------------------------------------------------------------------
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
    insert_table(remove, Remove),
% -------------------------------------------------------------------
% Return = (R,E,T,U,R,N), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody ;
% -------------------------------------------------------------------
    Return = sort_list_random([
            "Return" ++ ?SP ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> "Distinct" ++ ?SP;
                _ -> []
            end ++
            lists:nth(rand:uniform(ReturnBody_Length), ReturnBody)
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    insert_table(return, Return),
% ----------------------------------------------
% Set = (S,E,T), SetItem, { ',', SetItem } ;
% ----------------------------------------------
% wwe ???
% Set = (S,E,T), SP, SetItem, { ',', SetItem } ;
% ----------------------------------------------
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
% -------------------------------------------------------------------
% Unwind = (U,N,W,I,N,D), [SP], Expression, SP, (A,S), SP, Variable ;
% -------------------------------------------------------------------
% wwe ???
% Unwind = (U,N,W,I,N,D), SP, Expression, SP, (A,S), SP, Variable ;
% -------------------------------------------------------------------
    Unwind = sort_list_random([
            "Unwind" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression) ++
            ?SP ++ "As" ++ ?SP ++
            lists:nth(rand:uniform(Variable_Length), Variable)
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    insert_table(unwind, Unwind),
% ----------------------------------------------------------------------------
% With = (W,I,T,H), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody, [[SP], Where] ;
% ----------------------------------------------------------------------------
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
    insert_table(with, With),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 91
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------------------
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
% ---------------------
    Clause_Part_1 = sort_list_random(
        Match ++
            Unwind ++
            Create ++
            Delete ++
            Remove ++
            With ++
            Return),
    Clause_Part_1_Length = length(Clause_Part_1),
    insert_table(clause_part_1, Clause_Part_1),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses clause (1) ~n", [Clause_Part_1_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 92
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------------------------------------------------
% MergeAction = ((O,N), SP, (M,A,T,C,H), SP, Set)
%             | ((O,N), SP, (C,R,E,A,T,E), SP, Set) ;
% ---------------------------------------------------
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

% -------------------------------------------------------------
% Merge = (M,E,R,G,E), [SP], PatternPart, { SP, MergeAction } ;
% -------------------------------------------------------------
% wwe ???
% Merge = (M,E,R,G,E), SP, PatternPart, { SP, MergeAction } ;
% -------------------------------------------------------------
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
    insert_table(merge, Merge),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 94
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------
% Clause = ...
%        | Merge
%        | ...
%        | Foreach
%        | ...
% ----------------
    Clause = sort_list_random(
        Clause_Part_1 ++
        Merge),
    Clause_Length = length(Clause),
    insert_table(clause, Clause),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses clause ~n", [Clause_Length])),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 95
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------------------
% SingleQuery = Clause, { [SP], Clause } ;
% ----------------------------------------
% wwe ???
% SingleQuery = Clause, { SP, Clause } ;
% ----------------------------------------
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
    insert_table(single_query, SingleQuery),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 96
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------
% Union = ((U,N,I,O,N), SP, (A,L,L), [SP], SingleQuery)
%       | ((U,N,I,O,N), [SP], SingleQuery) ;
% -----------------------------------------------------
% wwe ???
% Union = ((U,N,I,O,N), SP, (A,L,L), SP, SingleQuery)
%       | ((U,N,I,O,N), SP, SingleQuery) ;
% -----------------------------------------------------
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

% ---------------------------------------------
% RegularQuery = SingleQuery, { [SP], Union } ;
% ---------------------------------------------
% wwe ???
% RegularQuery = SingleQuery, { SP, Union } ;
% ---------------------------------------------
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
    insert_table(regular_query, RegularQuery),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 98
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------
% Query = RegularQuery
%       | BulkImportQuery ;
% -------------------------
    Query_Curr = sort_list_random(RegularQuery),
    Query = case length(Query_Curr) > ?MAX_QUERY of
                true -> lists:sublist(Query_Curr, 1, ?MAX_QUERY);
                _ -> Query_Curr
            end,
    insert_table(query, Query),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 99
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------
% Statement = Command
%           | Query ;
% -------------------
    Statement_Curr = sort_list_random(Query),
    Statement = case length(Statement_Curr) > ?MAX_STATEMENT of
                    true -> lists:sublist(Statement_Curr, 1, ?MAX_STATEMENT);
                    _ -> Statement_Curr
                end,
    insert_table(statement, Statement),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 100
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------------
% Cypher = [SP], QueryOptions, Statement, [[SP], ';'], [SP] ;
% -----------------------------------------------------------
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
    insert_table(cypher, Cypher),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating code of rules Expression, Expression2, ..., Expression12.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code_expression(Max, Atom, NodeLabels, PropertyLookup) ->
    Atom_Length = length(Atom),
    NodeLabels_Length = length(NodeLabels),
    PropertyLookup_Length = length(PropertyLookup),
% -----------------------------------------------------
% Expression2 = Atom, { PropertyLookup | NodeLabels } ;
% -----------------------------------------------------
    Expression2_Prev = case ets:lookup(?CODE_TEMPLATES, expression2) of
                           [{_, Expression2_Exist}] -> Expression2_Exist;
                           _ -> []
                       end,
    Expression2_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression2_Prev ++
        [lists:nth(rand:uniform(Atom_Length), Atom) ++
            case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 7 of
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
    Expression2 = case length(Expression2_Curr) > Max of
                      true -> lists:sublist(Expression2_Curr, 1, Max);
                      _ -> Expression2_Curr
                  end,
    Expression2_Length = length(Expression2),
    insert_table(expression2, Expression2),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (2) ~n", [Expression2_Length])),
% -------------------------------------------------------------------------------
% Expression3 = Expression2, { ([SP], '[', Expression, ']')
%                            | ([SP], '[', [Expression], '..', [Expression], ']')
%                            | ((([SP], '=~')
%                            | (SP, (I,N))
%                            | (SP, (S,T,A,R,T,S), SP, (W,I,T,H))
%                            | (SP, (E,N,D,S), SP, (W,I,T,H))
%                            | (SP, (C,O,N,T,A,I,N,S))), [SP], Expression2)
%                            | (SP, (I,S), SP, (N,U,L,L))
%                            | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L)) } ;
% -------------------------------------------------------------------------------
    Expression3_Prev = case ets:lookup(?CODE_TEMPLATES, expression3) of
                           [{_, Expression3_Exist}] -> Expression3_Exist;
                           _ -> []
                       end,
    Expression3_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression3_Prev ++
        [lists:nth(rand:uniform(Expression2_Length), Expression2) ++
            case rand:uniform(?PRIME) rem 23 of
                1 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]" ++
                    ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]";
                2 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ".." ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]" ++
                    ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ".." ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]";
                3 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ".." ++ "]" ++
                    ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ".." ++ "]";
                4 -> ?SP_OPT ++ "[" ++ ".." ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]" ++
                    ?SP_OPT ++ "[" ++ ".." ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]";
                5 -> ?SP ++ "=~" ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ?SP ++ "=~" ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                6 -> ?SP ++ "In" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ?SP ++ "In" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                7 -> ?SP ++ "Starts" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ?SP ++ "Starts" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                8 -> ?SP ++ "Ends" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ?SP ++ "Ends" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                9 -> ?SP ++ "Contains" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ?SP ++ "Contains" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                10 -> ?SP ++ "Is" ++ ?SP ++ "Null" ++
                    ?SP ++ "Is" ++ ?SP ++ "Null";
                11 -> ?SP ++ "Is" ++ ?SP ++ "Not" ++ ?SP ++ "Null" ++
                    ?SP ++ "Is" ++ ?SP ++ "Not" ++ ?SP ++ "Null";
                12 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]";
                13 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ".." ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]";
                14 -> ?SP_OPT ++ "[" ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    ".." ++ "]";
                15 -> ?SP_OPT ++ "[" ++
                    ".." ++ lists:nth(rand:uniform(Expression2_Length), Expression2) ++
                    "]";
                16 -> ?SP ++ "=~" ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                17 -> ?SP ++ "In" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                18 -> ?SP ++ "Starts" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                19 -> ?SP ++ "Ends" ++ ?SP ++ "With" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                20 -> ?SP ++ "Contains" ++ ?SP ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Expression2_Length), Expression2);
                21 -> ?SP ++ "Is" ++ ?SP ++ "Null";
                22 -> ?SP ++ "Is" ++ ?SP ++ "Not" ++ ?SP ++ "Null";
                _ -> []
            end
            || _ <- lists:seq(1, Max)
        ]
    ))),
    Expression3 = case length(Expression3_Curr) > Max of
                      true -> lists:sublist(Expression3_Curr, 1, Max);
                      _ -> Expression3_Curr
                  end,
    Expression3_Length = length(Expression3),
    insert_table(expression3, Expression3),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (3) ~n", [Expression3_Length])),
% ------------------------------------------------
% Expression4 = { ('+' | '-'), SP }, Expression3 ;
% ------------------------------------------------
    Expression4_Prev = case ets:lookup(?CODE_TEMPLATES, expression4) of
                           [{_, Expression4_Exist}] -> Expression4_Exist;
                           _ -> []
                       end,
    Expression4_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression4_Prev ++
        [
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 5 of
                    1 -> "+" ++ ?SP ++ "+" ++ ?SP;
                    2 -> "-" ++ ?SP ++ "-" ++ ?SP;
                    3 -> "+" ++ ?SP;
                    4 -> "-" ++ ?SP;
                    _ -> []
                end ++
                lists:nth(rand:uniform(Expression3_Length), Expression3)
            || _ <- lists:seq(1, Max)
        ]))),
    Expression4 = case length(Expression4_Curr) > Max of
                      true -> lists:sublist(Expression4_Curr, 1, Max);
                      _ -> Expression4_Curr
                  end,
    Expression4_Length = length(Expression4),
    insert_table(expression4, Expression4),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (4) ~n", [Expression4_Length])),
    % --------------------------------------------------------------------------
    % Expression5 = Expression4, { [SP], '^', [SP], Expression4 } ;
    % --------------------------------------------------------------------------
    Expression5_Prev = case ets:lookup(?CODE_TEMPLATES, expression5) of
                           [{_, Expression5_Exist}] -> Expression5_Exist;
                           _ -> []
                       end,
    Expression5_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression5_Prev ++
        [
                lists:nth(rand:uniform(Expression4_Length), Expression4) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 3 of
                    1 ->
                        ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression4_Length), Expression4) ++
                            ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression4_Length), Expression4);
                    2 ->
                        ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression4_Length), Expression4);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression5 = case length(Expression5_Curr) > Max of
                      true -> lists:sublist(Expression5_Curr, 1, Max);
                      _ -> Expression5_Curr
                  end,
    Expression5_Length = length(Expression5),
    insert_table(expression5, Expression5),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (5) ~n", [Expression5_Length])),
    % --------------------------------------------------------------------------
    % Expression6 = Expression5, { ([SP], '*', [SP], Expression5) | ([SP], '/', [SP], Expression5) | ([SP], '%', [SP], Expression5) } ;
    % --------------------------------------------------------------------------
    Expression6_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                           [{_, Expression6_Exist}] -> Expression6_Exist;
                           _ -> []
                       end,
    Expression6_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression6_Prev ++
        [
                lists:nth(rand:uniform(Expression5_Length), Expression5) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 7 of
                    1 ->
                        ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5) ++
                            ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    2 ->
                        ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5) ++
                            ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    3 ->
                        ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5) ++
                            ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    4 ->
                        ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    5 ->
                        ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    6 ->
                        ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression6 = case length(Expression6_Curr) > Max of
                      true -> lists:sublist(Expression6_Curr, 1, Max);
                      _ -> Expression6_Curr
                  end,
    Expression6_Length = length(Expression6),
    insert_table(expression6, Expression6),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (6) ~n", [Expression6_Length])),
% ------------------------------------------------------------------------------------------------
% Expression7 = Expression6, { ([SP], '+', [SP], Expression6) | ([SP], '-', [SP], Expression6) } ;
% ------------------------------------------------------------------------------------------------
    Expression7_Prev = case ets:lookup(?CODE_TEMPLATES, expression7) of
                           [{_, Expression7_Exist}] -> Expression7_Exist;
                           _ -> []
                       end,
    Expression7_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression7_Prev ++
        [
                lists:nth(rand:uniform(Expression6_Length), Expression6) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 5 of
                    1 ->
                        ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression6_Length), Expression6) ++
                            ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression6_Length), Expression6);
                    2 ->
                        ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(Expression6_Length), Expression6) ++
                            ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(Expression6_Length), Expression6);
                    3 ->
                        ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression6_Length), Expression6);
                    4 ->
                        ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(Expression6_Length), Expression6);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression7 = case length(Expression7_Curr) > Max of
                      true -> lists:sublist(Expression7_Curr, 1, Max);
                      _ -> Expression7_Curr
                  end,
    Expression7_Length = length(Expression7),
    insert_table(expression7, Expression7),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (7) ~n", [Expression7_Length])),
% ---------------------------------------------------------
% PartialComparisonExpression = ('=', [SP], Expression7)
%                             | ('<>', [SP], Expression7)
%                             | ('!=', [SP], Expression7)
%                             | ('<', [SP], Expression7)
%                             | ('>', [SP], Expression7)
%                             | ('<=', [SP], Expression7)
%                             | ('>=', [SP], Expression7) ;
% ---------------------------------------------------------
    PartialComparisonExpression_Prev = case ets:lookup(?CODE_TEMPLATES, partial_comparison_expression) of
                                           [{_, PartialComparisonExpression_Exist}] ->
                                               PartialComparisonExpression_Exist;
                                           _ -> []
                                       end,
    PartialComparisonExpression_Curr = sort_list_random(sets:to_list(sets:from_list(
        PartialComparisonExpression_Prev ++
        [
            case rand:uniform(?PRIME) rem 7 of
                1 ->
                    "=" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                2 ->
                    "<>" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                3 ->
                    "!=" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                4 ->
                    "<" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                5 ->
                    ">" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                6 ->
                    "<=" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                _ ->
                    ">=" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7)
            end
            || _ <- lists:seq(1, Max)
        ]))),
    PartialComparisonExpression = case length(PartialComparisonExpression_Curr) > Max of
                                      true ->
                                          lists:sublist(PartialComparisonExpression_Curr, 1, Max);
                                      _ -> PartialComparisonExpression_Curr
                                  end,
    PartialComparisonExpression_Length = length(PartialComparisonExpression),
    insert_table(partial_comparison_expression, PartialComparisonExpression),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses partial_comparison_expression ~n", [PartialComparisonExpression_Length])),
% ------------------------------------------------------------------
% Expression8 = Expression7, { [SP], PartialComparisonExpression } ;
% ------------------------------------------------------------------
    Expression8_Prev = case ets:lookup(?CODE_TEMPLATES, expression8) of
                           [{_, Expression8_Exist}] -> Expression8_Exist;
                           _ -> []
                       end,
    Expression8_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression8_Prev ++
        [
                lists:nth(rand:uniform(Expression7_Length), Expression7) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 3 of
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
    Expression8 = case length(Expression8_Curr) > Max of
                      true -> lists:sublist(Expression8_Curr, 1, Max);
                      _ -> Expression8_Curr
                  end,
    Expression8_Length = length(Expression8),
    insert_table(expression8, Expression8),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (8) ~n", [Expression8_Length])),
% ----------------------------------------------
% Expression9 = { (N,O,T), [SP] }, Expression8 ;
% ----------------------------------------------
    Expression9_Prev = case ets:lookup(?CODE_TEMPLATES, expression9) of
                           [{_, Expression9_Exist}] -> Expression9_Exist;
                           _ -> []
                       end,
    Expression9_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression9_Prev ++
        [
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 3 of
                    1 -> "Not" ++ ?SP_OPT ++ "Not" ++ ?SP_OPT;
                    2 -> "Not" ++ ?SP_OPT;
                    _ -> []
                end ++
                lists:nth(rand:uniform(Expression8_Length), Expression8)
            || _ <- lists:seq(1, Max)
        ]))),
    Expression9 = case length(Expression9_Curr) > Max of
                      true -> lists:sublist(Expression9_Curr, 1, Max);
                      _ -> Expression9_Curr
                  end,
    Expression9_Length = length(Expression9),
    insert_table(expression9, Expression9),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (9) ~n", [Expression9_Length])),
% --------------------------------------------------------------
% Expression10 = Expression9, { SP, (A,N,D), SP, Expression9 } ;
% --------------------------------------------------------------
    Expression10_Prev = case ets:lookup(?CODE_TEMPLATES, expression10) of
                            [{_, Expression10_Exist}] -> Expression10_Exist;
                            _ -> []
                        end,
    Expression10_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression10_Prev ++
        [
                lists:nth(rand:uniform(Expression9_Length), Expression9) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 3 of
                    1 ->
                        ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(Expression9_Length), Expression9) ++
                            ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(Expression9_Length), Expression9);
                    2 ->
                        ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(Expression9_Length), Expression9);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression10 = case length(Expression10_Curr) > Max of
                       true -> lists:sublist(Expression10_Curr, 1, Max);
                       _ -> Expression10_Curr
                   end,
    Expression10_Length = length(Expression10),
    insert_table(expression10, Expression10),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (10) ~n", [Expression10_Length])),
% ----------------------------------------------------------------
% Expression11 = Expression10, { SP, (X,O,R), SP, Expression10 } ;
% ----------------------------------------------------------------
    Expression11_Prev = case ets:lookup(?CODE_TEMPLATES, expression11) of
                            [{_, Expression11_Exist}] -> Expression11_Exist;
                            _ -> []
                        end,
    Expression11_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression11_Prev ++
        [
                lists:nth(rand:uniform(Expression10_Length), Expression10) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 3 of
                    1 ->
                        ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(Expression10_Length), Expression10) ++
                            ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(Expression10_Length), Expression10);
                    2 ->
                        ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(Expression10_Length), Expression10);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression11 = case length(Expression11_Curr) > Max of
                       true -> lists:sublist(Expression11_Curr, 1, Max);
                       _ -> Expression11_Curr
                   end,
    Expression11_Length = length(Expression11),
    insert_table(expression11, Expression11),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (11) ~n", [Expression11_Length])),
% --------------------------------------------------------------
% Expression12 = Expression11, { SP, (O,R), SP, Expression11 } ;
% --------------------------------------------------------------
    Expression12_Prev = case ets:lookup(?CODE_TEMPLATES, expression12) of
                            [{_, Expression12_Exist}] -> Expression12_Exist;
                            _ -> []
                        end,
    Expression12_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression12_Prev ++
        [
                lists:nth(rand:uniform(Expression11_Length), Expression11) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 3 of
                    1 ->
                        ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(Expression11_Length), Expression11) ++
                            ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(Expression11_Length), Expression11);
                    2 ->
                        ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(Expression11_Length), Expression11);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression12 = case length(Expression12_Curr) > Max of
                       true -> lists:sublist(Expression12_Curr, 1, Max);
                       _ -> Expression12_Curr
                   end,
    Expression12_Length = length(Expression12),
    insert_table(expression12, Expression12),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression (12) ~n", [Expression12_Length])),
% ---------------------------
% Expression = Expression12 ;
% ---------------------------
    Expression_Length = length(Expression12),
    insert_table(expression, Expression12),
    erlang:display(io:format("create_code ===> ~5.. B generated clauses expression ~n", [Expression_Length])),
    Expression12.

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
    ?debugFmt("wwe debugging file_create_eunit/2 ===> [~8.. B] Rule: ~p~n", [length(Code), Rule]),
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

file_create_ct_all([]) ->
    ok;
file_create_ct_all([Rule | Rules]) ->
    file_create_ct(Rule),
    file_create_ct_all(Rules).

file_create_ct(Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),
    ?debugFmt("wwe debugging file_create_ct/2 ===> [~8.. B] Rule: ~p~n", [length(Code), Rule]),
    FileName = case lists:member(Rule, [command, cypher, query, statement]) of
                   true -> "performance_";
                   _ -> "reliability_"
               end ++ atom_to_list(Rule) ++ "_SUITE",
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

    file_write_ct(File, Code).

file_write_ct(File, []) ->
    file:close(File);
file_write_ct(File, [H | T]) ->
    io:format(File, "~s~n", ["    ocparse_test" ++ ":common_test_source(\"" ++ H ++ "\")" ++ case T of
                                                                                                 [] ->
                                                                                                     ".";
                                                                                                 _ ->
                                                                                                     ","
                                                                                             end]),
    file_write_ct(File, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Insert generated code into helper table.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_table(Rule, Code) ->
    ?debugFmt("wwe debugging insert_table/3 ===> [~8.. B] Rule: ~p~n", [length(Code), Rule]),
    ets:insert(?CODE_TEMPLATES, {Rule, Code}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Randomising unique lists.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_list_random(L) ->
    F = fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end,
    lists:sort(F, sets:to_list(sets:from_list(L))).
