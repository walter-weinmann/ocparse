-module(test_data_generator).

-export([generate/0]).

-define(CODE_TEMPLATES, code_templates).
-define(DASH, "-").
-define(LEFT_ARROW_HEAD, ">").
-define(MAX, 1000).
-define(NODEBUG, true).
-define(PATH_CT, "ct").
-define(PATH_EUNIT, "test").
-define(PRIME, 101).
-define(RIGHT_ARROW_HEAD, ">").
-define(SP, " ").
-define(WS, []).

-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate Test Data.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    ?debugFmt("wwe debugging generate/0 ===> Start ~n", []),
    crypto:start(),
    create_code_templates(false),
    file_create_ct(false, expression),
    file_create_eunit(false, expression),
    create_code_templates(true),
    crypto:stop(),
    file_create_ct(true, expression),
    file_create_eunit(true, expression).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions - creating Common Test data files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct(Legacy, Rule) ->
    ?debugFmt("wwe debugging file_create_ct/2 ===> Start ~n Legacy: ~p~n Rule: ~p~n", [Legacy, Rule]),
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),
    FileName = "generic_" ++ atom_to_list(Rule) ++ case Legacy of
                                                       true -> "_legacy";
                                                       _ -> []
                                                   end ++ "_SUITE",
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
    io:format(File, "~s~n", ["    octest:ct_string(\"" ++ H ++ "\")" ++ case T of
                                                                            [] -> ".";
                                                                            _ -> ","
                                                                        end]),
    file_write_ct(File, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions - creating EUnit data files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_eunit(Legacy, Rule) ->
    ?debugFmt("wwe debugging file_create_eunit/2 ===> Start ~n Legacy: ~p~n Rule: ~p~n", [Legacy, Rule]),
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),
    FileName = "generic_" ++ atom_to_list(Rule) ++ case Legacy of
                                                       true -> ".legacy";
                                                       _ -> ".tst"
                                                   end,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Helper functions - creating code base.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code_templates(Legacy) ->
    ?debugFmt("wwe debugging create_code_templates/1 ===> Start ~n Legacy: ~p~n", [Legacy]),
    try
        ets:new(?CODE_TEMPLATES, [set, named_table]),
        ?debugFmt("wwe debugging create_code_templates/1 ===> new table ~n", [])
    catch
        error:badarg ->
            ets:delete(?CODE_TEMPLATES),
            ets:new(?CODE_TEMPLATES, [set, named_table]),
            ?debugFmt("wwe debugging create_code_templates/1 ===> existing table ~n", [])
    end,

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------------------------------------------------------------------------------------
% Atom = ...
%      | (T,R,U,E)
%      | (F,A,L,S,E)
%      | (N,U,L,L)
%      | ((C,O,U,N,T), '(', '*', ')')
%      | ... ;
% ---------------------------------------------------------------------------------------
    AtomCount = ["Count(*)"],
    insert_table(atom_false, AtomCount),
    AtomFalse = ["False"],
    insert_table(atom_false, AtomFalse),
    AtomNull = ["Null"],
    insert_table(atom_null, AtomNull),
    AtomTrue = ["True"],
    insert_table(atom_true, AtomTrue),
% ---------------------------------------------------------------------------------------
% DecimalInteger = (('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'), [DigitString])
%                | '0' ;
% ---------------------------------------------------------------------------------------
    DecimalInteger = ["0", "12", "1000", "123456789"],
    DecimalInteger_Length = length(DecimalInteger),
    insert_table(decimal_integer, DecimalInteger),
% ---------------------------------------------------------------------------------------------------------------------------------------------------------------
% (* Any character except "`", enclosed within `backticks`. Backticks are escaped with double backticks. *)EscapedSymbolicName = { '`', { ANY - ('`') }, '`' }- ;
% ---------------------------------------------------------------------------------------------------------------------------------------------------------------
    EscapedSymbolicName = ["`escaped symbolic name 1`", "`esn1`", "``"],
    insert_table(escaped_symbolic_name, EscapedSymbolicName),
% -------------------------
% Explain = E,X,P,L,A,I,N ;
% -------------------------
    Explain = ["Explain"],
    insert_table(explain, Explain),
% --------------------------------------------------------------------------------------------------------
% ExponentDecimalReal = ({ Digit | '.' }- | DecimalInteger), ((E) | (E)), (DigitString | DecimalInteger) ;
% --------------------------------------------------------------------------------------------------------
    ExponentDecimalReal = [".e1", ".E12", "0.e0", "0.E12", "0e1", "0E12", "1e0", "12E12", "12.e0", "123.E654"],
    insert_table(exponent_decimal_real, ExponentDecimalReal),
% ---------------------------------
% HexInteger = ('0',X), HexString ;
% ---------------------------------
    HexInteger = ["0x0", "0X1", "0xabc", "0X0123456789ABCDEF"],
    insert_table(hex_integer, HexInteger),
%-------------------------------
% LiteralIds = { WS, ',', WS } ;
%-------------------------------
    LiteralIds = [?WS ++ "," ++ ?WS],
    insert_table(literal_ids, LiteralIds),
% ---------------------------------
% OctalInteger = '0', OctalString ;
% ---------------------------------
    OctalInteger = ["00", "01", "01234567"],
    insert_table(octal_integer, OctalInteger),
% -------------------------
% Profile = P,R,O,F,I,L,E ;
% -------------------------
    Profile = ["Profile"],
    insert_table(profile, Profile),
% ----------------------------------------------------------------------------------------
% RegularDecimalReal = ({ Digit } | DecimalInteger), '.', (DigitString | DecimalInteger) ;
% ----------------------------------------------------------------------------------------
    RegularDecimalReal = [".0", ".12", "0.0", "0.12", "12.0", "123.654"],
    insert_table(regular_decimal_real, RegularDecimalReal),
% -----------------------------------------------------------------
% StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
%               | ("'", { ANY - ("'" | '\') | EscapedChar }, "'") ;
% -----------------------------------------------------------------
    StringLiteral = ["\\\"This is a string\\\"", "'This is another string'"],
    insert_table(string_literal, StringLiteral),
% -------------------------------------------------------------
% UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;
% -------------------------------------------------------------
    UnescapedSymbolicName = ["symbolic_name_1", "sn2"],
    insert_table(unescaped_symbolic_name, UnescapedSymbolicName),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------
% DoubleLiteral = ExponentDecimalReal
%               | RegularDecimalReal ;
% ------------------------------------
    DoubleLiteral = ExponentDecimalReal ++ RegularDecimalReal,
    insert_table(double_literal, DoubleLiteral),
% ---------------------------------
% IntegerLiteral = HexInteger
%                | OctalInteger
%                | DecimalInteger ;
% ---------------------------------
    IntegerLiteral = HexInteger ++ OctalInteger ++ DecimalInteger,
    IntegerLiteral_Length = length(IntegerLiteral),
    insert_table(integer_literal, IntegerLiteral),
% ------------------------------------
% SymbolicName = UnescapedSymbolicName
%              | EscapedSymbolicName ;
% ------------------------------------
    SymbolicName = UnescapedSymbolicName ++ EscapedSymbolicName,
    SymbolicName_Length = length(SymbolicName),
    insert_table(symbolic_name, SymbolicName),
% -----------------------------------------------------
% VersionNumber = DecimalInteger, '.', DecimalInteger ;
% -----------------------------------------------------
    VersionNumber = [lists:nth(crypto:rand_uniform(1, DecimalInteger_Length), DecimalInteger) ++ "." ++ lists:nth(crypto:rand_uniform(1, DecimalInteger_Length), DecimalInteger) || _ <- lists:seq(1, DecimalInteger_Length)],
    VersionNumber_Length = length(VersionNumber),
    insert_table(version_number, VersionNumber),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------------------------------------------------------------
% ConfigurationOption = SymbolicName, WS, '=', WS, SymbolicName ;
% ---------------------------------------------------------------
    ConfigurationOption = [lists:nth(crypto:rand_uniform(1, SymbolicName_Length), SymbolicName) ++ ?WS ++ "=" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, SymbolicName_Length), SymbolicName) || _ <- lists:seq(1, SymbolicName_Length)],
    ConfigurationOption_Length = length(ConfigurationOption),
    insert_table(configuration_option, ConfigurationOption),
% -----------------------------
% FunctionName = SymbolicName ;
% -----------------------------
    FunctionName = SymbolicName,
    insert_table(function_name, FunctionName),
% --------------------------
% LabelName = SymbolicName ;
% --------------------------
    LabelName = SymbolicName,
    insert_table(label_name, LabelName),
% ---------------------------------------------------------------------
% LegacyParameter = '{', WS, (SymbolicName | DecimalInteger), WS, '}' ;
% ---------------------------------------------------------------------
    case Legacy of
        true ->
            LegacyParameter = ["{" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, SymbolicName_Length), SymbolicName) ++ ?WS ++ "}" || _ <- lists:seq(1, SymbolicName_Length)] ++
                ["{" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, DecimalInteger_Length), DecimalInteger) ++ ?WS ++ "}" || _ <- lists:seq(1, DecimalInteger_Length)];
        _ ->
            LegacyParameter = []
    end,
    insert_table(legacy_parameter, LegacyParameter),
% --------------------------------
% NumberLiteral = DoubleLiteral
%               | IntegerLiteral ;
% --------------------------------
    NumberLiteral = DoubleLiteral ++ IntegerLiteral,
    insert_table(number_literal, NumberLiteral),
% --------------------------------------------------
% Parameter = '$', (SymbolicName | DecimalInteger) ;
% --------------------------------------------------
    Parameter = ["$" ++ lists:nth(crypto:rand_uniform(1, SymbolicName_Length), SymbolicName) || _ <- lists:seq(1, SymbolicName_Length)] ++
        ["$" ++ lists:nth(crypto:rand_uniform(1, DecimalInteger_Length), DecimalInteger) || _ <- lists:seq(1, DecimalInteger_Length)],
    insert_table(parameter, Parameter),
% --------------------------------
% PropertyKeyName = SymbolicName ;
% --------------------------------
    PropertyKeyName = SymbolicName,
    insert_table(property_key_name, PropertyKeyName),
% ---------------------------------------------------------------------------
% RangeLiteral = WS, [IntegerLiteral, WS], ['..', WS, [IntegerLiteral, WS]] ;
% ---------------------------------------------------------------------------
    RangeLiteral = [?WS ++ case rand:uniform(?PRIME) rem 5 of
                               4 ->
                                   lists:nth(crypto:rand_uniform(1, IntegerLiteral_Length), IntegerLiteral) ++ ?WS ++ ".." ++ ?WS ++ lists:nth(crypto:rand_uniform(1, IntegerLiteral_Length), IntegerLiteral) ++ ?WS;
                               3 ->
                                   lists:nth(crypto:rand_uniform(1, IntegerLiteral_Length), IntegerLiteral) ++ ?WS ++ ".." ++ ?WS;
                               2 -> lists:nth(crypto:rand_uniform(1, IntegerLiteral_Length), IntegerLiteral) ++ ?WS;
                               1 ->
                                   ".." ++ ?WS ++ lists:nth(crypto:rand_uniform(1, IntegerLiteral_Length), IntegerLiteral) ++ ?WS;
                               _ -> ".." ++ ?WS
                           end || _ <- lists:seq(1, IntegerLiteral_Length)],
    insert_table(node_label, RangeLiteral),
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

% -------------------------------------------
% Atom_Part_1 = NumberLiteral
%             | StringLiteral
%             | Parameter
%             | LegacyParameter
%             | (T,R,U,E)
%             | (F,A,L,S,E)
%             | (N,U,L,L)
%             | ...
%             | ((C,O,U,N,T), '(', '*', ')')
%             | ...
%             | Variable ;
% -------------------------------------------
    Atom_Part_1 = NumberLiteral ++
        StringLiteral ++
        Parameter ++
        LegacyParameter ++
        AtomTrue ++
        AtomFalse ++
        AtomNull ++
        AtomCount ++
        Variable,
    insert_table(atom, Atom_Part_1),
% --------------------------------------------------------------------------------
% CypherOption = (C,Y,P,H,E,R), [SP, VersionNumber], { SP, ConfigurationOption } ;
% --------------------------------------------------------------------------------
    CypherOption = ["Cypher"] ++
        ["Cypher" ++ case rand:uniform(?PRIME) rem 4 of
                         3 ->
                             ?SP ++ lists:nth(crypto:rand_uniform(1, VersionNumber_Length), VersionNumber) ++ ?SP ++ CO ++ ?SP ++ lists:nth(crypto:rand_uniform(1, ConfigurationOption_Length), ConfigurationOption);
                         2 ->
                             ?SP ++ CO ++ ?SP ++ lists:nth(crypto:rand_uniform(1, ConfigurationOption_Length), ConfigurationOption);
                         1 ->
                             ?SP ++ lists:nth(crypto:rand_uniform(1, VersionNumber_Length), VersionNumber) ++ ?SP ++ CO;
                         _ -> ?SP ++ CO
                     end || CO <- ConfigurationOption],
    insert_table(cypher_option, CypherOption),
% -----------------------------------------------------------------------------------------------------------
% IdentifiedIndexLookup = ':', SymbolicName, '(', SymbolicName, '=', (StringLiteral | LegacyParameter), ')' ;
% ------------------------------------------------------------------------------------------------------------
    IdentifiedIndexLookup = [":" ++ SN ++ "(" ++ SN ++ "=" ++ SL ++ ")" || SN <- SymbolicName, SL <- StringLiteral] ++
        [":" ++ SN ++ "(" ++ SN ++ "=" ++ LP ++ ")" || SN <- SymbolicName, LP <- LegacyParameter],
    insert_table(identified_index_lookup, IdentifiedIndexLookup),
% -----------------------------------------------------------
% IdLookup = '(', (LiteralIds | LegacyParameter | '*'), ')' ;
% -----------------------------------------------------------
    IdLookup = ["(" ++ P ++ ")" || P <- LiteralIds ++ LegacyParameter ++ ["*"]],
    insert_table(id_lookup, IdLookup),
% ---------------------------------------------------------------------------
% IndexQuery = ':', SymbolicName, '(', (StringLiteral | LegacyParameter), ')'
% ---------------------------------------------------------------------------
    IndexQuery = [":" ++ SN ++ "(" ++ SL ++ ")" || SN <- SymbolicName, SL <- StringLiteral] ++
        [":" ++ SN ++ "(" ++ LP ++ ")" || SN <- SymbolicName, LP <- LegacyParameter],
    insert_table(index_query, IndexQuery),
% ----------------------------
% NodeLabel = ':', LabelName ;
% ----------------------------
    NodeLabel = [":" ++ LN || LN <- LabelName],
    NodeLabel_Length = length(NodeLabel),
    insert_table(node_label, NodeLabel),
% ----------------------------------------------------------------------------------
% PropertyLookup = WS, '.', WS, ((PropertyKeyName, ('?' | '!')) | PropertyKeyName) ;
% ----------------------------------------------------------------------------------
    PropertyLookup = [?WS ++ "." ++ ?WS ++ PKN || PKN <- PropertyKeyName] ++
        [?WS ++ "." ++ ?WS ++ PKN ++ "!" || PKN <- PropertyKeyName] ++
        [?WS ++ "." ++ ?WS ++ PKN ++ "?" || PKN <- PropertyKeyName],
    PropertyLookup_Length = length(PropertyLookup),
    insert_table(property_lookup, PropertyLookup),
% ---------------------------------------------------------------------------
% RelationshipTypes = ':', RelTypeName, { WS, '|', [':'], WS, RelTypeName } ;
% ---------------------------------------------------------------------------
    RelationshipTypes = [":" ++ RTN ++ case rand:uniform(?PRIME) rem 6 of
                                           5 ->
                                               ?WS ++ "|" ++ ":" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, RelTypeName_Length), RelTypeName) ++ ?WS ++ "|" ++ ":" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, RelTypeName_Length), RelTypeName);
                                           4 ->
                                               ?WS ++ "|" ++ ":" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, RelTypeName_Length), RelTypeName) ++ ?WS ++ "|" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, RelTypeName_Length), RelTypeName);
                                           3 ->
                                               ?WS ++ "|" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, RelTypeName_Length), RelTypeName) ++ ?WS ++ "|" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, RelTypeName_Length), RelTypeName);
                                           2 ->
                                               ?WS ++ "|" ++ ":" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, RelTypeName_Length), RelTypeName);
                                           1 ->
                                               ?WS ++ "|" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, RelTypeName_Length), RelTypeName);
                                           _ -> []
                                       end || RTN <- RelTypeName],
    insert_table(relation_shiptypes, RelationshipTypes),
% ----------------------------
% RelType = ':', RelTypeName ;
% ----------------------------
    RelType = [":" ++ RTN || RTN <- RelTypeName],
    insert_table(rel_type, RelType),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------
% AnyCypherOption = CypherOption
%                 | Explain
%                 | Profile ;
% ------------------------------
    AnyCypherOption = CypherOption ++ Explain ++ Profile,
    insert_table(any_cypher_option, AnyCypherOption),
% ----------------------------------------------------------------------------------------------
% Hint = WS, (((U,S,I,N,G), SP, (I,N,D,E,X), SP, Variable, NodeLabel, '(', PropertyKeyName, ')')
%           | ((U,S,I,N,G), SP, (J,O,I,N), SP, (O,N), SP, Variable, { WS, ',', WS, Variable })
%           | ((U,S,I,N,G), SP, (S,C,A,N), SP, Variable, NodeLabel)) ;
% ----------------------------------------------------------------------------------------------
    Hint = [?WS ++ "Using" ++ ?SP ++ "Index" ++ ?SP ++ V ++ NL ++ "(" ++ PKN ++ ")" || V <- Variable, NL <- NodeLabel, PKN <- PropertyKeyName] ++
        [?WS ++ "Using" ++ ?SP ++ "Join" ++ ?SP ++ "On" ++ ?SP ++ V ++ case rand:uniform(?PRIME) rem 2 of
                                                                           1 ->
                                                                               ?WS ++ "," ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Variable_Length), Variable);
                                                                           _ -> []
                                                                       end || V <- Variable] ++
        [?WS ++ "Using" ++ ?SP ++ "Scan" ++ ?SP ++ V ++ NL || V <- Variable, NL <- NodeLabel],
    insert_table(hint, Hint),
% --------------------------------------------------------------------------
% Index = (I,N,D,E,X), SP, (O,N), WS, NodeLabel, '(', PropertyKeyName, ')' ;
% --------------------------------------------------------------------------
    Index = ["Index" ++ ?SP ++ "On" ++ ?WS ++ NL ++ "(" ++ PKN ++ ")" || NL <- NodeLabel, PKN <- PropertyKeyName],
    insert_table(index, Index),
% -------------------------------------------
% NodeLabels = NodeLabel, { WS, NodeLabel } ;
% -------------------------------------------
    NodeLabels = [NL ++ case rand:uniform(?PRIME) rem 2 of
                            1 -> ?WS ++ lists:nth(crypto:rand_uniform(1, NodeLabel_Length), NodeLabel);
                            _ -> []
                        end || NL <- NodeLabel],
    insert_table(node_labels, NodeLabels),
% -------------------------------------------------------------------------
% NodeLookup = (N,O,D,E), (IdentifiedIndexLookup | IndexQuery | IdLookup) ;
% -------------------------------------------------------------------------
    NodeLookup = ["Node" ++ IIL || IIL <- IdentifiedIndexLookup] ++
        ["Node" ++ IQ || IQ <- IndexQuery] ++
        ["Node" ++ IL || IL <- IdLookup],
    insert_table(node_lookup, NodeLookup),
% ----------------------------------------------------
% PropertyExpression = Atom, { WS, PropertyLookup }- ;
% ----------------------------------------------------
    PropertyExpression_Part_1 = sets:to_list(sets:from_list([A ++ ?WS ++ PL ++ case rand:uniform(?PRIME) rem 2 of
                                                                                   1 ->
                                                                                       ?WS ++ lists:nth(crypto:rand_uniform(1, PropertyLookup_Length), PropertyLookup);
                                                                                   _ -> []
                                                                               end || A <- Atom_Part_1, PL <- PropertyLookup])),
    insert_table(property_expression, PropertyExpression_Part_1),
% -------------------------------------------------------------------------------------------------------------
% RelationshipLookup = ((R,E,L,A,T,I,O,N,S,H,I,P) | (R,E,L)), (IdentifiedIndexLookup | IndexQuery | IdLookup) ;
% -------------------------------------------------------------------------------------------------------------
    RelationshipLookup = [case rand:uniform(?PRIME) rem 2 of
                              1 -> "Rel";
                              _ -> "Relationship"
                          end ++ I || I <- IdentifiedIndexLookup ++ IndexQuery ++ IdLookup],
    insert_table(relationship_lookup, RelationshipLookup),
% ----------------------------------------------------------------------------------------------------------------------------------
% RelationshipPatternSyntax = ('(', WS, ')',                Dash, '[', Variable, RelType, ']', Dash,                 '(', WS, ')')
%                           | ('(', WS, ')',                Dash, '[', Variable, RelType, ']', Dash, RightArrowHead, '(', WS, ')')
%                           | ('(', WS, ')', LeftArrowHead, Dash, '[', Variable, RelType, ']', Dash,                 '(', WS, ')') ;
% ----------------------------------------------------------------------------------------------------------------------------------
    RelationshipPatternSyntax = sets:to_list(sets:from_list(["(" ++ ?WS ++ ")" ++ case rand:uniform(?PRIME) rem 3 of
                                                                                      2 ->
                                                                                          ?DASH ++ "[" ++ V ++ RT ++ "]" ++ ?DASH;
                                                                                      1 ->
                                                                                          ?DASH ++ "[" ++ V ++ RT ++ "]" ++ ?DASH ++ ?RIGHT_ARROW_HEAD;
                                                                                      _ ->
                                                                                          ?LEFT_ARROW_HEAD ++ ?DASH ++ "[" ++ V ++ RT ++ "]" ++ ?DASH
                                                                                  end ++ "(" ++ ?WS ++ ")" || V <- Variable, RT <- RelType])),
    insert_table(relationship_pattern_syntax, RelationshipPatternSyntax),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------------------
% CreateIndex = (C,R,E,A,T,E), SP, Index ;
% ----------------------------------------
    CreateIndex = ["Create" ++ ?SP ++ I || I <- Index],
    insert_table(create_index, CreateIndex),
% ----------------------------------
% DropIndex = (D,R,O,P), SP, Index ;
% ----------------------------------
    DropIndex = ["Drop" ++ ?SP ++ I || I <- Index],
    insert_table(drop_index, DropIndex),
% ---------------------------
% Expression = Expression12 ;
% ---------------------------
    Expression_Part_1 = create_expression12(?MAX, Legacy, Atom_Part_1, NodeLabels, PropertyLookup),
    insert_table(expression, Expression_Part_1),
% --------------------------------
% Lookup = NodeLookup
%        | RelationshipLookup ;
% --------------------------------
    Lookup = NodeLookup ++ RelationshipLookup,
    insert_table(lookup, Lookup),
% -----------------------------------
% RemoveItem = (Variable, NodeLabels)
%            | PropertyExpression
% -----------------------------------
    RemoveItem_Part_1 = [V ++ NL || V <- Variable, NL <- NodeLabel] ++
        [PE || PE <- PropertyExpression_Part_1],
    insert_table(remove_item, RemoveItem_Part_1),
% ----------------------------------------
% QueryOptions = { AnyCypherOption, WS } ;
% ----------------------------------------
    QueryOptions = [ACO ++ ?WS || ACO <- AnyCypherOption],
    insert_table(query_options, QueryOptions),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% --------------------------------------------
% StartPoint = Variable, WS, '=', WS, Lookup ;
% --------------------------------------------
    StartPoint = [V ++ ?WS ++ "=" ++ ?WS ++ L || V <- Variable, L <- Lookup],
    insert_table(start_point, StartPoint),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 99
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ok.

create_expression12(Max, _Legacy, Atom, NodeLabels, PropertyLookup) ->
    ?debugFmt("wwe debugging create_expression/4 ===> Start ~n Legacy: ~p~n", [_Legacy]),
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
    Expression2_Curr = sets:to_list(sets:from_list(Expression2_Prev ++
    [lists:nth(crypto:rand_uniform(1, Atom_Length), Atom) ++ case rand:uniform(?PRIME) rem 7 of
                                                                 6 ->
                                                                     lists:nth(crypto:rand_uniform(1, NodeLabels_Length), NodeLabels);
                                                                 5 ->
                                                                     lists:nth(crypto:rand_uniform(1, NodeLabels_Length), NodeLabels) ++ lists:nth(crypto:rand_uniform(1, NodeLabels_Length), NodeLabels);
                                                                 4 ->
                                                                     lists:nth(crypto:rand_uniform(1, NodeLabels_Length), NodeLabels) ++ lists:nth(crypto:rand_uniform(1, PropertyLookup_Length), PropertyLookup);
                                                                 3 ->
                                                                     lists:nth(crypto:rand_uniform(1, PropertyLookup_Length), PropertyLookup);
                                                                 2 ->
                                                                     lists:nth(crypto:rand_uniform(1, PropertyLookup_Length), PropertyLookup) ++ lists:nth(crypto:rand_uniform(1, NodeLabels_Length), NodeLabels);
                                                                 1 ->
                                                                     lists:nth(crypto:rand_uniform(1, PropertyLookup_Length), PropertyLookup) ++ lists:nth(crypto:rand_uniform(1, PropertyLookup_Length), PropertyLookup);
                                                                 _ -> []
                                                             end || _ <- lists:seq(1, Max)])),
    Expression2 = case length(Expression2_Curr) > Max of
                      true -> lists:sublist(Expression2_Curr, 1, Max);
                      _ -> Expression2_Curr
                  end,
    Expression2_Length = length(Expression2),
    insert_table(expression, Expression2),
% -----------------------------------------------------------------------------
% Expression3 = Expression2, { (WS, '[', Expression, ']')
%                            | (WS, '[', [Expression], '..', [Expression], ']')
%                            | (((WS, '=~')
%                              | (SP, (I,N))
%                              | (SP, (S,T,A,R,T,S), SP, (W,I,T,H))
%                              | (SP, (E,N,D,S), SP, (W,I,T,H))
%                              | (SP, (C,O,N,T,A,I,N,S))), WS, Expression2)
%                            | (SP, (I,S), SP, (N,U,L,L))
%                            | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L)) } ;
% -----------------------------------------------------------------------------
    Expression3_Prev = case ets:lookup(?CODE_TEMPLATES, expression3) of
                           [{_, Expression3_Exist}] -> Expression3_Exist;
                           _ -> []
                       end,
    Expression3_Curr = Expression3_Prev ++ [lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2) ++ case rand:uniform(?PRIME) rem 12 of
                                                                                                                      11 ->
                                                                                                                          ?WS ++ "[" ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2) ++ "]";
                                                                                                                      10 ->
                                                                                                                          ?WS ++ "[" ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2) ++ ".." ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2) ++ "]";
                                                                                                                      9 ->
                                                                                                                          ?WS ++ "[" ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2) ++ ".." ++ "]";
                                                                                                                      8 ->
                                                                                                                          ?WS ++ "[" ++ ".." ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2) ++ "]";
                                                                                                                      7 ->
                                                                                                                          ?SP ++ "=~" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2);
                                                                                                                      6 ->
                                                                                                                          ?SP ++ "In" ++ ?SP ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2);
                                                                                                                      5 ->
                                                                                                                          ?SP ++ "Starts" ++ ?SP ++ "With" ++ ?SP ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2);
                                                                                                                      4 ->
                                                                                                                          ?SP ++ "Ends" ++ ?SP ++ "With" ++ ?SP ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2);
                                                                                                                      3 ->
                                                                                                                          ?SP ++ "Contains" ++ ?SP ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression2_Length), Expression2);
                                                                                                                      2 ->
                                                                                                                          ?SP ++ "Is" ++ ?SP ++ "Null";
                                                                                                                      1 ->
                                                                                                                          ?SP ++ "Is" ++ ?SP ++ "Not" ++ ?SP ++ "Null";
                                                                                                                      _ ->
                                                                                                                          []
                                                                                                                  end || _ <- lists:seq(1, Expression2_Length)],
    Expression3 = case length(Expression3_Curr) > Max of
                      true -> lists:sublist(Expression3_Curr, 1, Max);
                      _ -> Expression3_Curr
                  end,
    Expression3_Length = length(Expression3),
    insert_table(expression, Expression3),
% ------------------------------------------------
% Expression4 = { ('+' | '-'), WS }, Expression3 ;
% ------------------------------------------------
    Expression4_Prev = case ets:lookup(?CODE_TEMPLATES, expression4) of
                           [{_, Expression4_Exist}] -> Expression4_Exist;
                           _ -> []
                       end,
    Expression4_Curr = Expression4_Prev ++ [case rand:uniform(?PRIME) rem 3 of
                                                2 -> "+" ++ ?WS;
                                                1 -> "-" ++ ?WS;
                                                _ -> []
                                            end ++ lists:nth(crypto:rand_uniform(1, Expression3_Length), Expression3) || _ <- lists:seq(1, Expression3_Length)],
    Expression4 = case length(Expression4_Curr) > Max of
                      true -> lists:sublist(Expression4_Curr, 1, Max);
                      _ -> Expression4_Curr
                  end,
    Expression4_Length = length(Expression4),
    insert_table(expression, Expression4),
% ---------------------------------------------------------
% Expression5 = Expression4, { WS, '^', WS, Expression4 } ;
% ---------------------------------------------------------
    Expression5_Prev = case ets:lookup(?CODE_TEMPLATES, expression5) of
                           [{_, Expression5_Exist}] -> Expression5_Exist;
                           _ -> []
                       end,
    Expression5_Curr = Expression5_Prev ++ [lists:nth(crypto:rand_uniform(1, Expression4_Length), Expression4) ++ case rand:uniform(?PRIME) rem 4 of
                                                                                                                      3 ->
                                                                                                                          ?WS ++ "^" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression4_Length), Expression4);
                                                                                                                      2 ->
                                                                                                                          ?WS ++ "^" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression4_Length), Expression4);
                                                                                                                      1 ->
                                                                                                                          ?WS ++ "^" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression4_Length), Expression4);
                                                                                                                      _ ->
                                                                                                                          []
                                                                                                                  end || _ <- lists:seq(1, Expression4_Length)],
    Expression5 = case length(Expression5_Curr) > Max of
                      true -> lists:sublist(Expression5_Curr, 1, Max);
                      _ -> Expression5_Curr
                  end,
    Expression5_Length = length(Expression5),
    insert_table(expression, Expression5),
% ---------------------------------------------------------------------------------------------------------------------
% Expression6 = Expression5, { (WS, '*', WS, Expression5) | (WS, '/', WS, Expression5) | (WS, '%', WS, Expression5) } ;
% ---------------------------------------------------------------------------------------------------------------------
    Expression6_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                           [{_, Expression6_Exist}] -> Expression6_Exist;
                           _ -> []
                       end,
    Expression6_Curr = Expression6_Prev ++ [lists:nth(crypto:rand_uniform(1, Expression5_Length), Expression5) ++ case rand:uniform(?PRIME) rem 4 of
                                                                                                                      3 ->
                                                                                                                          ?WS ++ "*" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression5_Length), Expression5);
                                                                                                                      2 ->
                                                                                                                          ?WS ++ "/" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression5_Length), Expression5);
                                                                                                                      1 ->
                                                                                                                          ?WS ++ "%" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression5_Length), Expression5);
                                                                                                                      _ ->
                                                                                                                          []
                                                                                                                  end || _ <- lists:seq(1, Expression5_Length)],
    Expression6 = case length(Expression6_Curr) > Max of
                      true -> lists:sublist(Expression6_Curr, 1, Max);
                      _ -> Expression6_Curr
                  end,
    Expression6_Length = length(Expression6),
    insert_table(expression, Expression6),
% -----------------------------------------------------------------------------------------
% Expression7 = Expression6, { (WS, '+', WS, Expression6) | (WS, '-', WS, Expression6) } ;
% -----------------------------------------------------------------------------------------
    Expression7_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                           [{_, Expression7_Exist}] -> Expression7_Exist;
                           _ -> []
                       end,
    Expression7_Curr = Expression7_Prev ++ [lists:nth(crypto:rand_uniform(1, Expression6_Length), Expression6) ++ case rand:uniform(?PRIME) rem 3 of
                                                                                                                      2 ->
                                                                                                                          ?WS ++ "+" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression6_Length), Expression6);
                                                                                                                      1 ->
                                                                                                                          ?WS ++ "-" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression6_Length), Expression6);
                                                                                                                      _ ->
                                                                                                                          []
                                                                                                                  end || _ <- lists:seq(1, Expression6_Length)],
    Expression7 = case length(Expression7_Curr) > Max of
                      true -> lists:sublist(Expression7_Curr, 1, Max);
                      _ -> Expression7_Curr
                  end,
    Expression7_Length = length(Expression7),
    insert_table(expression, Expression7),
% ----------------------------------------------------------------
% Expression8 = Expression7, { WS, PartialComparisonExpression } ;
% ----------------------------------------------------------------
% PartialComparisonExpression = ('=', WS, Expression7)
%                             | ('<>', WS, Expression7)
%                             | ('!=', WS, Expression7)
%                             | ('<', WS, Expression7)
%                             | ('>', WS, Expression7)
%                             | ('<=', WS, Expression7)
%                             | ('>=', WS, Expression7) ;
% ----------------------------------------------------------------
    Expression8_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                           [{_, Expression8_Exist}] -> Expression8_Exist;
                           _ -> []
                       end,
    Expression8_Curr = Expression8_Prev ++ [lists:nth(crypto:rand_uniform(1, Expression7_Length), Expression7) ++ case rand:uniform(?PRIME) rem 8 of
                                                                                                                      7 ->
                                                                                                                          ?WS ++ "=" ++ ?WS ++ lists:nth(crypto:rand_uniform(1, Expression7_Length), Expression7);
                                                                                                                      6 ->
                                                                                                                          ?WS ++ "<>" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression7_Length), Expression7);
                                                                                                                      5 ->
                                                                                                                          ?WS ++ "!=" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression7_Length), Expression7);
                                                                                                                      4 ->
                                                                                                                          ?WS ++ "<" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression7_Length), Expression7);
                                                                                                                      3 ->
                                                                                                                          ?WS ++ ">" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression7_Length), Expression7);
                                                                                                                      2 ->
                                                                                                                          ?WS ++ "<=" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression7_Length), Expression7);
                                                                                                                      1 ->
                                                                                                                          ?WS ++ ">=" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression7_Length), Expression7);
                                                                                                                      _ ->
                                                                                                                          []
                                                                                                                  end || _ <- lists:seq(1, Expression7_Length)],
    Expression8 = case length(Expression8_Curr) > Max of
                      true -> lists:sublist(Expression8_Curr, 1, Max);
                      _ -> Expression8_Curr
                  end,
    Expression8_Length = length(Expression8),
    insert_table(expression, Expression8),
% ------------------------------------------------
% Expression9 = { SP, (N,O,T), SP }, Expression8 ;
% ------------------------------------------------
    Expression9_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                           [{_, Expression9_Exist}] -> Expression9_Exist;
                           _ -> []
                       end,
    Expression9_Curr = Expression9_Prev ++ [case rand:uniform(?PRIME) rem 2 of
                                                1 ->
                                                    ?SP ++ "Not" ++ ?SP;
                                                _ ->
                                                    []
                                            end ++ lists:nth(crypto:rand_uniform(1, Expression8_Length), Expression8) || _ <- lists:seq(1, Expression8_Length)],
    Expression9 = case length(Expression9_Curr) > Max of
                      true -> lists:sublist(Expression9_Curr, 1, Max);
                      _ -> Expression9_Curr
                  end,
    Expression9_Length = length(Expression9),
    insert_table(expression, Expression9),
% --------------------------------------------------------------
% Expression10 = Expression9, { SP, (A,N,D), SP, Expression9 } ;
% --------------------------------------------------------------
    Expression10_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                            [{_, Expression10_Exist}] -> Expression10_Exist;
                            _ -> []
                        end,
    Expression10_Curr = Expression10_Prev ++ [lists:nth(crypto:rand_uniform(1, Expression9_Length), Expression9) ++ case rand:uniform(?PRIME) rem 2 of
                                                                                                                        1 ->
                                                                                                                            ?SP ++ "And" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression9_Length), Expression9);
                                                                                                                        _ ->
                                                                                                                            []
                                                                                                                    end || _ <- lists:seq(1, Expression9_Length)],
    Expression10 = case length(Expression10_Curr) > Max of
                       true -> lists:sublist(Expression10_Curr, 1, Max);
                       _ -> Expression10_Curr
                   end,
    Expression10_Length = length(Expression10),
    insert_table(expression, Expression10),
% --------------------------------------------------------------
% Expression11 = Expression10, { SP, (X,O,R), SP, Expression10 } ;
% --------------------------------------------------------------
    Expression11_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                            [{_, Expression11_Exist}] -> Expression11_Exist;
                            _ -> []
                        end,
    Expression11_Curr = Expression11_Prev ++ [lists:nth(crypto:rand_uniform(1, Expression10_Length), Expression10) ++ case rand:uniform(?PRIME) rem 2 of
                                                                                                                          1 ->
                                                                                                                              ?SP ++ "Xor" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression10_Length), Expression10);
                                                                                                                          _ ->
                                                                                                                              []
                                                                                                                      end || _ <- lists:seq(1, Expression10_Length)],
    Expression11 = case length(Expression11_Curr) > Max of
                       true -> lists:sublist(Expression11_Curr, 1, Max);
                       _ -> Expression11_Curr
                   end,
    Expression11_Length = length(Expression11),
    insert_table(expression, Expression11),
% ----------------------------------------------------------------
% Expression12 = Expression11, { SP, (O,R), SP, Expression11 } ;
% ----------------------------------------------------------------
    Expression12_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                            [{_, Expression12_Exist}] -> Expression12_Exist;
                            _ -> []
                        end,
    Expression12_Curr = Expression12_Prev ++ [lists:nth(crypto:rand_uniform(1, Expression11_Length), Expression11) ++ case rand:uniform(?PRIME) rem 2 of
                                                                                                                          1 ->
                                                                                                                              ?SP ++ "Or" ++ ?SP ++ lists:nth(crypto:rand_uniform(1, Expression11_Length), Expression11);
                                                                                                                          _ ->
                                                                                                                              []
                                                                                                                      end || _ <- lists:seq(1, Expression11_Length)],
    Expression12 = case length(Expression12_Curr) > Max of
                       true -> lists:sublist(Expression12_Curr, 1, Max);
                       _ -> Expression12_Curr
                   end,
    insert_table(expression, Expression12),

    Expression2.

insert_table(Rule, Code) ->
    %?debugFmt("wwe debugging insert_table/2 ===> Rule: ~p [~p]~n", [Rule, length(Code)]),
    ets:insert(?CODE_TEMPLATES, {Rule, Code}).
