-module(test_generator).

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
-define(ALL_ATOM_LEGACY, [literal,
    parameter,
    legacy_parameter,
    case_expression,
    atom_count,
    list_comprehension,
    atom_square_bracket,
    atom_filter,
    atom_extract,
    reduce,
    atom_all,
    atom_any,
    atom_none,
    atom_single,
    shortest_path_pattern,
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
-define(ALL_CLAUSE_LEGACY, [load_csv,
    start,
    match,
    unwind,
    merge,
    create,
    create_unique,
    set,
    delete,
    remove,
    foreach,
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Generate Test Data.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    ?debugFmt("wwe debugging generate/0 ===> Start ~n", []),

    % ----------------------------------------------------------------------------------------------
    % Standard version -----------------------------------------------------------------------------
    % ----------------------------------------------------------------------------------------------

    ?debugFmt("wwe debugging generate/0 ===> Start Standard Version ~n", []),

    create_code(false),

    % generic common tests
    ok = file_create_ct_all(false,
        ?ALL_CLAUSE),

    % performance common tests
    ok = file_create_ct_all(false,
        [cypher,
            query,
            statement]),

    % generic eunit tests
    ok = file_create_eunit_all(false,
        ?ALL_CLAUSE),

    % ----------------------------------------------------------------------------------------------
    % Legacy version -------------------------------------------------------------------------------
    % ----------------------------------------------------------------------------------------------

    ?debugFmt("wwe debugging generate/0 ===> Start Legacy Version ~n", []),

    create_code(true),

    % generic common tests
    ok = file_create_ct_all(true,
        ?ALL_CLAUSE_LEGACY ++
        ?ALL_COMMAND),

    % performance common tests
    ok = file_create_ct_all(true,
        [command,
            cypher,
            query,
            statement]),

    % generic eunit tests
    ok = file_create_eunit_all(true,
        ?ALL_CLAUSE_LEGACY ++
        ?ALL_COMMAND),

    ?debugFmt("wwe debugging generate/0 ===> End ~n", []),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating code base.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(Legacy) ->
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
    insert_table(Legacy, atom_count, AtomCount),
    AtomNull = ["Null"],
    insert_table(Legacy, atom_null, AtomNull),
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
    insert_table(Legacy, boolean_literal, BooleanLiteral),
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
    insert_table(Legacy, decimal_integer, DecimalInteger),
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
    insert_table(Legacy, escaped_symbolic_name, EscapedSymbolicName),
% -------------------------
% Explain = E,X,P,L,A,I,N ;
% -------------------------
    Explain = case Legacy of
                  true -> [
                      "Explain"
                  ];
                  _ -> []
              end,
    insert_table(Legacy, explain, Explain),
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
    insert_table(Legacy, exponent_decimal_real, ExponentDecimalReal),
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
    insert_table(Legacy, hex_integer, HexInteger),
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
    insert_table(Legacy, octal_integer, OctalInteger),
% -------------------------
% Profile = P,R,O,F,I,L,E ;
% -------------------------
    Profile = case Legacy of
                  true -> [
                      "Profile"
                  ];
                  _ -> []
              end,
    insert_table(Legacy, profile, Profile),
% ----------------------------------------------------------------------------------------
% RegularDecimalReal = ({ Digit } | DecimalInteger), '.', (DigitString | DecimalInteger) ;
% ----------------------------------------------------------------------------------------
    RegularDecimalReal = sort_list_random(
        case Legacy of
            true -> ["1.0", "2.12", "0.0", "0.12", "12.0", "123.654"];
            _ -> [".0", ".12", "0.0", "0.12", "12.0", "123.654"]
        end
    ),
    insert_table(Legacy, regular_decimal_real, RegularDecimalReal),
% -----------------------------------------------------------------
% StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
%               | ("'", { ANY - ("'" | '\') | EscapedChar }, "'") ;
% -----------------------------------------------------------------
    StringLiteral = sort_list_random([
        "\\\"d_str\\\"",
        "'s_str'"
    ]),
    StringLiteral_Length = length(StringLiteral),
    insert_table(Legacy, string_literal, StringLiteral),
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
    insert_table(Legacy, unescaped_symbolic_name, UnescapedSymbolicName),

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
    insert_table(Legacy, double_literal, DoubleLiteral),
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
    insert_table(Legacy, integer_literal, IntegerLiteral),
% ------------------------------------
% SymbolicName = UnescapedSymbolicName
%              | EscapedSymbolicName ;
% ------------------------------------
    SymbolicName = sort_list_random(
        UnescapedSymbolicName ++
        EscapedSymbolicName),
    SymbolicName_Length = length(SymbolicName),
    insert_table(Legacy, symbolic_name, SymbolicName),
% -----------------------------------------------------
% VersionNumber = DecimalInteger, '.', DecimalInteger ;
% -----------------------------------------------------
    VersionNumber = case Legacy of
                        true ->
                            sort_list_random([
                                    DI ++ "." ++
                                    lists:nth(rand:uniform(DecimalInteger_Length), DecimalInteger)
                                || DI <- DecimalInteger
                            ]);
                        _ -> []
                    end,
    VersionNumber_Length = length(VersionNumber),
    insert_table(Legacy, version_number, VersionNumber),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------------------------------------------------------------
% ConfigurationOption = SymbolicName, [SP], '=', [SP], SymbolicName ;
% ---------------------------------------------------------------
    ConfigurationOption = case Legacy of
                              true -> sort_list_random([
                                      SN ++ ?SP_OPT ++ "=" ++
                                      ?SP_OPT ++ lists:nth(rand:uniform(SymbolicName_Length), SymbolicName)
                                  || SN <- SymbolicName
                              ]);
                              _ -> []
                          end,
    ConfigurationOption_Length = length(ConfigurationOption),
    insert_table(Legacy, configuration_option, ConfigurationOption),
% -----------------------------
% FunctionName = SymbolicName ;
% -----------------------------
    FunctionName = SymbolicName ++
        ["count"] ++
        case Legacy of
            true -> ["exists"];
            _ -> []
        end,
    FunctionName_Length = length(FunctionName),
    insert_table(Legacy, function_name, FunctionName),
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
    insert_table(Legacy, literal_ids, LiteralIds),
% --------------------------
% LabelName = SymbolicName ;
% --------------------------
    LabelName = SymbolicName,
    insert_table(Legacy, label_name, LabelName),
% -------------------------------------------------------------------------
% LegacyParameter = '{', [SP], (SymbolicName | DecimalInteger), [SP], '}' ;
% -------------------------------------------------------------------------
    LegacyParameter = case Legacy of
                          true ->
                              sort_list_random([
                                      "{" ++ ?SP_OPT ++ SN ++ ?SP_OPT ++ "}"
                                  || SN <- SymbolicName
                              ] ++
                              [
                                      "{" ++ ?SP_OPT ++ DI ++ ?SP_OPT ++ "}"
                                  || DI <- DecimalInteger
                              ]);
                          _ -> []
                      end,
    LegacyParameter_Length = length(LegacyParameter),
    insert_table(Legacy, legacy_parameter, LegacyParameter),
% --------------------------------
% NumberLiteral = DoubleLiteral
%               | IntegerLiteral ;
% --------------------------------
    NumberLiteral = sort_list_random(
        DoubleLiteral ++
        IntegerLiteral),
    insert_table(Legacy, number_literal, NumberLiteral),
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
    insert_table(Legacy, parameter, Parameter),
% --------------------------------------------------------------------------------------------------
% PeriodicCommitHint = (U,S,I,N,G), SP, (P,E,R,I,O,D,I,C), SP, (C,O,M,M,I,T), [SP, IntegerLiteral] ;
% --------------------------------------------------------------------------------------------------
    PeriodicCommitHint = sort_list_random(
        ["Using" ++ ?SP ++ "Periodic" ++ ?SP ++ "Commit" ++ ?SP] ++
        [
                "Using" ++ ?SP ++ "Periodic" ++ ?SP ++ "Commit" ++ ?SP ++ IL ++ ?SP
            || IL <- IntegerLiteral
        ]),
    PeriodicCommitHint_Length = length(PeriodicCommitHint),
    insert_table(Legacy, periodic_commit_hint, PeriodicCommitHint),
% --------------------------------
% PropertyKeyName = SymbolicName ;
% --------------------------------
    PropertyKeyName = SymbolicName,
    PropertyKeyName_Length = length(PropertyKeyName),
    insert_table(Legacy, property_key_name, PropertyKeyName),
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
    insert_table(Legacy, range_literal, RangeLiteral),
% ----------------------------
% RelTypeName = SymbolicName ;
% ----------------------------
    RelTypeName = SymbolicName,
    RelTypeName_Length = length(RelTypeName),
    insert_table(Legacy, rel_type_name, RelTypeName),
% -------------------------
% Variable = SymbolicName ;
% -------------------------
    Variable = SymbolicName,
    Variable_Length = length(Variable),
    insert_table(Legacy, variable, Variable),

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
    insert_table(Legacy, literal_part_1, Literal_Part_1),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------------------
% Atom = Literal
%      | Parameter
%      | LegacyParameter
%      | ...
%      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
%      | ...
%      | Variable ;
% ------------------------------------------------------
    Atom_Part_1 = sort_list_random(
        Literal_Part_1 ++
            Parameter ++
            case Legacy of
                true -> LegacyParameter;
                _ -> []
            end ++
            AtomCount ++
            Variable),
    insert_table(Legacy, atom_part_1, Atom_Part_1),
% --------------------------------------------------------------------------------
% CypherOption = (C,Y,P,H,E,R), [SP, VersionNumber], { SP, ConfigurationOption } ;
% --------------------------------------------------------------------------------
    CypherOption = case Legacy of
                       true ->
                           sort_list_random([
                               "Cypher"
                           ] ++
                           [
                                   "Cypher" ++
                                   case rand:uniform(?PRIME) rem 5 of
                                       1 -> ?SP ++ lists:nth(rand:uniform(VersionNumber_Length), VersionNumber) ++
                                           ?SP ++ CO ++
                                           ?SP ++ lists:nth(rand:uniform(ConfigurationOption_Length), ConfigurationOption);
                                       2 -> ?SP ++ CO ++
                                           ?SP ++ lists:nth(rand:uniform(ConfigurationOption_Length), ConfigurationOption);
                                       3 -> ?SP ++ lists:nth(rand:uniform(VersionNumber_Length), VersionNumber) ++
                                           ?SP ++ CO;
                                       4 -> ?SP ++ lists:nth(rand:uniform(VersionNumber_Length), VersionNumber);
                                       _ -> ?SP ++ CO
                                   end || CO <- ConfigurationOption
                           ]);
                       _ -> []
                   end,
    insert_table(Legacy, cypher_option, CypherOption),
% -----------------------------------------------------------------------------------------------------------
% IdentifiedIndexLookup = ':', SymbolicName, '(', SymbolicName, '=', (StringLiteral | LegacyParameter), ')' ;
% ------------------------------------------------------------------------------------------------------------
    IdentifiedIndexLookup_Targ = SymbolicName_Length + LegacyParameter_Length,
    IdentifiedIndexLookup = case Legacy of
                                true ->
                                    sort_list_random(
                                        [
                                                ":" ++ lists:nth(rand:uniform(SymbolicName_Length), SymbolicName) ++
                                                "(" ++ lists:nth(rand:uniform(SymbolicName_Length), SymbolicName) ++
                                                "=" ++ lists:nth(rand:uniform(StringLiteral_Length), StringLiteral) ++ ")"
                                            || _ <- lists:seq(1, IdentifiedIndexLookup_Targ)
                                        ] ++
                                        [
                                                ":" ++ lists:nth(rand:uniform(SymbolicName_Length), SymbolicName) ++
                                                "(" ++ lists:nth(rand:uniform(SymbolicName_Length), SymbolicName) ++
                                                "=" ++ lists:nth(rand:uniform(LegacyParameter_Length), LegacyParameter) ++ ")"
                                            || _ <- lists:seq(1, IdentifiedIndexLookup_Targ)
                                        ]
                                    );
                                _ -> []
                            end,
    insert_table(Legacy, identified_index_lookup, IdentifiedIndexLookup),
% -----------------------------------------------------------
% IdLookup = '(', (LiteralIds | LegacyParameter | '*'), ')' ;
% -----------------------------------------------------------
    IdLookup_Targ = LegacyParameter_Length,
    IdLookup = case Legacy of
                   true ->
                       sort_list_random(
                           [
                                   "(" ++ LI ++ ")"
                               || LI <- LiteralIds
                           ] ++
                               [
                                       "(" ++
                                       ?SP ++ lists:nth(rand:uniform(LegacyParameter_Length), LegacyParameter) ++
                                       ")"
                                   || _ <- lists:seq(1, IdLookup_Targ)
                               ] ++
                               [
                                       "(" ++ "*" ++ ")"
                               ]
                       );
                   _ -> []
               end,
    insert_table(Legacy, id_lookup, IdLookup),
% ---------------------------------------------------------------------------
% IndexQuery = ':', SymbolicName, '(', (StringLiteral | LegacyParameter), ')'
% ---------------------------------------------------------------------------
    IndexQuery_Targ = SymbolicName_Length + LegacyParameter_Length + StringLiteral_Length,
    IndexQuery = case Legacy of
                     true ->
                         sort_list_random(
                             [
                                     ":" ++ lists:nth(rand:uniform(SymbolicName_Length), SymbolicName) ++
                                     "(" ++ lists:nth(rand:uniform(StringLiteral_Length), StringLiteral) ++ ")"
                                 || _ <- lists:seq(1, IndexQuery_Targ)
                             ] ++
                             [
                                     ":" ++ lists:nth(rand:uniform(SymbolicName_Length), SymbolicName) ++
                                     "(" ++ lists:nth(rand:uniform(LegacyParameter_Length), LegacyParameter) ++ ")"
                                 || _ <- lists:seq(1, IndexQuery_Targ)
                             ]
                         );
                     _ -> []
                 end,
    insert_table(Legacy, index_query, IndexQuery),
% ----------------------------
% NodeLabel = ':', LabelName ;
% ----------------------------
    NodeLabel = sort_list_random([
            ":" ++ LN
        || LN <- LabelName
    ]),
    NodeLabel_Length = length(NodeLabel),
    insert_table(Legacy, node_label, NodeLabel),
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
    insert_table(Legacy, property_lookup, PropertyLookup),
% -------------------------------------------------------------------------------
% RelationshipTypes = ':', RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;
% -------------------------------------------------------------------------------
    RelationshipTypes = sort_list_random([
            ":" ++ RTN ++
            case rand:uniform(?PRIME) rem 3 of
                1 -> ?SP_OPT ++ "|" ++ ":" ++ ?SP_OPT ++ lists:nth(rand:uniform(RelTypeName_Length), RelTypeName);
                2 -> ?SP_OPT ++ "|" ++ ?SP_OPT ++ lists:nth(rand:uniform(RelTypeName_Length), RelTypeName);
                _ -> []
            end
        || RTN <- RelTypeName
    ]),
    RelationshipTypes_Length = length(RelationshipTypes),
    insert_table(Legacy, relationship_types, RelationshipTypes),
% ----------------------------
% RelType = ':', RelTypeName ;
% ----------------------------
    RelType = sort_list_random([
            ":" ++ RTN
        || RTN <- RelTypeName
    ]),
    RelType_Length = length(RelType),
    insert_table(Legacy, rel_type, RelType),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------
% AnyCypherOption = CypherOption
%                 | Explain
%                 | Profile ;
% ------------------------------
    AnyCypherOption = case Legacy of
                          true -> sort_list_random(
                              CypherOption ++
                                  Explain ++
                                  Profile);
                          _ -> []
                      end,
    AnyCypherOption_Length = length(AnyCypherOption),
    insert_table(Legacy, any_cypher_option, AnyCypherOption),
% ------------------------------------------------------------------------------------------------
% Hint = [SP], (((U,S,I,N,G), SP, (I,N,D,E,X), SP, Variable, NodeLabel, '(', PropertyKeyName, ')')
%           | ((U,S,I,N,G), SP, (J,O,I,N), SP, (O,N), SP, Variable, { [SP], ',', [SP], Variable })
%           | ((U,S,I,N,G), SP, (S,C,A,N), SP, Variable, NodeLabel)) ;
% ------------------------------------------------------------------------------------------------
% wwe ???
% Hint = SP, (((U,S,I,N,G), SP, (I,N,D,E,X), SP, Variable, NodeLabel, '(', PropertyKeyName, ')')
%           | ((U,S,I,N,G), SP, (J,O,I,N), SP, (O,N), SP, Variable, { [SP], ',', [SP], Variable })
%           | ((U,S,I,N,G), SP, (S,C,A,N), SP, Variable, NodeLabel)) ;
% ------------------------------------------------------------------------------------------------
    Hint_Targ = NodeLabel_Length + PropertyKeyName_Length + Variable_Length,
    Hint = case Legacy of
               true -> sort_list_random(
                   [
                           ?SP ++ "Using" ++ ?SP ++ "Index" ++ ?SP ++
                           lists:nth(rand:uniform(Variable_Length), Variable) ++
                           lists:nth(rand:uniform(NodeLabel_Length), NodeLabel) ++
                           "(" ++ lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName) ++ ")"
                       || _ <- lists:seq(1, Hint_Targ)
                   ] ++
                       [
                               ?SP ++ "Using" ++ ?SP ++ "Join" ++ ?SP ++ "On" ++ ?SP ++
                               lists:nth(rand:uniform(Variable_Length), Variable) ++
                               case rand:uniform(?PRIME) rem 3 of
                                   1 ->
                                       ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT ++
                                           "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Variable_Length), Variable);
                                   2 -> ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Variable_Length), Variable);
                                   _ -> []
                               end
                           || _ <- lists:seq(1, Hint_Targ)
                       ] ++
                       [
                               ?SP ++ "Using" ++ ?SP ++ "Scan" ++ ?SP ++
                               lists:nth(rand:uniform(Variable_Length), Variable) ++
                               lists:nth(rand:uniform(NodeLabel_Length), NodeLabel)
                           || _ <- lists:seq(1, Hint_Targ)
                       ]
               );
               _ -> []
           end,
    Hint_Length = length(Hint),
    insert_table(Legacy, hint, Hint),
% ----------------------------------------------------------------------------
% Index = (I,N,D,E,X), SP, (O,N), [SP], NodeLabel, '(', PropertyKeyName, ')' ;
% ----------------------------------------------------------------------------
    Index_Targ = NodeLabel_Length + PropertyKeyName_Length,
    Index = case Legacy of
                true ->
                    sort_list_random([
                            "Index" ++ ?SP ++ "On" ++ ?SP_OPT ++
                            lists:nth(rand:uniform(NodeLabel_Length), NodeLabel) ++
                            "(" ++ lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName) ++ ")"
                        || _ <- lists:seq(1, Index_Targ)
                    ]);
                _ -> []
            end,
    Index_Length = length(Index),
    insert_table(Legacy, index, Index),
% ---------------------------------------------
% NodeLabels = NodeLabel, { [SP], NodeLabel } ;
% ---------------------------------------------
    NodeLabels = sort_list_random([
            NL ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> ?SP_OPT ++ lists:nth(rand:uniform(NodeLabel_Length), NodeLabel);
                _ -> []
            end
        || NL <- NodeLabel]),
    NodeLabels_Length = length(NodeLabels),
    insert_table(Legacy, node_labels, NodeLabels),
% -------------------------------------------------------------------------------
% NodeLookup = (N,O,D,E), [SP], (IdentifiedIndexLookup | IndexQuery | IdLookup) ;
% -------------------------------------------------------------------------------
    NodeLookup = sort_list_random([
            "Node" ++ ?SP_OPT ++ IIL
        || IIL <- IdentifiedIndexLookup
    ] ++
        [
                "Node" ++ ?SP_OPT ++ IQ
            || IQ <- IndexQuery
        ] ++
        [
                "Node" ++ ?SP_OPT ++ IL
            || IL <- IdLookup
        ]),
    insert_table(Legacy, node_lookup, NodeLookup),
% -------------------------------------------------------------------------------------------------------------
% RelationshipLookup = ((R,E,L,A,T,I,O,N,S,H,I,P) | (R,E,L)), (IdentifiedIndexLookup | IndexQuery | IdLookup) ;
% -------------------------------------------------------------------------------------------------------------
    RelationshipLookup = sort_list_random([
            case rand:uniform(?PRIME) rem 2 of
                1 -> "Rel";
                _ -> "Relationship"
            end ++
            I
        || I <- IdentifiedIndexLookup ++ IndexQuery ++ IdLookup
    ]),
    insert_table(Legacy, relationship_lookup, RelationshipLookup),
% --------------------------------------------------------------------------------------------------------------------------------------
% RelationshipPatternSyntax = ('(', [SP], ')',                Dash, '[', Variable, RelType, ']', Dash,                 '(', [SP], ')')
%                           | ('(', [SP], ')',                Dash, '[', Variable, RelType, ']', Dash, RightArrowHead, '(', [SP], ')')
%                           | ('(', [SP], ')', LeftArrowHead, Dash, '[', Variable, RelType, ']', Dash,                 '(', [SP], ')') ;
% --------------------------------------------------------------------------------------------------------------------------------------
    RelationshipPatternSyntax = sort_list_random([
            "(" ++ ?SP_OPT ++ ")" ++
            case rand:uniform(?PRIME) rem 3 of
                1 -> ?DASH ++ "[" ++
                    lists:nth(rand:uniform(Variable_Length), Variable) ++
                    lists:nth(rand:uniform(RelType_Length), RelType) ++
                    "]" ++ ?DASH;
                2 -> ?DASH ++ "[" ++
                    lists:nth(rand:uniform(Variable_Length), Variable) ++
                    lists:nth(rand:uniform(RelType_Length), RelType) ++
                    "]" ++ ?DASH ++ ?RIGHT_ARROW_HEAD;
                _ -> ?LEFT_ARROW_HEAD ++ ?DASH ++ "[" ++
                    lists:nth(rand:uniform(Variable_Length), Variable) ++
                    lists:nth(rand:uniform(RelType_Length), RelType) ++
                    "]" ++ ?DASH
            end ++
            "(" ++ ?SP_OPT ++ ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    RelationshipPatternSyntax_Length = length(RelationshipPatternSyntax),
    insert_table(Legacy, relationship_pattern_syntax, RelationshipPatternSyntax),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------------------------
% Expression = Expression12 ;
% ---------------------------
    Expression_Part_1 = sort_list_random(create_code_expression(?MAX_RULE_EXPRESSION, Legacy, Atom_Part_1, NodeLabels, PropertyLookup)),
    Expression_Part_1_Length = length(Expression_Part_1),
% -----------------------------
% Lookup = NodeLookup
%        | RelationshipLookup ;
% -----------------------------
    Lookup = case Legacy of
                 true -> sort_list_random(
                     NodeLookup ++
                     RelationshipLookup);
                 _ -> []
             end,
    Lookup_Length = length(Lookup),
    insert_table(Legacy, lookup, Lookup),
% ------------------------------------------
% QueryOptions = { AnyCypherOption, [SP] } ;
% ------------------------------------------
% wwe ???
% QueryOptions = { AnyCypherOption, SP } ;
% ------------------------------------------
    QueryOptions = case Legacy of
                       true ->
                           sort_list_random([
                                   ACO ++ ?SP ++
                                   case rand:uniform(?PRIME) rem 2 of
                                       1 -> lists:nth(rand:uniform(AnyCypherOption_Length), AnyCypherOption) ++ ?SP;
                                       _ -> []
                                   end
                               || ACO <- AnyCypherOption
                           ]);
                       _ -> []
                   end,
    QueryOptions_Length = length(QueryOptions),
    insert_table(Legacy, query_options, QueryOptions),
% -------------------------------------
% Where = (W,H,E,R,E), SP, Expression ;
% -------------------------------------
    Where = sort_list_random([
            "Where" ++ ?SP ++ E
        || E <- Expression_Part_1
    ]),
    Where_Length = length(Where),
    insert_table(Legacy, where, Where),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------------------------------------
% CaseAlternatives = (W,H,E,N), [SP], Expression, [SP], (T,H,E,N), [SP], Expression ;
% -----------------------------------------------------------------------------------
    CaseAlternatives = case Legacy of
                           true ->
                               sort_list_random([
                                       "When" ++ ?SP ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP ++
                                       "Then" ++ ?SP ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1)
                                   || _ <- lists:seq(1, ?MAX_RULE_ATOM)
                               ]);
                           _ -> []
                       end,
    CaseAlternatives_Length = length(CaseAlternatives),
    insert_table(Legacy, case_alternatives, CaseAlternatives),
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
                4 -> lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT ++
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT;
                5 -> lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                _ -> []
            end ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, function_invocation, FunctionInvocation),
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
    insert_table(Legacy, id_in_coll, IdInColl),
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
                2 -> "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++ ?SP_OPT;
                _ -> []
            end ++
            "]"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, list_literal, ListLiteral),
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
    insert_table(Legacy, map_literal, MapLiteral),
% ------------------------------------------------------------
% ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;
% ------------------------------------------------------------
    ParenthesizedExpression = sort_list_random([
            "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
            ?SP_OPT ++ ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, parenthesized_expression, ParenthesizedExpression),
% ------------------------------------------------
% StartPoint = Variable, [SP], '=', [SP], Lookup ;
% ------------------------------------------------
    StartPoint_Targ = max(Lookup_Length, 30),
    StartPoint = case Legacy of
                     true ->
                         sort_list_random([
                                 lists:nth(rand:uniform(Variable_Length), Variable) ++
                                 ?SP_OPT ++ "=" ++ ?SP_OPT ++
                                 lists:nth(rand:uniform(Lookup_Length), Lookup)
                             || _ <- lists:seq(1, StartPoint_Targ)
                         ]);
                     _ -> []
                 end,
    StartPoint_Length = length(StartPoint),
    insert_table(Legacy, start_point, StartPoint),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% CaseExpression = (((C,A,S,E), { [SP], CaseAlternatives }-) | ((C,A,S,E), [SP], Expression, { [SP], CaseAlternatives }-)), [[SP], (E,L,S,E), [SP], Expression], [SP], (E,N,D) ;
% ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% wwe
% CaseExpression = (((C,A,S,E), { [SP], CaseAlternatives }-) | ((C,A,S,E), [SP], Expression, { [SP], CaseAlternatives }-)), [SP, (E,L,S,E), SP, Expression], [SP], (E,N,D) ;
% ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    CaseExpression_Targ = CaseAlternatives_Length,
    CaseExpression = case Legacy of
                         true -> sort_list_random([
                                 "Case" ++ ?SP_OPT ++
                                 case rand:uniform(?PRIME) rem 8 of
                                     1 ->
                                         ?SP ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                                             ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                                             ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                                             ?SP ++ "Else" ++ ?SP ++
                                             lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                                     2 ->
                                         ?SP ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                                             ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                                             ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives);
                                     3 ->
                                         ?SP ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                                             ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives);
                                     4 ->
                                         ?SP ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1) ++
                                             ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                                             ?SP ++ "Else" ++ ?SP ++
                                             lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                                     5 ->
                                         ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                                             ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives);
                                     6 ->
                                         ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                                             ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                                             ?SP ++ "Else" ++ ?SP ++
                                             lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                                     7 ->
                                         ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives);
                                     _ ->
                                         ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                                             ?SP ++ "Else" ++ ?SP ++
                                             lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1)
                                 end
                                 ++ ?SP ++ "End"
                             || _ <- lists:seq(1, CaseExpression_Targ)
                         ]);
                         _ -> []
                     end,
    insert_table(Legacy, case_expression, CaseExpression),
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
    insert_table(Legacy, filter_expression, FilterExpression),
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
    insert_table(Legacy, literal, Literal),
% ------------------------------
% Properties = MapLiteral
%            | Parameter
%            | LegacyParameter ;
% ------------------------------
    Properties = sort_list_random(
        MapLiteral ++
            Parameter ++
            case Legacy of
                true -> LegacyParameter;
                _ -> []
            end),
    Properties_Length = length(Properties),
    insert_table(Legacy, properties, Properties),
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
    insert_table(Legacy, reduce, Reduce),

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
                1 -> ?SP_OPT ++ "|" ++ lists:nth(rand:uniform(Expression_Part_1_Length), Expression_Part_1);
                _ -> []
            end ++
            "]"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, list_comprehension, ListComprehension),
% ----------------------------------------------------------------------------------------
% NodePattern = '(', [SP], [Variable, [SP]], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
% --------------------------------------------------------------------------------
% wwe ???
% NodePattern = '(', [SP], [Variable, SP], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
% ----------------------------------------------------------------------------------------
    NodePattern = sort_list_random([
            "(" ++ ?SP_OPT ++
            case rand:uniform(?PRIME) rem 8 of
                1 -> lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP ++
                    lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                2 -> lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP ++
                    lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT;
                3 -> lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP ++
                    lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                4 -> lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP;
                5 -> lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT ++
                    lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                6 -> lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT;
                7 -> lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT;
                _ -> []
            end ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    NodePattern_Length = length(NodePattern),
    insert_table(Legacy, node_pattern, NodePattern),
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
                    25 -> lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                        lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                        lists:nth(rand:uniform(Properties_Length), Properties);
                    26 -> lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                    lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    27 -> lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++
                    lists:nth(rand:uniform(Properties_Length), Properties);
                    28 -> lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes);
                    29 -> lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral) ++
                    lists:nth(rand:uniform(Properties_Length), Properties);
                    30 -> lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral);
                    31 -> lists:nth(rand:uniform(Properties_Length), Properties);
                    _ -> []
                end ++
                "]"
            || _ <- lists:seq(1, ?MAX_RULE_ATOM)
        ]),
    RelationshipDetail_Length = length(RelationshipDetail),
    insert_table(Legacy, relationship_detail, RelationshipDetail),

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
    insert_table(Legacy, relationship_pattern, RelationshipPattern),

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
    insert_table(Legacy, pattern_element_chain, PatternElementChain),

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
    insert_table(Legacy, pattern_element, PatternElement),
% --------------------------------------------------------------------
% RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;
% --------------------------------------------------------------------
    RelationshipsPattern = sort_list_random([
            lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                    ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain);
                _ -> ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain)
            end
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, relationships_pattern, RelationshipsPattern),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 14
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------------------------
% ShortestPathPattern = ((S,H,O,R,T,E,S,T,P,A,T,H), '(', PatternElement, ')')
%                     | ((A,L,L,S,H,O,R,T,E,S,T,P,A,T,H,S), '(', PatternElement, ')') ;
% -------------------------------------------------------------------------------------
    ShortestPathPattern = case Legacy of
                              true -> sort_list_random([
                                      case rand:uniform(?PRIME) rem 2 of
                                          1 -> "Shortestpath";
                                          _ -> "Allshortestpaths"
                                      end ++
                                      "(" ++ lists:nth(rand:uniform(PatternElement_Length), PatternElement) ++ ")"
                                  || _ <- lists:seq(1, ?MAX_RULE_ATOM)
                              ]);
                              _ -> []
                          end,
    insert_table(Legacy, shortest_path_pattern, ShortestPathPattern),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 15
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------
% AnonymousPatternPart = ShortestPathPattern
%                      | PatternElement ;
% ------------------------------------------
    AnonymousPathPattern = sort_list_random(
        case Legacy of
            true -> ShortestPathPattern;
            _ -> []
        end ++
        PatternElement),
    AnonymousPathPattern_Length = length(AnonymousPathPattern),
    insert_table(Legacy, anonymous_path_pattern, AnonymousPathPattern),

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
    insert_table(Legacy, atom_filter, AtomFilter),

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
    insert_table(Legacy, atom_extract, AtomExtract),

    AtomAll = sort_list_random([
            "All" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, atom_all, AtomAll),

    AtomAny = sort_list_random([
            "Any" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, atom_any, AtomAny),

    AtomNone = sort_list_random([
            "None" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, atom_none, AtomNone),

    AtomSingle = sort_list_random([
            "Single" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ]),
    insert_table(Legacy, atom_single, AtomSingle),
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
            case Legacy of
                true -> CaseExpression;
                _ -> []
            end ++
            ListComprehension ++
            AtomFilter ++
            AtomExtract ++
            case Legacy of
                true -> Reduce;
                _ -> []
            end ++
            AtomAll ++
            AtomAny ++
            AtomNone ++
            AtomSingle ++
            case Legacy of
                true -> ShortestPathPattern;
                _ -> []
            end ++
            RelationshipsPattern ++
% wwe ???
% ParenthesizedExpression ++
            FunctionInvocation),
    insert_table(Legacy, atom, Atom),
    Atom_Length = length(Atom),
% ---------------------------
% Expression = Expression12 ;
% ---------------------------
    Expression = sort_list_random(create_code_expression(?MAX_RULE_EXPRESSION * 2, Legacy, Atom, NodeLabels, PropertyLookup)),
    Expression_Length = length(Expression),

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
    insert_table(Legacy, limit, Limit),
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
            _ -> lists:nth(rand:uniform(AnonymousPathPattern_Length), AnonymousPathPattern)
        end
        || _ <- lists:seq(1, PatternPart_Targ)
    ]),
    PatternPart_Length = length(PatternPart),
    insert_table(Legacy, pattern_part, PatternPart),
% ------------------------------------------------------
% PropertyExpression = Atom, { [SP], PropertyLookup }- ;
% ------------------------------------------------------
    PropertyExpression_Targ = max(Atom_Length, PropertyLookup_Length),
    PropertyExpression = sort_list_random([
            lists:nth(rand:uniform(Atom_Length), Atom) ++
            case rand:uniform(?PRIME) rem 5 of
                3 -> ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                2 -> ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                _ -> ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup)
            end
        || _ <- lists:seq(1, PropertyExpression_Targ)
    ]),
    PropertyExpression_Length = length(PropertyExpression),
    insert_table(Legacy, property_expression, PropertyExpression),
% -----------------------------------
% RemoveItem = (Variable, NodeLabels)
%            | PropertyExpression ;
% -----------------------------------
    RemoveItem_Targ = max(NodeLabels_Length, PropertyExpression_Length),
    RemoveItem = sort_list_random([
        case rand:uniform(?PRIME) rem 2 of
            1 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
            lists:nth(rand:uniform(NodeLabels_Length), NodeLabels);
            _ -> lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression)
        end
        || _ <- lists:seq(1, RemoveItem_Targ)
    ]),
    RemoveItem_Length = length(RemoveItem),
    insert_table(Legacy, remove_item, RemoveItem),
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
    insert_table(Legacy, return_item, ReturnItem),
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
            1 -> lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression) ++
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
    insert_table(Legacy, set_item, SetItem),
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
    insert_table(Legacy, skip, Skip),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 52
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% NodePropertyExistenceConstraint = (C,O,N,S,T,R,A,I,N,T), SP, (O,N), [SP], '(', Variable, NodeLabel, ')', [SP], (A,S,S,E,R,T), SP, (E,X,I,S,T,S), [SP], '(', PropertyExpression, ')' ;
% -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    NodePropertyExistenceConstraint_Targ = NodeLabel_Length + PropertyExpression_Length + Variable_Length,
    NodePropertyExistenceConstraint = sort_list_random([
            "Constraint" ++ ?SP ++ "On" ++ ?SP_OPT ++ "(" ++
            lists:nth(rand:uniform(Variable_Length), Variable) ++
            lists:nth(rand:uniform(NodeLabel_Length), NodeLabel) ++ ?SP_OPT ++
            ")" ++ ?SP_OPT ++ "Assert" ++ ?SP ++ "Exists" ++ ?SP_OPT ++ "(" ++
            lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression) ++
            ")"
        || _ <- lists:seq(1, NodePropertyExistenceConstraint_Targ)
    ]),
    NodePropertyExistenceConstraint_Length = length(NodePropertyExistenceConstraint),
    insert_table(Legacy, node_property_existence_constraint, NodePropertyExistenceConstraint),
% ---------------------------------------------------------
% Pattern = PatternPart, { [SP], ',', [SP], PatternPart } ;
% ---------------------------------------------------------
    Pattern_Targ = PatternPart_Length,
    Pattern = sort_list_random([
            lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
            case rand:uniform(?PRIME) rem 2 of
                1 -> ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(PatternPart_Length), PatternPart);
                _ -> []
            end
        || _ <- lists:seq(1, Pattern_Targ)
    ]),
    Pattern_Length = length(Pattern),
    insert_table(Legacy, pattern, Pattern),
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% RelationshipPropertyExistenceConstraint = (C,O,N,S,T,R,A,I,N,T), SP, (O,N), [SP], RelationshipPatternSyntax, [SP], (A,S,S,E,R,T), SP, (E,X,I,S,T,S), [SP], '(', PropertyExpression, ')' ;
% -----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    RelationshipPropertyExistenceConstraint_Targ = RelationshipPatternSyntax_Length + PropertyExpression_Length,
    RelationshipPropertyExistenceConstraint = sort_list_random([
            "Constraint" ++ ?SP ++ "On" ++ ?SP_OPT ++
            lists:nth(rand:uniform(RelationshipPatternSyntax_Length), RelationshipPatternSyntax) ++
            ?SP_OPT ++ "Assert" ++ ?SP ++ "Exists" ++ ?SP_OPT ++ "(" ++
            lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression) ++
            ")"
        || _ <- lists:seq(1, RelationshipPropertyExistenceConstraint_Targ)
    ]),
    RelationshipPropertyExistenceConstraint_Length = length(RelationshipPropertyExistenceConstraint),
    insert_table(Legacy, relationship_property_existence_constraint, RelationshipPropertyExistenceConstraint),
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
    insert_table(Legacy, return_items, ReturnItems),
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
    insert_table(Legacy, sort_item, SortItem),
% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
% UniqueConstraint = (C,O,N,S,T,R,A,I,N,T), SP, (O,N), [SP], '(', Variable, NodeLabel, ')', [SP], (A,S,S,E,R,T), SP, PropertyExpression, SP, (I,S), SP, (U,N,I,Q,U,E) ;
% ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
    UniqueConstraint_Targ = NodeLabel_Length + PropertyExpression_Length + Variable_Length,
    UniqueConstraint = sort_list_random([
            "Constraint" ++ ?SP ++ "On" ++ ?SP_OPT ++ "(" ++
            lists:nth(rand:uniform(Variable_Length), Variable) ++
            lists:nth(rand:uniform(NodeLabel_Length), NodeLabel) ++ ?SP_OPT ++
            ")" ++ ?SP_OPT ++ "Assert" ++ ?SP ++
            lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression) ++
            ?SP ++ "Is" ++ ?SP ++ "Unique"
        || _ <- lists:seq(1, UniqueConstraint_Targ)
    ]),
    UniqueConstraint_Length = length(UniqueConstraint),
    insert_table(Legacy, unique_constraint, UniqueConstraint),

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
                1 -> "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem) ++
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem);
                2 -> "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem);
                _ -> []
            end
        || _ <- lists:seq(1, Order_Targ)
    ]),
    Order_Length = length(Order),
    insert_table(Legacy, order, Order),
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
    insert_table(Legacy, return_body, ReturnBody),

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
    insert_table(Legacy, create, Create),
% ----------------------------------------
% CreateIndex = (C,R,E,A,T,E), SP, Index ;
% ----------------------------------------
    CreateIndex = case Legacy of
                      true ->
                          sort_list_random([
                                  "Create" ++ ?SP ++
                                  lists:nth(rand:uniform(Index_Length), Index)
                              || _ <- lists:seq(1, ?MAX_COMMAND)
                          ]);
                      _ -> []
                  end,
    insert_table(Legacy, create_index, CreateIndex),
% --------------------------------------------------------------------------------------------
% CreateNodePropertyExistenceConstraint = (C,R,E,A,T,E), SP, NodePropertyExistenceConstraint ;
% --------------------------------------------------------------------------------------------
    CreateNodePropertyExistenceConstraint = case Legacy of
                                                true -> sort_list_random([
                                                        "Create" ++ ?SP ++
                                                        lists:nth(rand:uniform(NodePropertyExistenceConstraint_Length), NodePropertyExistenceConstraint)
                                                    || _ <- lists:seq(1, ?MAX_COMMAND)
                                                ]);
                                                _ -> []
                                            end,
    insert_table(Legacy, create_node_property_existence_constraint, CreateNodePropertyExistenceConstraint),
% ------------------------------------------------------------------------------------------------------------
% CreateRelationshipPropertyExistenceConstraint = (C,R,E,A,T,E), SP, RelationshipPropertyExistenceConstraint ;
% ------------------------------------------------------------------------------------------------------------
    CreateRelationshipPropertyExistenceConstraint = case Legacy of
                                                        true ->
                                                            sort_list_random([
                                                                    "Create" ++ ?SP ++
                                                                    lists:nth(rand:uniform(RelationshipPropertyExistenceConstraint_Length), RelationshipPropertyExistenceConstraint)
                                                                || _ <- lists:seq(1, ?MAX_COMMAND)
                                                            ]);
                                                        _ -> []
                                                    end,
    insert_table(Legacy, create_relationship_property_existence_constraint, CreateRelationshipPropertyExistenceConstraint),
% ----------------------------------------------------------------
% CreateUnique = (C,R,E,A,T,E), SP, (U,N,I,Q,U,E), [SP], Pattern ;
% ----------------------------------------------------------------
% wwe ???
% CreateUnique = (C,R,E,A,T,E), SP, (U,N,I,Q,U,E), SP, Pattern ;
% ----------------------------------------------------------------
    CreateUnique = case Legacy of
                       true -> sort_list_random([
                               "Create" ++ ?SP ++ "Unique" ++ ?SP ++
                               lists:nth(rand:uniform(Pattern_Length), Pattern)
                           || _ <- lists:seq(1, ?MAX_CLAUSE)
                       ]);
                       _ -> []
                   end,
    insert_table(Legacy, create_unique, CreateUnique),
% --------------------------------------------------------------
% CreateUniqueConstraint = (C,R,E,A,T,E), SP, UniqueConstraint ;
% --------------------------------------------------------------
    CreateUniqueConstraint = case Legacy of
                                 true -> sort_list_random([
                                         "Create" ++ ?SP ++
                                         lists:nth(rand:uniform(UniqueConstraint_Length), UniqueConstraint)
                                     || _ <- lists:seq(1, ?MAX_COMMAND)
                                 ]);
                                 _ -> []
                             end,
    insert_table(Legacy, create_unique_constraint, CreateUniqueConstraint),
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
    insert_table(Legacy, delete, Delete),
% ----------------------------------
% DropIndex = (D,R,O,P), SP, Index ;
% ----------------------------------
    DropIndex = case Legacy of
                    true -> sort_list_random([
                            "Drop" ++ ?SP ++
                            lists:nth(rand:uniform(Index_Length), Index)
                        || _ <- lists:seq(1, ?MAX_COMMAND)
                    ]);
                    _ -> []
                end,
    insert_table(Legacy, drop_index, DropIndex),
% --------------------------------------------------------------------------------------
% DropNodePropertyExistenceConstraint = (D,R,O,P), SP, NodePropertyExistenceConstraint ;
% --------------------------------------------------------------------------------------
    DropNodePropertyExistenceConstraint = case Legacy of
                                              true -> sort_list_random([
                                                      "Drop" ++ ?SP ++
                                                      lists:nth(rand:uniform(NodePropertyExistenceConstraint_Length), NodePropertyExistenceConstraint)
                                                  || _ <- lists:seq(1, ?MAX_COMMAND)
                                              ]);
                                              _ -> []
                                          end,
    insert_table(Legacy, drop_node_property_existence_constraint, DropNodePropertyExistenceConstraint),
% ------------------------------------------------------------------------------------------------------
% DropRelationshipPropertyExistenceConstraint = (D,R,O,P), SP, RelationshipPropertyExistenceConstraint ;
% ------------------------------------------------------------------------------------------------------
    DropRelationshipPropertyExistenceConstraint = case Legacy of
                                                      true ->
                                                          sort_list_random([
                                                                  "Drop" ++ ?SP ++
                                                                  lists:nth(rand:uniform(RelationshipPropertyExistenceConstraint_Length), RelationshipPropertyExistenceConstraint)
                                                              || _ <- lists:seq(1, ?MAX_COMMAND)
                                                          ]);
                                                      _ -> []
                                                  end,
    insert_table(Legacy, drop_relationship_property_existence_constraint, DropRelationshipPropertyExistenceConstraint),
% --------------------------------------------------------
% DropUniqueConstraint = (D,R,O,P), SP, UniqueConstraint ;
% --------------------------------------------------------
    DropUniqueConstraint = case Legacy of
                               true -> sort_list_random([
                                       "Drop" ++ ?SP ++
                                       lists:nth(rand:uniform(UniqueConstraint_Length), UniqueConstraint)
                                   || _ <- lists:seq(1, ?MAX_COMMAND)
                               ]);
                               _ -> []
                           end,
    insert_table(Legacy, drop_unique_constraint, DropUniqueConstraint),
% ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
% LoadCSV = (L,O,A,D), SP, (C,S,V), SP, [(W,I,T,H), SP, (H,E,A,D,E,R,S), SP], (F,R,O,M), SP, Expression, SP, (A,S), SP, Variable, SP, [(F,I,E,L,D,T,E,R,M,I,N,A,T,O,R), SP, StringLiteral] ;
% ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
    LoadCSV = case Legacy of
                  true -> sort_list_random([
                          "Load" ++ ?SP ++ "Csv" ++ ?SP ++
                          case rand:uniform(?PRIME) rem 4 of
                              1 -> "With" ++ ?SP ++ "Headers" ++ ?SP ++
                                  "From" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP ++
                                  "As" ++ ?SP ++ lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP ++
                                  "Fieldterminator" ++ ?SP ++ lists:nth(rand:uniform(StringLiteral_Length), StringLiteral);
                              2 -> "With" ++ ?SP ++ "Headers" ++ ?SP ++
                                  "From" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP ++
                                  "As" ++ ?SP ++ lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP;
                              3 ->
                                  "From" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP ++
                                      "As" ++ ?SP ++ lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP ++
                                      "Fieldterminator" ++ ?SP ++ lists:nth(rand:uniform(StringLiteral_Length), StringLiteral);
                              _ ->
                                  "From" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP ++
                                      "As" ++ ?SP ++ lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP
                          end
                      || _ <- lists:seq(1, ?MAX_CLAUSE)
                  ]);
                  _ -> []
              end,
    insert_table(Legacy, load_csv, LoadCSV),
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
                case Legacy of
                    true -> lists:nth(rand:uniform(Hint_Length), Hint) ++
                    lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            2 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                case Legacy of
                    true -> lists:nth(rand:uniform(Hint_Length), Hint) ++
                    lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end;
            3 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                case Legacy of
                    true -> lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            4 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                case Legacy of
                    true -> lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end;
            5 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            6 -> "Optional" ++ ?SP ++
                "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern);
            7 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                case Legacy of
                    true -> lists:nth(rand:uniform(Hint_Length), Hint) ++
                    lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            8 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                case Legacy of
                    true -> lists:nth(rand:uniform(Hint_Length), Hint) ++
                    lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end;
            9 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                case Legacy of
                    true -> lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            10 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                case Legacy of
                    true -> lists:nth(rand:uniform(Hint_Length), Hint);
                    _ -> []
                end;
            11 -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern) ++
                ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
            _ -> "Match" ++ ?SP ++
                lists:nth(rand:uniform(Pattern_Length), Pattern)
        end
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    insert_table(Legacy, match, Match),
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
    insert_table(Legacy, remove, Remove),
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
    insert_table(Legacy, return, Return),
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
    insert_table(Legacy, set, Set),
% -------------------------------------------------------------------------------
% Start = (S,T,A,R,T), SP, StartPoint, { [SP], ',', [SP], StartPoint }, [Where] ;
% -------------------------------------------------------------------------------
    Start = case Legacy of
                true -> sort_list_random([
                        "Start" ++ ?SP ++
                        lists:nth(rand:uniform(StartPoint_Length), StartPoint) ++ ?SP ++
                        case rand:uniform(?PRIME) rem 4 of
                            1 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                                lists:nth(rand:uniform(StartPoint_Length), StartPoint) ++
                                lists:nth(rand:uniform(Where_Length), Where);
                            2 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                                lists:nth(rand:uniform(StartPoint_Length), StartPoint);
                            3 -> lists:nth(rand:uniform(Where_Length), Where);
                            _ -> []
                        end
                    || _ <- lists:seq(1, ?MAX_CLAUSE)
                ]);
                _ -> []
            end,
    insert_table(Legacy, start, Start),
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
    insert_table(Legacy, unwind, Unwind),
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
    insert_table(Legacy, with, With),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 91
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ---------------------
% Clause = LoadCSV
%        | Start
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
        case Legacy of
            true -> LoadCSV;
            _ -> []
        end ++
            Start ++
            Match ++
            Unwind ++
            Create ++
            CreateUnique ++
            Delete ++
            Remove ++
            With ++
            Return),
    Clause_Part_1_Length = length(Clause_Part_1),
    insert_table(Legacy, clause_part_1, Clause_Part_1),
% -------------------------------------------------------
% Command = CreateIndex
%         | DropIndex
%         | CreateUniqueConstraint
%         | DropUniqueConstraint
%         | CreateNodePropertyExistenceConstraint
%         | DropNodePropertyExistenceConstraint
%         | CreateRelationshipPropertyExistenceConstraint
%         | DropRelationshipPropertyExistenceConstraint ;
% -------------------------------------------------------
    Command_Curr = sort_list_random(
        case Legacy of
            true -> CreateIndex ++
                DropIndex ++
                CreateUniqueConstraint ++
                DropUniqueConstraint ++
                CreateNodePropertyExistenceConstraint ++
                DropNodePropertyExistenceConstraint ++
                CreateRelationshipPropertyExistenceConstraint ++
                DropRelationshipPropertyExistenceConstraint;
            _ -> []
        end),
    Command =
        case length(Command_Curr) > ?MAX_COMMAND of
            true -> lists:sublist(Command_Curr, 1, ?MAX_COMMAND);
            _ -> Command_Curr
        end,
    insert_table(Legacy, command, Command),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 92
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------------------------------------------------------------------------------------
% Foreach = (F,O,R,E,A,C,H), [SP], '(', [SP], Variable, SP, (I,N), SP, Expression, [SP], '|', { SP, Clause }-, [SP], ')' ;
% ------------------------------------------------------------------------------------------------------------------------
    Foreach = case Legacy of
                  true -> sort_list_random([
                          "Foreach" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
                          lists:nth(rand:uniform(Variable_Length), Variable) ++
                          ?SP ++ "In" ++ ?SP ++
                          lists:nth(rand:uniform(Expression_Length), Expression) ++
                          ?SP_OPT ++ "|" ++
                          case rand:uniform(?PRIME) rem 2 of
                              1 -> ?SP ++ lists:nth(rand:uniform(Clause_Part_1_Length), Clause_Part_1) ++
                                  ?SP ++ lists:nth(rand:uniform(Clause_Part_1_Length), Clause_Part_1);
                              _ -> ?SP ++ lists:nth(rand:uniform(Clause_Part_1_Length), Clause_Part_1)
                          end ++
                          ?SP_OPT ++ ")"
                      || _ <- lists:seq(1, ?MAX_CLAUSE)
                  ]);
                  _ -> []
              end,
    insert_table(Legacy, foreach, Foreach),
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
    insert_table(Legacy, merge_action, MergeAction),

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
                1 -> ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction) ++
                    ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction);
                2 -> ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE)
    ]),
    insert_table(Legacy, merge, Merge),

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
            Merge ++
            case Legacy of
                true -> Foreach;
                _ -> []
            end),
    Clause_Length = length(Clause),
    insert_table(Legacy, clause, Clause),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 95
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ------------------------------------------
% LoadCSVQuery = LoadCSV, { [SP], Clause } ;
% ------------------------------------------
% wwe ???
% LoadCSVQuery = LoadCSV, { SP, Clause } ;
% ------------------------------------------
    LoadCSVQuery = sort_list_random([
            LC ++
            case rand:uniform(?PRIME) rem 3 of
                1 -> ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause) ++
                    ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause);
                2 -> ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause);
                _ -> []
            end
        || LC <- LoadCSV
    ]),
    insert_table(Legacy, load_csv_query, LoadCSVQuery),
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
    insert_table(Legacy, single_query, SingleQuery),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 96
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ----------------------------------------------------------
% BulkImportQuery = PeriodicCommitHint, [SP], LoadCSVQuery ;
% ----------------------------------------------------------
    BulkImportQuery = sort_list_random([
            lists:nth(rand:uniform(PeriodicCommitHint_Length), PeriodicCommitHint) ++ ?SP_OPT ++ LCQ
        || LCQ <- LoadCSVQuery
    ]),
    insert_table(Legacy, bulk_import_query, BulkImportQuery),
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
    insert_table(Legacy, union, Union),

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
    insert_table(Legacy, regular_query, RegularQuery),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 98
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------------
% Query = RegularQuery
%       | BulkImportQuery ;
% -------------------------
    Query_Curr = sort_list_random(
        RegularQuery ++
        case Legacy of
            true -> BulkImportQuery;
            _ -> []
        end),
    Query = case length(Query_Curr) > ?MAX_QUERY of
                true -> lists:sublist(Query_Curr, 1, ?MAX_QUERY);
                _ -> Query_Curr
            end,
    insert_table(Legacy, query, Query),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 99
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -------------------
% Statement = Command
%           | Query ;
% -------------------
    Statement_Curr = sort_list_random(
        case Legacy of
            true -> Command;
            _ -> []
        end ++
        Query),
    Statement = case length(Statement_Curr) > ?MAX_STATEMENT of
                    true -> lists:sublist(Statement_Curr, 1, ?MAX_STATEMENT);
                    _ -> Statement_Curr
                end,
    insert_table(Legacy, statement, Statement),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 100
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% -----------------------------------------------------------
% Cypher = [SP], QueryOptions, Statement, [[SP], ';'], [SP] ;
% -----------------------------------------------------------
    Cypher_Curr = sort_list_random([
            ?SP_OPT ++
            case Legacy of
                true -> lists:nth(rand:uniform(QueryOptions_Length), QueryOptions);
                _ -> []
            end ++
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
    insert_table(Legacy, cypher, Cypher),

    ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating code of rules Expression, Expression2, ..., Expression12.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code_expression(Max, _Legacy, Atom, NodeLabels, PropertyLookup) ->
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
                4 -> lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                5 -> lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                lists:nth(rand:uniform(NodeLabels_Length), NodeLabels);
                6 -> lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
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
    insert_table(_Legacy, expression2, Expression2),
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
    insert_table(_Legacy, expression3, Expression3),
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
    insert_table(_Legacy, expression4, Expression4),
% -------------------------------------------------------------
% Expression5 = Expression4, { [SP], '^', [SP], Expression4 } ;
% -------------------------------------------------------------
    Expression5_Prev = case ets:lookup(?CODE_TEMPLATES, expression5) of
                           [{_, Expression5_Exist}] -> Expression5_Exist;
                           _ -> []
                       end,
    Expression5_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression5_Prev ++
        [
                lists:nth(rand:uniform(Expression4_Length), Expression4) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 3 of
                    1 -> ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression4_Length), Expression4) ++
                        ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression4_Length), Expression4);
                    2 -> ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression4_Length), Expression4);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression5 = case length(Expression5_Curr) > Max of
                      true -> lists:sublist(Expression5_Curr, 1, Max);
                      _ -> Expression5_Curr
                  end,
    Expression5_Length = length(Expression5),
    insert_table(_Legacy, expression5, Expression5),
% ---------------------------------------------------------------------------------------------------------------------------------
% Expression6 = Expression5, { ([SP], '*', [SP], Expression5) | ([SP], '/', [SP], Expression5) | ([SP], '%', [SP], Expression5) } ;
% ---------------------------------------------------------------------------------------------------------------------------------
    Expression6_Prev = case ets:lookup(?CODE_TEMPLATES, expression6) of
                           [{_, Expression6_Exist}] -> Expression6_Exist;
                           _ -> []
                       end,
    Expression6_Curr = sort_list_random(sets:to_list(sets:from_list(
        Expression6_Prev ++
        [
                lists:nth(rand:uniform(Expression5_Length), Expression5) ++
                case rand:uniform(?PRIME) rem ?MAX_BASE_VAR * 7 of
                    1 -> ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5) ++
                        ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    2 -> ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5) ++
                        ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    3 -> ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5) ++
                        ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    4 -> ?SP_OPT ++ "*" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    5 -> ?SP_OPT ++ "/" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    6 -> ?SP_OPT ++ "%" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression5_Length), Expression5);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression6 = case length(Expression6_Curr) > Max of
                      true -> lists:sublist(Expression6_Curr, 1, Max);
                      _ -> Expression6_Curr
                  end,
    Expression6_Length = length(Expression6),
    insert_table(_Legacy, expression6, Expression6),
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
                    1 -> ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression6_Length), Expression6) ++
                        ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression6_Length), Expression6);
                    2 -> ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(Expression6_Length), Expression6) ++
                        ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(Expression6_Length), Expression6);
                    3 -> ?SP_OPT ++ "+" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression6_Length), Expression6);
                    4 -> ?SP_OPT ++ "-" ++ ?SP ++ lists:nth(rand:uniform(Expression6_Length), Expression6);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression7 = case length(Expression7_Curr) > Max of
                      true -> lists:sublist(Expression7_Curr, 1, Max);
                      _ -> Expression7_Curr
                  end,
    Expression7_Length = length(Expression7),
    insert_table(_Legacy, expression7, Expression7),
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
                1 -> "=" ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                2 -> "<>" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                3 -> "!=" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                4 -> "<" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                5 -> ">" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                6 -> "<=" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7);
                _ -> ">=" ++ ?SP ++ lists:nth(rand:uniform(Expression7_Length), Expression7)
            end
            || _ <- lists:seq(1, Max)
        ]))),
    PartialComparisonExpression = case length(PartialComparisonExpression_Curr) > Max of
                                      true -> lists:sublist(PartialComparisonExpression_Curr, 1, Max);
                                      _ -> PartialComparisonExpression_Curr
                                  end,
    PartialComparisonExpression_Length = length(PartialComparisonExpression),
    insert_table(_Legacy, partial_comparison_expression, PartialComparisonExpression),
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
    insert_table(_Legacy, expression8, Expression8),
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
    insert_table(_Legacy, expression9, Expression9),
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
                    1 -> ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(Expression9_Length), Expression9) ++
                        ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(Expression9_Length), Expression9);
                    2 -> ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(Expression9_Length), Expression9);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression10 = case length(Expression10_Curr) > Max of
                       true -> lists:sublist(Expression10_Curr, 1, Max);
                       _ -> Expression10_Curr
                   end,
    Expression10_Length = length(Expression10),
    insert_table(_Legacy, expression10, Expression10),
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
                    1 -> ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(Expression10_Length), Expression10) ++
                        ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(Expression10_Length), Expression10);
                    2 -> ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(Expression10_Length), Expression10);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression11 = case length(Expression11_Curr) > Max of
                       true -> lists:sublist(Expression11_Curr, 1, Max);
                       _ -> Expression11_Curr
                   end,
    Expression11_Length = length(Expression11),
    insert_table(_Legacy, expression11, Expression11),
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
                    1 -> ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(Expression11_Length), Expression11) ++
                        ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(Expression11_Length), Expression11);
                    2 -> ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(Expression11_Length), Expression11);
                    _ -> []
                end
            || _ <- lists:seq(1, Max)
        ]))),
    Expression12 = case length(Expression12_Curr) > Max of
                       true -> lists:sublist(Expression12_Curr, 1, Max);
                       _ -> Expression12_Curr
                   end,
    insert_table(_Legacy, expression12, Expression12),
% ---------------------------
% Expression = Expression12 ;
% ---------------------------
    insert_table(_Legacy, expression, Expression12),
    Expression12.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Creating EUnit data files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_eunit_all(_Legacy, []) ->
    ok;
file_create_eunit_all(Legacy, [Rule | Rules]) ->
    file_create_eunit(Legacy, Rule),
    file_create_eunit_all(Legacy, Rules).

file_create_eunit(Legacy, Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),
    ?debugFmt("wwe debugging file_create_eunit/2 ===> [~8.. B] Rule: ~p ~s~n", [length(Code), Rule, case Legacy of
                                                                                                        true ->
                                                                                                            "/ legacy";
                                                                                                        _ -> []
                                                                                                    end]),
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
% Creating Common Test data files.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_Legacy, []) ->
    ok;
file_create_ct_all(Legacy, [Rule | Rules]) ->
    file_create_ct(Legacy, Rule),
    file_create_ct_all(Legacy, Rules).

file_create_ct(Legacy, Rule) ->
    [{Rule, Code}] = ets:lookup(?CODE_TEMPLATES, Rule),
    ?debugFmt("wwe debugging file_create_ct/2 ===> [~8.. B] Rule: ~p ~s~n", [length(Code), Rule, case Legacy of
                                                                                                     true -> "/ legacy";
                                                                                                     _ -> []
                                                                                                 end]),
    FileName = case lists:member(Rule, [command, cypher, query, statement]) of
                   true -> "performance_";
                   _ -> "generic_"
               end ++ atom_to_list(Rule) ++ case Legacy of
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

    file_write_ct(Legacy, File, Code).

file_write_ct(_Legacy, File, []) ->
    file:close(File);
file_write_ct(Legacy, File, [H | T]) ->
    io:format(File, "~s~n", ["    octest" ++ case Legacy of
                                                 true -> "_legacy";
                                                 _ -> []
                                             end ++ ":ct_string(\"" ++ H ++ "\")" ++ case T of
                                                                                         [] -> ".";
                                                                                         _ -> ","
                                                                                     end]),
    file_write_ct(Legacy, File, T).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Insert generated code into helper table.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

insert_table(_Legacy, Rule, Code) ->
    ?debugFmt("wwe debugging insert_table/3 ===> [~8.. B] Rule: ~p ~s~n", [length(Code), Rule, case _Legacy of
                                                                                                   true -> "/ legacy";
                                                                                                   _ -> []
                                                                                               end]),
    ets:insert(?CODE_TEMPLATES, {Rule, Code}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Randomising unique lists.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sort_list_random(L) ->
    F = fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end,
    lists:sort(F, sets:to_list(sets:from_list(L))).
