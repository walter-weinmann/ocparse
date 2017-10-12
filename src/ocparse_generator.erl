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
    cypher
]).
-define(ALL_CLAUSE_CT_RELIABILITY, [
    create,
    cypher,
    merge,
    multiPartQuery,
    query,
    readOnlyEnd,
    readUpdateEnd,
    regularQuery,
    return,
    singlePartQuery,
    singleQuery,
    standaloneCall,
    statement,
    updatingEnd,
    updatingStartClause
]).
-define(ALL_CLAUSE_CT_RELIABILITY_DETAILED, [
    special
]).

-define(ALL_CLAUSE_EUNIT, [
    special
]).

-define(CODE_TEMPLATES, code_templates).
-define(CREATE_CODE_END,
    [_CodeFirst | _] = Code,
    {_, _MemorySize} = erlang:process_info(self(), memory),
    ?debugFmt("~ntime (ms)          ===  ~12.. B rule: ~s ~n", [erlang:monotonic_time(1000) - _Start, atom_to_list(Rule)]),
    ?debugFmt("~nmemory (bytes)     ===  ~12.. B rule: ~s ~n", [_MemorySize, atom_to_list(Rule)]),
    ?debugFmt("~ncode size (bytes) <===  ~12.. B rule: ~s ~n", [length(_CodeFirst), atom_to_list(Rule)]),
    ok
).
-define(CREATE_CODE_START,
    [garbage_collect(Pid) || Pid <- processes()],
    _Start = erlang:monotonic_time(1000)
).
-define(DASH, "-").

-define(F_RANDOM, fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end).

-define(LEFT_ARROW_HEAD, "<").

-define(MAX_BASIC_RULE, 200).
% -define(MAX_BASIC_RULE, 250).
-define(MAX_CYPHER, ?MAX_BASIC_RULE * 8).

-define(PATH_CT, "test").
-define(PATH_EUNIT, "test").

-define(RIGHT_ARROW_HEAD, ">").
-define(SP, " ").
-define(SP_OPT, []).

-define(TIMETRAP_MINUTES, 30).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate Test Data.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    file:delete(?CODE_TEMPLATES),

    dets:open_file(?CODE_TEMPLATES, [
        {auto_save, 0}
    ]),

    create_code(),

    % performance common tests with compacted test cases
    ok = file_create_ct_all("performance", "compacted", ?ALL_CLAUSE_CT_PERFORMANCE),

    % reliability common tests with compacted test cases
    ok = file_create_ct_all("reliability", "compacted", ?ALL_CLAUSE_CT_RELIABILITY),

    % reliability common tests with detailed test cases
    ok = file_create_ct_all("reliability", "detailed_", ?ALL_CLAUSE_CT_RELIABILITY_DETAILED),

    % reliability eunit tests with compacted test cases
    ok = file_create_eunit_all("reliability", ?ALL_CLAUSE_EUNIT),

    dets:close(?CODE_TEMPLATES).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code base.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code() ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(booleanLiteral),
    create_code(decimalInteger),
    create_code(escapedSymbolicName),
    create_code(exponentDecimalReal),
    create_code(hexInteger),
    create_code(hexLetter),
    create_code(octalInteger),
    create_code(regularDecimalReal),
    create_code(reservedWord),
    create_code(stringLiteral),
    create_code(unescapedSymbolicName),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(doubleLiteral),
    create_code(integerLiteral),
    create_code(symbolicName),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(functionName),
    create_code(namespace),
    create_code(numberLiteral),
    create_code(parameter),
    create_code(procedureResultField),
    create_code(rangeLiteral),
    create_code(schemaName),
    create_code(variable),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 4
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(literal_1),
    create_code(labelName),
    create_code(procedureName),
    create_code(propertyKeyName),
    create_code(relTypeName),
    create_code(yieldItem),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 5
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(atom_1),
    create_code(implicitProcedureInvocation),
    create_code(nodeLabel),
    create_code(propertyLookup),
    create_code(relationshipTypes),
    create_code(yieldItems),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 6
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(nodeLabels),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 7
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code_expression(?MAX_BASIC_RULE),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 11
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(caseAlternatives),
    create_code(functionInvocation),
    create_code(idInColl),
    create_code(listLiteral),
    create_code(mapLiteral),
    create_code(where),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 12
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(caseAlternativesList),
    create_code(filterExpression),
    create_code(literal),
    create_code(properties),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 13
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(caseExpression),
    create_code(listComprehension),
    create_code(nodePattern),
    create_code(relationshipDetail),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 14
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(relationshipPattern),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 15
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(patternElementChain),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 16
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(patternElementChainList),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 17
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(patternElement),
    create_code(relationshipsPattern),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 18
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(patternComprehension),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 19
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(atom_2),
    create_code(patternPart),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 20
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code_expression(?MAX_BASIC_RULE),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 21
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(atom),
    create_code(explicitProcedureInvocation),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 50
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(delete),
    create_code(limit),
    create_code(pattern),
    create_code(propertyExpression),
    create_code(returnItem),
    create_code(skip),
    create_code(sortItem),
    create_code(unwind),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 51
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(create),
    create_code(inQueryCall),
    create_code(match),
    create_code(order),
    create_code(removeItem),
    create_code(returnItems),
    create_code(setItem),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 52
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(remove),
    create_code(returnBody),
    create_code(set),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 53
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(mergeAction),
    create_code(return),
    create_code(with),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 54
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(merge),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 90
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(readingClause),
    create_code(updatingClause),
    create_code(updatingStartClause),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 91
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(readingClauseList),
    create_code(updatingClauseList),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 92
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(readOnlyEnd),
    create_code(readPartUpdatingPartWithList),
    create_code(readUpdateEnd),
    create_code(updatingEnd),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 93
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(singlePartQuery),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 94
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(multiPartQuery),
    create_code(standaloneCall),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 95
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(singleQuery),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 96
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(union),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 97
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(regularQuery),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 98
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(query),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 100
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(cypher),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 101
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(special),

    ok.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = Literal
%%      | Parameter
%%      | CaseExpression
%%      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
%%      | ListComprehension
%%      | PatternComprehension
%%      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((E,X,T,R,A,C,T), [SP], '(', [SP], FilterExpression, [SP], [[SP], '|', Expression], ')')
%%      | ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | RelationshipsPattern
%%      | ParenthesizedExpression
%%      | FunctionInvocation
%%      | Variable
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atom = Rule) ->
    ?CREATE_CODE_START,
    [{caseExpression, CaseExpression}] = dets:lookup(?CODE_TEMPLATES, caseExpression),
    CaseExpression_Length = length(CaseExpression),
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),
    [{functionInvocation, FunctionInvocation}] = dets:lookup(?CODE_TEMPLATES, functionInvocation),
    FunctionInvocation_Length = length(FunctionInvocation),
    [{listComprehension, ListComprehension}] = dets:lookup(?CODE_TEMPLATES, listComprehension),
    ListComprehension_Length = length(ListComprehension),
    [{literal, Literal}] = dets:lookup(?CODE_TEMPLATES, literal),
    [{parameter, Parameter}] = dets:lookup(?CODE_TEMPLATES, parameter),
%% wwe
%%    [{parenthesizedExpression, ParenthesizedExpression}] = dets:lookup(?CODE_TEMPLATES, parenthesizedExpression),
%%    ParenthesizedExpression_Length = length(ParenthesizedExpression),
    [{patternComprehension, PatternComprehension}] = dets:lookup(?CODE_TEMPLATES, patternComprehension),
    PatternComprehension_Length = length(PatternComprehension),
    [{relationshipsPattern, RelationshipsPattern}] = dets:lookup(?CODE_TEMPLATES, relationshipsPattern),
    RelationshipsPattern_Length = length(RelationshipsPattern),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),

    Code = lists:append([
        Literal,
        Parameter,
        [
            "Count(*)",
            "Count ( * )"
        ],
        Variable,
        [
            case rand:uniform(8) rem 8 of
                1 ->
                    lists:nth(rand:uniform(CaseExpression_Length), CaseExpression);
                2 ->
                    lists:nth(rand:uniform(ListComprehension_Length), ListComprehension);
                3 ->
                    lists:nth(rand:uniform(PatternComprehension_Length), PatternComprehension);
                4 -> lists:append([
                    case rand:uniform(5) rem 5 of
                        1 -> "Filter";
                        2 -> "All";
                        3 -> "Any";
                        4 -> "None";
                        _ -> "Single"
                    end,
                    ?SP_OPT,
                    "(",
                    ?SP_OPT,
                    lists:nth(rand:uniform(FilterExpression_Length), FilterExpression),
                    ?SP_OPT,
                    ")"
                ]);
                5 -> lists:append([
                    "Extract",
                    ?SP_OPT,
                    "(",
                    ?SP_OPT,
                    lists:nth(rand:uniform(FilterExpression_Length), FilterExpression),
                    ?SP_OPT,
                    case rand:uniform(2) rem 2 of
                        1 ->
                            lists:append([?SP_OPT, "|", lists:nth(rand:uniform(Expression_Length), Expression)]);
                        _ -> []
                    end,
                    ")"
                ]);
                6 ->
                    lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern);
%% wwe
%%                7 ->
%%                    lists:nth(rand:uniform(ParenthesizedExpression_Length), ParenthesizedExpression);
                _ ->
                    lists:nth(rand:uniform(FunctionInvocation_Length), FunctionInvocation)
            end
            || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
        ]
    ]),
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = Literal
%%      | Parameter
%%      | CaseExpression
%%      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
%%      | ListComprehension
%%      | PatternComprehension
%%      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((E,X,T,R,A,C,T), [SP], '(', [SP], FilterExpression, [SP], [[SP], '|', Expression], ')')
%%      | ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | RelationshipsPattern
%%      | ParenthesizedExpression
%%      | FunctionInvocation
%%      | Variable
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atom_1 = _Rule) ->
    ?CREATE_CODE_START,
    [{literal, Literal}] = dets:lookup(?CODE_TEMPLATES, literal),
    [{parameter, Parameter}] = dets:lookup(?CODE_TEMPLATES, parameter),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),

    Code = lists:append([
        Literal,
        Parameter,
        [
            "Count(*)",
            "Count ( * )"
        ],
        Variable
    ]),
    store_code(atom, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = Literal
%%      | Parameter
%%      | CaseExpression
%%      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
%%      | ListComprehension
%%      | PatternComprehension
%%      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((E,X,T,R,A,C,T), [SP], '(', [SP], FilterExpression, [SP], [[SP], '|', Expression], ')')
%%      | ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | RelationshipsPattern
%%      | ParenthesizedExpression
%%      | FunctionInvocation
%%      | Variable
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atom_2 = _Rule) ->
    ?CREATE_CODE_START,
    [{caseExpression, CaseExpression}] = dets:lookup(?CODE_TEMPLATES, caseExpression),
    CaseExpression_Length = length(CaseExpression),
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),
    [{functionInvocation, FunctionInvocation}] = dets:lookup(?CODE_TEMPLATES, functionInvocation),
    FunctionInvocation_Length = length(FunctionInvocation),
    [{listComprehension, ListComprehension}] = dets:lookup(?CODE_TEMPLATES, listComprehension),
    ListComprehension_Length = length(ListComprehension),
    [{literal, Literal}] = dets:lookup(?CODE_TEMPLATES, literal),
    [{parameter, Parameter}] = dets:lookup(?CODE_TEMPLATES, parameter),
%% wwe
%%    [{parenthesizedExpression, ParenthesizedExpression}] = dets:lookup(?CODE_TEMPLATES, parenthesizedExpression),
%%    ParenthesizedExpression_Length = length(ParenthesizedExpression),
    [{patternComprehension, PatternComprehension}] = dets:lookup(?CODE_TEMPLATES, patternComprehension),
    PatternComprehension_Length = length(PatternComprehension),
    [{relationshipsPattern, RelationshipsPattern}] = dets:lookup(?CODE_TEMPLATES, relationshipsPattern),
    RelationshipsPattern_Length = length(RelationshipsPattern),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),

    Code = lists:append([
        Literal,
        Parameter,
        [
            "Count(*)",
            "Count ( * )"
        ],
        Variable,
        [
            case rand:uniform(7) rem 7 of
                1 ->
                    lists:nth(rand:uniform(CaseExpression_Length), CaseExpression);
                2 ->
                    lists:nth(rand:uniform(ListComprehension_Length), ListComprehension);
                3 ->
                    lists:nth(rand:uniform(PatternComprehension_Length), PatternComprehension);
                4 -> lists:append([
                    case rand:uniform(5) rem 5 of
                        1 -> "Filter";
                        2 -> "All";
                        3 -> "Any";
                        4 -> "None";
                        _ -> "Single"
                    end,
                    ?SP_OPT,
                    "(",
                    ?SP_OPT,
                    lists:nth(rand:uniform(FilterExpression_Length), FilterExpression),
                    ?SP_OPT,
                    ")"
                ]);
                5 ->
                    lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern);
%% wwe
%%                6 ->
%%                    lists:nth(rand:uniform(ParenthesizedExpression_Length), ParenthesizedExpression);
                _ ->
                    lists:nth(rand:uniform(FunctionInvocation_Length), FunctionInvocation)
            end
            || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
        ]
    ]),
    store_code(atom, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BooleanLiteral = (T,R,U,E)
%%                | (F,A,L,S,E) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(booleanLiteral = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "true",
        "false"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CaseAlternatives = (W,H,E,N), [SP], Expression, [SP], (T,H,E,N), [SP], Expression ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% CaseAlternatives = (W,H,E,N), SP, Expression, SP, (T,H,E,N), SP, Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(caseAlternatives = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
        lists:append([
            "When",
            ?SP,
            lists:nth(rand:uniform(Expression_Length), Expression),
            ?SP,
            "Then",
            ?SP,
            lists:nth(rand:uniform(Expression_Length), Expression)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CaseAlternativesList = { [SP], CaseAlternatives } ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% CaseAlternativesList = { SP, CaseAlternatives } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(caseAlternativesList = Rule) ->
    ?CREATE_CODE_START,
    [{caseAlternatives, CaseAlternatives}] = dets:lookup(?CODE_TEMPLATES, caseAlternatives),
    CaseAlternatives_Length = length(CaseAlternatives),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:append(
                [
                    ?SP,
                    lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives),
                    ?SP,
                    lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives),
                    ?SP,
                    lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives)
                ]);
            2 -> lists:append(
                [
                    ?SP,
                    lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives),
                    ?SP,
                    lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives)
                ]);
            _ ->
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives)
        end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CaseExpression = (((C,A,S,E), { [SP], CaseAlternatives }-) | ((C,A,S,E), [SP], Expression, { [SP], CaseAlternatives }-)), [[SP], (E,L,S,E), [SP], Expression], [SP], (E,N,D) ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% CaseExpression = (((C,A,S,E), SP, { [SP], CaseAlternatives }-) | ((C,A,S,E), SP, Expression, { [SP], CaseAlternatives }-)), [SP, (E,L,S,E), SP, Expression], SP, (E,N,D) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(caseExpression = Rule) ->
    ?CREATE_CODE_START,
    [{caseAlternativesList, CaseAlternativesList}] = dets:lookup(?CODE_TEMPLATES, caseAlternativesList),
    CaseAlternativesList_Length = length(CaseAlternativesList),
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
        lists:append([
            "Case",
            case rand:uniform(3) rem 3 of
                1 -> [];
                _ ->
                    ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression)
            end,
            ?SP,
            lists:nth(rand:uniform(CaseAlternativesList_Length), CaseAlternativesList),
            case rand:uniform(3) rem 3 of
                1 -> [];
                _ -> lists:append([
                    ?SP,
                    "Else",
                    ?SP,
                    lists:nth(rand:uniform(Expression_Length), Expression)
                ])
            end,
            ?SP,
            "End"
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Create = (C,R,E,A,T,E), [SP], Pattern ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% Create = (C,R,E,A,T,E), SP, Pattern ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(create = Rule) ->
    ?CREATE_CODE_START,
    [{pattern, Pattern}] = dets:lookup(?CODE_TEMPLATES, pattern),
    Pattern_Length = length(Pattern),

    Code = [
        lists:append([
            "Create",
            ?SP,
            lists:nth(rand:uniform(Pattern_Length), Pattern)
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cypher = [SP], Statement, [[SP], ';'], [SP], EOI ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cypher = Rule) ->
    ?CREATE_CODE_START,
    [{statement, Statement}] = dets:lookup(?CODE_TEMPLATES, statement),
    Statement_Length = length(Statement),

    Code = [
        lists:append([
            ?SP_OPT,
            lists:nth(rand:uniform(Statement_Length), Statement),
            case rand:uniform(2) rem 2 of
                1 -> ?SP_OPT ++ ";";
                _ -> []
            end,
            ?SP_OPT
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DecimalInteger = (('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'), [DigitString])
%%                | '0' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(decimalInteger = Rule) ->
    ?CREATE_CODE_START,

    Code = [
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
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(delete = Rule) ->
    ?CREATE_CODE_START,
    [{expressionCommalist, ExpressionCommalist}] = dets:lookup(?CODE_TEMPLATES, expressionCommalist),
    ExpressionCommalist_Length = length(ExpressionCommalist),

    Code = [
        case rand:uniform(2) rem 2 of
            1 -> lists:append(
                [
                    "Detach",
                    ?SP,
                    "Delete",
                    ?SP,
                    lists:nth(rand:uniform(ExpressionCommalist_Length), ExpressionCommalist)
                ]);
            _ -> lists:append(
                [
                    "Delete",
                    ?SP,
                    lists:nth(rand:uniform(ExpressionCommalist_Length), ExpressionCommalist)
                ])
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% DoubleLiteral = ExponentDecimalReal
%%               | RegularDecimalReal
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(doubleLiteral = Rule) ->
    ?CREATE_CODE_START,
    [{exponentDecimalReal, ExponentDecimalReal}] = dets:lookup(?CODE_TEMPLATES, exponentDecimalReal),
    [{regularDecimalReal, RegularDecimalReal}] = dets:lookup(?CODE_TEMPLATES, regularDecimalReal),

    Code = ExponentDecimalReal ++ RegularDecimalReal,
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (* Any character except "`", enclosed within `backticks`. Backticks are escaped with double backticks. *)EscapedSymbolicName = { '`', { ANY - ('`') }, '`' }- ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(escapedSymbolicName = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "`1esn_SYMB_NAME_esn`",
        "`2esn_SYMB_NAME_esn`",
        "`3esn_SYMB_NAME_esn`",
        "`4esn_SYMB_NAME_esn`",
        "`5esn_SYMB_NAME_esn`",
        "`6esn_SYMB_NAME_esn`",
        "`7esn_SYMB_NAME_esn`",
        "`8esn_SYMB_NAME_esn`",
        "`aesn_SYMB_NAME_esn`",
        "`Aesn_SYMB_NAME_esn`",
        "`besn_SYMB_NAME_esn`",
        "`Besn_SYMB_NAME_esn`",
        "`.esn_SYMB_NAME_esn`",
        "`,esn_SYMB_NAME_esn`",
        "`@esn_SYMB_NAME_esn`",
        "``"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ExplicitProcedureInvocation = ProcedureName, [SP], '(', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(explicitProcedureInvocation = Rule) ->
    ?CREATE_CODE_START,
    [{expressionCommalist, ExpressionCommalist}] = dets:lookup(?CODE_TEMPLATES, expressionCommalist),
    ExpressionCommalist_Length = length(ExpressionCommalist),
    [{procedureName, ProcedureName}] = dets:lookup(?CODE_TEMPLATES, procedureName),
    ProcedureName_Length = length(ProcedureName),

    Code = [
        lists:append([
            lists:nth(rand:uniform(ProcedureName_Length), ProcedureName),
            ?SP_OPT,
            "(",
            ?SP_OPT,
            case rand:uniform(10) rem 10 of
                1 -> [];
                _ ->
                    lists:nth(rand:uniform(ExpressionCommalist_Length), ExpressionCommalist)
            end,
            ?SP_OPT,
            ")"
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ExponentDecimalReal = ({ Digit | '.' }- | DecimalInteger), ((E) | (E)), (DigitString | DecimalInteger) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(exponentDecimalReal = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "9e0",
        "9e1",
        "9e-1",
        "9e12",
        "9E-12",
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
        ".1E1",
        ".1e-1",
        ".12e12",
        ".12e-12",
        "1.9e0",
        "2.9e1",
        "3.9e-1",
        "4.9E12",
        "5.9e-12",
        "6.0e0",
        "7.0e-0",
        "8.1e1",
        "9.1E-1",
        "10.12e12",
        "11.12e-12"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FilterExpression = IdInColl, [[SP], Where] ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% FilterExpression = IdInColl, [SP, Where] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(filterExpression = Rule) ->
    ?CREATE_CODE_START,
    [{idInColl, IdInColl}] = dets:lookup(?CODE_TEMPLATES, idInColl),
    IdInColl_Length = length(IdInColl),
    [{where, Where}] = dets:lookup(?CODE_TEMPLATES, where),
    Where_Length = length(Where),

    Code = [
            lists:nth(rand:uniform(IdInColl_Length), IdInColl) ++
            case rand:uniform(2) rem 2 of
                1 -> ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionInvocation = Rule) ->
    ?CREATE_CODE_START,
    [{expressionCommalist, ExpressionCommalist}] = dets:lookup(?CODE_TEMPLATES, expressionCommalist),
    ExpressionCommalist_Length = length(ExpressionCommalist),
    [{functionName, FunctionName}] = dets:lookup(?CODE_TEMPLATES, functionName),
    FunctionName_Length = length(FunctionName),

    Code =
        [
            lists:append([
                lists:nth(rand:uniform(FunctionName_Length), FunctionName),
                ?SP_OPT,
                "(",
                ?SP_OPT,
                case rand:uniform(4) rem 4 of
                    1 -> "Distinct";
                    2 -> lists:append(
                        [
                            "Distinct",
                            ?SP,
                            lists:nth(rand:uniform(ExpressionCommalist_Length), ExpressionCommalist)
                        ]);
                    3 ->
                        lists:nth(rand:uniform(ExpressionCommalist_Length), ExpressionCommalist);
                    _ -> []
                end,
                ?SP_OPT,
                ")"
            ])
            || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FunctionName = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionName = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = ["exists"] ++
        [re:replace(SN, "_SYMB_NAME_", "_FUNCT_NAME_", [{return, list}]) || SN <- SymbolicName],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HexInteger = ('0',X), HexString ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(hexInteger = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "0x0",
        "0x7",
        "0xa",
        "0xabc",
        "0xabcdef",
        "0x12a",
        "0x3456abc",
        "0x7890abcdef",
        "0x0123456789ABCDEF"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% HexLetter = (A)
%%           | (B)
%%           | (C)
%%           | (D)
%%           | (E)
%%           | (F)
%%           ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(hexLetter = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "A",
        "B",
        "C",
        "D",
        "E",
        "F"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IdInColl = Variable, SP, (I,N), SP, Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(idInColl = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
        lists:append([
            lists:nth(rand:uniform(Variable_Length), Variable),
            ?SP,
            "In",
            ?SP,
            lists:nth(rand:uniform(Expression_Length), Expression)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ImplicitProcedureInvovcation = ProcedureName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(implicitProcedureInvocation = Rule) ->
    ?CREATE_CODE_START,
    [{procedureName, ProcedureName}] = dets:lookup(?CODE_TEMPLATES, procedureName),

    Code = [re:replace(PN, "_PROC_NAME_", "_IMPL_PROC_INVOC_", [{return, list}]) || PN <- ProcedureName],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% InQueryCall = (C,A,L,L), SP, ExplicitProcedureInvocation, [[SP], (Y,I,E,L,D), SP, YieldItems] ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% InQueryCall = (C,A,L,L), SP, ExplicitProcedureInvocation, [SP, (Y,I,E,L,D), SP, YieldItems] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(inQueryCall = Rule) ->
    ?CREATE_CODE_START,
    [{explicitProcedureInvocation, ExplicitProcedureInvocation}] = dets:lookup(?CODE_TEMPLATES, explicitProcedureInvocation),
    ExplicitProcedureInvocation_Length = length(ExplicitProcedureInvocation),
    [{yieldItems, YieldItems}] = dets:lookup(?CODE_TEMPLATES, yieldItems),
    YieldItems_Length = length(YieldItems),

    Code = [
        lists:append([
            "Call",
            ?SP,
            case rand:uniform(2) rem 2 of
                1 ->
                    lists:nth(rand:uniform(ExplicitProcedureInvocation_Length), ExplicitProcedureInvocation);
                _ -> lists:append(
                    [
                        lists:nth(rand:uniform(ExplicitProcedureInvocation_Length), ExplicitProcedureInvocation),
                        ?SP,
                        "Yield",
                        ?SP,
                        lists:nth(rand:uniform(YieldItems_Length), YieldItems)
                    ])
            end
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% IntegerLiteral = HexInteger
%%                | OctalInteger
%%                | DecimalInteger
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(integerLiteral = Rule) ->
    ?CREATE_CODE_START,
    [{decimalInteger, DecimalInteger}] = dets:lookup(?CODE_TEMPLATES, decimalInteger),
    [{hexInteger, HexInteger}] = dets:lookup(?CODE_TEMPLATES, hexInteger),
    [{octalInteger, OctalInteger}] = dets:lookup(?CODE_TEMPLATES, octalInteger),

    Code = lists:append([HexInteger, OctalInteger, DecimalInteger]),
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LabelName = SchemaName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(labelName = Rule) ->
    ?CREATE_CODE_START,
    [{schemaName, SchemaName}] = dets:lookup(?CODE_TEMPLATES, schemaName),

    Code = [re:replace(SN, "_SCHEMA_NAME_", "_LABEL_NAME_", [{return, list}]) || SN <- SchemaName],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Limit = (L,I,M,I,T), SP, Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(limit = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
        lists:append([
            "Limit",
            ?SP,
            lists:nth(rand:uniform(Expression_Length), Expression)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ListComprehension = '[', FilterExpression, [[SP], '|', Expression], ']' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(listComprehension = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),

    Code = [
        lists:append([
            "[",
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression),
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ -> lists:append(
                    [
                        ?SP_OPT,
                        "|",
                        lists:nth(rand:uniform(Expression_Length), Expression)
                    ])
            end,
            "]"
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(listLiteral = Rule) ->
    ?CREATE_CODE_START,
    [{expressionCommalist, ExpressionCommalist}] = dets:lookup(?CODE_TEMPLATES, expressionCommalist),
    ExpressionCommalist_Length = length(ExpressionCommalist),

    Code = [
        lists:append([
            "[",
            ?SP_OPT,
            lists:nth(rand:uniform(ExpressionCommalist_Length), ExpressionCommalist),
            ?SP_OPT,
            "]"
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Literal = NumberLiteral
%%         | StringLiteral
%%         | BooleanLiteral
%%         | (N,U,L,L)
%%         | MapLiteral
%%         | ListLiteral
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(literal = Rule) ->
    ?CREATE_CODE_START,
    [{booleanLiteral, BooleanLiteral}] = dets:lookup(?CODE_TEMPLATES, booleanLiteral),
    [{listLiteral, ListLiteral}] = dets:lookup(?CODE_TEMPLATES, listLiteral),
    ListLiteral_Length = length(ListLiteral),
    [{mapLiteral, MapLiteral}] = dets:lookup(?CODE_TEMPLATES, mapLiteral),
    MapLiteral_Length = length(MapLiteral),
    [{numberLiteral, NumberLiteral}] = dets:lookup(?CODE_TEMPLATES, numberLiteral),
    [{stringLiteral, StringLiteral}] = dets:lookup(?CODE_TEMPLATES, stringLiteral),

    Code = lists:append([
        NumberLiteral,
        StringLiteral,
        BooleanLiteral,
        ["Null"],
        [
            case rand:uniform(2) rem 2 of
                1 -> lists:nth(rand:uniform(MapLiteral_Length), MapLiteral);
                _ -> lists:nth(rand:uniform(ListLiteral_Length), ListLiteral)
            end
            || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
        ]
    ]),
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Literal = NumberLiteral
%%         | StringLiteral
%%         | BooleanLiteral
%%         | (N,U,L,L)
%%         | MapLiteral
%%         | ListLiteral
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(literal_1 = _Rule) ->
    ?CREATE_CODE_START,
    [{booleanLiteral, BooleanLiteral}] = dets:lookup(?CODE_TEMPLATES, booleanLiteral),
    [{numberLiteral, NumberLiteral}] = dets:lookup(?CODE_TEMPLATES, numberLiteral),
    [{stringLiteral, StringLiteral}] = dets:lookup(?CODE_TEMPLATES, stringLiteral),

    Code = lists:append([
        NumberLiteral,
        StringLiteral,
        BooleanLiteral,
        ["Null"]
    ]),
    store_code(literal, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MapLiteral = '{', [SP], [PropertyKeyName, [SP], ':', [SP], Expression, [SP], { ',', [SP], PropertyKeyName, [SP], ':', [SP], Expression, [SP] }], '}' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(mapLiteral = Rule) ->
    ?CREATE_CODE_START,

    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{propertyKeyName, PropertyKeyName}] = dets:lookup(?CODE_TEMPLATES, propertyKeyName),
    PropertyKeyName_Length = length(PropertyKeyName),

    Code = lists:append(
        [
            lists:append(["{", ?SP_OPT, "}"])
        ],
        [
            lists:append([
                "{",
                ?SP_OPT,
                lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName),
                ?SP_OPT,
                ":",
                ?SP_OPT,
                lists:nth(rand:uniform(Expression_Length), Expression),
                ?SP_OPT,
                case rand:uniform(3) rem 3 of
                    1 -> lists:append([
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName),
                        ?SP_OPT,
                        ":",
                        ?SP_OPT,
                        lists:nth(rand:uniform(Expression_Length), Expression),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName),
                        ?SP_OPT,
                        ":",
                        ?SP_OPT,
                        lists:nth(rand:uniform(Expression_Length), Expression),
                        ?SP_OPT
                    ]);
                    2 -> lists:append([
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName),
                        ?SP_OPT,
                        ":",
                        ?SP_OPT,
                        lists:nth(rand:uniform(Expression_Length), Expression),
                        ?SP_OPT
                    ]);
                    _ -> []
                end,
                "}"
            ])
            || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
        ]
    ),
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), [SP], Pattern, [[SP], Where] ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% Match = [(O,P,T,I,O,N,A,L), SP], (M,A,T,C,H), SP, Pattern, [SP, Where] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(match = Rule) ->
    ?CREATE_CODE_START,
    [{pattern, Pattern}] = dets:lookup(?CODE_TEMPLATES, pattern),
    Pattern_Length = length(Pattern),
    [{where, Where}] = dets:lookup(?CODE_TEMPLATES, where),
    Where_Length = length(Where),

    Code = [
        case rand:uniform(4) rem 4 of
            1 -> lists:append(
                [
                    "Optional",
                    ?SP,
                    "Match",
                    ?SP,
                    lists:nth(rand:uniform(Pattern_Length), Pattern),
                    ?SP,
                    lists:nth(rand:uniform(Where_Length), Where)
                ]);
            2 -> lists:append(
                [
                    "Optional",
                    ?SP,
                    "Match",
                    ?SP,
                    lists:nth(rand:uniform(Pattern_Length), Pattern)
                ]);
            3 -> lists:append(
                [
                    "Match",
                    ?SP,
                    lists:nth(rand:uniform(Pattern_Length), Pattern),
                    ?SP,
                    lists:nth(rand:uniform(Where_Length), Where)
                ]);
            _ -> lists:append(
                [
                    "Match",
                    ?SP,
                    lists:nth(rand:uniform(Pattern_Length), Pattern)
                ])
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Merge = (M,E,R,G,E), [SP], PatternPart, { SP, MergeAction } ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% Merge = (M,E,R,G,E), SP, PatternPart, { SP, MergeAction } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(merge = Rule) ->
    ?CREATE_CODE_START,
    [{mergeAction, MergeAction}] = dets:lookup(?CODE_TEMPLATES, mergeAction),
    MergeAction_Length = length(MergeAction),
    [{patternPart, PatternPart}] = dets:lookup(?CODE_TEMPLATES, patternPart),
    PatternPart_Length = length(PatternPart),

    Code = [
        lists:append([
            "Merge",
            ?SP,
            lists:nth(rand:uniform(PatternPart_Length), PatternPart),
            case rand:uniform(3) rem 3 of
                1 -> lists:append(
                    [
                        ?SP,
                        lists:nth(rand:uniform(MergeAction_Length), MergeAction),
                        ?SP,
                        lists:nth(rand:uniform(MergeAction_Length), MergeAction)
                    ]);
                2 ->
                    ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction);
                _ -> []
            end
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MergeAction = ((O,N), SP, (M,A,T,C,H), SP, Set)
%%             | ((O,N), SP, (C,R,E,A,T,E), SP, Set) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(mergeAction = Rule) ->
    ?CREATE_CODE_START,
    [{set, Set}] = dets:lookup(?CODE_TEMPLATES, set),
    Set_Length = length(Set),

    Code = [
        lists:append([
            "On",
            ?SP,
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        "Match",
                        ?SP,
                        lists:nth(rand:uniform(Set_Length), Set)
                    ]);
                _ -> lists:append(
                    [
                        "Create",
                        ?SP,
                        lists:nth(rand:uniform(Set_Length), Set)
                    ])
            end
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MultiPartQuery = (ReadPart | (UpdatingStartClause, [SP], UpdatingPart)), With, [SP], { ReadPart, UpdatingPart, With, [SP] }, SinglePartQuery ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% MultiPartQuery = (ReadPart | (UpdatingStartClause, SP, UpdatingPart)), With, SP, { ReadPart, UpdatingPart, With, SP }, SinglePartQuery ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(multiPartQuery = Rule) ->
    ?CREATE_CODE_START,
    [{readingClauseList, ReadingClauseList}] = dets:lookup(?CODE_TEMPLATES, readingClauseList),
    ReadingClauseList_Length = length(ReadingClauseList),
    [{readPartUpdatingPartWithList, ReadPartUpdatingPartWithList}] = dets:lookup(?CODE_TEMPLATES, readPartUpdatingPartWithList),
    ReadPartUpdatingPartWithList_Length = length(ReadPartUpdatingPartWithList),
    [{singlePartQuery, SinglePartQuery}] = dets:lookup(?CODE_TEMPLATES, singlePartQuery),
    SinglePartQuery_Length = length(SinglePartQuery),
    [{updatingClauseList, UpdatingClauseList}] = dets:lookup(?CODE_TEMPLATES, updatingClauseList),
    UpdatingClauseList_Length = length(UpdatingClauseList),
    [{updatingStartClause, UpdatingStartClause}] = dets:lookup(?CODE_TEMPLATES, updatingStartClause),
    UpdatingStartClause_Length = length(UpdatingStartClause),
    [{with, With}] = dets:lookup(?CODE_TEMPLATES, with),
    With_Length = length(With),

    Code = [
        lists:append([
            case rand:uniform(2) rem 2 of
                1 -> case rand:uniform(20) rem 20 of
                         1 -> [];
                         _ ->
                             lists:nth(rand:uniform(ReadingClauseList_Length), ReadingClauseList)
                     end;
                _ -> lists:append(
                    [
                        lists:nth(rand:uniform(UpdatingStartClause_Length), UpdatingStartClause),
                        ?SP,
                        case rand:uniform(20) rem 20 of
                            1 -> [];
                            _ ->
                                lists:nth(rand:uniform(UpdatingClauseList_Length), UpdatingClauseList)
                        end
                    ])
            end,
            ?SP,
            lists:nth(rand:uniform(With_Length), With),
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ ->
                    ?SP ++ lists:nth(rand:uniform(ReadPartUpdatingPartWithList_Length), ReadPartUpdatingPartWithList)
            end,
            ?SP,
            lists:nth(rand:uniform(SinglePartQuery_Length), SinglePartQuery)
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Namespace = { SymbolicName, '.' } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(namespace = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Namespace = [re:replace(SN, "_SYMB_NAME_", "_NAMESPACE_", [{return, list}]) ++ "." || SN <- SymbolicName],
    Namespace_Length = length(Namespace),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:append(
                [
                    lists:nth(rand:uniform(Namespace_Length), Namespace),
                    lists:nth(rand:uniform(Namespace_Length), Namespace),
                    lists:nth(rand:uniform(Namespace_Length), Namespace)
                ]);
            2 -> lists:nth(rand:uniform(Namespace_Length), Namespace)
            ++ lists:nth(rand:uniform(Namespace_Length), Namespace);
            _ ->
                lists:nth(rand:uniform(Namespace_Length), Namespace)
        end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NodeLabel = ':', [SP], LabelName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(nodeLabel = Rule) ->
    ?CREATE_CODE_START,
    [{labelName, LabelName}] = dets:lookup(?CODE_TEMPLATES, labelName),

    Code = [
        lists:append([":", ?SP_OPT, LN]) || LN <- LabelName
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NodeLabels = NodeLabel, { [SP], NodeLabel } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(nodeLabels = Rule) ->
    ?CREATE_CODE_START,
    [{nodeLabel, NodeLabel}] = dets:lookup(?CODE_TEMPLATES, nodeLabel),
    NodeLabel_Length = length(NodeLabel),

    Code = [
            lists:nth(rand:uniform(NodeLabel_Length), NodeLabel) ++
            case rand:uniform(3) rem 3 of
                1 -> lists:append([
                    ?SP_OPT,
                    lists:nth(rand:uniform(NodeLabel_Length), NodeLabel),
                    ?SP_OPT,
                    lists:nth(rand:uniform(NodeLabel_Length), NodeLabel)
                ]);
                2 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(NodeLabel_Length), NodeLabel);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NodePattern = '(', [SP], [Variable, [SP]], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% NodePattern = '(', [SP], [Variable, SP], [NodeLabels, [SP]], [Properties, [SP]], ')' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(nodePattern = Rule) ->
    ?CREATE_CODE_START,
    [{nodeLabels, NodeLabels}] = dets:lookup(?CODE_TEMPLATES, nodeLabels),
    NodeLabels_Length = length(NodeLabels),
    [{properties, Properties}] = dets:lookup(?CODE_TEMPLATES, properties),
    Properties_Length = length(Properties),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
        lists:append([
            "(",
            ?SP_OPT,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ -> lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP

            end,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ ->
                    lists:nth(rand:uniform(NodeLabels_Length), NodeLabels) ++ ?SP_OPT

            end,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ ->
                    lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT

            end,
            ")"
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NumberLiteral = DoubleLiteral
%%               | IntegerLiteral
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(numberLiteral = Rule) ->
    ?CREATE_CODE_START,
    [{doubleLiteral, DoubleLiteral}] = dets:lookup(?CODE_TEMPLATES, doubleLiteral),
    [{integerLiteral, IntegerLiteral}] = dets:lookup(?CODE_TEMPLATES, integerLiteral),

    Code = DoubleLiteral ++ IntegerLiteral,
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OctalInteger = '0', OctalString ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(octalInteger = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "00",
        "01",
        "07",
        "010",
        "0321"
        "01234"
        "054321"
        "0123456"
        "01234567"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Order = (O,R,D,E,R), SP, (B,Y), SP, SortItem, { ',', [SP], SortItem } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(order = Rule) ->
    ?CREATE_CODE_START,
    [{sortItem, SortItem}] = dets:lookup(?CODE_TEMPLATES, sortItem),
    SortItem_Length = length(SortItem),

    Code = [
        lists:append([
            "Order",
            ?SP,
            "By",
            ?SP,
            lists:nth(rand:uniform(SortItem_Length), SortItem),
            case rand:uniform(3) rem 3 of
                1 -> lists:append(
                    [
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(SortItem_Length), SortItem),
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(SortItem_Length), SortItem)
                    ]);
                2 -> lists:append(
                    [
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(SortItem_Length), SortItem)
                    ]);
                _ -> []
            end
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parameter = '$', (SymbolicName | DecimalInteger) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameter = Rule) ->
    ?CREATE_CODE_START,
    [{decimalInteger, DecimalInteger}] = dets:lookup(?CODE_TEMPLATES, decimalInteger),
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = lists:append(
        ["$" ++ re:replace(SN, "_SYMB_NAME_", "_PARAMETER_", [{return, list}]) || SN <- SymbolicName],
        ["$" ++ DI || DI <- DecimalInteger]
    ),
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Pattern = PatternPart, { [SP], ',', [SP], PatternPart } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(pattern = Rule) ->
    ?CREATE_CODE_START,
    [{patternPart, PatternPart}] = dets:lookup(?CODE_TEMPLATES, patternPart),
    PatternPart_Length = length(PatternPart),

    Code = [
            lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
            case rand:uniform(3) rem 3 of
                1 -> lists:append(
                    [
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PatternPart_Length), PatternPart),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PatternPart_Length), PatternPart),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PatternPart_Length), PatternPart)
                    ]);
                2 -> lists:append(
                    [
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PatternPart_Length), PatternPart),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PatternPart_Length), PatternPart)
                    ]);
                _ -> lists:append(
                    [
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(PatternPart_Length), PatternPart)
                    ])
            end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [(W,H,E,R,E), [SP], Expression, [SP]], '|', [SP], Expression, [SP], ']' ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [(W,H,E,R,E), SP, Expression, [SP]], '|', [SP], Expression, [SP], ']' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(patternComprehension = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{relationshipsPattern, RelationshipsPattern}] = dets:lookup(?CODE_TEMPLATES, relationshipsPattern),
    RelationshipsPattern_Length = length(RelationshipsPattern),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
        lists:append([
            "[",
            ?SP_OPT,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ -> lists:append([
                    lists:nth(rand:uniform(Variable_Length), Variable),
                    ?SP_OPT,
                    "=",
                    ?SP_OPT
                ])
            end,
            lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern),
            ?SP_OPT,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ -> lists:append([
                    "Where",
                    ?SP,
                    lists:nth(rand:uniform(Expression_Length), Expression),
                    ?SP_OPT
                ])
            end,
            "|",
            ?SP_OPT,
            lists:nth(rand:uniform(Expression_Length), Expression),
            ?SP_OPT,
            "]"
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PatternElement = (NodePattern, { [SP], PatternElementChain })
%%                | ('(', PatternElement, ')') ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(patternElement = Rule) ->
    ?CREATE_CODE_START,
    [{nodePattern, NodePattern}] = dets:lookup(?CODE_TEMPLATES, nodePattern),
    NodePattern_Length = length(NodePattern),
    [{patternElementChainList, PatternElementChainList}] = dets:lookup(?CODE_TEMPLATES, patternElementChainList),
    PatternElementChainList_Length = length(PatternElementChainList),

    Code = [
        case rand:uniform(2) rem 2 of
            1 -> lists:append([
                lists:nth(rand:uniform(NodePattern_Length), NodePattern),
                ?SP_OPT,
                case rand:uniform(4) rem 4 of
                    1 -> [];
                    _ ->
                        lists:nth(rand:uniform(PatternElementChainList_Length), PatternElementChainList)

                end
            ]);
            _ -> lists:append([
                "(",
                lists:nth(rand:uniform(NodePattern_Length), NodePattern),
                ?SP_OPT,
                case rand:uniform(4) rem 4 of
                    1 -> [];
                    _ ->
                        lists:nth(rand:uniform(PatternElementChainList_Length), PatternElementChainList)

                end,
                ")"
            ])
        end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, 0, false),
    store_code(anonymousPatternPart, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PatternElementChain = RelationshipPattern, [SP], NodePattern ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(patternElementChain = Rule) ->
    ?CREATE_CODE_START,
    [{nodePattern, NodePattern}] = dets:lookup(?CODE_TEMPLATES, nodePattern),
    NodePattern_Length = length(NodePattern),
    [{relationshipPattern, RelationshipPattern}] = dets:lookup(?CODE_TEMPLATES, relationshipPattern),
    RelationshipPattern_Length = length(RelationshipPattern),

    Code = [
        lists:append([
            lists:nth(rand:uniform(RelationshipPattern_Length), RelationshipPattern),
            ?SP_OPT,
            lists:nth(rand:uniform(NodePattern_Length), NodePattern)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PatternElementChainList = { [SP], PatternElementChain }
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(patternElementChainList = Rule) ->
    ?CREATE_CODE_START,
    [{patternElementChain, PatternElementChain}] = dets:lookup(?CODE_TEMPLATES, patternElementChain),
    PatternElementChain_Length = length(PatternElementChain),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:append(
                [
                    ?SP,
                    lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain),
                    ?SP,
                    lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain),
                    ?SP,
                    lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain)
                ]);
            2 -> lists:append(
                [
                    ?SP,
                    lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain),
                    ?SP,
                    lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain)
                ]);
            _ ->
                ?SP ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain)
        end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PatternPart = (Variable, [SP], '=', [SP], AnonymousPatternPart)
%%             | AnonymousPatternPart ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(patternPart = Rule) ->
    ?CREATE_CODE_START,
    [{anonymousPatternPart, AnonymousPatternPart}] = dets:lookup(?CODE_TEMPLATES, anonymousPatternPart),
    AnonymousPatternPart_Length = length(AnonymousPatternPart),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ -> lists:append(
                    [
                        lists:nth(rand:uniform(Variable_Length), Variable),
                        ?SP_OPT,
                        "=",
                        ?SP_OPT
                    ])
            end ++
            lists:nth(rand:uniform(AnonymousPatternPart_Length), AnonymousPatternPart)
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProcedureName = Namespace* SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(procedureName = Rule) ->
    ?CREATE_CODE_START,
    [{namespace, Namespace}] = dets:lookup(?CODE_TEMPLATES, namespace),
    Namespace_Length = length(Namespace),
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    ProcedureName = [re:replace(SN, "_SYMB_NAME_", "_PROC_NAME_", [{return, list}]) || SN <- SymbolicName],
    ProcedureName_Length = length(ProcedureName),

    Code = [
            case rand:uniform(2) rem 2 of
                1 ->
                    lists:nth(rand:uniform(Namespace_Length), Namespace);
                _ -> []
            end ++
            lists:nth(rand:uniform(ProcedureName_Length), ProcedureName)
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProcedureResultField = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(procedureResultField = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = [re:replace(SN, "_SYMB_NAME_", "_PROC_RESULT_FIELD_", [{return, list}]) || SN <- SymbolicName],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Properties = MapLiteral
%%            | Parameter
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(properties = Rule) ->
    ?CREATE_CODE_START,
    [{mapLiteral, MapLiteral}] = dets:lookup(?CODE_TEMPLATES, mapLiteral),
    MapLiteral_Length = length(MapLiteral),
    [{parameter, Parameter}] = dets:lookup(?CODE_TEMPLATES, parameter),
    Parameter_Length = length(Parameter),

    Code = [
        case rand:uniform(2) rem 2 of
            1 -> lists:nth(rand:uniform(MapLiteral_Length), MapLiteral);
            _ -> lists:nth(rand:uniform(Parameter_Length), Parameter)
        end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PropertyExpression = Atom, { [SP], PropertyLookup }- ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(propertyExpression = Rule) ->
    ?CREATE_CODE_START,
    [{atom, Atom}] = dets:lookup(?CODE_TEMPLATES, atom),
    Atom_Length = length(Atom),
    [{propertyLookup, PropertyLookup}] = dets:lookup(?CODE_TEMPLATES, propertyLookup),
    PropertyLookup_Length = length(PropertyLookup),

    Code = [
            lists:nth(rand:uniform(Atom_Length), Atom) ++
            case rand:uniform(3) rem 3 of
                1 -> lists:append(
                    [
                        ?SP_OPT,
                        lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup),
                        ?SP_OPT,
                        lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup),
                        ?SP_OPT,
                        lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup)
                    ]);
                2 -> lists:append(
                    [
                        ?SP_OPT,
                        lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup),
                        ?SP_OPT,
                        lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup)
                    ]);
                _ ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup)
            end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PropertyKeyName = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(propertyKeyName = Rule) ->
    ?CREATE_CODE_START,
    [{schemaName, SchemaName}] = dets:lookup(?CODE_TEMPLATES, schemaName),

    Code = [
        re:replace(SN, "_SCHEMA_NAME_", "_PROP_KEY_NAME_", [{return, list}]) || SN <- SchemaName
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PropertyLookup = '.', [SP], (PropertyKeyName) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(propertyLookup = Rule) ->
    ?CREATE_CODE_START,
    [{propertyKeyName, PropertyKeyName}] = dets:lookup(?CODE_TEMPLATES, propertyKeyName),

    Code = [
        lists:append([".", ?SP_OPT, PK]) || PK <- PropertyKeyName
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Query = RegularQuery
%%       | StandaloneCall
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query = Rule) ->
    ?CREATE_CODE_START,
    [{regularQuery, RegularQuery}] = dets:lookup(?CODE_TEMPLATES, regularQuery),
    RegularQuery_Length = length(RegularQuery),
    [{standaloneCall, StandaloneCall}] = dets:lookup(?CODE_TEMPLATES, standaloneCall),
    StandaloneCall_Length = length(StandaloneCall),

    Code = [
        case rand:uniform(2) rem 2 of
            1 -> lists:nth(rand:uniform(RegularQuery_Length), RegularQuery);
            _ -> lists:nth(rand:uniform(StandaloneCall_Length), StandaloneCall)
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    store_code(statement, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RangeLiteral = '*', [SP], [IntegerLiteral, [SP]], ['..', [SP], [IntegerLiteral, [SP]]] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(rangeLiteral = Rule) ->
    ?CREATE_CODE_START,
    [{integerLiteral, IntegerLiteral}] = dets:lookup(?CODE_TEMPLATES, integerLiteral),
    IntegerLiteral_Length = length(IntegerLiteral),

    Code = lists:append(
        [
            "*",
            " * "
        ],
        [
            lists:append([
                "*",
                ?SP_OPT,
                lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral),
                ?SP,
                case rand:uniform(5) rem 5 of
                    1 -> [];
                    2 -> ?SP_OPT ++ "..";
                    _ -> lists:append(
                        [
                            ?SP_OPT,
                            "..",
                            ?SP_OPT,
                            lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral),
                            ?SP_OPT
                        ])
                end
            ])
            || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
        ]
    ),
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ReadingClause = Match
%%               | Unwind
%%               | InQueryCall
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(readingClause = Rule) ->
    ?CREATE_CODE_START,
    [{inQueryCall, InQueryCall}] = dets:lookup(?CODE_TEMPLATES, inQueryCall),
    InQueryCall_Length = length(InQueryCall),
    [{match, Match}] = dets:lookup(?CODE_TEMPLATES, match),
    Match_Length = length(Match),
    [{unwind, Unwind}] = dets:lookup(?CODE_TEMPLATES, unwind),
    Unwind_Length = length(Unwind),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:nth(rand:uniform(Match_Length), Match);
            2 -> lists:nth(rand:uniform(Unwind_Length), Unwind);
            _ -> lists:nth(rand:uniform(InQueryCall_Length), InQueryCall)
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% { ReadingClause, [SP] }
%% -----------------------------------------------------------------------------
%% wwe ???
%% { ReadingClause, SP }
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(readingClauseList = Rule) ->
    ?CREATE_CODE_START,
    [{readingClause, ReadingClause}] = dets:lookup(?CODE_TEMPLATES, readingClause),
    ReadingClause_Length = length(ReadingClause),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:append(
                [
                    lists:nth(rand:uniform(ReadingClause_Length), ReadingClause),
                    ?SP,
                    lists:nth(rand:uniform(ReadingClause_Length), ReadingClause),
                    ?SP,
                    lists:nth(rand:uniform(ReadingClause_Length), ReadingClause),
                    ?SP
                ]);
            2 -> lists:append(
                [
                    lists:nth(rand:uniform(ReadingClause_Length), ReadingClause),
                    ?SP,
                    lists:nth(rand:uniform(ReadingClause_Length), ReadingClause),
                    ?SP
                ]);
            _ ->
                lists:nth(rand:uniform(ReadingClause_Length), ReadingClause) ++ ?SP
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    store_code(readPart, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ReadOnlyEnd = ReadPart, Return ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% ReadOnlyEnd = ReadPart, SP, Return ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(readOnlyEnd = Rule) ->
    ?CREATE_CODE_START,
    [{readingClauseList, ReadingClauseList}] = dets:lookup(?CODE_TEMPLATES, readingClauseList),
    ReadingClauseList_Length = length(ReadingClauseList),
    [{return, Return}] = dets:lookup(?CODE_TEMPLATES, return),
    Return_Length = length(Return),

    Code = [
        lists:append([
            lists:nth(rand:uniform(ReadingClauseList_Length), ReadingClauseList),
            ?SP,
            lists:nth(rand:uniform(Return_Length), Return)
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% { ReadPart, UpdatingPart, With, [SP]
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(readPartUpdatingPartWithList = Rule) ->
    ?CREATE_CODE_START,
    [{readingClauseList, ReadingClauseList}] = dets:lookup(?CODE_TEMPLATES, readingClauseList),
    ReadingClauseList_Length = length(ReadingClauseList),
%% wwe ???
%%    [{updatingClauseList, UpdatingClauseList}] = dets:lookup(?CODE_TEMPLATES, updatingClauseList),
%%    UpdatingClauseList_Length = length(UpdatingClauseList),
    [{with, With}] = dets:lookup(?CODE_TEMPLATES, with),
    With_Length = length(With),

    ReadPartUpdatingPartWith = [
        lists:append([
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ ->
                    lists:nth(rand:uniform(ReadingClauseList_Length), ReadingClauseList)
            end,
%% wwe ???
%%            case rand:uniform(20) rem 20 of
%%                1 -> [];
%%                _ ->
%%                    ?SP ++
%%                    lists:nth(rand:uniform(UpdatingClauseList_Length), UpdatingClauseList)
%%            end,
            ?SP,
            lists:nth(rand:uniform(With_Length), With),
            ?SP_OPT
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    ReadPartUpdatingPartWith_Length = length(ReadPartUpdatingPartWith),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:append(
                [
                    lists:nth(rand:uniform(ReadPartUpdatingPartWith_Length), ReadPartUpdatingPartWith),
                    ?SP,
                    lists:nth(rand:uniform(ReadPartUpdatingPartWith_Length), ReadPartUpdatingPartWith),
                    ?SP,
                    lists:nth(rand:uniform(ReadPartUpdatingPartWith_Length), ReadPartUpdatingPartWith)
                ]);
            2 -> lists:append(
                [
                    lists:nth(rand:uniform(ReadPartUpdatingPartWith_Length), ReadPartUpdatingPartWith),
                    ?SP,
                    lists:nth(rand:uniform(ReadPartUpdatingPartWith_Length), ReadPartUpdatingPartWith)
                ]);
            _ ->
                lists:nth(rand:uniform(ReadPartUpdatingPartWith_Length), ReadPartUpdatingPartWith)
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ReadUpdateEnd = ReadingClause, { [SP], ReadingClause }, { [SP], UpdatingClause }-, [[SP], Return] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(readUpdateEnd = Rule) ->
    ?CREATE_CODE_START,
    [{readingClauseList, ReadingClauseList}] = dets:lookup(?CODE_TEMPLATES, readingClauseList),
    ReadingClauseList_Length = length(ReadingClauseList),
    [{return, Return}] = dets:lookup(?CODE_TEMPLATES, return),
    Return_Length = length(Return),
    [{updatingClauseList, UpdatingClauseList}] = dets:lookup(?CODE_TEMPLATES, updatingClauseList),
    UpdatingClauseList_Length = length(UpdatingClauseList),

    Code = [
        lists:append([
            lists:nth(rand:uniform(ReadingClauseList_Length), ReadingClauseList),
            ?SP,
            lists:nth(rand:uniform(UpdatingClauseList_Length), UpdatingClauseList),
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        ?SP,
                        lists:nth(rand:uniform(Return_Length), Return)
                    ]);
                _ -> []
            end
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RegularDecimalReal = ({ Digit } | DecimalInteger), '.', (DigitString | DecimalInteger) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(regularDecimalReal = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        ".0",
        ".12",
        "0.0",
        "0.12",
        "12.0",
        "123.654"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RegularQuery = SingleQuery, { [SP], Union } ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% RegularQuery = SingleQuery, { SP, Union } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(regularQuery = Rule) ->
    ?CREATE_CODE_START,
    [{singleQuery, SingleQuery}] = dets:lookup(?CODE_TEMPLATES, singleQuery),
    SingleQuery_Length = length(SingleQuery),
    [{union, Union}] = dets:lookup(?CODE_TEMPLATES, union),
    Union_Length = length(Union),

    Code = [
            lists:nth(rand:uniform(SingleQuery_Length), SingleQuery) ++
            case rand:uniform(4) rem 4 of
                1 -> lists:append(
                    [
                        ?SP,
                        lists:nth(rand:uniform(Union_Length), Union),
                        ?SP,
                        lists:nth(rand:uniform(Union_Length), Union),
                        ?SP,
                        lists:nth(rand:uniform(Union_Length), Union)
                    ]);
                2 -> lists:append(
                    [
                        ?SP,
                        lists:nth(rand:uniform(Union_Length), Union),
                        ?SP,
                        lists:nth(rand:uniform(Union_Length), Union)
                    ]);
                3 -> ?SP ++ lists:nth(rand:uniform(Union_Length), Union);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RelationshipDetail = '[', [SP], [Variable, [SP]], [RelationshipTypes, [SP]], [RangeLiteral], [Properties, [SP]], ']' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relationshipDetail = Rule) ->
    ?CREATE_CODE_START,
    [{properties, Properties}] = dets:lookup(?CODE_TEMPLATES, properties),
    Properties_Length = length(Properties),
    [{rangeLiteral, RangeLiteral}] = dets:lookup(?CODE_TEMPLATES, rangeLiteral),
    RangeLiteral_Length = length(RangeLiteral),
    [{relationshipTypes, RelationshipTypes}] = dets:lookup(?CODE_TEMPLATES, relationshipTypes),
    RelationshipTypes_Length = length(RelationshipTypes),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
        lists:append([
            "[",
            ?SP_OPT,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ ->
                    lists:nth(rand:uniform(Variable_Length), Variable) ++ ?SP_OPT
            end,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ ->
                    lists:nth(rand:uniform(RelationshipTypes_Length), RelationshipTypes) ++ ?SP_OPT
            end,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ -> lists:nth(rand:uniform(RangeLiteral_Length), RangeLiteral)
            end,
            case rand:uniform(2) rem 2 of
                1 -> [];
                _ ->
                    lists:nth(rand:uniform(Properties_Length), Properties) ++ ?SP_OPT
            end,
            "]"
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RelationshipPattern = (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
%                     | (LeftArrowHead, [SP], Dash, [SP], [RelationshipDetail], [SP], Dash)
%                     | (                     Dash, [SP], [RelationshipDetail], [SP], Dash, [SP], RightArrowHead)
%                     | (                     Dash, [SP], [RelationshipDetail], [SP], Dash) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relationshipPattern = Rule) ->
    ?CREATE_CODE_START,
    [{relationshipDetail, RelationshipDetail}] = dets:lookup(?CODE_TEMPLATES, relationshipDetail),
    RelationshipDetail_Length = length(RelationshipDetail),

    Code = [
        lists:append([
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ -> ?LEFT_ARROW_HEAD ++ ?SP_OPT
            end,
            ?DASH,
            ?SP_OPT,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ ->
                    lists:nth(rand:uniform(RelationshipDetail_Length), RelationshipDetail)
            end,
            ?SP_OPT,
            ?DASH,
            case rand:uniform(4) rem 4 of
                1 -> [];
                _ -> ?SP_OPT ++ ?RIGHT_ARROW_HEAD
            end
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relationshipsPattern = Rule) ->
    ?CREATE_CODE_START,
    [{nodePattern, NodePattern}] = dets:lookup(?CODE_TEMPLATES, nodePattern),
    NodePattern_Length = length(NodePattern),
    [{patternElementChainList, PatternElementChainList}] = dets:lookup(?CODE_TEMPLATES, patternElementChainList),
    PatternElementChainList_Length = length(PatternElementChainList),

    Code = [
        lists:append([
            lists:nth(rand:uniform(NodePattern_Length), NodePattern),
            ?SP_OPT,
            lists:nth(rand:uniform(PatternElementChainList_Length), PatternElementChainList)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RelationshipTypes = ':', [SP], RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relationshipTypes = Rule) ->
    ?CREATE_CODE_START,
    [{relTypeName, RelTypeName}] = dets:lookup(?CODE_TEMPLATES, relTypeName),
    RelTypeName_Length = length(RelTypeName),

    Code = [
        lists:append([
            ":",
            ?SP_OPT,
            lists:nth(rand:uniform(RelTypeName_Length), RelTypeName),
            case rand:uniform(3) rem 3 of
                1 -> lists:append([
                    ?SP_OPT,
                    "|",
                    case rand:uniform(2) rem 2 of
                        1 -> ":";
                        _ -> []
                    end,
                    ?SP_OPT,
                    lists:nth(rand:uniform(RelTypeName_Length), RelTypeName),
                    ?SP_OPT,
                    "|",
                    case rand:uniform(2) rem 2 of
                        1 -> ":";
                        _ -> []
                    end,
                    ?SP_OPT,
                    lists:nth(rand:uniform(RelTypeName_Length), RelTypeName)
                ]);
                2 -> lists:append([
                    ?SP_OPT,
                    "|",
                    case rand:uniform(2) rem 2 of
                        1 -> ":";
                        _ -> []
                    end,
                    ?SP_OPT,
                    lists:nth(rand:uniform(RelTypeName_Length), RelTypeName)
                ]);
                _ -> []
            end
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RelTypeName = SchemaName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relTypeName = Rule) ->
    ?CREATE_CODE_START,
    [{schemaName, SchemaName}] = dets:lookup(?CODE_TEMPLATES, schemaName),

    Code = [re:replace(SN, "_SCHEMA_NAME_", "_REL_TYPE_NAME_", [{return, list}]) || SN <- SchemaName],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Remove = (R,E,M,O,V,E), SP, RemoveItem, { [SP], ',', [SP], RemoveItem } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(remove = Rule) ->
    ?CREATE_CODE_START,
    [{removeItem, RemoveItem}] = dets:lookup(?CODE_TEMPLATES, removeItem),
    RemoveItem_Length = length(RemoveItem),

    Code = [
        lists:append([
            "Remove",
            ?SP,
            lists:nth(rand:uniform(RemoveItem_Length), RemoveItem),
            case rand:uniform(3) rem 3 of
                1 -> lists:append(
                    [
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(RemoveItem_Length), RemoveItem),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(RemoveItem_Length), RemoveItem)
                    ]);
                2 -> lists:append(
                    [
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(RemoveItem_Length), RemoveItem)
                    ]);
                _ -> []
            end
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RemoveItem = (Variable, NodeLabels)
%%            | PropertyExpression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(removeItem = Rule) ->
    ?CREATE_CODE_START,
    [{nodeLabels, NodeLabels}] = dets:lookup(?CODE_TEMPLATES, nodeLabels),
    NodeLabels_Length = length(NodeLabels),
    [{propertyExpression, PropertyExpression}] = dets:lookup(?CODE_TEMPLATES, propertyExpression),
    PropertyExpression_Length = length(PropertyExpression),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
        case rand:uniform(2) rem 2 of
            1 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
            lists:nth(rand:uniform(NodeLabels_Length), NodeLabels);
            _ ->
                lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression)
        end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ReservedWord = (A,L,L)
%%              | (A,S,C)
%%              | (A,S,C,E,N,D,I,N,G)
%%              | (B,Y)
%%              | (C,R,E,A,T,E)
%%              | (D,E,L,E,T,E)
%%              | (D,E,S,C)
%%              | (D,E,S,C,E,N,D,I,N,G)
%%              | (D,E,T,A,C,H)
%%              | (E,X,I,S,T,S)
%%              | (L,I,M,I,T)
%%              | (M,A,T,C,H)
%%              | (M,E,R,G,E)
%%              | (O,N)
%%              | (O,P,T,I,O,N,A,L)
%%              | (O,R,D,E,R)
%%              | (R,E,M,O,V,E)
%%              | (R,E,T,U,R,N)
%%              | (S,E,T)
%%              | (S,K,I,P)
%%              | (W,H,E,R,E)
%%              | (W,I,T,H)
%%              | (U,N,I,O,N)
%%              | (U,N,W,I,N,D)
%%              | (A,N,D)
%%              | (A,S)
%%              | (C,O,N,T,A,I,N,S)
%%              | (D,I,S,T,I,N,C,T)
%%              | (E,N,D,S)
%%              | (I,N)
%%              | (I,S)
%%              | (N,O,T)
%%              | (O,R)
%%              | (S,T,A,R,T,S)
%%              | (X,O,R)
%%              | (F,A,L,S,E)
%%              | (T,R,U,E)
%%              | (N,U,L,L)
%%              | (C,O,N,S,T,R,A,I,N,T)
%%              | (D,O)
%%              | (F,O,R)
%%              | (R,E,Q,U,I,R,E)
%%              | (U,N,I,Q,U,E)
%%              | (C,A,S,E)
%%              | (W,H,E,N)
%%              | (T,H,E,N)
%%              | (E,L,S,E)
%%              | (E,N,D)
%%              | (M,A,N,D,A,T,O,R,Y)
%%              | (S,C,A,L,A,R)
%%              | (O,F)
%%              | (A,D,D)
%%              | (D,R,O,P)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(reservedWord = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "adD",
        "alL",
        "anD",
        "aS",
        "asC",
        "ascendinG",
        "bY",
        "casE",
        "constrainT",
        "containS",
        "creatE",
        "deletE",
        "desC",
        "descendinG",
        "detacH",
        "distincT",
        "dO",
        "droP",
        "elsE",
        "enD",
        "endS",
        "existS",
        "falsE",
        "foR",
        "iN",
        "iS",
        "limiT",
        "mandatorY",
        "matcH",
        "mergE",
        "noT",
        "nulL",
        "oF",
        "oN",
        "optionaL",
        "oR",
        "ordeR",
        "removE",
        "requirE",
        "returN",
        "scalaR",
        "seT",
        "skiP",
        "startS",
        "theN",
        "truE",
        "unioN",
        "uniquE",
        "unwinD",
        "wheN",
        "wherE",
        "witH",
        "xoR"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return = (R,E,T,U,R,N), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(return = Rule) ->
    ?CREATE_CODE_START,
    [{returnBody, ReturnBody}] = dets:lookup(?CODE_TEMPLATES, returnBody),
    ReturnBody_Length = length(ReturnBody),

    Code = [
        lists:append([
            "Return",
            ?SP,
            case rand:uniform(2) rem 2 of
                1 -> "Distinct" ++ ?SP;
                _ -> []
            end,
            lists:nth(rand:uniform(ReturnBody_Length), ReturnBody)
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ReturnBody = ReturnItems, [SP, Order], [SP, Skip], [SP, Limit] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(returnBody = Rule) ->
    ?CREATE_CODE_START,
    [{limit, Limit}] = dets:lookup(?CODE_TEMPLATES, limit),
    Limit_Length = length(Limit),
    [{order, Order}] = dets:lookup(?CODE_TEMPLATES, order),
    Order_Length = length(Order),
    [{returnItems, ReturnItems}] = dets:lookup(?CODE_TEMPLATES, returnItems),
    ReturnItems_Length = length(ReturnItems),
    [{skip, Skip}] = dets:lookup(?CODE_TEMPLATES, skip),
    Skip_Length = length(Skip),

    Code = [
            lists:nth(rand:uniform(ReturnItems_Length), ReturnItems) ++
            case rand:uniform(8) rem 8 of
                1 -> lists:append(
                    [
                        ?SP,
                        lists:nth(rand:uniform(Order_Length), Order),
                        ?SP,
                        lists:nth(rand:uniform(Skip_Length), Skip),
                        ?SP,
                        lists:nth(rand:uniform(Limit_Length), Limit)
                    ]);
                2 -> lists:append(
                    [
                        ?SP,
                        lists:nth(rand:uniform(Order_Length), Order),
                        ?SP,
                        lists:nth(rand:uniform(Skip_Length), Skip)
                    ]);
                3 -> lists:append(
                    [
                        ?SP,
                        lists:nth(rand:uniform(Order_Length), Order),
                        ?SP,
                        lists:nth(rand:uniform(Limit_Length), Limit)
                    ]);
                4 -> ?SP ++ lists:nth(rand:uniform(Order_Length), Order);
                5 -> lists:append(
                    [
                        ?SP,
                        lists:nth(rand:uniform(Skip_Length), Skip),
                        ?SP,
                        lists:nth(rand:uniform(Limit_Length), Limit)
                    ]);
                6 -> ?SP ++ lists:nth(rand:uniform(Skip_Length), Skip);
                7 -> ?SP ++ lists:nth(rand:uniform(Limit_Length), Limit);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ReturnItem = (Expression, SP, (A,S), SP, Variable)
%%            | Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(returnItem = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
            lists:nth(rand:uniform(Expression_Length), Expression) ++
            case rand:uniform(2) rem 2 of
                1 -> lists:append(
                    [
                        ?SP,
                        "As",
                        ?SP,
                        lists:nth(rand:uniform(Variable_Length), Variable)
                    ]);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ReturnItems = ('*', { [SP], ',', [SP], ReturnItem })
%%             | (ReturnItem, { [SP], ',', [SP], ReturnItem }) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(returnItems = Rule) ->
    ?CREATE_CODE_START,
    [{returnItem, ReturnItem}] = dets:lookup(?CODE_TEMPLATES, returnItem),
    ReturnItem_Length = length(ReturnItem),

    Code = lists:append(
        [
            " * "
        ],
        [
            case rand:uniform(5) rem 5 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(ReturnItem_Length), ReturnItem),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(ReturnItem_Length), ReturnItem),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(ReturnItem_Length), ReturnItem)
                    ]);
                2 -> lists:append(
                    [
                        "*",
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(ReturnItem_Length), ReturnItem),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(ReturnItem_Length), ReturnItem)
                    ]);
                3 -> lists:append(
                    [
                        lists:nth(rand:uniform(ReturnItem_Length), ReturnItem),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(ReturnItem_Length), ReturnItem)
                    ]);
                4 -> lists:append(
                    [
                        "*",
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(ReturnItem_Length), ReturnItem)
                    ]);
                _ -> lists:nth(rand:uniform(ReturnItem_Length), ReturnItem)
            end
            || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
        ]
    ),
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SchemaName = SymbolicName
%%            | ReservedWord
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(schemaName = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),
%% wwe
%%    [{reservedWord, ReservedWord}] = dets:lookup(?CODE_TEMPLATES, reservedWord),
    ReservedWord = [],

    Code = [
        re:replace(SN, "_SYMB_NAME_", "_SCHEMA_NAME_", [{return, list}]) || SN <- SymbolicName
    ],
    store_code(Rule, Code ++ ReservedWord, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Set = (S,E,T), [SP], SetItem, { ',', SetItem } ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% Set = (S,E,T), SP, SetItem, { ',', SetItem } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(set = Rule) ->
    ?CREATE_CODE_START,
    [{setItem, SetItem}] = dets:lookup(?CODE_TEMPLATES, setItem),
    SetItem_Length = length(SetItem),

    Code = [
        lists:append([
            "Set",
            ?SP,
            lists:nth(rand:uniform(SetItem_Length), SetItem),
            case rand:uniform(3) rem 3 of
                1 -> lists:append(
                    [
                        ",",
                        lists:nth(rand:uniform(SetItem_Length), SetItem),
                        ",",
                        lists:nth(rand:uniform(SetItem_Length), SetItem)
                    ]);
                2 -> "," ++ lists:nth(rand:uniform(SetItem_Length), SetItem);
                _ -> []
            end
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SetItem = (PropertyExpression, [SP], '=', [SP], Expression)
%%         | (Variable, [SP], '=', [SP], Expression)
%%         | (Variable, [SP], '+=', [SP], Expression)
%%         | (Variable, [SP], NodeLabels) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(setItem = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{nodeLabels, NodeLabels}] = dets:lookup(?CODE_TEMPLATES, nodeLabels),
    NodeLabels_Length = length(NodeLabels),
    [{propertyExpression, PropertyExpression}] = dets:lookup(?CODE_TEMPLATES, propertyExpression),
    PropertyExpression_Length = length(PropertyExpression),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
        case rand:uniform(4) rem 4 of
            1 -> lists:append(
                [
                    lists:nth(rand:uniform(PropertyExpression_Length), PropertyExpression),
                    ?SP_OPT,
                    "=",
                    ?SP_OPT,
                    lists:nth(rand:uniform(Expression_Length), Expression)
                ]);
            2 -> lists:append(
                [
                    lists:nth(rand:uniform(Variable_Length), Variable),
                    ?SP_OPT,
                    "=",
                    ?SP_OPT,
                    lists:nth(rand:uniform(Expression_Length), Expression)
                ]);
            3 -> lists:append(
                [
                    lists:nth(rand:uniform(Variable_Length), Variable),
                    ?SP_OPT,
                    "+=",
                    ?SP_OPT,
                    lists:nth(rand:uniform(Expression_Length), Expression)
                ]);
            _ -> lists:append(
                [
                    lists:nth(rand:uniform(Variable_Length), Variable),
                    ?SP_OPT,
                    lists:nth(rand:uniform(NodeLabels_Length), NodeLabels)
                ])
        end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SinglePartQuery = ReadOnlyEnd
%%                 | ReadUpdateEnd
%%                 | UpdatingEnd
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(singlePartQuery = Rule) ->
    ?CREATE_CODE_START,
    [{readOnlyEnd, ReadOnlyEnd}] = dets:lookup(?CODE_TEMPLATES, readOnlyEnd),
    ReadOnlyEnd_Length = length(ReadOnlyEnd),
    [{readUpdateEnd, ReadUpdateEnd}] = dets:lookup(?CODE_TEMPLATES, readUpdateEnd),
    ReadUpdateEnd_Length = length(ReadUpdateEnd),
    [{updatingEnd, UpdatingEnd}] = dets:lookup(?CODE_TEMPLATES, updatingEnd),
    UpdatingEnd_Length = length(UpdatingEnd),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:nth(rand:uniform(ReadOnlyEnd_Length), ReadOnlyEnd);
            2 -> lists:nth(rand:uniform(ReadUpdateEnd_Length), ReadUpdateEnd);
            _ -> lists:nth(rand:uniform(UpdatingEnd_Length), UpdatingEnd)
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SingleQuery = SinglePartQuery
%%             | MultiPartQuery
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(singleQuery = Rule) ->
    ?CREATE_CODE_START,
    [{multiPartQuery, MultiPartQuery}] = dets:lookup(?CODE_TEMPLATES, multiPartQuery),
    MultiPartQuery_Length = length(MultiPartQuery),
    [{singlePartQuery, SinglePartQuery}] = dets:lookup(?CODE_TEMPLATES, singlePartQuery),
    SinglePartQuery_Length = length(SinglePartQuery),

    Code = [
        case rand:uniform(2) rem 2 of
            1 ->
                lists:nth(rand:uniform(SinglePartQuery_Length), SinglePartQuery);
            _ -> lists:nth(rand:uniform(MultiPartQuery_Length), MultiPartQuery)
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Skip = (S,K,I,P), SP, Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(skip = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
        lists:append([
            "Skip",
            ?SP,
            lists:nth(rand:uniform(Expression_Length), Expression)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% SortItem = Expression, [SP, ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(sortItem = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
            lists:nth(rand:uniform(Expression_Length), Expression) ++
            case rand:uniform(4) rem 4 of
                1 -> ?SP ++ "Descending";
                2 -> ?SP ++ "Desc";
                3 -> ?SP ++ "Ascending";
                _ -> ?SP ++ "Asc"
            end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Special variations.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(special = Rule) ->
    ?CREATE_CODE_START,
    Code = [
        %% ---------------------------------------------------------------------
        %% Problem: rangeLiteral
        %% ---------------------------------------------------------------------
        %% RangeLiteral = '*', [SP], [IntegerLiteral, [SP]], ['..', [SP], [IntegerLiteral, [SP]]] ;
        %% ---------------------------------------------------------------------
        %% match -> MATCH pattern
        %% pattern -> pattern_part_commalist
        %% pattern_part_commalist -> pattern_part
        %% pattern_part -> anonymous_pattern_part
        %% anonymous_pattern_part -> pattern_element
        %% pattern_element -> node_pattern pattern_element_chain_list
        %% node_pattern -> '(' ')'
        %% pattern_element_chain -> relationship_pattern node_pattern
        %% relationship_pattern -> '<' '-' relationship_detail '-' '>'
        %% relationship_detail -> '[' range_literal ']'
        "Match ()<-[*]->() Return a",
        %% ---------------------------------------------------------------------
        %% Problem: relationship_detail
        %% ---------------------------------------------------------------------
        %% RelationshipDetail = '[', [SP], [Variable, [SP]], [RelationshipTypes, [SP]], [RangeLiteral], [Properties, [SP]], ']' ;
        %% ---------------------------------------------------------------------
        %% match -> MATCH pattern
        %% pattern -> pattern_part_commalist
        %% pattern_part_commalist -> pattern_part
        %% pattern_part -> anonymous_pattern_part
        %% anonymous_pattern_part -> pattern_element
        %% pattern_element -> node_pattern pattern_element_chain_list
        %% node_pattern -> '(' ')'
        %% pattern_element_chain -> relationship_pattern node_pattern
        %% relationship_pattern -> '<' '-' relationship_detail '-' '>'
        %% relationship_detail -> '[' ']'
        "Match ()<-[]->() Return a",
        %% ---------------------------------------------------------------------
        %% Problem: relationship_detail
        %% ---------------------------------------------------------------------
        %% match -> MATCH pattern
        %% pattern -> pattern_part_commalist
        %% pattern_part_commalist -> pattern_part
        %% pattern_part -> anonymous_pattern_part
        %% anonymous_pattern_part -> pattern_element
        %% pattern_element -> node_pattern pattern_element_chain_list
        %% node_pattern -> '(' ')'
        %% pattern_element_chain -> relationship_pattern node_pattern
        %% relationship_pattern -> '<' '-' relationship_detail '-' '>'
        %% relationship_detail -> '[' relationship_types range_literal ']'
        "Match ()<-[:rtn *]->() Return a",
        %% ---------------------------------------------------------------------
        %% Problem: relationship_detail
        %% ---------------------------------------------------------------------
        %% match -> MATCH pattern
        %% pattern -> pattern_part_commalist
        %% pattern_part_commalist -> pattern_part
        %% pattern_part -> anonymous_pattern_part
        %% anonymous_pattern_part -> pattern_element
        %% pattern_element -> node_pattern pattern_element_chain_list
        %% node_pattern -> '(' ')'
        %% pattern_element_chain -> relationship_pattern node_pattern
        %% relationship_pattern -> '<' '-' relationship_detail '-' '>'
        %% relationship_detail -> '[' variable ']'
        "Match ()<-[vn]->() Return a",
        %% ---------------------------------------------------------------------
        %% Problem: relationship_detail
        %% ---------------------------------------------------------------------
        %% match -> MATCH pattern
        %% pattern -> pattern_part_commalist
        %% pattern_part_commalist -> pattern_part
        %% pattern_part -> anonymous_pattern_part
        %% anonymous_pattern_part -> pattern_element
        %% pattern_element -> node_pattern pattern_element_chain_list
        %% node_pattern -> '(' ')'
        %% pattern_element_chain -> relationship_pattern node_pattern
        %% relationship_pattern -> '<' '-' relationship_detail '-' '>'
        %% relationship_detail -> '[' variable properties ']'
        "Match ()<-[vn $1]->() Return a",
        %% ---------------------------------------------------------------------
        %% Problem: relationship_detail
        %% ---------------------------------------------------------------------
        %% match -> MATCH pattern
        %% pattern -> pattern_part_commalist
        %% pattern_part_commalist -> pattern_part
        %% pattern_part -> anonymous_pattern_part
        %% anonymous_pattern_part -> pattern_element
        %% pattern_element -> node_pattern pattern_element_chain_list
        %% node_pattern -> '(' ')'
        %% pattern_element_chain -> relationship_pattern node_pattern
        %% relationship_pattern -> '<' '-' relationship_detail '-' '>'
        %% relationship_detail -> '[' variable range_literal properties ']'
        "Match ()<-[vn * $1]->() Return a",
        %% ---------------------------------------------------------------------
        %% Problem: relationship_detail
        %% ---------------------------------------------------------------------
        %% match -> MATCH pattern
        %% pattern -> pattern_part_commalist
        %% pattern_part_commalist -> pattern_part
        %% pattern_part -> anonymous_pattern_part
        %% anonymous_pattern_part -> pattern_element
        %% pattern_element -> node_pattern pattern_element_chain_list
        %% node_pattern -> '(' ')'
        %% pattern_element_chain -> relationship_pattern node_pattern
        %% relationship_pattern -> '<' '-' relationship_detail '-' '>'
        %% relationship_detail -> '[' variable relationship_types properties ']'
        "Match ()<-[vn :rtn  $1]->() Return a",
        %% ---------------------------------------------------------------------
        %% Problem: relationship_detail
        %% ---------------------------------------------------------------------
        %% match -> MATCH pattern
        %% pattern -> pattern_part_commalist
        %% pattern_part_commalist -> pattern_part
        %% pattern_part -> anonymous_pattern_part
        %% anonymous_pattern_part -> pattern_element
        %% pattern_element -> node_pattern pattern_element_chain_list
        %% node_pattern -> '(' ')'
        %% pattern_element_chain -> relationship_pattern node_pattern
        %% relationship_pattern -> '<' '-' relationship_detail '-' '>'
        %% relationship_detail -> '[' variable relationship_types range_literal ']'
        "Match ()<-[vn :rtn  *]->() Return a",
        %% ---------------------------------------------------------------------
        %% Problem: sort_item
        %% ---------------------------------------------------------------------
        %% SortItem = Expression, [[SP], ((A,S,C,E,N,D,I,N,G) | (A,S,C) | (D,E,S,C,E,N,D,I,N,G) | (D,E,S,C))] ;
        %% ---------------------------------------------------------------------
        %% with -> WITH          return_body
        %% return_items -> '*'
        %% return_body -> return_items order
        %% order -> ORDER BY sort_item_commalist
        %% sort_item -> expression
        "Match (a) With * Order by 5 Return a"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% StandaloneCall = (C,A,L,L), SP, (ExplicitProcedureInvocation | ImplicitProcedureInvocation), [SP, (Y,I,E,L,D), SP, YieldItems] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(standaloneCall = Rule) ->
    ?CREATE_CODE_START,
    [{explicitProcedureInvocation, ExplicitProcedureInvocation}] = dets:lookup(?CODE_TEMPLATES, explicitProcedureInvocation),
    ExplicitProcedureInvocation_Length = length(ExplicitProcedureInvocation),
    [{implicitProcedureInvocation, ImplicitProcedureInvocation}] = dets:lookup(?CODE_TEMPLATES, implicitProcedureInvocation),
    ImplicitProcedureInvocation_Length = length(ImplicitProcedureInvocation),
    [{yieldItems, YieldItems}] = dets:lookup(?CODE_TEMPLATES, yieldItems),
    YieldItems_Length = length(YieldItems),

    Code = [
        lists:append([
            "Call",
            ?SP,
            case rand:uniform(4) rem 4 of
                1 ->
                    lists:nth(rand:uniform(ExplicitProcedureInvocation_Length), ExplicitProcedureInvocation);
                2 -> lists:append(
                    [
                        lists:nth(rand:uniform(ExplicitProcedureInvocation_Length), ExplicitProcedureInvocation),
                        ?SP,
                        "Yield",
                        ?SP,
                        lists:nth(rand:uniform(YieldItems_Length), YieldItems)
                    ]);
                3 ->
                    lists:nth(rand:uniform(ImplicitProcedureInvocation_Length), ImplicitProcedureInvocation);
                _ -> lists:append(
                    [
                        lists:nth(rand:uniform(ImplicitProcedureInvocation_Length), ImplicitProcedureInvocation),
                        ?SP,
                        "Yield",
                        ?SP,
                        lists:nth(rand:uniform(YieldItems_Length), YieldItems)
                    ])
            end
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% StringLiteral = ('"', { ANY - ('"' | '\') | EscapedChar }, '"')
%%               | ("'", { ANY - ("'" | '\') | EscapedChar }, "'") ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(stringLiteral = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "\\\"d_str\\\"",
        "\\\"d_str_1\\\"",
        "\\\"d_str_2\\\"",
        "'s_str'",
        "'s_str_1'",
        "'s_str_2'"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SymbolicName = UnescapedSymbolicName
%%              | EscapedSymbolicName
%%              | HexLetter
%%              | (C,O,U,N,T)
%%              | (F,I,L,T,E,R)
%%              | (E,X,T,R,A,C,T)
%%              | (A,N,Y)
%%              | (A,L,L)
%%              | (N,O,N,E)
%%              | (S,I,N,G,L,E)
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(symbolicName = Rule) ->
    ?CREATE_CODE_START,
    [{escapedSymbolicName, EscapedSymbolicName}] = dets:lookup(?CODE_TEMPLATES, escapedSymbolicName),
    [{hexLetter, HexLetter}] = dets:lookup(?CODE_TEMPLATES, hexLetter),
    [{unescapedSymbolicName, UnescapedSymbolicName}] = dets:lookup(?CODE_TEMPLATES, unescapedSymbolicName),

    Code = lists:append([
        UnescapedSymbolicName,
        EscapedSymbolicName,
%% wwe
%%        HexLetter,
%%        [
%%            "anY",
%%            "counT",
%%            "extracT",
%%            "filteR",
%%            "nonE",
%%            "singlE"
%%        ]
        HexLetter
    ]),
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unescapedSymbolicName = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "usn1_SYMB_NAME_usn",
        "usn2_SYMB_NAME_usn",
        "_usn3_SYMB_NAME_usn",
        "_usn4_SYMB_NAME_usn",
        "@usn5_SYMB_NAME_usn",
        "@usn6_SYMB_NAME_usn",
        "#usn7_SYMB_NAME_usn",
        "#usn8_SYMB_NAME_usn",
        "usna_SYMB_NAME_usn",
        "usnb_SYMB_NAME_usn",
        "_usnc_SYMB_NAME_usn",
        "_usnd_SYMB_NAME_usn",
        "@usne_SYMB_NAME_usn",
        "@usnf_SYMB_NAME_usn",
        "#usng_SYMB_NAME_usn"
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Union = ((U,N,I,O,N), SP, (A,L,L), [SP], SingleQuery)
%%       | ((U,N,I,O,N), [SP], SingleQuery) ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% Union = ((U,N,I,O,N), SP, (A,L,L), SP, SingleQuery)
%%       | ((U,N,I,O,N), SP, SingleQuery) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(union = Rule) ->
    ?CREATE_CODE_START,
    [{singleQuery, SingleQuery}] = dets:lookup(?CODE_TEMPLATES, singleQuery),

    Code = [
        lists:append([
            "Union",
            case rand:uniform(2) rem 2 of
                1 -> ?SP ++ "All";
                _ -> []
            end,
            ?SP,
            SQ
        ])
        || SQ <- SingleQuery
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Unwind = (U,N,W,I,N,D), [SP], Expression, SP, (A,S), SP, Variable ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% Unwind = (U,N,W,I,N,D), SP, Expression, SP, (A,S), SP, Variable ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unwind = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
        lists:append([
            "Unwind",
            ?SP,
            lists:nth(rand:uniform(Expression_Length), Expression),
            ?SP,
            "As",
            ?SP,
            lists:nth(rand:uniform(Variable_Length), Variable)
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UpdatingClause = Create
%%                | Merge
%%                | Delete
%%                | Set
%%                | Remove
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(updatingClause = Rule) ->
    ?CREATE_CODE_START,
    [{create, Create}] = dets:lookup(?CODE_TEMPLATES, create),
    Create_Length = length(Create),
    [{delete, Delete}] = dets:lookup(?CODE_TEMPLATES, delete),
    Delete_Length = length(Delete),
    [{merge, Merge}] = dets:lookup(?CODE_TEMPLATES, merge),
    Merge_Length = length(Merge),
    [{remove, Remove}] = dets:lookup(?CODE_TEMPLATES, remove),
    Remove_Length = length(Remove),
    [{set, Set}] = dets:lookup(?CODE_TEMPLATES, set),
    Set_Length = length(Set),

    Code = [
        case rand:uniform(5) rem 5 of
            1 -> lists:nth(rand:uniform(Create_Length), Create);
            2 -> lists:nth(rand:uniform(Merge_Length), Merge);
            3 -> lists:nth(rand:uniform(Delete_Length), Delete);
            4 -> lists:nth(rand:uniform(Set_Length), Set);
            _ -> lists:nth(rand:uniform(Remove_Length), Remove)
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% { UpdatingClause, [SP] }
%% -----------------------------------------------------------------------------
%% wwe ???
%% { UpdatingClause, SP }
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(updatingClauseList = Rule) ->
    ?CREATE_CODE_START,
    [{updatingClause, UpdatingClause}] = dets:lookup(?CODE_TEMPLATES, updatingClause),
    UpdatingClause_Length = length(UpdatingClause),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:append(
                [
                    lists:nth(rand:uniform(UpdatingClause_Length), UpdatingClause),
                    ?SP,
                    lists:nth(rand:uniform(UpdatingClause_Length), UpdatingClause),
                    ?SP,
                    lists:nth(rand:uniform(UpdatingClause_Length), UpdatingClause),
                    ?SP
                ]);
            2 -> lists:append(
                [
                    lists:nth(rand:uniform(UpdatingClause_Length), UpdatingClause),
                    ?SP,
                    lists:nth(rand:uniform(UpdatingClause_Length), UpdatingClause),
                    ?SP
                ]);
            _ ->
                lists:nth(rand:uniform(UpdatingClause_Length), UpdatingClause) ++ ?SP
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, false),
    store_code(updatingPart, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UpdatingEnd = UpdatingStartClause, { [SP], UpdatingClause }, [[SP], Return] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(updatingEnd = Rule) ->
    ?CREATE_CODE_START,
    [{return, Return}] = dets:lookup(?CODE_TEMPLATES, return),
    Return_Length = length(Return),
    [{updatingClauseList, UpdatingClauseList}] = dets:lookup(?CODE_TEMPLATES, updatingClauseList),
    UpdatingClauseList_Length = length(UpdatingClauseList),
    [{updatingStartClause, UpdatingStartClause}] = dets:lookup(?CODE_TEMPLATES, updatingStartClause),
    UpdatingStartClause_Length = length(UpdatingStartClause),

    Code = [
        lists:append([
            lists:nth(rand:uniform(UpdatingStartClause_Length), UpdatingStartClause),
            case rand:uniform(2) rem 2 of
                1 ->
                    ?SP ++ lists:nth(rand:uniform(UpdatingClauseList_Length), UpdatingClauseList);
                _ -> []
            end,
            case rand:uniform(2) rem 2 of
                1 -> ?SP ++ lists:nth(rand:uniform(Return_Length), Return);
                _ -> []
            end
        ])
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UpdatingStartClause = Create
%%                     | Merge
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(updatingStartClause = Rule) ->
    ?CREATE_CODE_START,
    [{create, Create}] = dets:lookup(?CODE_TEMPLATES, create),
    Create_Length = length(Create),
    [{merge, Merge}] = dets:lookup(?CODE_TEMPLATES, merge),
    Merge_Length = length(Merge),

    Code = [
        case rand:uniform(2) rem 2 of
            1 -> lists:nth(rand:uniform(Create_Length), Create);
            _ -> lists:nth(rand:uniform(Merge_Length), Merge)
        end
        || _ <- lists:seq(1, ?MAX_CYPHER * 2)
    ],
    store_code(Rule, Code, ?MAX_CYPHER, true),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Variable = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(variable = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = [re:replace(SN, "_SYMB_NAME_", "_VARIABLE_", [{return, list}]) || SN <- SymbolicName],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Where = (W,H,E,R,E), SP, Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(where = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
        lists:append([
            "Where",
            ?SP,
            lists:nth(rand:uniform(Expression_Length), Expression)
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% With = (W,I,T,H), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody, [[SP], Where] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(with = Rule) ->
    ?CREATE_CODE_START,
    [{returnBody, ReturnBody}] = dets:lookup(?CODE_TEMPLATES, returnBody),
    ReturnBody_Length = length(ReturnBody),
    [{where, Where}] = dets:lookup(?CODE_TEMPLATES, where),
    Where_Length = length(Where),

    Code = [
        lists:append([
            "With",
            case rand:uniform(2) rem 2 of
                1 -> " Distinct";
                _ -> []
            end,
            ?SP,
            lists:nth(rand:uniform(ReturnBody_Length), ReturnBody),
            case rand:uniform(2) rem 2 of
                1 -> ?SP ++ lists:nth(rand:uniform(Where_Length), Where);
                _ -> []
            end
        ])
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% YieldItem = [ProcedureResultField, SP, (A,S), SP], Variable ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(yieldItem = Rule) ->
    ?CREATE_CODE_START,
    [{procedureResultField, ProcedureResultField}] = dets:lookup(?CODE_TEMPLATES, procedureResultField),
    ProcedureResultField_Length = length(ProcedureResultField),
    [{variable, Variable}] = dets:lookup(?CODE_TEMPLATES, variable),
    Variable_Length = length(Variable),

    Code = [
        case rand:uniform(2) rem 2 of
            1 -> lists:append(
                [
                    lists:nth(rand:uniform(ProcedureResultField_Length), ProcedureResultField),
                    ?SP,
                    "As",
                    ?SP,
                    lists:nth(rand:uniform(Variable_Length), Variable)
                ]);
            _ -> lists:nth(rand:uniform(Variable_Length), Variable)
        end
        || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
    ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% YieldItems = (YieldItem, { [SP], ',', [SP], YieldItem })
%%            | '-'
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(yieldItems = Rule) ->
    ?CREATE_CODE_START,
    [{yieldItem, YieldItem}] = dets:lookup(?CODE_TEMPLATES, yieldItem),
    YieldItem_Length = length(YieldItem),

    Code = ["-"] ++
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:append(
                    [
                        lists:nth(rand:uniform(YieldItem_Length), YieldItem),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT,
                        lists:nth(rand:uniform(YieldItem_Length), YieldItem),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT, lists:nth(rand:uniform(YieldItem_Length), YieldItem)
                    ]);
                2 -> lists:append(
                    [
                        lists:nth(rand:uniform(YieldItem_Length), YieldItem),
                        ?SP_OPT,
                        ",",
                        ?SP_OPT, lists:nth(rand:uniform(YieldItem_Length), YieldItem)
                    ]);
                _ -> lists:nth(rand:uniform(YieldItem_Length), YieldItem)
            end
            || _ <- lists:seq(1, ?MAX_BASIC_RULE * 2)
        ],
    store_code(Rule, Code, ?MAX_BASIC_RULE, false),
    ?CREATE_CODE_END.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AddOrSubtractExpression = MultiplyDivideModuloExpression, { ([SP], '+', [SP], MultiplyDivideModuloExpression) | ([SP], '-', [SP], MultiplyDivideModuloExpression) } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(addOrSubtractExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{multiplyDivideModuloExpression, MultiplyDivideModuloExpression}] = dets:lookup(?CODE_TEMPLATES, multiplyDivideModuloExpression),
    MultiplyDivideModuloExpression_Length = length(MultiplyDivideModuloExpression),

    Code = [
            lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression) ++
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(5) rem 5 of
                         1 -> lists:append(
                             [
                                 ?SP_OPT,
                                 case rand:uniform(2) rem 2 of
                                     1 -> "+";
                                     _ -> "-"
                                 end,
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression),
                                 ?SP_OPT,
                                 case rand:uniform(2) rem 2 of
                                     1 -> "+";
                                     _ -> "-"
                                 end,
                                 ?SP_OPT, lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression)
                             ]);
                         _ -> lists:append(
                             [
                                 ?SP_OPT,
                                 case rand:uniform(2) rem 2 of
                                     1 -> "+";
                                     _ -> "-"
                                 end,
                                 ?SP,
                                 lists:nth(rand:uniform(MultiplyDivideModuloExpression_Length), MultiplyDivideModuloExpression)
                             ])
                     end
            end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    store_code(notExpression, Code, Max, false),
    store_code(comparisonExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AndExpression = NotExpression, { SP, (A,N,D), SP, NotExpression } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(andExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{notExpression, NotExpression}] = dets:lookup(?CODE_TEMPLATES, notExpression),
    NotExpression_Length = length(NotExpression),

    Code = [
            lists:nth(rand:uniform(NotExpression_Length), NotExpression) ++
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(5) rem 5 of
                         1 -> lists:append(
                             [
                                 ?SP,
                                 "And",
                                 ?SP, lists:nth(rand:uniform(NotExpression_Length), NotExpression),
                                 ?SP,
                                 "And",
                                 ?SP,
                                 lists:nth(rand:uniform(NotExpression_Length), NotExpression)
                             ]);
                         _ -> lists:append(
                             [
                                 ?SP,
                                 "And",
                                 ?SP,
                                 lists:nth(rand:uniform(NotExpression_Length), NotExpression)
                             ])
                     end
            end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ComparisonExpression = AddOrSubtractExpression, { [SP], PartialComparisonExpression } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(comparisonExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{addOrSubtractExpression, AddOrSubtractExpression}] = dets:lookup(?CODE_TEMPLATES, addOrSubtractExpression),
    AddOrSubtractExpression_Length = length(AddOrSubtractExpression),
    [{partialComparisonExpression, PartialComparisonExpression}] = dets:lookup(?CODE_TEMPLATES, partialComparisonExpression),
    PartialComparisonExpression_Length = length(PartialComparisonExpression),

    Code = [
            lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression) ++
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(5) rem 5 of
                         1 -> lists:append(
                             [
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression),
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression)
                             ]);
                         _ -> ?SP_OPT ++
                         lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression)
                     end
            end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    store_code(notExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ExpressionCommalist = Expression, [SP], { ',', [SP], Expression, [SP] } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(expressionCommalist = Rule, Max) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
        case rand:uniform(3) rem 3 of
            1 -> lists:append(
                [
                    lists:nth(rand:uniform(Expression_Length), Expression),
                    ?SP_OPT,
                    ",",
                    ?SP_OPT,
                    lists:nth(rand:uniform(Expression_Length), Expression),
                    ?SP_OPT,
                    ",",
                    ?SP_OPT,
                    lists:nth(rand:uniform(Expression_Length), Expression)
                ]);
            2 -> lists:append(
                [
                    lists:nth(rand:uniform(Expression_Length), Expression),
                    ?SP_OPT,
                    ",",
                    ?SP_OPT,
                    lists:nth(rand:uniform(Expression_Length), Expression)
                ]);
            _ -> lists:nth(rand:uniform(Expression_Length), Expression)
        end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% MultiplyDivideModuloExpression = PowerOfExpression, { ([SP], '*', [SP], PowerOfExpression) | ([SP], '/', [SP], PowerOfExpression) | ([SP], '%', [SP], PowerOfExpression) } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(multiplyDivideModuloExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{powerOfExpression, PowerOfExpression}] = dets:lookup(?CODE_TEMPLATES, powerOfExpression),
    PowerOfExpression_Length = length(PowerOfExpression),

    Code = [
            lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression) ++
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(5) rem 5 of
                         1 -> lists:append(
                             [
                                 ?SP_OPT,
                                 case rand:uniform(3) rem 3 of
                                     1 -> "*";
                                     2 -> "/";
                                     _ -> "%"
                                 end,
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression),
                                 ?SP_OPT,
                                 case rand:uniform(3) rem 3 of
                                     1 -> "*";
                                     2 -> "/";
                                     _ -> "%"
                                 end,
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression)
                             ]);
                         _ -> lists:append(
                             [
                                 ?SP_OPT,
                                 case rand:uniform(3) rem 3 of
                                     1 -> "*";
                                     2 -> "/";
                                     _ -> "%"
                                 end,
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(PowerOfExpression_Length), PowerOfExpression)
                             ])
                     end

            end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    store_code(notExpression, Code, Max, false),
    store_code(comparisonExpression, Code, Max, false),
    store_code(addOrSubtractExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NotExpression = { (N,O,T), [SP] }, ComparisonExpression ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% NotExpression = { (N,O,T), SP }, ComparisonExpression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(notExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{comparisonExpression, ComparisonExpression}] = dets:lookup(?CODE_TEMPLATES, comparisonExpression),
    ComparisonExpression_Length = length(ComparisonExpression),

    Code = [
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(5) rem 5 of
                         1 -> lists:append(["Not", ?SP, "Not", ?SP]);
                         _ -> "Not" ++ ?SP
                     end
            end ++
            lists:nth(rand:uniform(ComparisonExpression_Length), ComparisonExpression)
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% OrExpression = XorExpression, { SP, (O,R), SP, XorExpression } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(orExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{xorExpression, XorExpression}] = dets:lookup(?CODE_TEMPLATES, xorExpression),
    XorExpression_Length = length(XorExpression),

    Code = [
            lists:nth(rand:uniform(XorExpression_Length), XorExpression) ++
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(5) rem 5 of
                         1 -> lists:append(
                             [
                                 ?SP,
                                 "Or",
                                 ?SP,
                                 lists:nth(rand:uniform(XorExpression_Length), XorExpression),
                                 ?SP,
                                 "Or",
                                 ?SP,
                                 lists:nth(rand:uniform(XorExpression_Length), XorExpression)
                             ]);
                         _ -> lists:append(
                             [
                                 ?SP,
                                 "Or",
                                 ?SP,
                                 lists:nth(rand:uniform(XorExpression_Length), XorExpression)
                             ])
                     end
            end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%create_code(parenthesizedExpression = Rule, Max) ->
%%    ?CREATE_CODE_START,
%%    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
%%    Expression_Length = length(Expression),
%%
%%    Code = [
%%        lists:append([
%%            "(",
%%            ?SP_OPT,
%%            lists:nth(rand:uniform(Expression_Length), Expression),
%%            ?SP_OPT,
%%            ")"
%%        ])
%%        || _ <- lists:seq(1, Max * 2)
%%    ],
%%    store_code(Rule, Code, Max, false),
%%    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PartialComparisonExpression = ('=', [SP], AddOrSubtractExpression)
%%                             | ('<>', [SP], AddOrSubtractExpression)
%%                             | ('<', [SP], AddOrSubtractExpression)
%%                             | ('>', [SP], AddOrSubtractExpression)
%%                             | ('<=', [SP], AddOrSubtractExpression)
%%                             | ('>=', [SP], AddOrSubtractExpression) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(partialComparisonExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{addOrSubtractExpression, AddOrSubtractExpression}] = dets:lookup(?CODE_TEMPLATES, addOrSubtractExpression),
    AddOrSubtractExpression_Length = length(AddOrSubtractExpression),

    Code = [
        lists:append(
            [case rand:uniform(6) rem 6 of
                 1 -> "=";
                 2 -> "<>";
                 3 -> "<";
                 4 -> ">";
                 5 -> "<=";
                 _ -> ">="
             end,
                ?SP,
                lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression)
            ])
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PowerOfExpression = UnaryAddOrSubtractExpression, { [SP], '^', [SP], UnaryAddOrSubtractExpression } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(powerOfExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{unaryAddOrSubtractExpression, UnaryAddOrSubtractExpression}] = dets:lookup(?CODE_TEMPLATES, unaryAddOrSubtractExpression),
    UnaryAddOrSubtractExpression_Length = length(UnaryAddOrSubtractExpression),

    Code = [
            lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression) ++
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(5) rem 5 of
                         1 -> lists:append(
                             [
                                 ?SP_OPT,
                                 "^",
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression),
                                 ?SP_OPT,
                                 "^",
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression)
                             ]);
                         _ -> lists:append(
                             [
                                 ?SP_OPT,
                                 "^",
                                 ?SP_OPT,
                                 lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression)
                             ])
                     end
            end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    store_code(notExpression, Code, Max, false),
    store_code(comparisonExpression, Code, Max, false),
    store_code(addOrSubtractExpression, Code, Max, false),
    store_code(multiplyDivideModuloExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PropertyOrLabelsExpression = Atom, { PropertyLookup | NodeLabels } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(propertyOrLabelsExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{atom, Atom}] = dets:lookup(?CODE_TEMPLATES, atom),
    Atom_Length = length(Atom),
    [{nodeLabels, NodeLabels}] = dets:lookup(?CODE_TEMPLATES, nodeLabels),
    NodeLabels_Length = length(NodeLabels),
    [{propertyLookup, PropertyLookup}] = dets:lookup(?CODE_TEMPLATES, propertyLookup),
    PropertyLookup_Length = length(PropertyLookup),

    Code = [
            lists:nth(rand:uniform(Atom_Length), Atom) ++
            case rand:uniform(7) rem 7 of
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
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    store_code(notExpression, Code, Max, false),
    store_code(comparisonExpression, Code, Max, false),
    store_code(addOrSubtractExpression, Code, Max, false),
    store_code(multiplyDivideModuloExpression, Code, Max, false),
    store_code(powerOfExpression, Code, Max, false),
    store_code(unaryAddOrSubtractExpression, Code, Max, false),
    store_code(stringListNullOperatorExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% StringListNullOperatorExpression = PropertyOrLabelsExpression, { ([SP], '[', Expression, ']')
%%                                  | ([SP], '[', [Expression], '..', [Expression], ']')
%%                                  | ((([SP], '=~')
%%                                  | (SP, (I,N))
%%                                  | (SP, (S,T,A,R,T,S), SP, (W,I,T,H))
%%                                  | (SP, (E,N,D,S), SP, (W,I,T,H))
%%                                  | (SP, (C,O,N,T,A,I,N,S))), [SP], PropertyOrLabelsExpression)
%%                                  | (SP, (I,S), SP, (N,U,L,L))
%%                                  | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L)) } ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% StringListNullOperatorExpression = PropertyOrLabelsExpression, { ([SP], '[', Expression, ']')
%%                                  | ([SP], '[', [Expression], '..', [Expression], ']')
%%                                  | ((([SP], '=~')
%%                                  | (SP, (I,N))
%%                                  | (SP, (S,T,A,R,T,S), SP, (W,I,T,H))
%%                                  | (SP, (E,N,D,S), SP, (W,I,T,H))
%%                                  | (SP, (C,O,N,T,A,I,N,S))), SP, PropertyOrLabelsExpression)
%%                                  | (SP, (I,S), SP, (N,U,L,L))
%%                                  | (SP, (I,S), SP, (N,O,T), SP, (N,U,L,L)) } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(stringListNullOperatorExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{propertyOrLabelsExpression, PropertyOrLabelsExpression}] = dets:lookup(?CODE_TEMPLATES, propertyOrLabelsExpression),
    PropertyOrLabelsExpression_Length = length(PropertyOrLabelsExpression),

    Code = [
            lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
            case rand:uniform(10) rem 10 of
                1 -> case rand:uniform(3) rem 3 of
                         1 -> lists:append(
                             [
                                 ?SP,
                                 "Is",
                                 ?SP,
                                 "Null"
                             ]);
                         2 -> lists:append(
                             [
                                 ?SP,
                                 "Is",
                                 ?SP,
                                 "Not",
                                 ?SP,
                                 "Null"
                             ]);
                         _ -> []
                     end;
                _ -> case rand:uniform(2) rem 2 of
                         1 -> case rand:uniform(5) rem 5 of
                                  1 -> lists:append(
                                      [
                                          ?SP_OPT,
                                          "[",
                                          lists:nth(rand:uniform(Expression_Length), Expression),
                                          "]"
                                      ]);
                                  2 -> lists:append(
                                      [
                                          ?SP_OPT,
                                          "[",
                                          "..",
                                          "]"
                                      ]);
                                  3 -> lists:append(
                                      [
                                          ?SP_OPT,
                                          "[",
                                          "..",
                                          lists:nth(rand:uniform(Expression_Length), Expression),
                                          "]"
                                      ]);
                                  4 -> lists:append(
                                      [
                                          ?SP_OPT,
                                          "[",
                                          lists:nth(rand:uniform(Expression_Length), Expression),
                                          "..",
                                          "]"
                                      ]);
                                  _ -> lists:append(
                                      [
                                          ?SP_OPT,
                                          "[",
                                          lists:nth(rand:uniform(Expression_Length), Expression),
                                          "..",
                                          lists:nth(rand:uniform(Expression_Length), Expression),
                                          "]"
                                      ])
                              end;
                         _ -> lists:append(
                             [
                                 ?SP,
                                 case rand:uniform(5) rem 5 of
                                     1 -> "=~";
                                     2 -> "In";
                                     3 -> "Starts With";
                                     4 -> "Ends With";
                                     _ -> "Contains"
                                 end,
                                 ?SP,
                                 lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression)
                             ])
                     end
            end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    store_code(notExpression, Code, Max, false),
    store_code(comparisonExpression, Code, Max, false),
    store_code(addOrSubtractExpression, Code, Max, false),
    store_code(multiplyDivideModuloExpression, Code, Max, false),
    store_code(powerOfExpression, Code, Max, false),
    store_code(unaryAddOrSubtractExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UnaryAddOrSubtractExpression = { ('+' | '-'), SP }, StringListNullOperatorExpression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unaryAddOrSubtractExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{stringListNullOperatorExpression, StringListNullOperatorExpression}] = dets:lookup(?CODE_TEMPLATES, stringListNullOperatorExpression),
    StringListNullOperatorExpression_Length = length(StringListNullOperatorExpression),

    Code = [
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(6) rem 6 of
                         1 -> lists:append(["+", ?SP, "+", ?SP]);
                         2 -> lists:append(["+", ?SP, "-", ?SP]);
                         3 -> "+" ++ ?SP;
                         4 -> lists:append(["-", ?SP, "+", ?SP]);
                         5 -> lists:append(["-", ?SP, "-", ?SP]);
                         _ -> "-" ++ ?SP
                     end
            end ++
            lists:nth(rand:uniform(StringListNullOperatorExpression_Length), StringListNullOperatorExpression)
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    store_code(notExpression, Code, Max, false),
    store_code(comparisonExpression, Code, Max, false),
    store_code(addOrSubtractExpression, Code, Max, false),
    store_code(multiplyDivideModuloExpression, Code, Max, false),
    store_code(powerOfExpression, Code, Max, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% XorExpression = AndExpression, { SP, (X,O,R), SP, AndExpression } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(xorExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{andExpression, AndExpression}] = dets:lookup(?CODE_TEMPLATES, andExpression),
    AndExpression_Length = length(AndExpression),

    Code = [
            lists:nth(rand:uniform(AndExpression_Length), AndExpression) ++
            case rand:uniform(20) rem 20 of
                1 -> [];
                _ -> case rand:uniform(5) rem 5 of
                         1 -> lists:append(
                             [
                                 ?SP,
                                 "Xor",
                                 ?SP,
                                 lists:nth(rand:uniform(AndExpression_Length), AndExpression),
                                 ?SP,
                                 "Xor",
                                 ?SP,
                                 lists:nth(rand:uniform(AndExpression_Length), AndExpression)
                             ]);
                         _ -> lists:append(
                             [
                                 ?SP,
                                 "Xor",
                                 ?SP,
                                 lists:nth(rand:uniform(AndExpression_Length), AndExpression)
                             ])
                     end
            end
        || _ <- lists:seq(1, Max * 2)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    ?CREATE_CODE_END.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code of rules ...Expression.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code_expression(Max) ->
    create_code(propertyOrLabelsExpression, Max),
    create_code(stringListNullOperatorExpression, Max),
    create_code(unaryAddOrSubtractExpression, Max),
    create_code(powerOfExpression, Max),
    create_code(multiplyDivideModuloExpression, Max),
    create_code(addOrSubtractExpression, Max),
    create_code(partialComparisonExpression, Max),
    create_code(comparisonExpression, Max),
    create_code(notExpression, Max),
    create_code(andExpression, Max),
    create_code(xorExpression, Max),
    create_code(orExpression, Max),

    create_code(expressionCommalist, Max).
%% wwe
%%    create_code(parenthesizedExpression, Max).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating Common Test data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_Type, _CompactedDetailed, []) ->
    ok;
file_create_ct_all(Type, CompactedDetailed, [Rule | Rules]) ->
    file_create_ct(Type, CompactedDetailed, Rule),
    file_create_ct_all(Type, CompactedDetailed, Rules).

file_create_ct(Type, CompactedDetailed, Rule) ->
    [{Rule, Code}] = dets:lookup(?CODE_TEMPLATES, Rule),

    CodeLength = length(Code),
    RuleString = atom_to_list(Rule),

    FileName = lists:append([Type, "_", CompactedDetailed, "_", RuleString, "_SUITE"]),
    {ok, File, _} = file:path_open([?PATH_CT], FileName ++ ".erl", [write]),

    erlang:display(io:format("final common tests ===> ~12.. B file_name: ~s ", [CodeLength, FileName ++ ".erl"])),

    {{Current_Year, Current_Month, Current_Day}, _} = calendar:local_time(),

    io:format(File, "~s~n", ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n", [lists:append(["%%% File        : ", FileName, ".erl"])]),
    io:format(File, "~s~n", [lists:append(["%%% Description : Test Suite for rule: ", RuleString, "."])]),
    io:format(File, "~s~n", ["%%%"]),
    io:format(File, "~s~n", ["%%% Created     : " ++ lists:flatten(io_lib:format("~2..0w.~2..0w.~4..0w", [Current_Day, Current_Month, Current_Year]))]),
    io:format(File, "~s~n", ["%%%-------------------------------------------------------------------"]),
    io:format(File, "~s~n", [lists:append(["-module(", FileName, ")."])]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["-export(["]),
    io:format(File, "~s~n", ["    all/0,"]),
    io:format(File, "~s~n", ["    end_per_suite/1,"]),
    io:format(File, "~s~n", ["    init_per_suite/1,"]),
    io:format(File, "~s~n", ["    suite/0,"]),

    case CompactedDetailed of
        "compacted" ->
            io:format(File, "~s~n", [lists:append(["    test_compacted/1"])]);
        _ -> file_write_ct_export(1, File, CodeLength, Code)
    end,

    io:format(File, "~s~n", ["])."]),
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
    io:format(File, "~s~n", [lists:append(["        {timetrap, {minutes, ", integer_to_list(?TIMETRAP_MINUTES), "}}"])]),
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
    io:format(File, "~s~n", ["    ["]),

    case CompactedDetailed of
        "compacted" ->
            io:format(File, "~s~n", [lists:append(["        test_compacted"])]);
        _ -> file_write_ct_all(1, File, CodeLength, Code)
    end,

    io:format(File, "~s~n", ["    ]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", ["%% TEST CASES"]),
    io:format(File, "~s~n", ["%%--------------------------------------------------------------------"]),
    io:format(File, "~s~n", [""]),

    case CompactedDetailed of
        "compacted" ->
            io:format(File, "~s~n", [lists:append(["test_compacted(_Config) ->"])]);
        _ -> ok
    end,

    file_write_ct(1, Type, CompactedDetailed, File, Code).

file_write_ct(_Current, _Type, CompactedDetailed, File, []) ->
    case CompactedDetailed of
        "compacted" -> io:format(File, "~s~n", ["    ok."]);
        _ -> ok
    end,
    file:close(File);
file_write_ct(Current, Type, CompactedDetailed, File, [H | T]) ->
    case CompactedDetailed of
        "compacted" -> io:format(File, "~s~n", [lists:append([
            "    ",
            case Type of
                "performance" -> "{ok, _} = ocparse:source_to_pt";
                _ -> "ocparse_test:common_test_source"
            end,
            "(\"",
            H,
            "\"),"
        ])]);
        _ ->
            io:format(File, "~s~n", [lists:append(["test_", integer_to_list(Current), "_", integer_to_list(length(H)), "(_Config) ->"])]),
            io:format(File, "~s~n", [lists:append([
                "    ",
                case Type of
                    "performance" -> "{ok, _} = ocparse:source_to_pt";
                    _ -> "ocparse_test:common_test_source"
                end,
                "(\"",
                H,
                "\")."
            ])]),
            io:format(File, "~s~n", [""])
    end,
    file_write_ct(Current + 1, Type, CompactedDetailed, File, T).

file_write_ct_all(Current, File, Target, [H])
    when Current == Target ->
    io:format(File, "~s~n", [lists:append(["        test_", integer_to_list(Current), "_", integer_to_list(length(H))])]);
file_write_ct_all(Current, File, Target, [H | T]) ->
    io:format(File, "~s~n", [lists:append(["        test_", integer_to_list(Current), "_", integer_to_list(length(H)), ","])]),
    file_write_ct_all(Current + 1, File, Target, T).

file_write_ct_export(Current, File, Target, [H])
    when Current == Target ->
    io:format(File, "~s~n", [lists:append(["    test_", integer_to_list(Current), "_", integer_to_list(length(H)), "/1"])]);
file_write_ct_export(Current, File, Target, [H | T]) ->
    io:format(File, "~s~n", [lists:append(["    test_", integer_to_list(Current), "_", integer_to_list(length(H)), "/1,"])]),
    file_write_ct_export(Current + 1, File, Target, T).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating EUnit data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_eunit_all(_Type, []) ->
    ok;
file_create_eunit_all(Type, [Rule | Rules]) ->
    file_create_eunit(Type, Rule),
    file_create_eunit_all(Type, Rules).

file_create_eunit(Type, Rule) ->
    [{Rule, Code}] = dets:lookup(?CODE_TEMPLATES, Rule),

    RuleStrimg = atom_to_list(Rule),

    FileName = lists:append([Type, "_", RuleStrimg, ".tst"]),
    {ok, File, _} = file:path_open([?PATH_EUNIT], FileName, [write]),

    erlang:display(io:format("final eunit  tests ===> ~12.. B file_name: ~s ", [length(Code), FileName])),

    io:format(File, "~s~n", ["%%-*- mode: erlang -*-"]),
    io:format(File, "~s~n", ["%%-*- coding: utf-8 -*-"]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["% Manual testing."]),
    io:format(File, "~s~n", ["[{tests, []}]."]),
    io:format(File, "~s~n", [""]),
    io:format(File, "~s~n", ["%%"]),
    io:format(File, "~s~n", ["%% Tests for rule: " ++ RuleStrimg]),
    io:format(File, "~s~n", ["%%"]),
    io:format(File, "~s~n", [""]),

    file_write_eunit(Type, File, Code).

file_write_eunit(_Type, File, []) ->
    file:close(File);
file_write_eunit(Type, File, [H | T]) ->
    io:format(File, "~s~n", [lists:append(["\"", H, "\"."])]),
    file_write_eunit(Type, File, T).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Store generated code in helper table.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_code(Rule, Code, Max, Strict) ->
%   erlang:display(io:format("store Code         ===> ~12.. B rule: ~s ", [length(Code), atom_to_list(Rule)])),

    case Max == 0 of
        true ->
%           erlang:display(io:format("store CodeNew      ===> ~12.. B rule: ~s ", [0, atom_to_list(Rule)])),
            ?debugFmt("~ncode lines         ===> ~12.. B rule: ~s ~n", [0, atom_to_list(Rule)]);
        _ ->
            CodeUnique = ordsets:to_list(ordsets:from_list(Code)),
            CodeUnique_Length = length(CodeUnique),
            CodeUniqueSorted = lists:sort(?F_RANDOM, CodeUnique),
            CodeUniqueLimited = case CodeUnique_Length > Max of
                                    true ->
                                        lists:sublist(CodeUniqueSorted, 1, Max);
                                    _ -> CodeUnique
                                end,
            CodeTotal = case dets:lookup(?CODE_TEMPLATES, Rule) of
                            [{Rule, CodeOld}] ->
                                lists:sort(?F_RANDOM, ordsets:to_list(ordsets:from_list(lists:append([CodeOld, CodeUniqueLimited]))));
                            _ -> CodeUniqueLimited
                        end,
            CodeTotal_Length = length(CodeTotal),
            CodeNew = case Strict andalso CodeTotal_Length > Max of
                          true ->
                              [lists:nth(rand:uniform(CodeTotal_Length), CodeTotal) || _ <- lists:seq(1, Max)];
                          _ -> CodeTotal
                      end,
            dets:insert(?CODE_TEMPLATES, {Rule, CodeNew}),
%           erlang:display(io:format("store CodeNew      ===> ~12.. B rule: ~s ", [length(CodeNew), atom_to_list(Rule)])),
            ?debugFmt("~ncode lines         ===> ~12.. B rule: ~s ~n", [length(CodeNew), atom_to_list(Rule)])
    end.
