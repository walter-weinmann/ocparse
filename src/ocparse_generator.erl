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
    create,
    cypher,
    delete,
    inQueryCall,
    match,
    merge,
    query,
    regularQuery,
    remove,
    return,
    set,
    special,
    standAloneCall,
    statement,
    unwind,
    with
]).

-define(ALL_CLAUSE_EUNIT, [
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
-define(LEFT_ARROW_HEAD, "<").

-define(MAX_CLAUSE, 2000).
-define(MAX_CYPHER, 2000).
-define(MAX_QUERY, 2000).
-define(MAX_RULE_ATOM, 100).                 % cumulative
-define(MAX_RULE_EXPRESSION, 100).           % cumulative
-define(MAX_RULE_OTHERS, 2000).              % max of (MAX_CLAUSE, MAX_CYPHER, MAX_QUERY, MAX_STATEMENT))
-define(MAX_STATEMENT, 2000).

-define(PATH_CT, "test").
-define(PATH_EUNIT, "test").

-define(RIGHT_ARROW_HEAD, ">").
-define(SP, " ").
-define(SP_OPT, []).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Generate Test Data.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generate() ->
    file:delete(?CODE_TEMPLATES),
    dets:open_file(?CODE_TEMPLATES, [
        {auto_save, 0},
        {repair, false}
    ]),
    dets:delete_all_objects(?CODE_TEMPLATES),

    create_code(),

    % performance common tests
    ok = file_create_ct_all(performance, ?ALL_CLAUSE_CT_PERFORMANCE),

    % reliability common tests
    ok = file_create_ct_all(reliability, ?ALL_CLAUSE_CT_RELIABILITY),

    % reliability eunit tests
    ok = file_create_eunit_all(?ALL_CLAUSE_EUNIT),

    dets:close(?CODE_TEMPLATES).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating code base.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code() ->

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 1
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(atomCount),
    create_code(booleanLiteral),
    create_code(decimalInteger),
    create_code(escapedSymbolicName),
    create_code(exponentDecimalReal),
    create_code(hexInteger),
    create_code(hexLetter),
    create_code(literalNull),
    create_code(octalInteger),
    create_code(regularDecimalReal),
    % wwe ???
    % currently not supported
    % create_code(reservedWord),
    create_code(stringLiteral),
    % wwe ???
    % currently not supported
    % create_code(symbolicName),
    create_code(unescapedSymbolicName),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 2
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(functionName),
    create_code(parameter),
    create_code(procedureName),
    create_code(procedureResultField),
    create_code(rangeLiteral),
    create_code(schemaName),
    create_code(variable),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 3
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(implicitProcedureInvocation),
    create_code(labelName),
    create_code(propertyKeyName),
    create_code(relTypeName),
    create_code(yieldItem),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 4
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(nodeLabel),
    create_code(propertyLookup),
    create_code(relationshipTypes),
    create_code(yieldItems),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 5
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(nodeLabels),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 6
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code_expression(?MAX_RULE_EXPRESSION),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 7
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(caseAlternatives),
    create_code(functionInvocation),
    create_code(idInColl),
    create_code(listLiteral),
    create_code(mapLiteral),
    % wwe ???
    % currently not supported
    % create_code(parenthesizedExpression),
    create_code(where),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 7
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(caseExpression),
    create_code(filterExpression),
    create_code(properties),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 8
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(atomFilter),
    create_code(atomExtract),
    create_code(atomAll),
    create_code(atomAny),
    create_code(atomNone),
    create_code(atomSingle),
    create_code(listComprehension),
    create_code(nodePattern),
    create_code(relationshipDetail),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 9
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(relationshipPattern),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 10
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(patternElementChain),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 11
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(patternElement),
    create_code(relationshipsPattern),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 12
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(anonymousPatternPart),
    create_code(patternComprehension),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 13
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code_expression(?MAX_RULE_EXPRESSION),
    create_code(patternPart),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 14
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
%% Level 91
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(singleQuery),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 92
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(union),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 93
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(regularQuery),
    create_code(standAloneCall),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 94
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(query),

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Level 95
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    create_code(statement),

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
%% AnonymousPatternPart = PatternElement ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(anonymousPatternPart = Rule) ->
    ?CREATE_CODE_START,

    [{patternElement, Code}] = dets:lookup(?CODE_TEMPLATES, patternElement),
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = ...
%%      | ((A,L,L), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ...
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atomAll = Rule) ->
    ?CREATE_CODE_START,
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),

    Code = [
            "All" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = ...
%%      | ((A,N,Y), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ...
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atomAny = Rule) ->
    ?CREATE_CODE_START,
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),

    Code = [
            "Any" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = ...
%%      | ((C,O,U,N,T), [SP], '(', [SP], '*', [SP], ')')
%%      | ... ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atomCount = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "Count(*)",
        "Count ( * )"
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = ...
%%      | ((E,X,T,R,A,C,T), [SP], '(', [SP], FilterExpression, [SP], [[SP], '|', Expression], ')')
%%      | ...
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atomExtract = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),

    Code = [
            "Extract" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            case rand:uniform(2) rem 2 of
                1 -> ?SP_OPT ++ "|" ++
                    lists:nth(rand:uniform(Expression_Length), Expression);
                _ -> []
            end ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = ...
%%      | ((F,I,L,T,E,R), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ...
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atomFilter = Rule) ->
    ?CREATE_CODE_START,
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),

    Code = [
            "Filter" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = ...
%%      | ((N,O,N,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ...
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atomNone = Rule) ->
    ?CREATE_CODE_START,
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),

    Code = [
            "None" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Atom = ...
%%      | ((S,I,N,G,L,E), [SP], '(', [SP], FilterExpression, [SP], ')')
%%      | ...
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(atomSingle = Rule) ->
    ?CREATE_CODE_START,
    [{filterExpression, FilterExpression}] = dets:lookup(?CODE_TEMPLATES, filterExpression),
    FilterExpression_Length = length(FilterExpression),

    Code = [
            "Single" ++ ?SP_OPT ++ "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++ ?SP_OPT ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CaseAlternatives = (W,H,E,N), [SP], Expression, [SP], (T,H,E,N), [SP], Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(caseAlternatives = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
            "When" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP ++
            "Then" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression)
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% CaseExpression = (((C,A,S,E), { [SP], CaseAlternatives }-) | ((C,A,S,E), [SP], Expression, { [SP], CaseAlternatives }-)), [[SP], (E,L,S,E), [SP], Expression], [SP], (E,N,D) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(caseExpression = Rule) ->
    ?CREATE_CODE_START,
    [{caseAlternatives, CaseAlternatives}] = dets:lookup(?CODE_TEMPLATES, caseAlternatives),
    CaseAlternatives_Length = length(CaseAlternatives),
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
        case rand:uniform(12) rem 12 of
            1 -> "Case" ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "End";
            2 -> "Case" ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "End";
            3 -> "Case" ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "End";
            4 -> "Case" ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "Else" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ "End";
            5 -> "Case" ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "Else" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ "End";
            6 -> "Case" ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "Else" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ "End";
            7 -> "Case" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "End";
            8 -> "Case" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "End";
            9 -> "Case" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "End";
            10 -> "Case" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "Else" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ "End";
            11 -> "Case" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "Else" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ "End";
            _ -> "Case" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ lists:nth(rand:uniform(CaseAlternatives_Length), CaseAlternatives) ++
                ?SP ++ "Else" ++ ?SP ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP ++ "End"
        end
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
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
            "Create" ++ ?SP ++
            lists:nth(rand:uniform(Pattern_Length), Pattern)
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Cypher = [SP], QueryOptions, Statement, [[SP], ';'], [SP] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(cypher = Rule) ->
    ?CREATE_CODE_START,
    [{statement, Statement}] = dets:lookup(?CODE_TEMPLATES, statement),

    CodeCurr = [
            ?SP_OPT ++
            S ++
            case rand:uniform(2) rem 2 of
                1 -> ?SP_OPT ++ ";";
                _ -> []
            end
            ++ ?SP_OPT
        || S <- Statement
    ],
    Code = case length(CodeCurr) > ?MAX_CYPHER of
               true -> lists:sublist(CodeCurr, 1, ?MAX_CYPHER);
               _ -> CodeCurr
           end,
    store_code(Rule, Code, ?MAX_CYPHER, false),
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(integerLiteral, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
    store_code(numberLiteral, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Delete = [(D,E,T,A,C,H), SP], (D,E,L,E,T,E), [SP], Expression, { [SP], ',', [SP], Expression } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(delete = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
        case rand:uniform(6) rem 6 of
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
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% (* Any character except "`", enclosed within `backticks`. Backticks are escaped with double backticks. *)EscapedSymbolicName = { '`', { ANY - ('`') }, '`' }- ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(escapedSymbolicName = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "`1esn_SYMN_esn`",
        "`2esn_SYMN_esn`",
        "`3esn_SYMN_esn`",
        "`4esn_SYMN_esn`",
        "`5esn_SYMN_esn`",
        "`6esn_SYMN_esn`",
        "`7esn_SYMN_esn`",
        "`8esn_SYMN_esn`",
        "`aesn_SYMN_esn`",
        "`Aesn_SYMN_esn`",
        "`besn_SYMN_esn`",
        "`Besn_SYMN_esn`",
        "`.esn_SYMN_esn`",
        "`,esn_SYMN_esn`",
        "`@esn_SYMN_esn`",
        "``"
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(symbolicName, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ExplicitProcedureInvocation = ProcedureName, [SP], '(', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(explicitProcedureInvocation = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{procedureName, ProcedureName}] = dets:lookup(?CODE_TEMPLATES, procedureName),
    ProcedureName_Length = length(ProcedureName),

    Code = [
            lists:nth(rand:uniform(ProcedureName_Length), ProcedureName) ++ "(" ++
            case rand:uniform(4) rem 4 of
                1 ->
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression);
                2 ->
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                        ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression);
                3 ->
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression);
                _ -> []
            end ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(doubleLiteral, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
    store_code(numberLiteral, Code, ?MAX_RULE_OTHERS, false),
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
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FunctionInvocation = FunctionName, [SP], '(', [SP], [(D,I,S,T,I,N,C,T), [SP]], [Expression, [SP], { ',', [SP], Expression, [SP] }], ')' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionInvocation = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),
    [{functionName, FunctionName}] = dets:lookup(?CODE_TEMPLATES, functionName),
    FunctionName_Length = length(FunctionName),

    Code = [
            lists:nth(rand:uniform(FunctionName_Length), FunctionName) ++ ?SP_OPT ++
            "(" ++ ?SP_OPT ++
            case rand:uniform(6) rem 6 of
                1 -> "Distinct" ++ ?SP ++
                    lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT ++
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT;
                2 -> "Distinct" ++ ?SP ++
                    lists:nth(rand:uniform(Expression_Length), Expression);
                3 -> "Distinct";
                4 ->
                    lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT ++
                        "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT;
                5 ->
                    lists:nth(rand:uniform(Expression_Length), Expression);
                _ -> []
            end ++
            ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% FunctionName = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(functionName = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = ["exists"] ++ [re:replace(SN, "_SYMN_", "_FN_", [{return, list}]) || SN <- SymbolicName],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(integerLiteral, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
    store_code(numberLiteral, Code, ?MAX_RULE_OTHERS, false),
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(symbolicName, Code, ?MAX_RULE_OTHERS, false),
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
            lists:nth(rand:uniform(Variable_Length), Variable) ++
            ?SP ++ "In" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression)
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ImplicitProcedureInvovcation = ProcedureName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(implicitProcedureInvocation = Rule) ->
    ?CREATE_CODE_START,
    [{procedureName, ProcedureName}] = dets:lookup(?CODE_TEMPLATES, procedureName),

    Code = [re:replace(PN, "_PN_", "_IPI_", [{return, list}]) || PN <- ProcedureName],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% InQueryCall = (C,A,L,L), SP, ExplicitProcedureInvocation, [[SP], (Y,I,E,L,D), SP, YieldItems] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(inQueryCall = Rule) ->
    ?CREATE_CODE_START,
    [{explicitProcedureInvocation, ExplicitProcedureInvocation}] = dets:lookup(?CODE_TEMPLATES, explicitProcedureInvocation),
    ExplicitProcedureInvocation_Length = length(ExplicitProcedureInvocation),
    [{yieldItems, YieldItems}] = dets:lookup(?CODE_TEMPLATES, yieldItems),
    YieldItems_Length = length(YieldItems),

    Code = [
            "Call" ++ ?SP ++
            case rand:uniform(2) rem 2 of
                1 -> lists:nth(rand:uniform(ExplicitProcedureInvocation_Length), ExplicitProcedureInvocation);
                _ -> lists:nth(rand:uniform(ExplicitProcedureInvocation_Length), ExplicitProcedureInvocation) ++
                    " Yield" ++ ?SP ++ lists:nth(rand:uniform(YieldItems_Length), YieldItems)
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% LabelName = SchemaName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(labelName = Rule) ->
    ?CREATE_CODE_START,
    [{schemaName, SchemaName}] = dets:lookup(?CODE_TEMPLATES, schemaName),

    Code = [re:replace(SN, "_SCHN_", "_LN_", [{return, list}]) || SN <- SchemaName],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Limit = (L,I,M,I,T), SP, Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(limit = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    TargetSize = min(?MAX_CLAUSE, Expression_Length),

    Code = [
            "Limit" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression)
        || _ <- lists:seq(1, TargetSize)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
            "[" ++
            lists:nth(rand:uniform(FilterExpression_Length), FilterExpression) ++
            case rand:uniform(2) rem 2 of
                1 ->
                    ?SP_OPT ++ "|" ++ lists:nth(rand:uniform(Expression_Length), Expression);
                _ -> []
            end ++
            "]"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ListLiteral = '[', [SP], [Expression, [SP], { ',', [SP], Expression, [SP] }], ']' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(listLiteral = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
            "[" ++ ?SP_OPT ++
            lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT ++
            case rand:uniform(3) rem 3 of
                1 ->
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT ++
                        "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT;
                2 ->
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT;
                _ -> []
            end ++
            "]"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Literal = ...
%%         | (N,U,L,L)
%%         | ... ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(literalNull = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "Null"
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
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
                "{" ++ ?SP_OPT ++ "}"
        ],
        [
                "{" ++ ?SP_OPT ++ lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName) ++
                ?SP_OPT ++ ":" ++ ?SP_OPT ++
                lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT ++
                case rand:uniform(2) rem 2 of
                    1 ->
                        "," ++ ?SP_OPT ++
                            lists:nth(rand:uniform(PropertyKeyName_Length), PropertyKeyName) ++
                            ?SP_OPT ++ ":" ++ ?SP_OPT ++
                            lists:nth(rand:uniform(Expression_Length), Expression) ++ ?SP_OPT;
                    _ -> []
                end ++
                "}"
            || _ <- lists:seq(1, ?MAX_RULE_ATOM)
        ]
    ),
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
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
        case rand:uniform(12) rem 12 of
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
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
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
            "Merge" ++ ?SP ++
            lists:nth(rand:uniform(PatternPart_Length), PatternPart) ++
            case rand:uniform(3) rem 3 of
                1 ->
                    ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction) ++
                        ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction);
                2 ->
                    ?SP ++ lists:nth(rand:uniform(MergeAction_Length), MergeAction);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
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
            "On" ++ ?SP ++
            case rand:uniform(2) rem 2 of
                1 -> "Match" ++ ?SP ++
                    lists:nth(rand:uniform(Set_Length), Set);
                _ -> "Create" ++ ?SP ++
                    lists:nth(rand:uniform(Set_Length), Set)
            end
        || _ <- lists:seq(1, Set_Length)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NodeLabel = ':', [SP], LabelName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(nodeLabel = Rule) ->
    ?CREATE_CODE_START,
    [{labelName, LabelName}] = dets:lookup(?CODE_TEMPLATES, labelName),

    Code = [
            ":" ++ ?SP_OPT ++ LN || LN <- LabelName
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% NodeLabels = NodeLabel, { [SP], NodeLabel } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(nodeLabels = Rule) ->
    ?CREATE_CODE_START,
    [{nodeLabel, NodeLabel}] = dets:lookup(?CODE_TEMPLATES, nodeLabel),
    NodeLabel_Length = length(NodeLabel),

    Code = [
            NL ++
            case rand:uniform(2) rem 2 of
                1 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(NodeLabel_Length), NodeLabel);
                _ -> []
            end
        || NL <- NodeLabel],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
            "(" ++ ?SP_OPT ++
            case rand:uniform(8) rem 8 of
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
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(integerLiteral, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
    store_code(numberLiteral, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Order = (O,R,D,E,R), SP, (B,Y), SP, SortItem, { ',', [SP], SortItem } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(order = Rule) ->
    ?CREATE_CODE_START,
    [{sortItem, SortItem}] = dets:lookup(?CODE_TEMPLATES, sortItem),
    SortItem_Length = length(SortItem),

    TargetSize = min(?MAX_CLAUSE, SortItem_Length),

    Code = [
            "Order" ++ ?SP ++ "By" ++ ?SP ++
            lists:nth(rand:uniform(SortItem_Length), SortItem) ++
            case rand:uniform(3) rem 3 of
                1 ->
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem) ++
                        "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem);
                2 ->
                    "," ++ ?SP_OPT ++ lists:nth(rand:uniform(SortItem_Length), SortItem);
                _ -> []
            end
        || _ <- lists:seq(1, TargetSize)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Parameter = '$', (SymbolicName | DecimalInteger) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parameter = Rule) ->
    ?CREATE_CODE_START,
    [{decimalInteger, DecimalInteger}] = dets:lookup(?CODE_TEMPLATES, decimalInteger),
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = lists:append(
        ["$" ++ re:replace(SN, "symbolic_name", "parameter", [{return, list}]) || SN <- SymbolicName],
        ["$" ++ DI || DI <- DecimalInteger]
    ),
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ParenthesizedExpression = '(', [SP], Expression, [SP], ')' ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(parenthesizedExpression = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
            "(" ++ ?SP_OPT ++
            lists:nth(rand:uniform(Expression_Length), Expression) ++
            ?SP_OPT ++ ")"
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_ATOM, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
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
            case rand:uniform(4) rem 4 of
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
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PatternComprehension = '[', [SP], [Variable, [SP], '=', [SP]], RelationshipsPattern, [SP], [(W,H,E,R,E), [SP], Expression, [SP]], '|', [SP], Expression, [SP], ']' ;
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
        case rand:uniform(4) rem 4 of
            1 -> "[" ++
                ?SP_OPT ++ lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern) ++
                ?SP_OPT ++ "|" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "]";
            2 -> "[" ++
                ?SP_OPT ++ lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern) ++
                ?SP_OPT ++ " where " ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "|" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "]";
            3 -> "[" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP_OPT ++ "=" ++
                ?SP_OPT ++ lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern) ++
                ?SP_OPT ++ "|" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "]";
            _ -> "[" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP_OPT ++ "=" ++
                ?SP_OPT ++ lists:nth(rand:uniform(RelationshipsPattern_Length), RelationshipsPattern) ++
                ?SP_OPT ++ " where " ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "|" ++
                ?SP_OPT ++ lists:nth(rand:uniform(Expression_Length), Expression) ++
                ?SP_OPT ++ "]"
        end
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PatternElement = (NodePattern, { [SP], PatternElementChain })
%%                | ('(', PatternElement, ')') ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(patternElement = Rule) ->
    ?CREATE_CODE_START,
    [{nodePattern, NodePattern}] = dets:lookup(?CODE_TEMPLATES, nodePattern),
    NodePattern_Length = length(NodePattern),
    [{patternElementChain, PatternElementChain}] = dets:lookup(?CODE_TEMPLATES, patternElementChain),
    PatternElementChain_Length = length(PatternElementChain),

    Code = [
        case rand:uniform(7) rem 7 of
            1 -> "(" ++ "(" ++
                lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                ")" ++ ")";
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
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
            lists:nth(rand:uniform(RelationshipPattern_Length), RelationshipPattern) ++
            ?SP_OPT ++
            lists:nth(rand:uniform(NodePattern_Length), NodePattern)
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
        case rand:uniform(2) rem 2 of
            1 -> lists:nth(rand:uniform(Variable_Length), Variable) ++
                ?SP_OPT ++ "=" ++ ?SP_OPT ++
                lists:nth(rand:uniform(AnonymousPatternPart_Length), AnonymousPatternPart);
            _ ->
                lists:nth(rand:uniform(AnonymousPatternPart_Length), AnonymousPatternPart)
        end
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProcedureName = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(procedureName = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = [re:replace(SN, "_SYMN_", "_PN_", [{return, list}]) || SN <- SymbolicName],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ProcedureResultField = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(procedureResultField = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = [re:replace(SN, "_SYMN_", "_PRF_", [{return, list}]) || SN <- SymbolicName],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Properties = MapLiteral
%%            | Parameter ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(properties = Rule) ->
    ?CREATE_CODE_START,
    [{mapLiteral, MapLiteral}] = dets:lookup(?CODE_TEMPLATES, mapLiteral),
    [{parameter, Parameter}] = dets:lookup(?CODE_TEMPLATES, parameter),

    Code = lists:append(
        [
            MapLiteral,
            Parameter
        ]
    ),
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
                1 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                        ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                        ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                2 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup) ++
                        ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup);
                _ ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PropertyLookup_Length), PropertyLookup)
            end
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PropertyKeyName = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(propertyKeyName = Rule) ->
    ?CREATE_CODE_START,
    [{schemaName, SchemaName}] = dets:lookup(?CODE_TEMPLATES, schemaName),

    Code = [
        re:replace(SN, "_SCHN_", "_PKN_", [{return, list}]) || SN <- SchemaName
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PropertyLookup = '.', [SP], (PropertyKeyName) ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(propertyLookup = Rule) ->
    ?CREATE_CODE_START,
    [{propertyKeyName, PropertyKeyName}] = dets:lookup(?CODE_TEMPLATES, propertyKeyName),

    Code = [
            "." ++ ?SP_OPT ++ PK || PK <- PropertyKeyName
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Query = RegularQuery
%%       | BulkImportQuery ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(query = Rule) ->
    ?CREATE_CODE_START,
    [{regularQuery, RegularQuery}] = dets:lookup(?CODE_TEMPLATES, regularQuery),
    RegularQuery_Length = length(RegularQuery),

    Code = case RegularQuery_Length > ?MAX_QUERY of
               true -> lists:sublist(RegularQuery, 1, ?MAX_QUERY);
               _ -> RegularQuery
           end,
    store_code(Rule, Code, ?MAX_QUERY, false),
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
            " * "
        ],
        [
                "*" ++ ?SP_OPT ++ IL ++
                ?SP ++ case rand:uniform(5) rem 5 of
                           1 -> [];
                           2 -> ?SP_OPT ++ "..";
                           _ -> ?SP_OPT ++ ".." ++ ?SP_OPT ++
                               lists:nth(rand:uniform(IntegerLiteral_Length), IntegerLiteral) ++ ?SP_OPT
                       end
            || IL <- IntegerLiteral
        ]
    ),
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(doubleLiteral, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
    store_code(numberLiteral, Code, ?MAX_RULE_OTHERS, false),
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
    [{union, Union}] = dets:lookup(?CODE_TEMPLATES, union),
    Union_Length = length(Union),

    Code = [
            SQ ++
            case rand:uniform(3) rem 3 of
                1 -> ?SP ++ lists:nth(rand:uniform(Union_Length), Union) ++
                    ?SP ++ lists:nth(rand:uniform(Union_Length), Union);
                2 -> ?SP ++ lists:nth(rand:uniform(Union_Length), Union);
                _ -> []
            end
        || SQ <- SingleQuery
    ],
    store_code(Rule, Code, ?MAX_QUERY, false),
    store_code(query, Code, ?MAX_QUERY, false),
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

    Code = lists:append(
        [
%%            currently not suported
%%            "[]",
%%            "[ ]"
        ],
        [
                "[" ++ ?SP_OPT ++
                case rand:uniform(15) rem 15 of
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
        ]
    ),
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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

    Code = lists:append(
        [
            "<-->",
            "<--",
            "-->",
            "--"
        ],
        [
            case rand:uniform(4) rem 4 of
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
        ]
    ),
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RelationshipsPattern = NodePattern, { [SP], PatternElementChain }- ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relationshipsPattern = Rule) ->
    ?CREATE_CODE_START,
    [{nodePattern, NodePattern}] = dets:lookup(?CODE_TEMPLATES, nodePattern),
    NodePattern_Length = length(NodePattern),
    [{patternElementChain, PatternElementChain}] = dets:lookup(?CODE_TEMPLATES, patternElementChain),
    PatternElementChain_Length = length(PatternElementChain),

    Code = [
            lists:nth(rand:uniform(NodePattern_Length), NodePattern) ++
            case rand:uniform(2) rem 2 of
                1 ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain) ++
                        ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain);
                _ ->
                    ?SP_OPT ++ lists:nth(rand:uniform(PatternElementChain_Length), PatternElementChain)
            end
        || _ <- lists:seq(1, ?MAX_RULE_ATOM)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RelationshipTypes = ':', [SP], RelTypeName, { [SP], '|', [':'], [SP], RelTypeName } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relationshipTypes = Rule) ->
    ?CREATE_CODE_START,
    [{relTypeName, RelTypeName}] = dets:lookup(?CODE_TEMPLATES, relTypeName),
    RelTypeName_Length = length(RelTypeName),

    Code = [
            ":" ++ ?SP_OPT ++ RTN ++
            case rand:uniform(3) rem 3 of
                1 ->
                    ?SP_OPT ++ "|" ++ ":" ++ ?SP_OPT ++ lists:nth(rand:uniform(RelTypeName_Length), RelTypeName);
                2 ->
                    ?SP_OPT ++ "|" ++ ?SP_OPT ++ lists:nth(rand:uniform(RelTypeName_Length), RelTypeName);
                _ -> []
            end
        || RTN <- RelTypeName
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% RelTypeName = SchemaName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(relTypeName = Rule) ->
    ?CREATE_CODE_START,
    [{schemaName, SchemaName}] = dets:lookup(?CODE_TEMPLATES, schemaName),

    Code = [re:replace(SN, "_SCHN_", "_RTN_", [{return, list}]) || SN <- SchemaName],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Remove = (R,E,M,O,V,E), SP, RemoveItem, { [SP], ',', [SP], RemoveItem } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(remove = Rule) ->
    ?CREATE_CODE_START,
    [{removeItem, RemoveItem}] = dets:lookup(?CODE_TEMPLATES, removeItem),
    RemoveItem_Length = length(RemoveItem),

    Code = [
            "Remove" ++ ?SP ++
            lists:nth(rand:uniform(RemoveItem_Length), RemoveItem) ++
            case rand:uniform(3) rem 3 of
                1 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                    lists:nth(rand:uniform(RemoveItem_Length), RemoveItem) ++
                    ?SP_OPT ++ "," ++ ?SP_OPT ++
                    lists:nth(rand:uniform(RemoveItem_Length), RemoveItem);
                2 -> ?SP_OPT ++ "," ++ ?SP_OPT ++
                    lists:nth(rand:uniform(RemoveItem_Length), RemoveItem);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
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
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
%%              ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(reservedWord = Rule) ->
    ?CREATE_CODE_START,

    Code = [
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
        "elsE",
        "enD",
        "endS",
        "existS",
        "falsE",
        "foR",
        "iN",
        "iS",
        "limiT",
        "matcH",
        "mergE",
        "noT",
        "nulL",
        "oN",
        "optionaL",
        "oR",
        "ordeR",
        "removE",
        "requirE",
        "returN",
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(schemaName, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Return = (R,E,T,U,R,N), [[SP], (D,I,S,T,I,N,C,T)], SP, ReturnBody ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(return = Rule) ->
    ?CREATE_CODE_START,
    [{returnBody, ReturnBody}] = dets:lookup(?CODE_TEMPLATES, returnBody),
    ReturnBody_Length = length(ReturnBody),

    Code = [
            "Return" ++ ?SP ++
            case rand:uniform(2) rem 2 of
                1 -> "Distinct" ++ ?SP;
                _ -> []
            end ++
            lists:nth(rand:uniform(ReturnBody_Length), ReturnBody)
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
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
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
                1 -> ?SP ++ "As" ++ ?SP ++
                    lists:nth(rand:uniform(Variable_Length), Variable);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
            || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
        ]
    ),
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SchemaName = SymbolicName
%%            | ReservedWord
%%            ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(schemaName = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = [
        re:replace(SN, "_SYMN_", "_SCHN_", [{return, list}]) || SN <- SymbolicName
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
            "Set" ++ ?SP ++
            lists:nth(rand:uniform(SetItem_Length), SetItem) ++
            case rand:uniform(3) rem 3 of
                1 -> "," ++ lists:nth(rand:uniform(SetItem_Length), SetItem) ++
                    "," ++ lists:nth(rand:uniform(SetItem_Length), SetItem);
                2 -> "," ++ lists:nth(rand:uniform(SetItem_Length), SetItem);
                _ -> []
            end
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
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
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SingleQuery = Clause, { [SP], Clause } ;
%% -----------------------------------------------------------------------------
%% wwe ???
%% SingleQuery = Clause, { SP, Clause } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(singleQuery = Rule) ->
    ?CREATE_CODE_START,
    [{clause, Clause}] = dets:lookup(?CODE_TEMPLATES, clause),
    Clause_Length = length(Clause),

    Code = [
            C ++
            case rand:uniform(3) rem 3 of
                1 -> ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause) ++
                    ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause);
                2 -> ?SP ++ lists:nth(rand:uniform(Clause_Length), Clause);
                _ -> []
            end
        || C <- Clause
    ],
    store_code(Rule, Code, ?MAX_QUERY, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Skip = (S,K,I,P), SP, Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(skip = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),
    Expression_Length = length(Expression),

    Code = [
            "Skip" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression)
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
        "Match ()<-[*]->()",
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
        "Match ()<-[]->()",
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
        "Match ()<-[:rtn *]->()",
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
        "Match ()<-[vn]->()",
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
        "Match ()<-[vn $1]->()",
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
        "Match ()<-[vn * $1]->()",
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
        "Match ()<-[vn :rtn  $1]->()",
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
        "Match ()<-[vn :rtn  *]->()",
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
        "With * Order by 5"
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% StandaloneCall = (C,A,L,L), SP, (ExplicitProcedureInvocation | ImplicitProcedureInvocation), [SP, (Y,I,E,L,D), SP, YieldItems] ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(standAloneCall = Rule) ->
    ?CREATE_CODE_START,
    [{explicitProcedureInvocation, ExplicitProcedureInvocation}] = dets:lookup(?CODE_TEMPLATES, explicitProcedureInvocation),
    ExplicitProcedureInvocation_Length = length(ExplicitProcedureInvocation),
    [{implicitProcedureInvocation, ImplicitProcedureInvocation}] = dets:lookup(?CODE_TEMPLATES, implicitProcedureInvocation),
    ImplicitProcedureInvocation_Length = length(ImplicitProcedureInvocation),
    [{yieldItems, YieldItems}] = dets:lookup(?CODE_TEMPLATES, yieldItems),
    YieldItems_Length = length(YieldItems),

    Code = [
            "Call" ++ ?SP ++
            case rand:uniform(4) rem 4 of
                1 -> lists:nth(rand:uniform(ExplicitProcedureInvocation_Length), ExplicitProcedureInvocation);
                2 -> lists:nth(rand:uniform(ExplicitProcedureInvocation_Length), ExplicitProcedureInvocation) ++
                    " Yield" ++ ?SP ++ lists:nth(rand:uniform(YieldItems_Length), YieldItems);
                3 -> lists:nth(rand:uniform(ImplicitProcedureInvocation_Length), ImplicitProcedureInvocation);
                _ -> lists:nth(rand:uniform(ImplicitProcedureInvocation_Length), ImplicitProcedureInvocation) ++
                    " Yield" ++ ?SP ++ lists:nth(rand:uniform(YieldItems_Length), YieldItems)
            end
        || _ <- lists:seq(1, ?MAX_QUERY * 2)
    ],
    store_code(Rule, Code, ?MAX_QUERY, false),
    store_code(query, Code, ?MAX_QUERY, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Statement = Command
%%           | Query ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(statement = Rule) ->
    ?CREATE_CODE_START,
    [{query, Query}] = dets:lookup(?CODE_TEMPLATES, query),
    Query_Length = length(Query),

    Code = case Query_Length > ?MAX_STATEMENT of
               true -> lists:sublist(Query, 1, ?MAX_STATEMENT);
               _ -> Query
           end,
    store_code(Rule, Code, ?MAX_STATEMENT, false),
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
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_OTHERS, false),
    store_code(literal, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% SymbolicName = ...
%%              | (C,O,U,N,T)
%%              | (F,I,L,T,E,R)
%%              | (E,X,T,R,A,C,T)
%%              | (A,N,Y)
%%              | (A,L,L)
%%              | (N,O,N,E)
%%              | (S,I,N,G,L,E)
%%              ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(symbolicName = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "alL",
        "anY",
        "counT",
        "extracT",
        "filteR",
        "nonE",
        "singlE"
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% UnescapedSymbolicName = IdentifierStart, { IdentifierPart } ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(unescapedSymbolicName = Rule) ->
    ?CREATE_CODE_START,

    Code = [
        "usn1_SYMN_usn",
        "usn2_SYMN_usn",
        "_usn3_SYMN_usn",
        "_usn4_SYMN_usn",
        "@usn5_SYMN_usn",
        "@usn6_SYMN_usn",
        "#usn7_SYMN_usn",
        "#usn8_SYMN_usn",
        "usna_SYMN_usn",
        "usnb_SYMN_usn",
        "_usnc_SYMN_usn",
        "_usnd_SYMN_usn",
        "@usne_SYMN_usn",
        "@usnf_SYMN_usn",
        "#usng_SYMN_usn"
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(symbolicName, Code, ?MAX_RULE_OTHERS, false),
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
            "Union" ++
            case rand:uniform(2) rem 2 of
                1 -> ?SP ++ "All";
                _ -> []
            end ++
            ?SP ++ SQ
        || SQ <- SingleQuery
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
            "Unwind" ++ ?SP ++
            lists:nth(rand:uniform(Expression_Length), Expression) ++
            ?SP ++ "As" ++ ?SP ++
            lists:nth(rand:uniform(Variable_Length), Variable)
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Variable = SymbolicName ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(variable = Rule) ->
    ?CREATE_CODE_START,
    [{symbolicName, SymbolicName}] = dets:lookup(?CODE_TEMPLATES, symbolicName),

    Code = [re:replace(SN, "_SYMN_", "_V_", [{return, list}]) || SN <- SymbolicName],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    store_code(atom, Code, ?MAX_RULE_ATOM, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Where = (W,H,E,R,E), SP, Expression ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(where = Rule) ->
    ?CREATE_CODE_START,
    [{expression, Expression}] = dets:lookup(?CODE_TEMPLATES, expression),

    Code = [
            "Where" ++ ?SP ++ E || E <- Expression
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
            "With" ++ ?SP ++
            case rand:uniform(4) rem 4 of
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
        || _ <- lists:seq(1, ?MAX_CLAUSE * 2)
    ],
    store_code(Rule, Code, ?MAX_CLAUSE, false),
    store_code(clause, Code, ?MAX_CLAUSE, false),
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
            1 -> lists:nth(rand:uniform(ProcedureResultField_Length), ProcedureResultField) ++
                " As " ++ lists:nth(rand:uniform(Variable_Length), Variable);
            _ -> ?SP ++ lists:nth(rand:uniform(Variable_Length), Variable)
        end
        || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
    ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
    ?CREATE_CODE_END;

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% YieldItems = (YieldItem, { [SP], ',', [SP], YieldItem })
%%            | '-'
%%            ;
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(yieldItems = Rule) ->
    ?CREATE_CODE_START,
    [{yieldItem, YieldItem}] = dets:lookup(?CODE_TEMPLATES, yieldItem),
    YieldItem_Length = length(YieldItem),

    Code = ["-"] ++
        [
            case rand:uniform(3) rem 3 of
                1 -> lists:nth(rand:uniform(YieldItem_Length), YieldItem) ++
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(YieldItem_Length), YieldItem) ++
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(YieldItem_Length), YieldItem);
                2 -> lists:nth(rand:uniform(YieldItem_Length), YieldItem) ++
                    ?SP_OPT ++ "," ++ ?SP_OPT ++ lists:nth(rand:uniform(YieldItem_Length), YieldItem);
                _ -> lists:nth(rand:uniform(YieldItem_Length), YieldItem)
            end
            || _ <- lists:seq(1, ?MAX_RULE_OTHERS)
        ],
    store_code(Rule, Code, ?MAX_RULE_OTHERS, false),
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
            case rand:uniform(5) rem 5 of
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
            case rand:uniform(3) rem 3 of
                1 ->
                    ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(NotExpression_Length), NotExpression) ++
                        ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(NotExpression_Length), NotExpression);
                2 ->
                    ?SP ++ "And" ++ ?SP ++ lists:nth(rand:uniform(NotExpression_Length), NotExpression);
                _ -> []
            end
        || _ <- lists:seq(1, Max)
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
            case rand:uniform(3) rem 3 of
                1 -> ?SP_OPT ++
                    lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression) ++
                    ?SP_OPT ++
                    lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression);
                2 -> ?SP_OPT ++
                lists:nth(rand:uniform(PartialComparisonExpression_Length), PartialComparisonExpression);
                _ -> []
            end
        || _ <- lists:seq(1, Max)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    store_code(orExpression, Code, Max, false),
    store_code(xorExpression, Code, Max, false),
    store_code(andExpression, Code, Max, false),
    store_code(notExpression, Code, Max, false),
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
            case rand:uniform(7) rem 7 of
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
            case rand:uniform(3) rem 3 of
                1 -> "Not" ++ ?SP ++ "Not" ++ ?SP;
                2 -> "Not" ++ ?SP;
                _ -> []
            end ++
            lists:nth(rand:uniform(ComparisonExpression_Length), ComparisonExpression)
        || _ <- lists:seq(1, Max)
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
            case rand:uniform(3) rem 3 of
                1 ->
                    ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(XorExpression_Length), XorExpression) ++
                        ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(XorExpression_Length), XorExpression);
                2 ->
                    ?SP ++ "Or" ++ ?SP ++ lists:nth(rand:uniform(XorExpression_Length), XorExpression);
                _ -> []
            end
        || _ <- lists:seq(1, Max)
    ],
    store_code(Rule, Code, Max, false),
    store_code(expression, Code, Max, false),
    ?CREATE_CODE_END;

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
        case rand:uniform(6) rem 6 of
            1 ->
                "=" ++ ?SP_OPT ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
            2 ->
                "<>" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
            3 ->
                "<" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
            4 ->
                ">" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
            5 ->
                "<=" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression);
            _ ->
                ">=" ++ ?SP ++ lists:nth(rand:uniform(AddOrSubtractExpression_Length), AddOrSubtractExpression)
        end
        || _ <- lists:seq(1, Max)
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
            case rand:uniform(3) rem 3 of
                1 ->
                    ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression) ++
                        ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression);
                2 ->
                    ?SP_OPT ++ "^" ++ ?SP_OPT ++ lists:nth(rand:uniform(UnaryAddOrSubtractExpression_Length), UnaryAddOrSubtractExpression);
                _ -> []
            end
        || _ <- lists:seq(1, Max)
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
        || _ <- lists:seq(1, Max)
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
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_code(stringListNullOperatorExpression = Rule, Max) ->
    ?CREATE_CODE_START,
    [{propertyOrLabelsExpression, PropertyOrLabelsExpression}] = dets:lookup(?CODE_TEMPLATES, propertyOrLabelsExpression),
    PropertyOrLabelsExpression_Length = length(PropertyOrLabelsExpression),

    Code = lists:append(
        [
                lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                "[..]"
        ],
        [
                lists:nth(rand:uniform(PropertyOrLabelsExpression_Length), PropertyOrLabelsExpression) ++
                case rand:uniform(23) rem 23 of
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
    ),
    store_code(Rule, Code, Max * 2, false),
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
            case rand:uniform(7) rem 7 of
                1 -> "+" ++ ?SP ++ "+" ++ ?SP;
                2 -> "+" ++ ?SP ++ "-" ++ ?SP;
                3 -> "+" ++ ?SP;
                4 -> "-" ++ ?SP ++ "+" ++ ?SP;
                5 -> "-" ++ ?SP ++ "-" ++ ?SP;
                6 -> "-" ++ ?SP;
                _ -> []
            end ++
            lists:nth(rand:uniform(StringListNullOperatorExpression_Length), StringListNullOperatorExpression)
        || _ <- lists:seq(1, Max)
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
            case rand:uniform(3) rem 3 of
                1 ->
                    ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(AndExpression_Length), AndExpression) ++
                        ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(AndExpression_Length), AndExpression);
                2 ->
                    ?SP ++ "Xor" ++ ?SP ++ lists:nth(rand:uniform(AndExpression_Length), AndExpression);
                _ -> []
            end
        || _ <- lists:seq(1, Max)
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
    create_code(orExpression, Max).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating EUnit data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_eunit_all([]) ->
    ok;
file_create_eunit_all([Rule | Rules]) ->
    file_create_eunit(Rule),
    file_create_eunit_all(Rules).

file_create_eunit(Rule) ->
    [{Rule, Code}] = dets:lookup(?CODE_TEMPLATES, Rule),
    erlang:display(io:format("final eunit  tests ===> ~12.. B type: ~s rule: ~s ~n", [length(Code), atom_to_list(reliability), atom_to_list(Rule)])),
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

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Creating Common Test data files.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

file_create_ct_all(_Type, []) ->
    ok;
file_create_ct_all(Type, [Rule | Rules]) ->
    file_create_ct(Type, Rule),
    file_create_ct_all(Type, Rules).

file_create_ct(Type, Rule) ->
    [{Rule, Code}] = dets:lookup(?CODE_TEMPLATES, Rule),
    erlang:display(io:format("final common tests ===> ~12.. B type: ~s rule: ~s ~n", [length(Code), atom_to_list(Type), atom_to_list(Rule)])),

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
                                               "{ok, _} = ocparse:source_to_pt";
                                           _ ->
                                               "ocparse_test:common_test_source"
                                       end ++ "(\"" ++ H ++ "\")" ++ case T of
                                                                         [] ->
                                                                             ".";
                                                                         _ ->
                                                                             ","
                                                                     end]),
    file_write_ct(Type, File, T).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Store generated code in helper table.
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

store_code(Rule, Code, Max, Sort) ->
    FRandom = fun(X, Y) -> erlang:phash2(X) < erlang:phash2(Y) end,

    case Max == 0 of
        true ->
            ?debugFmt("~ncode lines         ===> ~12.. B rule: ~s ~n", [length(Code), atom_to_list(Rule)]);
        _ ->
            CodeSet = sets:to_list(sets:from_list(Code)),
            % erlang:display(io:format("store CodeSet      ===> ~12.. B rule: ~s ~n", [length(CodeSet), atom_to_list(Rule)])),

            CodeNew = case length(CodeSet) > Max of
                          true ->
                              lists:sublist(CodeSet, 1, Max);
                          _ ->
                              CodeSet
                      end,

            CodeTotal = sets:to_list(sets:from_list(case dets:lookup(?CODE_TEMPLATES, Rule) of
                                                        [{Rule, CodeOld}] ->
                                                            lists:append([CodeOld, CodeNew]);
                                                        _ ->
                                                            CodeNew
                                                    end)),

            CodeSorted = case Sort of
                             true ->
                                 lists:sort(FRandom, CodeTotal);
                             _ ->
                                 CodeTotal
                         end,
            % erlang:display(io:format("store CodeSorted   ===> ~12.. B rule: ~s ~n", [length(CodeSorted), atom_to_list(Rule)])),

            dets:insert(?CODE_TEMPLATES, {Rule, CodeSorted}),
            ?debugFmt("~ncode lines         ===> ~12.. B rule: ~s ~n", [length(CodeSorted), atom_to_list(Rule)])
    end.
