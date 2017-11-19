%% -----------------------------------------------------------------------------
%%
%% ocparse.yrl: opencypher - lexer definition.
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

%% -*- erlang -*-
%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Definitions.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Rules.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% number literals
((\.[0-9]+)(e|E)[-]?[0-9]+)                               : {token, {'EXPONENT_DECIMAL_REAL', TokenLine, TokenChars}}.
(([0-9]+)(e|E)[-]?[0-9]+)                                 : {token, {'EXPONENT_DECIMAL_REAL', TokenLine, TokenChars}}.
(([0-9]+\.[0-9]+)(e|E)[-]?[0-9]+)                         : {token, {'EXPONENT_DECIMAL_REAL', TokenLine, TokenChars}}.
([0-9]*\.[0-9]+)                                          : {token, {'REGULAR_DECIMAL_REAL', TokenLine, TokenChars}}.
(0x([0-9]|[A-Fa-f])+)                                     : {token, {'HEX_INTEGER', TokenLine, TokenChars}}.
([A-F])                                                   : {token, {'HEX_LETTER', TokenLine, TokenChars}}.
(0[0-7]+)                                                 : {token, {'OCTAL_INTEGER', TokenLine, TokenChars}}.
(0|([1-9][0-9]*))                                         : {token, {'DECIMAL_INTEGER', TokenLine, TokenChars}}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% symbolic names
(`([^`]*)*`)                                              : {token, {'ESCAPED_SYMBOLIC_NAME', TokenLine, TokenChars}}.
([A-Za-z_@#][A-Za-z0-9_@#]*)                              : match_any(TokenChars, TokenLen, TokenLine, ?TOKEN_PATTERNS).

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% string literals
(\'([^\\\']*)*\')                                         : {token, {'STRING_LITERAL', TokenLine, TokenChars}}.
(\"([^\\\"]*)*\")                                         : {token, {'STRING_LITERAL', TokenLine, TokenChars}}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% comments
((//).*[\n\r]?)                                           : skip_token.
((/\*)(.|\n|\r)*(\*/))                                    : skip_token.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% punctuation
(\.\.|\+=)                                                : {token, {list_to_atom(TokenChars), TokenLine}}.
(=~|\<\>|\<=|\>=)                                         : {token, {list_to_atom(TokenChars), TokenLine}}.
([\^\.\|\*\+\(\)\[\]\{\}\-])                              : {token, {list_to_atom(TokenChars), TokenLine}}.
([=\<\>/%:,;0\$])                                         : {token, {list_to_atom(TokenChars), TokenLine}}.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% white space
([\n\r\s\t]+)                                             : skip_token.

%% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Erlang code.

%% -----------------------------------------------------------------------------
%%
%% oclexer.erl: opencypher - lexer.
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

-export([reserved_keywords/0]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-define(TOKEN_PATTERNS, [
    {"^(?i)(ADD)$",              'ADD'},
    {"^(?i)(ALL)$",              'ALL'},
    {"^(?i)(AND)$",              'AND'},
    {"^(?i)(ANY)$",              'ANY'},
    {"^(?i)(AS)$",               'AS'},
    {"^(?i)(ASC)$",              'ASC'},
    {"^(?i)(ASCENDING)$",        'ASCENDING'},
    {"^(?i)(BY)$",               'BY'},
    {"^(?i)(CALL)$",             'CALL'},
    {"^(?i)(CASE)$",             'CASE'},
    {"^(?i)(CONSTRAINT)$",       'CONSTRAINT'},
    {"^(?i)(CONTAINS)$",         'CONTAINS'},
    {"^(?i)(COUNT)$",            'COUNT'},
    {"^(?i)(CREATE)$",           'CREATE'},
    {"^(?i)(DELETE)$",           'DELETE'},
    {"^(?i)(DESC)$",             'DESC'},
    {"^(?i)(DESCENDING)$",       'DESCENDING'},
    {"^(?i)(DETACH)$",           'DETACH'},
    {"^(?i)(DISTINCT)$",         'DISTINCT'},
    {"^(?i)(DO)$",               'DO'},
    {"^(?i)(DROP)$",             'DROP'},
    {"^(?i)(ELSE)$",             'ELSE'},
    {"^(?i)(END)$",              'END'},
    {"^(?i)(ENDS)$",             'ENDS'},
    {"^(?i)(EXISTS)$",           'EXISTS'},
    {"^(?i)(EXTRACT)$",          'EXTRACT'},
    {"^(?i)(FALSE)$",            'FALSE'},
    {"^(?i)(FILTER)$",           'FILTER'},
    {"^(?i)(FOR)$",              'FOR'},
    {"^(?i)(IN)$",               'IN'},
    {"^(?i)(IS)$",               'IS'},
    {"^(?i)(LIMIT)$",            'LIMIT'},
    {"^(?i)(MANDATORY)$",        'MANDATORY'},
    {"^(?i)(MATCH)$",            'MATCH'},
    {"^(?i)(MERGE)$",            'MERGE'},
    {"^(?i)(NONE)$",             'NONE'},
    {"^(?i)(NOT)$",              'NOT'},
    {"^(?i)(NULL)$",             'NULL'},
    {"^(?i)(OF)$",               'OF'},
    {"^(?i)(ON)$",               'ON'},
    {"^(?i)(OPTIONAL)$",         'OPTIONAL'},
    {"^(?i)(OR)$",               'OR'},
    {"^(?i)(ORDER)$",            'ORDER'},
    {"^(?i)(REMOVE)$",           'REMOVE'},
    {"^(?i)(REQUIRE)$",          'REQUIRE'},
    {"^(?i)(RETURN)$",           'RETURN'},
    {"^(?i)(SCALAR)$",           'SCALAR'},
    {"^(?i)(SET)$",              'SET'},
    {"^(?i)(SINGLE)$",           'SINGLE'},
    {"^(?i)(SKIP)$",             'SKIP'},
    {"^(?i)(STARTS)$",           'STARTS'},
    {"^(?i)(THEN)$",             'THEN'},
    {"^(?i)(TRUE)$",             'TRUE'},
    {"^(?i)(UNION)$",            'UNION'},
    {"^(?i)(UNIQUE)$",           'UNIQUE'},
    {"^(?i)(UNWIND)$",           'UNWIND'},
    {"^(?i)(WHEN)$",             'WHEN'},
    {"^(?i)(WHERE)$",            'WHERE'},
    {"^(?i)(WITH)$",             'WITH'},
    {"^(?i)(XOR)$",              'XOR'},
    {"^(?i)(YIELD)$",            'YIELD'}
]).

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(Dbg(F,A), io:format(user, "[~p] "++F++"~n", [?LINE|A])).
-else.
-define(Dbg(F,A), ok).
-endif.

reserved_keywords() -> [T || {_, T} <- ?TOKEN_PATTERNS].

match_any(TokenChars, TokenLen, _TokenLine, []) ->
    {token, {'UNESCAPED_SYMBOLIC_NAME', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P,T}|TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match,[_]} ->
            {token, {T, TokenLine}};
        nomatch ->
            match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.
