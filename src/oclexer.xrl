%% -*- erlang -*-
Definitions.

Rules.

%% number literals
((([0-9]*\.)|[0-9]+)(e|E)[0-9]+)                          : {token, {'EXPONENT_DECIMAL_REAL', TokenLine, TokenChars}}.
([0-9]*\.[0-9]+)                                          : {token, {'REGULAR_DECIMAL_REAL', TokenLine, TokenChars}}.
(0(x|X)([0-9]|[A-Fa-f])+)                                 : {token, {'HEX_INTEGER', TokenLine, TokenChars}}.
(0[0-7]+)                                                 : {token, {'OCTAL_INTEGER', TokenLine, TokenChars}}.
(0|([1-9][0-9]*))                                         : {token, {'DECIMAL_INTEGER', TokenLine, TokenChars}}.

%% symbolic names
(`([^`]*)*`)                                              : {token, {'ESCAPED_SYMBOLIC_NAME', TokenLine, TokenChars}}.
([A-Za-z_@#][A-Za-z0-9_@#]*)                              : match_any(TokenChars, TokenLen, TokenLine, ?TokenPatters).

%% string literals
(\'([^\\\']*)*\')                                         : {token, {'STRING_LITERAL', TokenLine, TokenChars}}.
(\"([^\\\"]*)*\")                                         : {token, {'STRING_LITERAL', TokenLine, TokenChars}}.

% comments
((//).*[\n\r]?)                                           : skip_token.
((/\*)(.|\n|\r)*(\*/))                                    : skip_token.

%% punctuation
(\.\.|\-\-|\+=|\<\-\-\>|\<\-\-|\-\-\>)                    : {token, {list_to_atom(TokenChars), TokenLine}}.
(=~|\<\>|!=|\<=|\>=)                                      : {token, {list_to_atom(TokenChars), TokenLine}}.
([\^\.\|\?\*\+\(\)\[\]\{\}\-])                            : {token, {list_to_atom(TokenChars), TokenLine}}.
([=\<\>/%:,;!0\$])                                        : {token, {list_to_atom(TokenChars), TokenLine}}.

%% white space
([\n\r\s\t]+)                                             : skip_token.

Erlang code.

-export([reserved_keywords/0]).

-define(TokenPatters, [

    {"^(?i)(ALL)$",              'ALL'},
    {"^(?i)(ALLSHORTESTPATHS)$", 'ALLSHORTESTPATHS'},
    {"^(?i)(AND)$",              'AND'},
    {"^(?i)(ANY)$",              'ANY'},
    {"^(?i)(AS)$",               'AS'},
    {"^(?i)(ASC)$",              'ASC'},
    {"^(?i)(ASCENDING)$",        'ASCENDING'},
    {"^(?i)(ASSERT)$",           'ASSERT'},
    {"^(?i)(BY)$",               'BY'},
    {"^(?i)(CASE)$",             'CASE'},
    {"^(?i)(COMMIT)$",           'COMMIT'},
    {"^(?i)(CONSTRAINT)$",       'CONSTRAINT'},
    {"^(?i)(CONTAINS)$",         'CONTAINS'},
    {"^(?i)(COUNT)$",            'COUNT'},
    {"^(?i)(CREATE)$",           'CREATE'},
    {"^(?i)(CSV)$",              'CSV'},
    {"^(?i)(CYPHER)$",           'CYPHER'},
    {"^(?i)(DELETE)$",           'DELETE'},
    {"^(?i)(DESC)$",             'DESC'},
    {"^(?i)(DESCENDING)$",       'DESCENDING'},
    {"^(?i)(DETACH)$",           'DETACH'},
    {"^(?i)(DISTINCT)$",         'DISTINCT'},
    {"^(?i)(DROP)$",             'DROP'},
    {"^(?i)(ELSE)$",             'ELSE'},
    {"^(?i)(END)$",              'END'},
    {"^(?i)(ENDS)$",             'ENDS'},
    {"^(?i)(EXPLAIN)$",          'EXPLAIN'},
    {"^(?i)(EXTRACT)$",          'EXTRACT'},
    {"^(?i)(FALSE)$",            'FALSE'},
    {"^(?i)(FIELDTERMINATOR)$",  'FIELDTERMINATOR'},
    {"^(?i)(FILTER)$",           'FILTER'},
    {"^(?i)(FOREACH)$",          'FOREACH'},
    {"^(?i)(FROM)$",             'FROM'},
    {"^(?i)(HEADERS)$",          'HEADERS'},
    {"^(?i)(IN)$",               'IN'},
    {"^(?i)(INDEX)$",            'INDEX'},
    {"^(?i)(IS)$",               'IS'},
    {"^(?i)(JOIN)$",             'JOIN'},
    {"^(?i)(L_0X)$",             'L_0X'},
    {"^(?i)(L_SKIP)$",           'L_SKIP'},
    {"^(?i)(LIMIT)$",            'LIMIT'},
    {"^(?i)(LOAD)$",             'LOAD'},
    {"^(?i)(MATCH)$",            'MATCH'},
    {"^(?i)(MERGE)$",            'MERGE'},
    {"^(?i)(NODE)$",             'NODE'},
    {"^(?i)(NONE)$",             'NONE'},
    {"^(?i)(NOT)$",              'NOT'},
    {"^(?i)(NULL)$",             'NULL'},
    {"^(?i)(ON)$",               'ON'},
    {"^(?i)(OPTIONAL)$",         'OPTIONAL'},
    {"^(?i)(OR)$",               'OR'},
    {"^(?i)(ORDER)$",            'ORDER'},
    {"^(?i)(PERIODIC)$",         'PERIODIC'},
    {"^(?i)(PROFILE)$",          'PROFILE'},
    {"^(?i)(REDUCE)$",           'REDUCE'},
    {"^(?i)(REL)$",              'REL'},
    {"^(?i)(RELATIONSHIP)$",     'RELATIONSHIP'},
    {"^(?i)(REMOVE)$",           'REMOVE'},
    {"^(?i)(RETURN)$",           'RETURN'},
    {"^(?i)(SCAN)$",             'SCAN'},
    {"^(?i)(SET)$",              'SET'},
    {"^(?i)(SHORTESTPATH)$",     'SHORTESTPATH'},
    {"^(?i)(SINGLE)$",           'SINGLE'},
    {"^(?i)(SKIP)$",             'SKIP'},
    {"^(?i)(START)$",            'START'},
    {"^(?i)(STARTS)$",           'STARTS'},
    {"^(?i)(THEN)$",             'THEN'},
    {"^(?i)(TRUE)$",             'TRUE'},
    {"^(?i)(UNION)$",            'UNION'},
    {"^(?i)(UNIQUE)$",           'UNIQUE'},
    {"^(?i)(UNWIND)$",           'UNWIND'},
    {"^(?i)(USING)$",            'USING'},
    {"^(?i)(WHEN)$",             'WHEN'},
    {"^(?i)(WHERE)$",            'WHERE'},
    {"^(?i)(WITH)$",             'WITH'},
    {"^(?i)(XOR)$",              'XOR'}
]).

%-define(DEBUG, true).
-ifdef(DEBUG).
-define(Dbg(F,A), io:format(user, "[~p] "++F++"~n", [?LINE|A])).
-else.
-define(Dbg(F,A), ok).
-endif.

reserved_keywords() -> [T || {_, T} <- ?TokenPatters].

match_any(TokenChars, TokenLen, _TokenLine, []) ->
    {token, {'UNESCAPED_SYMBOLIC_NAME', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P,T}|TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match,[_]} ->
            {token, {T, TokenLine}};
        nomatch ->
            match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.
