%% -*- erlang -*-
Definitions.

Rules.

%% punctuation
%  (=|<>|<|>|<=|>=)                                    : {token, {'COMPARISON', TokenLine, list_to_atom(TokenChars)}}.
((\=\~))                                            : {token, {list_to_atom(TokenChars), TokenLine}}.
([\=\-\+\:\;\(\)\{\}\.\?\!\*\^0])                   : {token, {list_to_atom(TokenChars), TokenLine}}.

%% names
[A-Za-z][A-Za-z0-9_@#\$]*                           : match_any(TokenChars, TokenLen, TokenLine, ?TokenPatters).

%% numbers
([1-9][0-9]*)                                       : {token, {'UNSIGNED_DECIMAL_INTEGER', TokenLine, TokenChars}}.
(\-?([0-9]|\.)(e|E)(\-)?[0-9]+)                     : {token, {'EXPONENT_DECIMAL_REAL', TokenLine, TokenChars}}.
(\-?0X([0-9]|[A-F])+)                               : {token, {'HEX_INTEGER', TokenLine, TokenChars}}.
(\-?0[0-7]+)                                        : {token, {'OCTAL_INTEGER', TokenLine, TokenChars}}.
(-[1-9][0-9]*)                                      : {token, {'SIGNED_DECIMAL_INTEGER', TokenLine, TokenChars}}.
(\-[0-9]*\.[0-9]*)                                  : {token, {'SIGNED_FLOAT', TokenLine, TokenChars}}.
(\'([^\\\']*)*\')                                   : {token, {'STRING_LITERAL', TokenLine, TokenChars}}.
(\"([^\\\"]*)*\")                                   : {token, {'STRING_LITERAL', TokenLine, TokenChars}}.
([0-9]*\.[0-9]*)                                    : {token, {'UNSIGNED_FLOAT', TokenLine, TokenChars}}.

%% skip tokens
([\s\t\r\n]+)                                       : skip_token.                                                  %% white space
(\/\/.*[\n|\r])                                     : skip_token.                                                  %% comment

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
    {"^(?i)(EXISTS)$",           'EXISTS'},
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
    {token, {'NAME', TokenLen, TokenChars}};
match_any(TokenChars, TokenLen, TokenLine, [{P,T}|TPs]) ->
    case re:run(TokenChars, P, [{capture, first, list}]) of
        {match,[_]} ->
            if (T =:= 'AMMSC1') orelse
               (T =:= 'AMMSC2') orelse
               (T =:= 'AMMSC3') orelse
               (T =:= 'AMMSC4') orelse
               (T =:= 'FUNS') orelse
               (T =:= 'UFUN') -> {token, {T, TokenLine, list_to_atom(TokenChars)}};
            true              -> {token, {T, TokenLine}}
        end;
        nomatch     -> match_any(TokenChars, TokenLen, TokenLine, TPs)
    end.

match_fun(TokenLine, TokenChars) ->
    {match,[MatchedFunStr]} = re:run(TokenChars, "^fun.*end\\.", [ungreedy,dotall,{capture, all, list}]),
    {token, {'STRING', TokenLine, MatchedFunStr}, string:sub_string(TokenChars, length(MatchedFunStr)+1)}.

parse_json(TokenLine, TokenChars) ->
    ?Dbg("TokenChars=~p", [TokenChars]),
    parse_json(TokenLine, TokenChars, "", "", 0).

parse_json(TokenLine, [], Json, PushBack, 0) ->
    {token, {'JSON', TokenLine, lists:reverse(Json)}, PushBack};
parse_json(_TokenLine, [], Json, _PushBack, Open) ->
    {error, lists:flatten(
                io_lib:format("malformed JSON path '~s',"
                              " ~p bracket(s) not closed"
                             , [lists:reverse(Json), Open]))};
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when T =:= $]; T =:= $}; T =:= $) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    if Open > 0 ->
            parse_json(TokenLine, TokenChars, [T|Json], PushBack, Open-1);
        true ->
            parse_json(TokenLine, [], Json, [T|TokenChars], Open)
    end;
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when T =:= $[; T =:= ${; T =:= $( ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T|Json], PushBack, Open+1);
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when Open > 0 ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T|Json], PushBack, Open);
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when (Open =:= 0) andalso (
        (T =:= $:) orelse (T =:= $#)
        orelse (T =:= $_) orelse (T =:= $.)
        orelse ((T >= $A) andalso (T =< $Z))
        orelse ((T >= $a) andalso (T =< $z))
        orelse ((T >= $0) andalso (T =< $9))
      ) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    parse_json(TokenLine, TokenChars, [T|Json], PushBack, Open);
parse_json(TokenLine, [T|TokenChars], Json, PushBack, Open)
 when (Open =:= 0) ->
    ?Dbg("T=~p TChrs=~p Json=~p PB=~p Open=~p", [[T], TokenChars, Json, PushBack, Open]),
    {NewTokenChars, NewJson, NewPushBack} =
    case T of
        T when [T]=:=" ";T=:=$\n;T=:=$\t;T=:=$\r -> % white space characters
        case re:run(TokenChars, "^([[:space:]]*)(.*)", [{capture, [1,2], list}, dotall]) of
            {match,[WhiteSpace,[F|_] = Rest]} when F=:=$[;F=:=${;F=:=$( ->
                {Rest, lists:reverse(WhiteSpace)++[T|Json], PushBack};
            _ ->
                {[], Json, [T|TokenChars]}
        end;
        _ -> {[], Json, [T|TokenChars]}
    end,
    ?Dbg("NewTokenChars=~p NewJson=~p NewPushBack=~p", [NewTokenChars, NewJson, NewPushBack]),
    parse_json(TokenLine, NewTokenChars, NewJson, NewPushBack, Open).
