%% -*- erlang -*-
Header "%% Copyright (C) K2 Informatics GmbH"
"%% @private"
"%% @Author Walter Weinmann"
"%% @Email office@k2informatics.ch".

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Nonterminals
 any_cypher_option
% bulk_import_query
 clause
% command
% configuration_option
% cypher_option
 query
 query_options
 regular_query
 root
 single_query
 statement
% version_number
 .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Terminals
% ALL
% ALLSHORTESTPATHS
% AND
% ANY
% AS
% ASC
% ASCENDING
% ASSERT
% BY
% CASE
% COMMIT
% COMPARISON
% CONSTRAINT
% CONTAINS
% COUNT
% CREATE
% CSV
 CYPHER
% DELETE
% DESC
% DESCENDING
% DETACH
% DISTINCT
% DROP
% ELSE
% END
% ENDS
% EXISTS
 EXPLAIN
% EXTRACT
% FALSE
% FIELDTERMINATOR
% FILTER
% FOREACH
% FROM
% HEADERS
% IN
% INDEX
% IS
% JOIN
% L_0X
% L_SKIP
% LIMIT
% LOAD
 MATCH
% MERGE
% NODE
% NONE
% NOT
% NULL
% ON
% OPTIONAL
% OR
% ORDER
% PERIODIC
 PROFILE
% REDUCE
% REL
% RELATIONSHIP
% REMOVE
% RETURN
% SCAN
% SET
% SHORTESTPATH
% SINGLE
% START
% STARTS
% THEN
% TRUE
% UNION
% UNIQUE
% UNSIGNED_DECIMAL_INTEGER
% UNWIND
% USING
% WHEN
% WHERE
% WITH
% XOR
% '+'
% '-'
% '*'
% '/'
 ';'
% '('
% ')'
% ','
% '||'
% '|'
% '.'
% 'div'
.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Rootsymbol 
 root.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Endsymbol
 '$end'.
 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Operator precedences.

% Left        110 'OR'.
% Left        120 'AND'.
% Left        130 'NOT'.
% Nonassoc    200 COMPARISON. %% = <> < > <= >=
% Left        300 '+' '-'.
% Left        400 '*' '/'.
% %Unary      500 '-'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Grammar rules.

root -> query_options statement                                                                 : [{cypher, {'query options', '$1'}, {'statement', '$2'}}].
root -> query_options statement ';'                                                             : [{cypher, {'query options', '$1'}, {'statement', '$2'}}].
root -> statement                                                                               : [{cypher, {'statement', '$1'}}].
root -> statement ';'                                                                           : [{cypher, {'statement', '$1'}}].

query_options -> query_options any_cypher_option                                                : '$1' ++ ['$2'].
query_options -> any_cypher_option                                                              : ['$1'].

% any_cypher_option -> cypher_option                                                              : {'%1'}.
any_cypher_option -> CYPHER                                                                     : {'cypher', []}.
any_cypher_option -> EXPLAIN                                                                    : {'explain', []}.
any_cypher_option -> PROFILE                                                                    : {'profile', []}.

% cypher_option -> CYPHER version_number                                                          : {'cypher', [{'version', '$2'}]}.
% cypher_option -> CYPHER configuration_options                                                   : ['cypher', 'option', ''$2'].
% cypher_option -> cypher_option configuration_options                                            : '$1' ++ ['$2'].

% version_number -> UNSIGNED_DECIMAL_INTEGER '.' UNSIGNED_DECIMAL_INTEGER                         : {'$1', '$3'}.

statement -> query                                                                              : '$1'.

query -> regular_query                                                                          : '$1'.

regular_query -> single_query                                                                   : '$1'.

single_query -> clause                                                                          : '$1'.

clause -> MATCH                                                                                 : 'match'.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Expect 2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Erlang code.

-behaviour(application).
-behaviour(supervisor).

% application callbacks
-export([start/0, start/2, stop/1, stop/0]).

% Supervisor callbacks
-export([init/1]).

% parser and compiler interface
-export([pt_to_string/1, foldtd/3, foldbu/3
         , parsetree/1
         , parsetree_with_tokens/1
         , is_reserved/1]).

-include("ocparse.hrl").

-define(Dbg(__Rule, __Production),
begin
    io:format(user, "__ "??__Rule" (~p)~n", [__Production]),
    __Production
end). 

%%-----------------------------------------------------------------------------
%%                          dummy application interface
%%-----------------------------------------------------------------------------

start() ->
    jpparse:start(),
    application:start(?MODULE).
stop() ->
    application:stop(?MODULE),
    jpparse:stop().

start(_Type, _Args) -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State)        -> ok.

init([])            -> {ok, { {one_for_one, 5, 10}, []} }.

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

jpparse({_,_,X}) -> jpparse(X);
jpparse(X) ->
    {ok, Pt} = jpparse:parsetree(X),
    Pt.

unwrap({_,_,X}) -> X;
unwrap(X) -> X.

unwrap_bin({_,_,X}) when is_list(X) -> list_to_binary([X]);
unwrap_bin({_,_,X}) when is_atom(X) -> atom_to_binary(X, unicode).

strl2atom([]) -> '';
strl2atom(Strs) -> list_to_atom(lists:flatten(string:join([string:to_lower(unwrap(S)) || S <- Strs], " "))).

make_list(L) when is_list(L) -> L;
make_list(L) -> [L].

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  PARSER
%%-----------------------------------------------------------------------------
-spec parsetree(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, [tuple()]}.
parsetree(Cypher) ->
   case parsetree_with_tokens(Cypher) of
       {ok, {ParseTree, _Tokens}} -> {ok, ParseTree};
       Error -> Error
   end.

-spec parsetree_with_tokens(binary()|list()) ->
    {parse_error, term()} | {lex_error, term()} | {ok, {[tuple()], list()}}.
parsetree_with_tokens([]) -> {parse_error, invalid_string};
parsetree_with_tokens(<<>>) -> {parse_error, invalid_string};
parsetree_with_tokens(Cypher0) ->
    Cypher = re:replace(Cypher0, "(^[ \r\n]+)|([ \r\n]+$)", "",
                     [global, {return, list}]),
    [C|_] = lists:reverse(Cypher),
    NCypher = if C =:= $; -> Cypher; true -> string:strip(Cypher) ++ ";" end,
    case opencypher_lex:string(NCypher) of
        {ok, Toks, _} ->
            case ocparse:parse(Toks) of
                {ok, PTree} -> {ok, {PTree, Toks}};
                {error,{N,?MODULE,ErrorTerms}} ->
                    {parse_error, {lists:flatten([integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error,Error} -> {parse_error, {Error, Toks}}
            end;
        {error,Error,_} -> {lex_error, Error}
    end.

-spec is_reserved(binary() | atom() | list()) -> true | false.
is_reserved(Word) when is_binary(Word) ->
    is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word) ->
    is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word) ->
    lists:member(erlang:list_to_atom(string:to_upper(Word)),
                 opencypher_lex:reserved_keywords()).

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  COMPILER
%%-----------------------------------------------------------------------------

-spec pt_to_string(tuple()| list()) -> {error, term()} | binary().
pt_to_string(PTree) -> foldtd(fun(_,_) -> null_fun end, null_fun, PTree).

-spec foldtd(fun(), term(), tuple() | list()) -> {error, term()} | binary().
foldtd(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    ?LogDebug("wwe debugging foldtd/3 ===> Start ~n PTree: ~p~n", [PTree]),
    try ocparse_fold:fold(top_down, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Cypher, null_fun = Ctx} -> 
            ?LogDebug("wwe debugging foldtd/3 ===> ~n Cypher: ~p~n", [Cypher]),
            list_to_binary(string:strip(Cypher));
        {_Output, NewCtx} -> 
            ?LogDebug("wwe debugging foldtd/3 ===> ~n NewCtx: ~p~n", [NewCtx]),
            NewCtx
    catch
        _:Error -> {error, Error}
    end.

-spec foldbu(fun(), term(), tuple()) -> {error, term()} | binary().
foldbu(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_fold:fold(bottom_up, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Cypher, null_fun = Ctx} -> list_to_binary(string:strip(Cypher));
        {_Output, NewCtx} -> NewCtx
    catch
        _:Error -> {error, Error}
    end.
