%% Copyright (C) Walter Weinmann
%% @private
%% @Author Walter Weinmann
-module(ocparse_legacy).
-export([parse/1, parse_and_scan/1, format_error/1]).
-file("src/ocparse_legacy.yrl", 1171).


-behaviour(application).
-behaviour(supervisor).

% application callbacks
-export([start/0, start/2, stop/1, stop/0]).

% Supervisor callbacks
-export([init/1]).

% parser and compiler interface
-export([fold/3,
         fold_bu/3,
         fold_td/3, 
         is_reserved/1,
         parsetree/1,
         parsetree_to_string/1,
         parsetree_to_string_bu/1,
         parsetree_to_string_td/1,
         parsetree_with_tokens/1
        ]).

%-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-define(Dbg(__Rule, __Production),
begin
    io:format(user, "__ "??__Rule" (~p)~n", [__Production]),
    __Production
end). 

%%-----------------------------------------------------------------------------
%%                          dummy application interface
%%-----------------------------------------------------------------------------

start() ->
    application:start(?MODULE).
stop() ->
    application:stop(?MODULE).

start(_Type, _Args) -> 
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
stop(_State) -> 
    ok.

init([]) ->
     {ok, { {one_for_one, 5, 10}, []} }.

%%-----------------------------------------------------------------------------
%%                          parser helper functions
%%-----------------------------------------------------------------------------

unwrap({_,_,X}) -> 
    X.

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
    NCypher = if C =:= $; -> Cypher; true -> string:strip(Cypher) end,
    case oclexer_legacy:string(NCypher) of
        {ok, Toks, _} ->
            case ocparse_legacy:parse(Toks) of
                {ok, PTree} -> {ok, {PTree, Toks}};
                {error,{N,?MODULE,ErrorTerms}} ->
                    {parse_error, {lists:flatten([integer_to_list(N), ": ", ErrorTerms]), Toks}};
                {error,Error} -> {parse_error, {Error, Toks}}
            end;
        {error,Error,_} -> {lex_error, Error}
    end.

-spec is_reserved(binary() | atom() | list()) ->
    true | false.
is_reserved(Word) when is_binary(Word) ->
    is_reserved(erlang:binary_to_list(Word));
is_reserved(Word) when is_atom(Word) ->
    is_reserved(erlang:atom_to_list(Word));
is_reserved(Word) when is_list(Word) ->
    lists:member(erlang:list_to_atom(string:to_upper(Word)),
                 oclexer_legacy:reserved_keywords()).

%%-----------------------------------------------------------------------------


%%-----------------------------------------------------------------------------
%%                                  COMPILER
%%-----------------------------------------------------------------------------

-spec parsetree_to_string(tuple()| list()) ->
    {error, term()} | binary().
parsetree_to_string(PTree) ->
    parsetree_to_string_td(PTree).

-spec parsetree_to_string_bu(tuple()| list()) ->
    {error, term()} | binary().
parsetree_to_string_bu(PTree) ->
    fold_bu(fun(_,_) -> null_fun end, null_fun, PTree).

-spec parsetree_to_string_td(tuple()| list()) ->
    {error, term()} | binary().
parsetree_to_string_td(PTree) ->
    fold_td(fun(_,_) -> null_fun end, null_fun, PTree).

-spec fold(fun(), term(), tuple()) ->
    {error, term()} | binary().
fold(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    fold_td(Fun, Ctx, PTree). 
    
-spec fold_bu(fun(), term(), tuple()) ->
    {error, term()} | binary().
fold_bu(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_fold_legacy:fold(bottom_up, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Cypher, null_fun = Ctx} -> list_to_binary(string:strip(Cypher));
        {_Output, NewCtx} -> NewCtx
    catch
        _:Error -> {error, Error}
    end.

-spec fold_td(fun(), term(), tuple() | list()) ->
    {error, term()} | binary().
fold_td(Fun, Ctx, PTree) when is_function(Fun, 2) ->
    try ocparse_fold_legacy:fold(top_down, Fun, Ctx, 0, PTree) of
        {error,_} = Error -> Error;
        {Cypher, null_fun = Ctx} -> 
            list_to_binary(string:strip(Cypher));
        {_Output, NewCtx} -> 
            NewCtx
    catch
        _:Error -> {error, Error}
    end.

-file("c:/Software/erl8.0/lib/parsetools-2.1.2/include/yeccpre.hrl", 0).
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2015. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The parser generator will insert appropriate declarations before this line.%

-type yecc_ret() :: {'error', _} | {'ok', _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) ->
    yeccpars0(Tokens, {no_func, no_line}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]}
                     | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) ->
    yeccpars0([], {{F, A}, no_line}, 0, [], []);
parse_and_scan({M, F, A}) ->
    Arity = length(A),
    yeccpars0([], {{fun M:F/Arity, A}, no_line}, 0, [], []).

-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true ->
            Message;
        _ ->
            io_lib:write(Message)
    end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!
-compile({nowarn_unused_function, return_error/2}).
-spec return_error(integer(), any()) -> no_return().
return_error(Line, Message) ->
    throw({error, {Line, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) ->
    try yeccpars1(Tokens, Tzr, State, States, Vstack)
    catch 
        error: Error ->
            Stacktrace = erlang:get_stacktrace(),
            try yecc_error_type(Error, Stacktrace) of
                Desc ->
                    erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc},
                                 Stacktrace)
            catch _:_ -> erlang:raise(error, Error, Stacktrace)
            end;
        %% Probably thrown from return_error/2:
        throw: {error, {_Line, ?MODULE, _M}} = Error ->
            Error
    end.

yecc_error_type(function_clause, [{?MODULE,F,ArityOrArgs,_} | _]) ->
    case atom_to_list(F) of
        "yeccgoto_" ++ SymbolL ->
            {ok,[{atom,_,Symbol}],_} = erl_scan:string(SymbolL),
            State = case ArityOrArgs of
                        [S,_,_,_,_,_,_] -> S;
                        _ -> state_is_unknown
                    end,
            {Symbol, State, missing_in_goto_table}
    end.

yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
    yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);
yeccpars1([], {{F, A},_Line}, State, States, Vstack) ->
    case apply(F, A) of
        {ok, Tokens, Endline} ->
            yeccpars1(Tokens, {{F, A}, Endline}, State, States, Vstack);
        {eof, Endline} ->
            yeccpars1([], {no_func, Endline}, State, States, Vstack);
        {error, Descriptor, _Endline} ->
            {error, Descriptor}
    end;
yeccpars1([], {no_func, no_line}, State, States, Vstack) ->
    Line = 999999,
    yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [],
              {no_func, Line});
yeccpars1([], {no_func, Endline}, State, States, Vstack) ->
    yeccpars2(State, '$end', States, Vstack, yecc_end(Endline), [],
              {no_func, Endline}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).
yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
    yeccpars2(State, element(1, Token), [State1 | States],
              [Token0 | Vstack], Token, Tokens, Tzr);
yeccpars1(State1, State, States, Vstack, Token0, [], {{_F,_A}, _Line}=Tzr) ->
    yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_line}) ->
    Line = yecctoken_end_location(Token0),
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line});
yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Line}) ->
    yeccpars2(State, '$end', [State1 | States], [Token0 | Vstack],
              yecc_end(Line), [], {no_func, Line}).

%% For internal use only.
yecc_end({Line,_Column}) ->
    {'$end', Line};
yecc_end(Line) ->
    {'$end', Line}.

yecctoken_end_location(Token) ->
    try erl_anno:end_location(element(2, Token)) of
        undefined -> yecctoken_location(Token);
        Loc -> Loc
    catch _:_ -> yecctoken_location(Token)
    end.

-compile({nowarn_unused_function, yeccerror/1}).
yeccerror(Token) ->
    Text = yecctoken_to_string(Token),
    Location = yecctoken_location(Token),
    {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).
yecctoken_to_string(Token) ->
    try erl_scan:text(Token) of
        undefined -> yecctoken2string(Token);
        Txt -> Txt
    catch _:_ -> yecctoken2string(Token)
    end.

yecctoken_location(Token) ->
    try erl_scan:location(Token)
    catch _:_ -> element(2, Token)
    end.

-compile({nowarn_unused_function, yecctoken2string/1}).
yecctoken2string({atom, _, A}) -> io_lib:write(A);
yecctoken2string({integer,_,N}) -> io_lib:write(N);
yecctoken2string({float,_,F}) -> io_lib:write(F);
yecctoken2string({char,_,C}) -> io_lib:write_char(C);
yecctoken2string({var,_,V}) -> io_lib:format("~s", [V]);
yecctoken2string({string,_,S}) -> io_lib:write_string(S);
yecctoken2string({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string({_Cat, _, Val}) -> io_lib:format("~p",[Val]);
yecctoken2string({dot, _}) -> "'.'";
yecctoken2string({'$end', _}) ->
    [];
yecctoken2string({Other, _}) when is_atom(Other) ->
    io_lib:write(Other);
yecctoken2string(Other) ->
    io_lib:write(Other).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



-file("src/ocparse_legacy.erl", 332).

-dialyzer({nowarn_function, yeccpars2/7}).
yeccpars2(0=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(18=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(20=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(21=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(22=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(23=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(24=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(25=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(26=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(27=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(28=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(29=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(30=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(36=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(38=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(39=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(40=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(41=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(42=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(49=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(50=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(67=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(69=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(71=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(72=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_72(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(74=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(75=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(77=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(78=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_78(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(80=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_82(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(84=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(86=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(89=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(90=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_90(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(93=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(94=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(95=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_98(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(101=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(102=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(104=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(106=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(107=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(110=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(111=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(112=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(113=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_113(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(114=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_114(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(115=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_115(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(116=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_116(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(117=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_117(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(118=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_118(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(119=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_119(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(120=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_120(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(121=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_121(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(122=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_122(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(123=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_123(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(124=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_124(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(125=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_125(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(126=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_126(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(127=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_127(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(128=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_128(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(129=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_129(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(130=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_130(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(131=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_131(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(132=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_132(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(133=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_133(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(134=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_134(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(135=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_135(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(136=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_136(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(137=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_137(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(138=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_138(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(139=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_139(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(140=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_140(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(141=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_141(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(142=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_142(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(143=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_143(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(144=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_144(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(145=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_145(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(146=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_146(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(147=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_147(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(148=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_148(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(149=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_149(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(150=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_150(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(151=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_151(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(152=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_152(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(153=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_153(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(154=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_154(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(155=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_155(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(156=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_156(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(157=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_157(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(158=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_158(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(159=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_159(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(160=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_160(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(161=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_161(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(162=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_162(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(163=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_163(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(164=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_164(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(165=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_165(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(166=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_166(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(167=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_167(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(168=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_168(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(169=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_169(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(170=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_170(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(171=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_171(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(172=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_172(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(173=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_173(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(174=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_174(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(175=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_175(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(176=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_176(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(177=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_177(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(178=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_178(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(179=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_179(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(180=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_180(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(181=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_181(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(182=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_182(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(183=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(184=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_184(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(185=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_185(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(186=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_186(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(187=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_187(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(188=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_188(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(189=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_189(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(190=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_190(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(191=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_191(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(192=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_192(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(193=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_193(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(194=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_194(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(195=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_195(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(196=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_196(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(197=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_197(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(198=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_198(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(199=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_199(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(200=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_200(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(201=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_201(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(202=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_202(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(203=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_203(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(204=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_204(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(205=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_205(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(206=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_206(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(207=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(208=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_208(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(209=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_209(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(210=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(211=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_211(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(212=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_212(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(213=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(214=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_214(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(215=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_215(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(216=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_216(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(217=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_217(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(218=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_218(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(219=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_219(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(220=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_220(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(221=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_221(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(222=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(223=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_223(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(224=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_224(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(225=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_225(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(226=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_226(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(227=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_227(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(228=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_228(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(229=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_229(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(230=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_230(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(231=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_231(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(232=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_232(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(233=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_233(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(234=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_234(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(235=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_235(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(236=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_236(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(237=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_237(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(238=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_238(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(239=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_239(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(240=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_240(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(241=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_241(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(242=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_242(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(243=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_243(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(244=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_244(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(245=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_245(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(246=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_246(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(247=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_247(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(248=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_248(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(249=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_249(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(250=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_250(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(251=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_251(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(252=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_252(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(253=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_253(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(254=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_254(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(255=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_255(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(256=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_256(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(257=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(258=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_258(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(259=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_259(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(260=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_260(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(261=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_261(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(262=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_262(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(263=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_263(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(264=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_264(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(265=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_265(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(266=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_266(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(267=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_267(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(268=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_268(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(269=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_269(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(270=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_270(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(271=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_271(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(272=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_272(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(273=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_273(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(274=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_274(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(275=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_275(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(276=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_276(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(277=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_277(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(278=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_278(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(279=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_279(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(280=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_280(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(281=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_281(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(282=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_282(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(283=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_283(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(284=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_284(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(285=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_285(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(286=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_286(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(287=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(288=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_288(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(289=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(290=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_290(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(291=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_291(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(292=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_292(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(293=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(294=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_294(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(295=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_295(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(296=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_296(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(297=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_297(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(298=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(299=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_299(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(300=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_300(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(301=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(302=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_302(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(303=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_303(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(304=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(305=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_305(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(306=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_306(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(307=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_307(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(308=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_308(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(309=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_309(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(310=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_310(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(311=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_311(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(312=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_312(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(313=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_313(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(314=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_314(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(315=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_315(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(316=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_316(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(317=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_317(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(318=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_318(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(319=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_319(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(320=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_320(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(321=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_321(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(322=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_322(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(323=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_323(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(324=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_324(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(325=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_325(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(326=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_326(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(327=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_327(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(328=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_328(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(329=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_329(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(330=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_330(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(331=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_331(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(332=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_332(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(333=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_333(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(334=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_334(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(335=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(336=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_336(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(337=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_337(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(338=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_210(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(339=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_339(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(340=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_340(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(341=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(342=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_342(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(343=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_343(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(344=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_344(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(345=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_345(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(346=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_346(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(347=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_347(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(348=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_348(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(349=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_349(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(350=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_350(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(351=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(352=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_352(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(353=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_353(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(354=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_354(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(355=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_355(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(356=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_356(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(357=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_357(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(358=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_358(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(359=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_359(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(360=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(361=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(362=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_362(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(363=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(364=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_364(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(365=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_365(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(366=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_366(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(367=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_367(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(368=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_368(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(369=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_369(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(370=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_370(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(371=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_371(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(372=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_372(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(373=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(374=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_374(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(375=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_375(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(376=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_376(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(377=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_377(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(378=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_378(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(379=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(380=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_380(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(381=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_381(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(382=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_382(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(383=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_383(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(384=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_384(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(385=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_385(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(386=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_386(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(387=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_387(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(388=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_388(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(389=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_389(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(390=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_390(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(391=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_391(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(392=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_392(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(393=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_393(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(394=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_394(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(395=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_395(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(396=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_396(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(397=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_397(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(398=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_398(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(399=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_399(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(400=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_400(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(401=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_401(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(402=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_402(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(403=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_403(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(404=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_404(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(405=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_405(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(406=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_406(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(407=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_407(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(408=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_408(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(409=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_409(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(410=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_410(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(411=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_411(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(412=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_412(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(413=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_413(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(414=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_414(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(415=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_415(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(416=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_416(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(417=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_417(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(418=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_418(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(419=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_419(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(420=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_420(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(421=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_421(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(422=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_422(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(423=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_423(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(424=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_424(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(425=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_425(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(426=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_426(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(427=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_427(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(428=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_428(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(429=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_429(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(430=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_430(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(431=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_431(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(432=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_432(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(433=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_433(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(434=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_434(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(435=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_435(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(436=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_436(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(437=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_437(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(438=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_438(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(439=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_439(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(440=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_440(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(441=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_441(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(442=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_442(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(443=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_443(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(444=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_444(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(445=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_445(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(446=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_446(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(447=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_447(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(448=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_448(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(449=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_449(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(450=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(451=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_451(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(452=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_452(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(453=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_453(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(454=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_454(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(455=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(456=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_456(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(457=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_457(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(458=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_458(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(459=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_459(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(460=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_460(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(461=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_461(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(462=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(463=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(464=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_462(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(465=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_465(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(466=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_466(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(467=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_467(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(468=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_468(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(469=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(470=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_470(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(471=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_471(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(472=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_472(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(473=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_473(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(474=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_474(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(475=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_475(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(476=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_476(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(477=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_477(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(478=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_478(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(479=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_479(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(480=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_480(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(481=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_481(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(482=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_482(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(483=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_483(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(484=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_484(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(485=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_485(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(486=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_486(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(487=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_487(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(488=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_488(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(489=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_489(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(490=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_490(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(491=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_491(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(492=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_492(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(493=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_493(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(494=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_494(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(495=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_495(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(496=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_496(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(497=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_497(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(498=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_498(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(499=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_499(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(500=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_500(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(501=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_501(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(502=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_502(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(503=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_503(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(504=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_504(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(505=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_505(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(506=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_506(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(507=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_507(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(508=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_508(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(509=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_509(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(510=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(511=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_511(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(512=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_512(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(513=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_513(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(514=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_514(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(515=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_515(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(516=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_516(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(517=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_517(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(518=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_518(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(519=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_519(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(520=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_520(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(521=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_521(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(522=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_522(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(523=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(524=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_524(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(525=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_525(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(526=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_526(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(527=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_527(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(528=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_528(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(529=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_529(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(530=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_530(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(531=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_531(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(532=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_532(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(533=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_533(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(534=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_534(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(535=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_535(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(536=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_535(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(537=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_537(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(538=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_538(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(539=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_539(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(540=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_540(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(541=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_541(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(542=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_542(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(543=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_543(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(544=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_544(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(545=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_545(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(546=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_546(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(547=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_547(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(548=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(549=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_549(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(550=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_550(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(551=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_551(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(552=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_552(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(553=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(554=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_554(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(555=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_555(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(556=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_556(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(557=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_557(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(558=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_558(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(559=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_559(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(560=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(561=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_561(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(562=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_562(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(563=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_563(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(564=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_564(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(565=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(566=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_566(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(567=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_567(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(568=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_568(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(569=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_569(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(570=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_570(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(571=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_571(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(572=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_572(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(573=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_573(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(574=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_574(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(575=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(576=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_576(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(577=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(578=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_578(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(579=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_579(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(580=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_580(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(581=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_581(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(582=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_582(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(583=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_575(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(584=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_584(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(585=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_585(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(586=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_586(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(587=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_587(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(588=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(589=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_589(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(590=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_590(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(591=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(592=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_592(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(593=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_593(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(594=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_594(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(595=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_595(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(596=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_596(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(597=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(598=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_589(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(599=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_599(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(600=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_600(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(601=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_601(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(602=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_602(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(603=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_603(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(604=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_604(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(605=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_605(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(606=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_606(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(607=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_607(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(608=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_608(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(609=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_609(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(610=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_610(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(611=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_611(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(612=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_612(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(613=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_613(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(614=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_614(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(615=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_615(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(616=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_616(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(617=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_617(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(618=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_618(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(619=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(620=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_620(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(621=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_621(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(622=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_622(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(623=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_623(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(624=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_624(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(625=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_625(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(626=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_626(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(627=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_627(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(628=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(629=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_629(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(630=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_630(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(631=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_631(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(632=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_632(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(633=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_633(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(634=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(635=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_635(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(636=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(637=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_575(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(638=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_638(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(639=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(640=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_640(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(641=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_641(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(642=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(643=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_643(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(644=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_575(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(645=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_645(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(646=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(647=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_647(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(648=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_648(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(649=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_649(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(650=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_650(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(651=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_651(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(652=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_652(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(653=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_653(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(654=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_654(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(655=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_655(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(656=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_656(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(657=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_657(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(658=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_658(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(659=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_659(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(660=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_660(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(661=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_661(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(662=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_662(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(663=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_663(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(664=S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_664(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(Other, _, _, _, _, _, _) ->
 erlang:error({yecc_bug,"1.4",{missing_state_in_action_table, Other}}).

yeccpars2_0(S, 'CYPHER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'EXPLAIN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, 'PROFILE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_0_(Stack),
 yeccpars2_1(1, Cat, [0 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_1(S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'DETACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'DROP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'FOREACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'MERGE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'OPTIONAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'REMOVE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'RETURN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'START', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'UNWIND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'USING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_1(_S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_DELETE(Stack),
 yeccpars2_42(42, 'DELETE', [1 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_1_(Stack),
 yeccpars2_33(33, Cat, [1 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_2_(Stack),
 yeccgoto_any_cypher_option(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_3/7}).
yeccpars2_3(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) ->
 {ok, hd(Stack)};
yeccpars2_3(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_4(S, 'CYPHER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 6, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'EXPLAIN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 7, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(S, 'PROFILE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 8, Ss, Stack, T, Ts, Tzr);
yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_4_(Stack),
 yeccgoto_query_options(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_5_(Stack),
 yeccgoto_any_cypher_option_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_6(S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 11, Ss, Stack, T, Ts, Tzr);
yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_6_(Stack),
 yeccpars2_9(9, Cat, [6 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_7_(Stack),
 yeccgoto_any_cypher_option(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_8(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_8_(Stack),
 yeccgoto_any_cypher_option(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_9(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_9_(Stack),
 yeccpars2_13(_S, Cat, [9 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_version_number_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_11_(Stack),
 yeccgoto_version_number(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_12/7}).
yeccpars2_12(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_12(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_13_(Stack),
 yeccgoto_cypher_option(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_14(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_configuration_option_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_15_(Stack),
 yeccgoto_configuration_option_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_16(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_16_(Stack),
 yeccgoto_symbolic_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_17(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_17_(Stack),
 yeccgoto_symbolic_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_18_(Stack),
 yeccgoto_configuration_option_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_19/7}).
yeccpars2_19(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_20_(Stack),
 yeccgoto_configuration_option(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_21_(Stack),
 yeccgoto_any_cypher_option_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_22_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_23_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_24(S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 664, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_24_(Stack),
 yeccpars2_663(_S, Cat, [24 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_25_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_26(S, 'UNION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 658, Ss, Stack, T, Ts, Tzr);
yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_26_(Stack),
 yeccpars2_655(_S, Cat, [26 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_27_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_28_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_29_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_30_(Stack),
 yeccgoto_query(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_statement(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).
yeccpars2_32(S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_32(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_33/7}).
yeccpars2_33(S, 'MATCH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 628, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_34_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_35_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_36_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_37_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_38_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_39_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_40_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_41_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_42/7}).
yeccpars2_42(S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 626, Ss, Stack, T, Ts, Tzr);
yeccpars2_42(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_43_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_44_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_45_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_46_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_47_(Stack),
 yeccgoto_command(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_48_(Stack),
 yeccgoto_clause(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_statement(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_50(S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'DETACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'FOREACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'MERGE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'OPTIONAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'REMOVE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'RETURN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'START', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'UNWIND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_50_$end'(Stack),
 yeccgoto_single_query(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_50(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_50_;'(Stack),
 yeccgoto_single_query(hd(Ss), ';', Ss, NewStack, T, Ts, Tzr);
yeccpars2_50(_S, 'UNION', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_UNION(Stack),
 yeccgoto_single_query(hd(Ss), 'UNION', Ss, NewStack, T, Ts, Tzr);
yeccpars2_50(_S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_DELETE(Stack),
 yeccpars2_42(42, 'DELETE', [50 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_50(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_50_(Stack),
 yeccpars2_33(33, Cat, [50 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_51_(Stack),
 yeccgoto_clause_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_52_(Stack),
 yeccgoto_query(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_53(S, 'CONSTRAINT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 573, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'INDEX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 574, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, 'UNIQUE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 561, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_53_(Stack),
 yeccpars2_58(560, Cat, [53 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_54(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_54_(Stack),
 yeccgoto_detach_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_55/7}).
yeccpars2_55(S, 'CONSTRAINT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 573, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(S, 'INDEX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 574, Ss, Stack, T, Ts, Tzr);
yeccpars2_55(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_56/7}).
yeccpars2_56(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 553, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_57/7}).
yeccpars2_57(S, 'CSV', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 542, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_58(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_59(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_59_(Stack),
 yeccgoto_optional_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_60(S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_60(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_60/7}).
yeccpars2_cont_60(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'ANY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'CASE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'COUNT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'EXPONENT_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'EXTRACT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'FILTER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'NONE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'REDUCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'SIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'SINGLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_60(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_61(S, 'DISTINCT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_61_(Stack),
 yeccpars2_517(517, Cat, [61 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_62: see yeccpars2_60

%% yeccpars2_63: see yeccpars2_19

yeccpars2_64(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_64_(Stack),
 yeccpars2_73(73, Cat, [64 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_65/7}).
yeccpars2_65(S, 'PERIODIC', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 445, Ss, Stack, T, Ts, Tzr);
yeccpars2_65(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_66(S, 'DISTINCT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_66_(Stack),
 yeccpars2_67(67, Cat, [66 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_67(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_67_(Stack),
 yeccpars2_73(73, Cat, [67 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_68(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_68_(Stack),
 yeccgoto_distinct_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_69(S, 'ORDER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 426, Ss, Stack, T, Ts, Tzr);
yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_69_(Stack),
 yeccpars2_424(424, Cat, [69 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_70_(Stack),
 yeccgoto_return_items(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_71(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 422, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_71_(Stack),
 yeccgoto_return_item_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_72(S, 'WHERE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_72(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_72_(Stack),
 yeccpars2_421(_S, Cat, [72 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_73(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_73_(Stack),
 yeccpars2_60(111, Cat, [73 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_74(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_9_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_75_(Stack),
 yeccgoto_expression_9_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_76(S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_76(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_76_(Stack),
 yeccpars2_100(_S, Cat, [76 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_77_(Stack),
 yeccgoto_expression(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_78(S, 'OR', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_78(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_78_(Stack),
 yeccpars2_94(_S, Cat, [78 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_79(S, 'XOR', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_79_(Stack),
 yeccpars2_88(_S, Cat, [79 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_80(S, 'AS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_80_(Stack),
 yeccgoto_return_item(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_81(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 83, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_81_(Stack),
 yeccgoto_return_items(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_82(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_82_(Stack),
 yeccgoto_expression_9_addon(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_83(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_83_(Stack),
 yeccpars2_73(73, Cat, [83 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_84_(Stack),
 yeccgoto_return_items(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_85: see yeccpars2_19

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_86_(Stack),
 yeccgoto_return_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_87_(Stack),
 yeccgoto_variable(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_88_(Stack),
 yeccgoto_expression_11(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_89(S, 'XOR', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_11_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_90_(Stack),
 yeccgoto_expression_11_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_91(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_91_(Stack),
 yeccpars2_73(73, Cat, [91 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_92_(Stack),
 yeccgoto_expression_11_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_93_(Stack),
 yeccgoto_expression_11_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_94_(Stack),
 yeccgoto_expression_12(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_95(S, 'OR', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_12_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_96_(Stack),
 yeccgoto_expression_12_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_97(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_97_(Stack),
 yeccpars2_73(73, Cat, [97 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_98_(Stack),
 yeccgoto_expression_12_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_99_(Stack),
 yeccgoto_expression_12_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_100_(Stack),
 yeccgoto_expression_10(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_101(S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_10_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_102_(Stack),
 yeccgoto_expression_10_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_103(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_103_(Stack),
 yeccpars2_73(73, Cat, [103 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_104_(Stack),
 yeccgoto_expression_10_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_105_(Stack),
 yeccgoto_expression_10_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_106_(Stack),
 yeccgoto_expression_9_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_107_(Stack),
 yeccgoto_expression_9(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_108(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 412, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 413, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 416, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 417, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 418, Ss, Stack, T, Ts, Tzr);
yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_108_(Stack),
 yeccpars2_408(_S, Cat, [108 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_109(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr);
yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_109_(Stack),
 yeccpars2_399(_S, Cat, [109 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_110(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 393, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 394, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_110_(Stack),
 yeccpars2_389(_S, Cat, [110 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_111: see yeccpars2_60

yeccpars2_112(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_4_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_113_(Stack),
 yeccgoto_expression_4_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_114(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_114(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_114_(Stack),
 yeccpars2_117(_S, Cat, [114 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_115(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_115_(Stack),
 yeccgoto_expression_4_addon(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_116(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_116_(Stack),
 yeccgoto_expression_4_addon(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_117_(Stack),
 yeccgoto_expression_5(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_118(S, '^', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 120, Ss, Stack, T, Ts, Tzr);
yeccpars2_118(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_5_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_119_(Stack),
 yeccgoto_expression_5_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_120(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_120(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_120_(Stack),
 yeccpars2_60(111, Cat, [120 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_121_(Stack),
 yeccgoto_expression_5_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_122_(Stack),
 yeccgoto_expression_5_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_123_(Stack),
 yeccgoto_expression_4_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_124_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_125(_S, '!=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_!='(Stack),
 yeccgoto_variable(hd(Ss), '!=', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_$end'(Stack),
 yeccgoto_variable(hd(Ss), '$end', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '%', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_%'(Stack),
 yeccgoto_variable(hd(Ss), '%', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, ')', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_)'(Stack),
 yeccgoto_variable(hd(Ss), ')', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '*', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_*'(Stack),
 yeccgoto_variable(hd(Ss), '*', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_+'(Stack),
 yeccgoto_variable(hd(Ss), '+', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '+=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_+='(Stack),
 yeccgoto_variable(hd(Ss), '+=', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, ',', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_,'(Stack),
 yeccgoto_variable(hd(Ss), ',', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_-'(Stack),
 yeccgoto_variable(hd(Ss), '-', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '.', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_.'(Stack),
 yeccgoto_variable(hd(Ss), '.', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_..'(Stack),
 yeccgoto_variable(hd(Ss), '..', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '/', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_/'(Stack),
 yeccgoto_variable(hd(Ss), '/', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, ':', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_:'(Stack),
 yeccgoto_variable(hd(Ss), ':', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_;'(Stack),
 yeccgoto_variable(hd(Ss), ';', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '<', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_<'(Stack),
 yeccgoto_variable(hd(Ss), '<', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '<=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_<='(Stack),
 yeccgoto_variable(hd(Ss), '<=', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '<>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_<>'(Stack),
 yeccgoto_variable(hd(Ss), '<>', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_='(Stack),
 yeccgoto_variable(hd(Ss), '=', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '=~', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_=~'(Stack),
 yeccgoto_variable(hd(Ss), '=~', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '>', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_>'(Stack),
 yeccgoto_variable(hd(Ss), '>', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '>=', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_>='(Stack),
 yeccgoto_variable(hd(Ss), '>=', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'AND', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_AND(Stack),
 yeccgoto_variable(hd(Ss), 'AND', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'AS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_AS(Stack),
 yeccgoto_variable(hd(Ss), 'AS', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'ASC', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_ASC(Stack),
 yeccgoto_variable(hd(Ss), 'ASC', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'ASCENDING', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_ASCENDING(Stack),
 yeccgoto_variable(hd(Ss), 'ASCENDING', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'CONTAINS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_CONTAINS(Stack),
 yeccgoto_variable(hd(Ss), 'CONTAINS', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_CREATE(Stack),
 yeccgoto_variable(hd(Ss), 'CREATE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_DELETE(Stack),
 yeccgoto_variable(hd(Ss), 'DELETE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'DESC', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_DESC(Stack),
 yeccgoto_variable(hd(Ss), 'DESC', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'DESCENDING', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_DESCENDING(Stack),
 yeccgoto_variable(hd(Ss), 'DESCENDING', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'DETACH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_DETACH(Stack),
 yeccgoto_variable(hd(Ss), 'DETACH', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'ELSE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_ELSE(Stack),
 yeccgoto_variable(hd(Ss), 'ELSE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'END', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_END(Stack),
 yeccgoto_variable(hd(Ss), 'END', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'ENDS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_ENDS(Stack),
 yeccgoto_variable(hd(Ss), 'ENDS', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'FOREACH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_FOREACH(Stack),
 yeccgoto_variable(hd(Ss), 'FOREACH', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'IN', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_IN(Stack),
 yeccgoto_variable(hd(Ss), 'IN', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'IS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_IS(Stack),
 yeccgoto_variable(hd(Ss), 'IS', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'LIMIT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_LIMIT(Stack),
 yeccgoto_variable(hd(Ss), 'LIMIT', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_LOAD(Stack),
 yeccgoto_variable(hd(Ss), 'LOAD', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'MATCH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_MATCH(Stack),
 yeccgoto_variable(hd(Ss), 'MATCH', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'MERGE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_MERGE(Stack),
 yeccgoto_variable(hd(Ss), 'MERGE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'ON', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_ON(Stack),
 yeccgoto_variable(hd(Ss), 'ON', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'OPTIONAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_OPTIONAL(Stack),
 yeccgoto_variable(hd(Ss), 'OPTIONAL', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'OR', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_OR(Stack),
 yeccgoto_variable(hd(Ss), 'OR', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'ORDER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_ORDER(Stack),
 yeccgoto_variable(hd(Ss), 'ORDER', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'REMOVE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_REMOVE(Stack),
 yeccgoto_variable(hd(Ss), 'REMOVE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'RETURN', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_RETURN(Stack),
 yeccgoto_variable(hd(Ss), 'RETURN', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_SET(Stack),
 yeccgoto_variable(hd(Ss), 'SET', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'SKIP', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_SKIP(Stack),
 yeccgoto_variable(hd(Ss), 'SKIP', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'START', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_START(Stack),
 yeccgoto_variable(hd(Ss), 'START', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'STARTS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_STARTS(Stack),
 yeccgoto_variable(hd(Ss), 'STARTS', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'THEN', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_THEN(Stack),
 yeccgoto_variable(hd(Ss), 'THEN', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'UNION', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_UNION(Stack),
 yeccgoto_variable(hd(Ss), 'UNION', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'UNWIND', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_UNWIND(Stack),
 yeccgoto_variable(hd(Ss), 'UNWIND', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'WHEN', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_WHEN(Stack),
 yeccgoto_variable(hd(Ss), 'WHEN', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'WHERE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_WHERE(Stack),
 yeccgoto_variable(hd(Ss), 'WHERE', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_WITH(Stack),
 yeccgoto_variable(hd(Ss), 'WITH', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, 'XOR', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_XOR(Stack),
 yeccgoto_variable(hd(Ss), 'XOR', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_['(Stack),
 yeccgoto_variable(hd(Ss), '[', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, ']', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_]'(Stack),
 yeccgoto_variable(hd(Ss), ']', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '^', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_^'(Stack),
 yeccgoto_variable(hd(Ss), '^', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '|', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_|'(Stack),
 yeccgoto_variable(hd(Ss), '|', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, '}', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_125_}'(Stack),
 yeccgoto_variable(hd(Ss), '}', Ss, NewStack, T, Ts, Tzr);
yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_125_(Stack),
 yeccgoto_function_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_126_(Stack),
 yeccgoto_number_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_127_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_128_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_129_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_130_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_131_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_132_(Stack),
 yeccgoto_signed_integer_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_133_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_134/7}).
yeccpars2_134(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, '-->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, '<--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(S, '<-->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_134(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_135_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_136_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_137_(Stack),
 yeccgoto_signed_integer_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_138/7}).
yeccpars2_138(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 384, Ss, Stack, T, Ts, Tzr);
yeccpars2_138(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_139_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_140_(Stack),
 yeccgoto_expression_4(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_141(S, '=~', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, 'CONTAINS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, 'ENDS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, 'IN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, 'IS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, 'STARTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr);
yeccpars2_141(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_141_(Stack),
 yeccpars2_357(_S, Cat, [141 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_142_(Stack),
 yeccgoto_number_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_143_(Stack),
 yeccgoto_signed_integer_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_144_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_145(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_145(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_145_(Stack),
 yeccpars2_348(_S, Cat, [145 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_146(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_146(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_146_(Stack),
 yeccpars2_73(73, Cat, [146 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_147/7}).
yeccpars2_147(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 341, Ss, Stack, T, Ts, Tzr);
yeccpars2_147(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_148/7}).
yeccpars2_148(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 338, Ss, Stack, T, Ts, Tzr);
yeccpars2_148(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_149/7}).
yeccpars2_149(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 335, Ss, Stack, T, Ts, Tzr);
yeccpars2_149(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_150(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_150(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_150_('(Stack),
 yeccpars2_73(73, '(', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_150_+'(Stack),
 yeccpars2_73(73, '+', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_150_-'(Stack),
 yeccpars2_73(73, '-', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_ALL(Stack),
 yeccpars2_73(73, 'ALL', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_ALLSHORTESTPATHS(Stack),
 yeccpars2_73(73, 'ALLSHORTESTPATHS', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'ANY', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_ANY(Stack),
 yeccpars2_73(73, 'ANY', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'CASE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_CASE(Stack),
 yeccpars2_73(73, 'CASE', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'COUNT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_COUNT(Stack),
 yeccpars2_73(73, 'COUNT', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_DIGIT_STRING(Stack),
 yeccpars2_73(73, 'DIGIT_STRING', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_ESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'ESCAPED_SYMBOLIC_NAME', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_EXISTS(Stack),
 yeccpars2_73(73, 'EXISTS', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'EXPONENT_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_EXPONENT_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'EXPONENT_DECIMAL_REAL', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'EXTRACT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_EXTRACT(Stack),
 yeccpars2_73(73, 'EXTRACT', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_FALSE(Stack),
 yeccpars2_73(73, 'FALSE', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'FILTER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_FILTER(Stack),
 yeccpars2_73(73, 'FILTER', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_HEX_INTEGER(Stack),
 yeccpars2_73(73, 'HEX_INTEGER', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'NONE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_NONE(Stack),
 yeccpars2_73(73, 'NONE', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_NULL(Stack),
 yeccpars2_73(73, 'NULL', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'REDUCE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_REDUCE(Stack),
 yeccpars2_73(73, 'REDUCE', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_SHORTESTPATH(Stack),
 yeccpars2_73(73, 'SHORTESTPATH', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_SIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_DECIMAL_INTEGER', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_SIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_OCTAL_INTEGER', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'SIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_SIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'SIGNED_REGULAR_DECIMAL_REAL', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'SINGLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_SINGLE(Stack),
 yeccpars2_73(73, 'SINGLE', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_STRING_LITERAL(Stack),
 yeccpars2_73(73, 'STRING_LITERAL', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_TRUE(Stack),
 yeccpars2_73(73, 'TRUE', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_UNESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'UNESCAPED_SYMBOLIC_NAME', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_UNSIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_DECIMAL_INTEGER', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_UNSIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_OCTAL_INTEGER', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_UNSIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'UNSIGNED_REGULAR_DECIMAL_REAL', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_150_['(Stack),
 yeccpars2_73(73, '[', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_150_{'(Stack),
 yeccpars2_73(73, '{', [150 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_150(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_150_(Stack),
 yeccpars2_321(321, Cat, [150 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_151/7}).
yeccpars2_151(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 315, Ss, Stack, T, Ts, Tzr);
yeccpars2_151(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_152(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_152_(Stack),
 yeccgoto_decimal_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_153/7}).
yeccpars2_153(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 310, Ss, Stack, T, Ts, Tzr);
yeccpars2_153(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_154(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_154_(Stack),
 yeccgoto_double_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_155/7}).
yeccpars2_155(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 304, Ss, Stack, T, Ts, Tzr);
yeccpars2_155(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_156(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_156_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_157/7}).
yeccpars2_157(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 301, Ss, Stack, T, Ts, Tzr);
yeccpars2_157(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_158(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_158_(Stack),
 yeccgoto_hex_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_159/7}).
yeccpars2_159(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 298, Ss, Stack, T, Ts, Tzr);
yeccpars2_159(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_160(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_160_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_161/7}).
yeccpars2_161(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 289, Ss, Stack, T, Ts, Tzr);
yeccpars2_161(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_162/7}).
yeccpars2_162(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 210, Ss, Stack, T, Ts, Tzr);
yeccpars2_162(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_163(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_163_(Stack),
 yeccgoto_decimal_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_164(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_164_(Stack),
 yeccgoto_octal_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_165(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_165_(Stack),
 yeccgoto_double_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_166/7}).
yeccpars2_166(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 207, Ss, Stack, T, Ts, Tzr);
yeccpars2_166(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_167(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_167_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_168(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_168_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_169(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_169_(Stack),
 yeccgoto_decimal_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_170(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_170_(Stack),
 yeccgoto_octal_integer(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_171(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_171_(Stack),
 yeccgoto_double_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_172(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_172(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_172_(Stack),
 yeccpars2_73(73, Cat, [172 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_173(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_173(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_173_(Stack),
 yeccpars2_175(175, Cat, [173 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_174(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_174(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_174_(Stack),
 yeccgoto_property_key_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_175/7}).
yeccpars2_175(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 186, Ss, Stack, T, Ts, Tzr);
yeccpars2_175(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_property_key_name_expression_commalist_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_177(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 183, Ss, Stack, T, Ts, Tzr);
yeccpars2_177(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_177_(Stack),
 yeccgoto_property_key_name_expression_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_178/7}).
yeccpars2_178(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 181, Ss, Stack, T, Ts, Tzr);
yeccpars2_178(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_179/7}).
yeccpars2_179(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 180, Ss, Stack, T, Ts, Tzr);
yeccpars2_179(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_180(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_180_(Stack),
 yeccgoto_parameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_181(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_181(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_181_(Stack),
 yeccpars2_73(73, Cat, [181 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_182_(Stack),
 yeccgoto_property_key_name_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_183: see yeccpars2_19

yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_184_(Stack),
 yeccgoto_property_key_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_185_(Stack),
 yeccgoto_property_key_name_expression_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_186(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_186_(Stack),
 yeccgoto_map_literal(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_187(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_187_(Stack),
 yeccgoto_parameter(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_188/7}).
yeccpars2_188(S, 'IN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 205, Ss, Stack, T, Ts, Tzr);
yeccpars2_188(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_189(S, 'WHERE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_189(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_189_(Stack),
 yeccpars2_201(_S, Cat, [189 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_190(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 198, Ss, Stack, T, Ts, Tzr);
yeccpars2_190(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_190_(Stack),
 yeccpars2_196(196, Cat, [190 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_191/7}).
yeccpars2_191(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 195, Ss, Stack, T, Ts, Tzr);
yeccpars2_191(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_192(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 193, Ss, Stack, T, Ts, Tzr);
yeccpars2_192(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_192_(Stack),
 yeccgoto_expression_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_193(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_193(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_193_(Stack),
 yeccpars2_73(73, Cat, [193 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_194_(Stack),
 yeccgoto_expression_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_195(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_195_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_196/7}).
yeccpars2_196(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 200, Ss, Stack, T, Ts, Tzr);
yeccpars2_196(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_char_vertical_bar_expression_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_198(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_198(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_198_(Stack),
 yeccpars2_73(73, Cat, [198 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_199_(Stack),
 yeccgoto_char_vertical_bar_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_200(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_200_(Stack),
 yeccgoto_list_comprehension(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_201_(Stack),
 yeccgoto_filter_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_where_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_203(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_203(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_203_(Stack),
 yeccpars2_73(73, Cat, [203 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_204_(Stack),
 yeccgoto_where(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_205(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_205(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_205_(Stack),
 yeccpars2_73(73, Cat, [205 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_206_(Stack),
 yeccgoto_id_in_coll(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_207: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_208/7}).
yeccpars2_208(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 209, Ss, Stack, T, Ts, Tzr);
yeccpars2_208(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_209(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_209_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_210/7}).
yeccpars2_210(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_210(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_211/7}).
yeccpars2_211(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 288, Ss, Stack, T, Ts, Tzr);
yeccpars2_211(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_212(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, '-->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, '<--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(S, '<-->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_212(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_212_(Stack),
 yeccpars2_239(_S, Cat, [212 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_213(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_213(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_214/7}).
yeccpars2_214(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 233, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_214(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_215/7}).
yeccpars2_215(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 230, Ss, Stack, T, Ts, Tzr);
yeccpars2_215(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_216/7}).
yeccpars2_216(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 229, Ss, Stack, T, Ts, Tzr);
yeccpars2_216(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_217_(Stack),
 yeccgoto_properties(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_218/7}).
yeccpars2_218(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 227, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_218(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_219_(Stack),
 yeccgoto_node_labels(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_220_(Stack),
 yeccgoto_properties(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_221(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_221_(Stack),
 yeccgoto_node_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_222: see yeccpars2_19

yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_223_(Stack),
 yeccgoto_label_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_224_(Stack),
 yeccgoto_node_label(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_225/7}).
yeccpars2_225(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 228, Ss, Stack, T, Ts, Tzr);
yeccpars2_225(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_226_(Stack),
 yeccgoto_node_labels(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_227(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_227_(Stack),
 yeccgoto_node_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_228(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_228_(Stack),
 yeccgoto_node_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_229(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_229_(Stack),
 yeccgoto_pattern_element(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_230(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_230_(Stack),
 yeccgoto_node_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_231/7}).
yeccpars2_231(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 237, Ss, Stack, T, Ts, Tzr);
yeccpars2_231(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_232/7}).
yeccpars2_232(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 235, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_232(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_233(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_233_(Stack),
 yeccgoto_node_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_234/7}).
yeccpars2_234(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 236, Ss, Stack, T, Ts, Tzr);
yeccpars2_234(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_235(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_235_(Stack),
 yeccgoto_node_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_236(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_236_(Stack),
 yeccgoto_node_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_237(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_237_(Stack),
 yeccgoto_node_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_238/7}).
yeccpars2_238(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 287, Ss, Stack, T, Ts, Tzr);
yeccpars2_238(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_239_(Stack),
 yeccgoto_pattern_element(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_240(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, '-->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, '<--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(S, '<-->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_240(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_pattern_element_chain_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_241_(Stack),
 yeccgoto_pattern_element_chain_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_242(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_242(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_242_(Stack),
 yeccpars2_282(282, Cat, [242 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_243(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_243_(Stack),
 yeccgoto_relationship_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_244(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_244_(Stack),
 yeccgoto_relationship_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_245/7}).
yeccpars2_245(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 248, Ss, Stack, T, Ts, Tzr);
yeccpars2_245(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_246(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_246_(Stack),
 yeccgoto_relationship_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_247(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_247_(Stack),
 yeccgoto_relationship_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_248(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 251, Ss, Stack, T, Ts, Tzr);
yeccpars2_248(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_248_(Stack),
 yeccpars2_249(249, Cat, [248 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_249/7}).
yeccpars2_249(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 280, Ss, Stack, T, Ts, Tzr);
yeccpars2_249(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_relationship_detail_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_251(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_251(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_251_(Stack),
 yeccpars2_253(253, Cat, [251 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_252(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 254, Ss, Stack, T, Ts, Tzr);
yeccpars2_252(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_252_(Stack),
 yeccpars2_274(274, Cat, [252 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_253(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_253(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_253_(Stack),
 yeccpars2_255(255, Cat, [253 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_254(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_254_(Stack),
 yeccgoto_char_question_mark_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_255(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_255(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_255_(Stack),
 yeccpars2_262(262, Cat, [255 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_256_(Stack),
 yeccgoto_relationship_types_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_257: see yeccpars2_19

yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_258_(Stack),
 yeccgoto_rel_type_name(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_259(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 260, Ss, Stack, T, Ts, Tzr);
yeccpars2_259(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_259_(Stack),
 yeccgoto_relationship_types(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_260/7}).
yeccpars2_260(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_260(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_261_(Stack),
 yeccgoto_relationship_types(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_262/7}).
yeccpars2_262(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 272, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_262(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_263(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_263(_S, '..', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_263_..'(Stack),
 yeccpars2_264(264, '..', [263 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_263(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_263_(Stack),
 yeccpars2_266(_S, Cat, [263 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_264/7}).
yeccpars2_264(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 269, Ss, Stack, T, Ts, Tzr);
yeccpars2_264(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_unsigned_integer_literal_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_266_(Stack),
 yeccgoto_range_opt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_range_literal_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_268(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_268_(Stack),
 yeccgoto_unsigned_integer_literal(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_269(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_269(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_269_(Stack),
 yeccpars2_270(_S, Cat, [269 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_270_(Stack),
 yeccgoto_range_literal(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_271/7}).
yeccpars2_271(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 273, Ss, Stack, T, Ts, Tzr);
yeccpars2_271(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_272(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_272_(Stack),
 yeccgoto_relationship_detail(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_273(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_273_(Stack),
 yeccgoto_relationship_detail(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_274(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 257, Ss, Stack, T, Ts, Tzr);
yeccpars2_274(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_274_(Stack),
 yeccpars2_275(275, Cat, [274 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_275(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 263, Ss, Stack, T, Ts, Tzr);
yeccpars2_275(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_275_(Stack),
 yeccpars2_276(276, Cat, [275 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_276/7}).
yeccpars2_276(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 278, Ss, Stack, T, Ts, Tzr);
yeccpars2_276(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_276(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_277/7}).
yeccpars2_277(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 279, Ss, Stack, T, Ts, Tzr);
yeccpars2_277(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_278(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_278_(Stack),
 yeccgoto_relationship_detail(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_279(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_279_(Stack),
 yeccgoto_relationship_detail(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_280(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 281, Ss, Stack, T, Ts, Tzr);
yeccpars2_280(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_280_(Stack),
 yeccgoto_relationship_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_281(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_281_(Stack),
 yeccgoto_relationship_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_282/7}).
yeccpars2_282(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 283, Ss, Stack, T, Ts, Tzr);
yeccpars2_282(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_283(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 284, Ss, Stack, T, Ts, Tzr);
yeccpars2_283(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_283_(Stack),
 yeccgoto_relationship_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_284(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_284_(Stack),
 yeccgoto_relationship_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_285_(Stack),
 yeccgoto_pattern_element_chain_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_286_(Stack),
 yeccgoto_pattern_element_chain(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_287(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 221, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_287(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_288(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_288_(Stack),
 yeccgoto_shortest_path_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_289: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_290/7}).
yeccpars2_290(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 291, Ss, Stack, T, Ts, Tzr);
yeccpars2_290(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_291(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_291(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_291_(Stack),
 yeccpars2_73(73, Cat, [291 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_292/7}).
yeccpars2_292(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 293, Ss, Stack, T, Ts, Tzr);
yeccpars2_292(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_293: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_294/7}).
yeccpars2_294(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 295, Ss, Stack, T, Ts, Tzr);
yeccpars2_294(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_295(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_295(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_295_(Stack),
 yeccpars2_73(73, Cat, [295 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_296/7}).
yeccpars2_296(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 297, Ss, Stack, T, Ts, Tzr);
yeccpars2_296(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_297(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_297_(Stack),
 yeccgoto_reduce(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_298: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_299/7}).
yeccpars2_299(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 300, Ss, Stack, T, Ts, Tzr);
yeccpars2_299(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_300(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_300_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_301: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_302/7}).
yeccpars2_302(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 303, Ss, Stack, T, Ts, Tzr);
yeccpars2_302(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_303(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_303_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_304: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_305/7}).
yeccpars2_305(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 306, Ss, Stack, T, Ts, Tzr);
yeccpars2_305(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 307, Ss, Stack, T, Ts, Tzr);
yeccpars2_305(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_306(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_306_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_307(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_307(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_307_(Stack),
 yeccpars2_73(73, Cat, [307 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_308/7}).
yeccpars2_308(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 309, Ss, Stack, T, Ts, Tzr);
yeccpars2_308(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_309(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_309_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_310(S, 'DISTINCT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_310(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_310_(Stack),
 yeccpars2_311(311, Cat, [310 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_311(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_311(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_311_('(Stack),
 yeccpars2_73(73, '(', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_311_+'(Stack),
 yeccpars2_73(73, '+', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_311_-'(Stack),
 yeccpars2_73(73, '-', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_ALL(Stack),
 yeccpars2_73(73, 'ALL', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_ALLSHORTESTPATHS(Stack),
 yeccpars2_73(73, 'ALLSHORTESTPATHS', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'ANY', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_ANY(Stack),
 yeccpars2_73(73, 'ANY', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'CASE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_CASE(Stack),
 yeccpars2_73(73, 'CASE', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'COUNT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_COUNT(Stack),
 yeccpars2_73(73, 'COUNT', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_DIGIT_STRING(Stack),
 yeccpars2_73(73, 'DIGIT_STRING', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_ESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'ESCAPED_SYMBOLIC_NAME', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_EXISTS(Stack),
 yeccpars2_73(73, 'EXISTS', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'EXPONENT_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_EXPONENT_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'EXPONENT_DECIMAL_REAL', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'EXTRACT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_EXTRACT(Stack),
 yeccpars2_73(73, 'EXTRACT', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_FALSE(Stack),
 yeccpars2_73(73, 'FALSE', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'FILTER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_FILTER(Stack),
 yeccpars2_73(73, 'FILTER', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_HEX_INTEGER(Stack),
 yeccpars2_73(73, 'HEX_INTEGER', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'NONE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_NONE(Stack),
 yeccpars2_73(73, 'NONE', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_NULL(Stack),
 yeccpars2_73(73, 'NULL', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'REDUCE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_REDUCE(Stack),
 yeccpars2_73(73, 'REDUCE', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_SHORTESTPATH(Stack),
 yeccpars2_73(73, 'SHORTESTPATH', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_SIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_DECIMAL_INTEGER', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_SIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_OCTAL_INTEGER', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'SIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_SIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'SIGNED_REGULAR_DECIMAL_REAL', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'SINGLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_SINGLE(Stack),
 yeccpars2_73(73, 'SINGLE', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_STRING_LITERAL(Stack),
 yeccpars2_73(73, 'STRING_LITERAL', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_TRUE(Stack),
 yeccpars2_73(73, 'TRUE', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_UNESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'UNESCAPED_SYMBOLIC_NAME', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_UNSIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_DECIMAL_INTEGER', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_UNSIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_OCTAL_INTEGER', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_UNSIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'UNSIGNED_REGULAR_DECIMAL_REAL', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_311_['(Stack),
 yeccpars2_73(73, '[', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_311_{'(Stack),
 yeccpars2_73(73, '{', [311 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_311(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_311_(Stack),
 yeccpars2_312(312, Cat, [311 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_312/7}).
yeccpars2_312(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 314, Ss, Stack, T, Ts, Tzr);
yeccpars2_312(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_commalist_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_314(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_314_(Stack),
 yeccgoto_function_invocation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_315(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 317, Ss, Stack, T, Ts, Tzr);
yeccpars2_315(S, 'DISTINCT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_315(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_315_(Stack),
 yeccpars2_316(316, Cat, [315 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_316(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_316(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_316_('(Stack),
 yeccpars2_73(73, '(', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_316_+'(Stack),
 yeccpars2_73(73, '+', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_316_-'(Stack),
 yeccpars2_73(73, '-', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_ALL(Stack),
 yeccpars2_73(73, 'ALL', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_ALLSHORTESTPATHS(Stack),
 yeccpars2_73(73, 'ALLSHORTESTPATHS', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'ANY', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_ANY(Stack),
 yeccpars2_73(73, 'ANY', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'CASE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_CASE(Stack),
 yeccpars2_73(73, 'CASE', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'COUNT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_COUNT(Stack),
 yeccpars2_73(73, 'COUNT', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_DIGIT_STRING(Stack),
 yeccpars2_73(73, 'DIGIT_STRING', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_ESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'ESCAPED_SYMBOLIC_NAME', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_EXISTS(Stack),
 yeccpars2_73(73, 'EXISTS', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'EXPONENT_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_EXPONENT_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'EXPONENT_DECIMAL_REAL', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'EXTRACT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_EXTRACT(Stack),
 yeccpars2_73(73, 'EXTRACT', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_FALSE(Stack),
 yeccpars2_73(73, 'FALSE', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'FILTER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_FILTER(Stack),
 yeccpars2_73(73, 'FILTER', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_HEX_INTEGER(Stack),
 yeccpars2_73(73, 'HEX_INTEGER', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'NONE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_NONE(Stack),
 yeccpars2_73(73, 'NONE', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_NULL(Stack),
 yeccpars2_73(73, 'NULL', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'REDUCE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_REDUCE(Stack),
 yeccpars2_73(73, 'REDUCE', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_SHORTESTPATH(Stack),
 yeccpars2_73(73, 'SHORTESTPATH', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_SIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_DECIMAL_INTEGER', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_SIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_OCTAL_INTEGER', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'SIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_SIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'SIGNED_REGULAR_DECIMAL_REAL', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'SINGLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_SINGLE(Stack),
 yeccpars2_73(73, 'SINGLE', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_STRING_LITERAL(Stack),
 yeccpars2_73(73, 'STRING_LITERAL', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_TRUE(Stack),
 yeccpars2_73(73, 'TRUE', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_UNESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'UNESCAPED_SYMBOLIC_NAME', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_UNSIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_DECIMAL_INTEGER', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_UNSIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_OCTAL_INTEGER', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_UNSIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'UNSIGNED_REGULAR_DECIMAL_REAL', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_316_['(Stack),
 yeccpars2_73(73, '[', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_316_{'(Stack),
 yeccpars2_73(73, '{', [316 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_316(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_316_(Stack),
 yeccpars2_319(319, Cat, [316 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_317/7}).
yeccpars2_317(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 318, Ss, Stack, T, Ts, Tzr);
yeccpars2_317(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_318(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_318_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_319/7}).
yeccpars2_319(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 320, Ss, Stack, T, Ts, Tzr);
yeccpars2_319(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_320(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_320_(Stack),
 yeccgoto_function_invocation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_321/7}).
yeccpars2_321(S, 'WHEN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 325, Ss, Stack, T, Ts, Tzr);
yeccpars2_321(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_323(S, 'ELSE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 332, Ss, Stack, T, Ts, Tzr);
yeccpars2_323(S, 'WHEN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 325, Ss, Stack, T, Ts, Tzr);
yeccpars2_323(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_323_(Stack),
 yeccpars2_329(329, Cat, [323 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_324_(Stack),
 yeccgoto_case_alternatives_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_325(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_325(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_325_(Stack),
 yeccpars2_73(73, Cat, [325 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_326/7}).
yeccpars2_326(S, 'THEN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 327, Ss, Stack, T, Ts, Tzr);
yeccpars2_326(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_327(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_327(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_327_(Stack),
 yeccpars2_73(73, Cat, [327 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_328_(Stack),
 yeccgoto_case_alternatives(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_329/7}).
yeccpars2_329(S, 'END', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 334, Ss, Stack, T, Ts, Tzr);
yeccpars2_329(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_else_expression_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_331_(Stack),
 yeccgoto_case_alternatives_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_332(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_332(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_332_(Stack),
 yeccpars2_73(73, Cat, [332 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_333(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_333_(Stack),
 yeccgoto_else_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_334(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_334_(Stack),
 yeccgoto_case_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_335: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_336/7}).
yeccpars2_336(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 337, Ss, Stack, T, Ts, Tzr);
yeccpars2_336(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_337(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_337_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_338: see yeccpars2_210

-dialyzer({nowarn_function, yeccpars2_339/7}).
yeccpars2_339(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 340, Ss, Stack, T, Ts, Tzr);
yeccpars2_339(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_340(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_340_(Stack),
 yeccgoto_shortest_path_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_341: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_342/7}).
yeccpars2_342(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 343, Ss, Stack, T, Ts, Tzr);
yeccpars2_342(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_343(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_343_(Stack),
 yeccgoto_atom(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_344/7}).
yeccpars2_344(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 345, Ss, Stack, T, Ts, Tzr);
yeccpars2_344(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_345(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_345_(Stack),
 yeccgoto_parenthesized_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_2_addon(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_347(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_347(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_347_(Stack),
 yeccgoto_expression_2_addon(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_348_(Stack),
 yeccgoto_expression_2(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_349(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_349(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_349(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_2_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_350_(Stack),
 yeccgoto_expression_2_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_351: see yeccpars2_19

yeccpars2_352(S, '!', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 354, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(S, '?', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 355, Ss, Stack, T, Ts, Tzr);
yeccpars2_352(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_352_(Stack),
 yeccpars2_353(_S, Cat, [352 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_353(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_353_(Stack),
 yeccgoto_property_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_354(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_354_(Stack),
 yeccgoto_char_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_355(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_355_(Stack),
 yeccgoto_char_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_356_(Stack),
 yeccgoto_expression_2_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_357_(Stack),
 yeccgoto_expression_3(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_358(S, '=~', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 360, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'CONTAINS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 361, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'ENDS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 362, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'IN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 363, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'IS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 364, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, 'STARTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 365, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 366, Ss, Stack, T, Ts, Tzr);
yeccpars2_358(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_3_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_359_(Stack),
 yeccgoto_expression_3_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_360: see yeccpars2_60

%% yeccpars2_361: see yeccpars2_60

-dialyzer({nowarn_function, yeccpars2_362/7}).
yeccpars2_362(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 379, Ss, Stack, T, Ts, Tzr);
yeccpars2_362(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_363: see yeccpars2_60

-dialyzer({nowarn_function, yeccpars2_364/7}).
yeccpars2_364(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 375, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 376, Ss, Stack, T, Ts, Tzr);
yeccpars2_364(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_365/7}).
yeccpars2_365(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 373, Ss, Stack, T, Ts, Tzr);
yeccpars2_365(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_366(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_366(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_366_('(Stack),
 yeccpars2_73(73, '(', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_366_+'(Stack),
 yeccpars2_73(73, '+', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_366_-'(Stack),
 yeccpars2_73(73, '-', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_ALL(Stack),
 yeccpars2_73(73, 'ALL', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_ALLSHORTESTPATHS(Stack),
 yeccpars2_73(73, 'ALLSHORTESTPATHS', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'ANY', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_ANY(Stack),
 yeccpars2_73(73, 'ANY', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'CASE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_CASE(Stack),
 yeccpars2_73(73, 'CASE', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'COUNT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_COUNT(Stack),
 yeccpars2_73(73, 'COUNT', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_DIGIT_STRING(Stack),
 yeccpars2_73(73, 'DIGIT_STRING', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_ESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'ESCAPED_SYMBOLIC_NAME', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_EXISTS(Stack),
 yeccpars2_73(73, 'EXISTS', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'EXPONENT_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_EXPONENT_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'EXPONENT_DECIMAL_REAL', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'EXTRACT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_EXTRACT(Stack),
 yeccpars2_73(73, 'EXTRACT', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_FALSE(Stack),
 yeccpars2_73(73, 'FALSE', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'FILTER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_FILTER(Stack),
 yeccpars2_73(73, 'FILTER', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_HEX_INTEGER(Stack),
 yeccpars2_73(73, 'HEX_INTEGER', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'NONE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_NONE(Stack),
 yeccpars2_73(73, 'NONE', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_NULL(Stack),
 yeccpars2_73(73, 'NULL', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'REDUCE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_REDUCE(Stack),
 yeccpars2_73(73, 'REDUCE', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_SHORTESTPATH(Stack),
 yeccpars2_73(73, 'SHORTESTPATH', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_SIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_DECIMAL_INTEGER', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_SIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_OCTAL_INTEGER', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'SIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_SIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'SIGNED_REGULAR_DECIMAL_REAL', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'SINGLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_SINGLE(Stack),
 yeccpars2_73(73, 'SINGLE', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_STRING_LITERAL(Stack),
 yeccpars2_73(73, 'STRING_LITERAL', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_TRUE(Stack),
 yeccpars2_73(73, 'TRUE', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_UNESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'UNESCAPED_SYMBOLIC_NAME', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_UNSIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_DECIMAL_INTEGER', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_UNSIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_OCTAL_INTEGER', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_UNSIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'UNSIGNED_REGULAR_DECIMAL_REAL', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_366_['(Stack),
 yeccpars2_73(73, '[', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_366_{'(Stack),
 yeccpars2_73(73, '{', [366 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_366(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_366_(Stack),
 yeccpars2_367(367, Cat, [366 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_367/7}).
yeccpars2_367(S, '..', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 370, Ss, Stack, T, Ts, Tzr);
yeccpars2_367(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_368(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 369, Ss, Stack, T, Ts, Tzr);
yeccpars2_368(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_369(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_369_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_370(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_370(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_370_('(Stack),
 yeccpars2_73(73, '(', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_370_+'(Stack),
 yeccpars2_73(73, '+', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_370_-'(Stack),
 yeccpars2_73(73, '-', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_ALL(Stack),
 yeccpars2_73(73, 'ALL', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_ALLSHORTESTPATHS(Stack),
 yeccpars2_73(73, 'ALLSHORTESTPATHS', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'ANY', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_ANY(Stack),
 yeccpars2_73(73, 'ANY', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'CASE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_CASE(Stack),
 yeccpars2_73(73, 'CASE', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'COUNT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_COUNT(Stack),
 yeccpars2_73(73, 'COUNT', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_DIGIT_STRING(Stack),
 yeccpars2_73(73, 'DIGIT_STRING', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_ESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'ESCAPED_SYMBOLIC_NAME', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_EXISTS(Stack),
 yeccpars2_73(73, 'EXISTS', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'EXPONENT_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_EXPONENT_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'EXPONENT_DECIMAL_REAL', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'EXTRACT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_EXTRACT(Stack),
 yeccpars2_73(73, 'EXTRACT', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_FALSE(Stack),
 yeccpars2_73(73, 'FALSE', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'FILTER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_FILTER(Stack),
 yeccpars2_73(73, 'FILTER', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_HEX_INTEGER(Stack),
 yeccpars2_73(73, 'HEX_INTEGER', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'NONE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_NONE(Stack),
 yeccpars2_73(73, 'NONE', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_NULL(Stack),
 yeccpars2_73(73, 'NULL', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'REDUCE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_REDUCE(Stack),
 yeccpars2_73(73, 'REDUCE', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_SHORTESTPATH(Stack),
 yeccpars2_73(73, 'SHORTESTPATH', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_SIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_DECIMAL_INTEGER', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_SIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_OCTAL_INTEGER', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'SIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_SIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'SIGNED_REGULAR_DECIMAL_REAL', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'SINGLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_SINGLE(Stack),
 yeccpars2_73(73, 'SINGLE', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_STRING_LITERAL(Stack),
 yeccpars2_73(73, 'STRING_LITERAL', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_TRUE(Stack),
 yeccpars2_73(73, 'TRUE', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_UNESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'UNESCAPED_SYMBOLIC_NAME', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_UNSIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_DECIMAL_INTEGER', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_UNSIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_OCTAL_INTEGER', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_UNSIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'UNSIGNED_REGULAR_DECIMAL_REAL', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_370_['(Stack),
 yeccpars2_73(73, '[', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_370_{'(Stack),
 yeccpars2_73(73, '{', [370 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_370(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_370_(Stack),
 yeccpars2_371(371, Cat, [370 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_371/7}).
yeccpars2_371(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 372, Ss, Stack, T, Ts, Tzr);
yeccpars2_371(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_372(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_372_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_373: see yeccpars2_60

yeccpars2_374(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_374_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_375/7}).
yeccpars2_375(S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 377, Ss, Stack, T, Ts, Tzr);
yeccpars2_375(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_376(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_376_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_377(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_377_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_378(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_378_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_379: see yeccpars2_60

yeccpars2_380(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_380_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_381_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_382_(Stack),
 yeccgoto_expression_3_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_383_(Stack),
 yeccgoto_expression_3_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_384(S, 'DISTINCT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_384(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_384_(Stack),
 yeccpars2_385(385, Cat, [384 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_385(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_385(_S, '(', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_385_('(Stack),
 yeccpars2_73(73, '(', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, '+', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_385_+'(Stack),
 yeccpars2_73(73, '+', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, '-', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_385_-'(Stack),
 yeccpars2_73(73, '-', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_ALL(Stack),
 yeccpars2_73(73, 'ALL', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_ALLSHORTESTPATHS(Stack),
 yeccpars2_73(73, 'ALLSHORTESTPATHS', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'ANY', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_ANY(Stack),
 yeccpars2_73(73, 'ANY', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'CASE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_CASE(Stack),
 yeccpars2_73(73, 'CASE', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'COUNT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_COUNT(Stack),
 yeccpars2_73(73, 'COUNT', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_DIGIT_STRING(Stack),
 yeccpars2_73(73, 'DIGIT_STRING', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_ESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'ESCAPED_SYMBOLIC_NAME', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_EXISTS(Stack),
 yeccpars2_73(73, 'EXISTS', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'EXPONENT_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_EXPONENT_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'EXPONENT_DECIMAL_REAL', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'EXTRACT', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_EXTRACT(Stack),
 yeccpars2_73(73, 'EXTRACT', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_FALSE(Stack),
 yeccpars2_73(73, 'FALSE', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'FILTER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_FILTER(Stack),
 yeccpars2_73(73, 'FILTER', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_HEX_INTEGER(Stack),
 yeccpars2_73(73, 'HEX_INTEGER', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'NONE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_NONE(Stack),
 yeccpars2_73(73, 'NONE', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_NULL(Stack),
 yeccpars2_73(73, 'NULL', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'REDUCE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_REDUCE(Stack),
 yeccpars2_73(73, 'REDUCE', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_SHORTESTPATH(Stack),
 yeccpars2_73(73, 'SHORTESTPATH', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_SIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_DECIMAL_INTEGER', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_SIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'SIGNED_OCTAL_INTEGER', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'SIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_SIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'SIGNED_REGULAR_DECIMAL_REAL', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'SINGLE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_SINGLE(Stack),
 yeccpars2_73(73, 'SINGLE', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_STRING_LITERAL(Stack),
 yeccpars2_73(73, 'STRING_LITERAL', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_TRUE(Stack),
 yeccpars2_73(73, 'TRUE', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_UNESCAPED_SYMBOLIC_NAME(Stack),
 yeccpars2_73(73, 'UNESCAPED_SYMBOLIC_NAME', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_UNSIGNED_DECIMAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_DECIMAL_INTEGER', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_UNSIGNED_OCTAL_INTEGER(Stack),
 yeccpars2_73(73, 'UNSIGNED_OCTAL_INTEGER', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_UNSIGNED_REGULAR_DECIMAL_REAL(Stack),
 yeccpars2_73(73, 'UNSIGNED_REGULAR_DECIMAL_REAL', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, '[', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_385_['(Stack),
 yeccpars2_73(73, '[', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, '{', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_385_{'(Stack),
 yeccpars2_73(73, '{', [385 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_385(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_385_(Stack),
 yeccpars2_386(386, Cat, [385 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_386/7}).
yeccpars2_386(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 387, Ss, Stack, T, Ts, Tzr);
yeccpars2_386(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_387(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_387_(Stack),
 yeccgoto_function_invocation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_388(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 242, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(S, '--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 243, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(S, '-->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 244, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 245, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(S, '<--', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 246, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(S, '<-->', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 247, Ss, Stack, T, Ts, Tzr);
yeccpars2_388(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_388_(Stack),
 yeccgoto_relationships_pattern(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_389_(Stack),
 yeccgoto_expression_6(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_390(S, '%', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 392, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 393, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(S, '/', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 394, Ss, Stack, T, Ts, Tzr);
yeccpars2_390(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_6_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_391_(Stack),
 yeccgoto_expression_6_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_392(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_392(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_392(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_392_(Stack),
 yeccpars2_60(111, Cat, [392 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_393(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_393(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_393_(Stack),
 yeccpars2_60(111, Cat, [393 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_394(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_394(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_394_(Stack),
 yeccpars2_60(111, Cat, [394 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_395_(Stack),
 yeccgoto_expression_6_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_396_(Stack),
 yeccgoto_expression_6_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_397_(Stack),
 yeccgoto_expression_6_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_398_(Stack),
 yeccgoto_expression_6_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_399_(Stack),
 yeccgoto_expression_7(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_400(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 402, Ss, Stack, T, Ts, Tzr);
yeccpars2_400(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 403, Ss, Stack, T, Ts, Tzr);
yeccpars2_400(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_7_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_401_(Stack),
 yeccgoto_expression_7_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_402(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_402(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_402_(Stack),
 yeccpars2_60(111, Cat, [402 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_403(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_403(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_403_(Stack),
 yeccpars2_60(111, Cat, [403 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_404_(Stack),
 yeccgoto_expression_7_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_405(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_405_(Stack),
 yeccgoto_expression_7_addon(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_406(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_406_(Stack),
 yeccgoto_expression_7_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_407(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_8_addon(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_408(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_408_(Stack),
 yeccgoto_expression_8(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_409(S, '!=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 412, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 413, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(S, '<=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 414, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(S, '<>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 415, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 416, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 417, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(S, '>=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 418, Ss, Stack, T, Ts, Tzr);
yeccpars2_409(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_expression_8_addon_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_410(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_410_(Stack),
 yeccgoto_expression_8_addon_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_411(S, '+', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 115, Ss, Stack, T, Ts, Tzr);
yeccpars2_411(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 116, Ss, Stack, T, Ts, Tzr);
yeccpars2_411(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_411_(Stack),
 yeccpars2_60(111, Cat, [411 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_412(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_412_(Stack),
 yeccgoto_comparison(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_413(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_413_(Stack),
 yeccgoto_comparison(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_414(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_414_(Stack),
 yeccgoto_comparison(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_415(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_415_(Stack),
 yeccgoto_comparison(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_416(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_416_(Stack),
 yeccgoto_comparison(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_417(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_417_(Stack),
 yeccgoto_comparison(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_418(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_418_(Stack),
 yeccgoto_comparison(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_419(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_419_(Stack),
 yeccgoto_partial_comparison_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_420_(Stack),
 yeccgoto_expression_8_addon_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_421_(Stack),
 yeccgoto_with(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_422(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_422(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_422_(Stack),
 yeccpars2_73(73, Cat, [422 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_423_(Stack),
 yeccgoto_return_item_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_424(S, 'SKIP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 439, Ss, Stack, T, Ts, Tzr);
yeccpars2_424(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_424_(Stack),
 yeccpars2_437(437, Cat, [424 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_order_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_426/7}).
yeccpars2_426(S, 'BY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 427, Ss, Stack, T, Ts, Tzr);
yeccpars2_426(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_427(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_427(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_427_(Stack),
 yeccpars2_73(73, Cat, [427 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_428_(Stack),
 yeccgoto_order(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_429(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 435, Ss, Stack, T, Ts, Tzr);
yeccpars2_429(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_429_(Stack),
 yeccgoto_sort_item_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_430(S, 'ASC', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 431, Ss, Stack, T, Ts, Tzr);
yeccpars2_430(S, 'ASCENDING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 432, Ss, Stack, T, Ts, Tzr);
yeccpars2_430(S, 'DESC', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 433, Ss, Stack, T, Ts, Tzr);
yeccpars2_430(S, 'DESCENDING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 434, Ss, Stack, T, Ts, Tzr);
yeccpars2_430(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_430_(Stack),
 yeccgoto_sort_item(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_431(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_431_(Stack),
 yeccgoto_sort_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_432(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_432_(Stack),
 yeccgoto_sort_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_433(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_433_(Stack),
 yeccgoto_sort_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_434(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_434_(Stack),
 yeccgoto_sort_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_435(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_435(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_435_(Stack),
 yeccpars2_73(73, Cat, [435 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_436_(Stack),
 yeccgoto_sort_item_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_437(S, 'LIMIT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 443, Ss, Stack, T, Ts, Tzr);
yeccpars2_437(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_437_(Stack),
 yeccpars2_441(_S, Cat, [437 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_438(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_skip_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_439(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_439(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_439_(Stack),
 yeccpars2_73(73, Cat, [439 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_440(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_440_(Stack),
 yeccgoto_skip(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_441_(Stack),
 yeccgoto_return_body(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_442(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_limit_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_443(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_443(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_443_(Stack),
 yeccpars2_73(73, Cat, [443 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_444(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_444_(Stack),
 yeccgoto_limit(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_445/7}).
yeccpars2_445(S, 'COMMIT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 446, Ss, Stack, T, Ts, Tzr);
yeccpars2_445(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_446(S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_446(S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_446(S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_446(S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_446(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_446(S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_446(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_446_(Stack),
 yeccpars2_447(_S, Cat, [446 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_447(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_447_(Stack),
 yeccgoto_periodic_commit_hint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_448(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_signed_integer_literal_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_449/7}).
yeccpars2_449(S, 'AS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 450, Ss, Stack, T, Ts, Tzr);
yeccpars2_449(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_450: see yeccpars2_19

yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_451_(Stack),
 yeccgoto_unwind(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_452/7}).
yeccpars2_452(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 458, Ss, Stack, T, Ts, Tzr);
yeccpars2_452(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_453(S, 'WHERE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_453(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_453_(Stack),
 yeccpars2_457(_S, Cat, [453 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_454(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 455, Ss, Stack, T, Ts, Tzr);
yeccpars2_454(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_454_(Stack),
 yeccgoto_start_point_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_455: see yeccpars2_19

yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_456_(Stack),
 yeccgoto_start_point_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_457(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_457_(Stack),
 yeccgoto_start(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_458/7}).
yeccpars2_458(S, 'NODE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 462, Ss, Stack, T, Ts, Tzr);
yeccpars2_458(S, 'REL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 463, Ss, Stack, T, Ts, Tzr);
yeccpars2_458(S, 'RELATIONSHIP', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 464, Ss, Stack, T, Ts, Tzr);
yeccpars2_458(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_459(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_459_(Stack),
 yeccgoto_lookup(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_460(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_460_(Stack),
 yeccgoto_lookup(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_461_(Stack),
 yeccgoto_start_point(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_462/7}).
yeccpars2_462(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 468, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 469, Ss, Stack, T, Ts, Tzr);
yeccpars2_462(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_463: see yeccpars2_462

%% yeccpars2_464: see yeccpars2_462

yeccpars2_465(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_465_(Stack),
 yeccgoto_relationship_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_466(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_466_(Stack),
 yeccgoto_relationship_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_467(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_467_(Stack),
 yeccgoto_relationship_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_468/7}).
yeccpars2_468(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 488, Ss, Stack, T, Ts, Tzr);
yeccpars2_468(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_468(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 475, Ss, Stack, T, Ts, Tzr);
yeccpars2_468(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_469: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_470/7}).
yeccpars2_470(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 471, Ss, Stack, T, Ts, Tzr);
yeccpars2_470(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_471(S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 474, Ss, Stack, T, Ts, Tzr);
yeccpars2_471(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 475, Ss, Stack, T, Ts, Tzr);
yeccpars2_471(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_472/7}).
yeccpars2_472(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 479, Ss, Stack, T, Ts, Tzr);
yeccpars2_472(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_473/7}).
yeccpars2_473(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 478, Ss, Stack, T, Ts, Tzr);
yeccpars2_473(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_474/7}).
yeccpars2_474(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 477, Ss, Stack, T, Ts, Tzr);
yeccpars2_474(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_475(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 179, Ss, Stack, T, Ts, Tzr);
yeccpars2_475(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_476/7}).
yeccpars2_476(S, '}', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 187, Ss, Stack, T, Ts, Tzr);
yeccpars2_476(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_477(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_477_(Stack),
 yeccgoto_index_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_478(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_478_(Stack),
 yeccgoto_index_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_479/7}).
yeccpars2_479(S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 481, Ss, Stack, T, Ts, Tzr);
yeccpars2_479(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 475, Ss, Stack, T, Ts, Tzr);
yeccpars2_479(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_480/7}).
yeccpars2_480(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 483, Ss, Stack, T, Ts, Tzr);
yeccpars2_480(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_481/7}).
yeccpars2_481(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 482, Ss, Stack, T, Ts, Tzr);
yeccpars2_481(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_482(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_482_(Stack),
 yeccgoto_identified_index_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_483(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_483_(Stack),
 yeccgoto_identified_index_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_484(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_484_(Stack),
 yeccgoto_literal_ids(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_485(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 492, Ss, Stack, T, Ts, Tzr);
yeccpars2_485(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_485_(Stack),
 yeccgoto_unsigned_integer_literal_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_486/7}).
yeccpars2_486(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 491, Ss, Stack, T, Ts, Tzr);
yeccpars2_486(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_487/7}).
yeccpars2_487(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 490, Ss, Stack, T, Ts, Tzr);
yeccpars2_487(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_488/7}).
yeccpars2_488(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 489, Ss, Stack, T, Ts, Tzr);
yeccpars2_488(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_489(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_489_(Stack),
 yeccgoto_id_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_490(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_490_(Stack),
 yeccgoto_id_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_491(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_491_(Stack),
 yeccgoto_id_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_492/7}).
yeccpars2_492(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 268, Ss, Stack, T, Ts, Tzr);
yeccpars2_492(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_493(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_493_(Stack),
 yeccgoto_unsigned_integer_literal_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_494(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_494_(Stack),
 yeccgoto_relationship_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_495_(Stack),
 yeccgoto_relationship_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_496(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_496_(Stack),
 yeccgoto_relationship_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_497(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_497_(Stack),
 yeccgoto_node_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_498(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_498_(Stack),
 yeccgoto_node_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_499(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_499_(Stack),
 yeccgoto_node_lookup(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_500(S, '+=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 513, Ss, Stack, T, Ts, Tzr);
yeccpars2_500(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_500(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 514, Ss, Stack, T, Ts, Tzr);
yeccpars2_500(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_500_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_501(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_501_(Stack),
 yeccgoto_set(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_502(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 510, Ss, Stack, T, Ts, Tzr);
yeccpars2_502(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_502_(Stack),
 yeccgoto_set_item_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_503/7}).
yeccpars2_503(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 508, Ss, Stack, T, Ts, Tzr);
yeccpars2_503(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_504/7}).
yeccpars2_504(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_504(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_505(S, '.', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 351, Ss, Stack, T, Ts, Tzr);
yeccpars2_505(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_505_(Stack),
 yeccgoto_property_expression(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_506(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_506_(Stack),
 yeccgoto_property_lookup_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_507_(Stack),
 yeccgoto_property_lookup_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_508(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_508(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_508_(Stack),
 yeccpars2_73(73, Cat, [508 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_509_(Stack),
 yeccgoto_set_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_510: see yeccpars2_60

yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_511_(Stack),
 yeccgoto_set_item_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_512(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_512(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_512_(Stack),
 yeccgoto_set_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_513(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_513(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_513_(Stack),
 yeccpars2_73(73, Cat, [513 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_514(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_514(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_514_(Stack),
 yeccpars2_73(73, Cat, [514 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_515_(Stack),
 yeccgoto_set_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_516_(Stack),
 yeccgoto_set_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_517(S, '*', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);
yeccpars2_517(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_517(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_517_(Stack),
 yeccpars2_73(73, Cat, [517 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_518(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_518_(Stack),
 yeccgoto_return(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_519(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_519(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_519_(Stack),
 yeccgoto_atom(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_520(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_520_(Stack),
 yeccgoto_remove(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_521(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 523, Ss, Stack, T, Ts, Tzr);
yeccpars2_521(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_521_(Stack),
 yeccgoto_remove_item_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_522(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_522_(Stack),
 yeccgoto_remove_item(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_523: see yeccpars2_60

yeccpars2_524(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_524_(Stack),
 yeccgoto_remove_item_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_525(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_525(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_525_(Stack),
 yeccgoto_remove_item(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_526/7}).
yeccpars2_526(S, '=', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 540, Ss, Stack, T, Ts, Tzr);
yeccpars2_526(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_527_(Stack),
 yeccgoto_anonymous_pattern_part(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_528(S, 'ON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 534, Ss, Stack, T, Ts, Tzr);
yeccpars2_528(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_528_(Stack),
 yeccpars2_531(_S, Cat, [528 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_529(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_529_(Stack),
 yeccgoto_anonymous_pattern_part(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_530_(Stack),
 yeccgoto_pattern_part(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_531(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_531_(Stack),
 yeccgoto_merge(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_532(S, 'ON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 534, Ss, Stack, T, Ts, Tzr);
yeccpars2_532(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_merge_action_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_533(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_533_(Stack),
 yeccgoto_merge_action_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_534/7}).
yeccpars2_534(S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 535, Ss, Stack, T, Ts, Tzr);
yeccpars2_534(S, 'MATCH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 536, Ss, Stack, T, Ts, Tzr);
yeccpars2_534(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_535/7}).
yeccpars2_535(S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_535(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_536: see yeccpars2_535

yeccpars2_537(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_537_(Stack),
 yeccgoto_merge_action(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_538(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_538_(Stack),
 yeccgoto_merge_action(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_539(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_539_(Stack),
 yeccgoto_merge_action_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_540/7}).
yeccpars2_540(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 213, Ss, Stack, T, Ts, Tzr);
yeccpars2_540(S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_540(S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_540(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_541(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_541_(Stack),
 yeccgoto_pattern_part(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_542(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 544, Ss, Stack, T, Ts, Tzr);
yeccpars2_542(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_542_(Stack),
 yeccpars2_543(543, Cat, [542 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_543/7}).
yeccpars2_543(S, 'FROM', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 546, Ss, Stack, T, Ts, Tzr);
yeccpars2_543(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_544/7}).
yeccpars2_544(S, 'HEADERS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 545, Ss, Stack, T, Ts, Tzr);
yeccpars2_544(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_545(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_545_(Stack),
 yeccgoto_with_headers_opt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_546(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_546(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_546_(Stack),
 yeccpars2_73(73, Cat, [546 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_547/7}).
yeccpars2_547(S, 'AS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 548, Ss, Stack, T, Ts, Tzr);
yeccpars2_547(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_548: see yeccpars2_19

yeccpars2_549(S, 'FIELDTERMINATOR', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 551, Ss, Stack, T, Ts, Tzr);
yeccpars2_549(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_549_(Stack),
 yeccpars2_550(_S, Cat, [549 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_550(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_550_(Stack),
 yeccgoto_load_csv(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_551/7}).
yeccpars2_551(S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 552, Ss, Stack, T, Ts, Tzr);
yeccpars2_551(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_552(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_552_(Stack),
 yeccgoto_field_terminator_opt(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_553: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_554/7}).
yeccpars2_554(S, 'IN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 555, Ss, Stack, T, Ts, Tzr);
yeccpars2_554(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_555(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_555(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_555_(Stack),
 yeccpars2_73(73, Cat, [555 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_556/7}).
yeccpars2_556(S, '|', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 557, Ss, Stack, T, Ts, Tzr);
yeccpars2_556(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_557(S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'DETACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'FOREACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'MERGE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'OPTIONAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'REMOVE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'RETURN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'START', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'UNWIND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_557(_S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_557_DELETE(Stack),
 yeccpars2_42(42, 'DELETE', [557 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_557(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_557_(Stack),
 yeccpars2_33(33, Cat, [557 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_558(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 568, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'DETACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'FOREACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'MERGE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'OPTIONAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'REMOVE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'RETURN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'START', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'UNWIND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_558(_S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_558_DELETE(Stack),
 yeccpars2_42(42, 'DELETE', [558 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_558(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_558_(Stack),
 yeccpars2_33(33, Cat, [558 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_559(S, 'UNIQUE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 561, Ss, Stack, T, Ts, Tzr);
yeccpars2_559(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_559_(Stack),
 yeccpars2_58(560, Cat, [559 | Ss], NewStack, T, Ts, Tzr).

%% yeccpars2_560: see yeccpars2_58

yeccpars2_561(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_561_(Stack),
 yeccgoto_unique_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_562(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_562_(Stack),
 yeccgoto_pattern(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_563(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 565, Ss, Stack, T, Ts, Tzr);
yeccpars2_563(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_563_(Stack),
 yeccgoto_pattern_part_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_564(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_564_(Stack),
 yeccgoto_create(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_565: see yeccpars2_58

yeccpars2_566(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_566_(Stack),
 yeccgoto_pattern_part_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_567(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_567_(Stack),
 yeccgoto_clause_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_568(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_568_(Stack),
 yeccgoto_for_each(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_569(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_569_(Stack),
 yeccgoto_drop_unique_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_570(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_570_(Stack),
 yeccgoto_drop_relationship_property_existence_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_571(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_571_(Stack),
 yeccgoto_drop_node_property_existence_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_572(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_572_(Stack),
 yeccgoto_drop_index(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_573/7}).
yeccpars2_573(S, 'ON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 580, Ss, Stack, T, Ts, Tzr);
yeccpars2_573(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_574/7}).
yeccpars2_574(S, 'ON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 575, Ss, Stack, T, Ts, Tzr);
yeccpars2_574(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_575/7}).
yeccpars2_575(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 222, Ss, Stack, T, Ts, Tzr);
yeccpars2_575(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_576/7}).
yeccpars2_576(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 577, Ss, Stack, T, Ts, Tzr);
yeccpars2_576(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_577: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_578/7}).
yeccpars2_578(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 579, Ss, Stack, T, Ts, Tzr);
yeccpars2_578(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_579(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_579_(Stack),
 yeccgoto_index(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_580/7}).
yeccpars2_580(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 582, Ss, Stack, T, Ts, Tzr);
yeccpars2_580(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_581/7}).
yeccpars2_581(S, 'ASSERT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 617, Ss, Stack, T, Ts, Tzr);
yeccpars2_581(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_582(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 584, Ss, Stack, T, Ts, Tzr);
yeccpars2_582(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_583: see yeccpars2_575

-dialyzer({nowarn_function, yeccpars2_584/7}).
yeccpars2_584(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 585, Ss, Stack, T, Ts, Tzr);
yeccpars2_584(S, '<', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 586, Ss, Stack, T, Ts, Tzr);
yeccpars2_584(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_585/7}).
yeccpars2_585(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 597, Ss, Stack, T, Ts, Tzr);
yeccpars2_585(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_586/7}).
yeccpars2_586(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 587, Ss, Stack, T, Ts, Tzr);
yeccpars2_586(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_587/7}).
yeccpars2_587(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 588, Ss, Stack, T, Ts, Tzr);
yeccpars2_587(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_588: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_589/7}).
yeccpars2_589(S, ':', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 591, Ss, Stack, T, Ts, Tzr);
yeccpars2_589(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_590/7}).
yeccpars2_590(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 593, Ss, Stack, T, Ts, Tzr);
yeccpars2_590(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_591: see yeccpars2_19

yeccpars2_592(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_592_(Stack),
 yeccgoto_rel_type(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_593/7}).
yeccpars2_593(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 594, Ss, Stack, T, Ts, Tzr);
yeccpars2_593(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_594/7}).
yeccpars2_594(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 595, Ss, Stack, T, Ts, Tzr);
yeccpars2_594(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_595/7}).
yeccpars2_595(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 596, Ss, Stack, T, Ts, Tzr);
yeccpars2_595(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_596(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_596_(Stack),
 yeccgoto_relationship_pattern_syntax(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_597: see yeccpars2_19

%% yeccpars2_598: see yeccpars2_589

-dialyzer({nowarn_function, yeccpars2_599/7}).
yeccpars2_599(S, ']', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 600, Ss, Stack, T, Ts, Tzr);
yeccpars2_599(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_600/7}).
yeccpars2_600(S, '-', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 601, Ss, Stack, T, Ts, Tzr);
yeccpars2_600(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_601/7}).
yeccpars2_601(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 602, Ss, Stack, T, Ts, Tzr);
yeccpars2_601(S, '>', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 603, Ss, Stack, T, Ts, Tzr);
yeccpars2_601(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_602/7}).
yeccpars2_602(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 606, Ss, Stack, T, Ts, Tzr);
yeccpars2_602(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_603/7}).
yeccpars2_603(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 604, Ss, Stack, T, Ts, Tzr);
yeccpars2_603(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_604/7}).
yeccpars2_604(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 605, Ss, Stack, T, Ts, Tzr);
yeccpars2_604(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_605(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_605_(Stack),
 yeccgoto_relationship_pattern_syntax(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_606(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_606_(Stack),
 yeccgoto_relationship_pattern_syntax(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_607/7}).
yeccpars2_607(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 608, Ss, Stack, T, Ts, Tzr);
yeccpars2_607(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_608/7}).
yeccpars2_608(S, 'ASSERT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 609, Ss, Stack, T, Ts, Tzr);
yeccpars2_608(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_609(S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_609(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_609(S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 611, Ss, Stack, T, Ts, Tzr);
yeccpars2_609(S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_609(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_609(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_609(S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_cont_60(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_610/7}).
yeccpars2_610(S, 'IS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 615, Ss, Stack, T, Ts, Tzr);
yeccpars2_610(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_611/7}).
yeccpars2_611(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 612, Ss, Stack, T, Ts, Tzr);
yeccpars2_611(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_612(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 146, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 147, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'ALLSHORTESTPATHS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 148, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'ANY', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 149, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'CASE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 150, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'COUNT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 151, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'DIGIT_STRING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 152, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'DISTINCT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 68, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'ESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 153, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'EXPONENT_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 154, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'EXTRACT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 155, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'FALSE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 156, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'FILTER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 157, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'HEX_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 158, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'NONE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 159, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'NULL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 160, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'REDUCE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 161, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'SHORTESTPATH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 162, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'SIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 163, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'SIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 164, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'SIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 165, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'SINGLE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 166, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'STRING_LITERAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 167, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'TRUE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 168, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'UNESCAPED_SYMBOLIC_NAME', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'UNSIGNED_DECIMAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 169, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'UNSIGNED_OCTAL_INTEGER', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 170, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, 'UNSIGNED_REGULAR_DECIMAL_REAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 171, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, '[', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 172, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(S, '{', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 173, Ss, Stack, T, Ts, Tzr);
yeccpars2_612(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_612_(Stack),
 yeccpars2_311(311, Cat, [612 | Ss], NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_613/7}).
yeccpars2_613(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 614, Ss, Stack, T, Ts, Tzr);
yeccpars2_613(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_614(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_614_(Stack),
 yeccgoto_node_property_existence_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_615/7}).
yeccpars2_615(S, 'UNIQUE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 616, Ss, Stack, T, Ts, Tzr);
yeccpars2_615(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_616(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_616_(Stack),
 yeccgoto_unique_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_617/7}).
yeccpars2_617(S, 'EXISTS', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 618, Ss, Stack, T, Ts, Tzr);
yeccpars2_617(_, _, _, _, T, _, _) ->
 yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_618/7}).
yeccpars2_618(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 619, Ss, Stack, T, Ts, Tzr);
yeccpars2_618(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_619: see yeccpars2_60

-dialyzer({nowarn_function, yeccpars2_620/7}).
yeccpars2_620(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 621, Ss, Stack, T, Ts, Tzr);
yeccpars2_620(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_621(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_621_(Stack),
 yeccgoto_relationship_property_existence_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_622(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_622_(Stack),
 yeccgoto_create_unique_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_623(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_623_(Stack),
 yeccgoto_create_relationship_property_existence_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_624(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_624_(Stack),
 yeccgoto_create_node_property_existence_constraint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_625_(Stack),
 yeccgoto_create_index(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_626(S, 'NOT', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_626(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_626_(Stack),
 yeccpars2_73(73, Cat, [626 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_627(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_627_(Stack),
 yeccgoto_delete(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_628: see yeccpars2_58

yeccpars2_629(S, 'USING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 633, Ss, Stack, T, Ts, Tzr);
yeccpars2_629(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_629_(Stack),
 yeccpars2_630(630, Cat, [629 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_630(S, 'WHERE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 203, Ss, Stack, T, Ts, Tzr);
yeccpars2_630(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_630_(Stack),
 yeccpars2_650(_S, Cat, [630 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_631(S, 'USING', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 633, Ss, Stack, T, Ts, Tzr);
yeccpars2_631(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_hint_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_632(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_632_(Stack),
 yeccgoto_hint_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_633/7}).
yeccpars2_633(S, 'INDEX', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 634, Ss, Stack, T, Ts, Tzr);
yeccpars2_633(S, 'JOIN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 635, Ss, Stack, T, Ts, Tzr);
yeccpars2_633(S, 'SCAN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 636, Ss, Stack, T, Ts, Tzr);
yeccpars2_633(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_634: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_635/7}).
yeccpars2_635(S, 'ON', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 639, Ss, Stack, T, Ts, Tzr);
yeccpars2_635(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_636: see yeccpars2_19

%% yeccpars2_637: see yeccpars2_575

yeccpars2_638(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_638_(Stack),
 yeccgoto_hint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_639: see yeccpars2_19

yeccpars2_640(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_|Nss] = Ss,
 NewStack = yeccpars2_640_(Stack),
 yeccgoto_hint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_641(S, ',', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 642, Ss, Stack, T, Ts, Tzr);
yeccpars2_641(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_641_(Stack),
 yeccgoto_variable_commalist(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

%% yeccpars2_642: see yeccpars2_19

yeccpars2_643(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_643_(Stack),
 yeccgoto_variable_commalist(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_644: see yeccpars2_575

-dialyzer({nowarn_function, yeccpars2_645/7}).
yeccpars2_645(S, '(', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 646, Ss, Stack, T, Ts, Tzr);
yeccpars2_645(_, _, _, _, T, _, _) ->
 yeccerror(T).

%% yeccpars2_646: see yeccpars2_19

-dialyzer({nowarn_function, yeccpars2_647/7}).
yeccpars2_647(S, ')', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 648, Ss, Stack, T, Ts, Tzr);
yeccpars2_647(_, _, _, _, T, _, _) ->
 yeccerror(T).

yeccpars2_648(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_648_(Stack),
 yeccgoto_hint(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_649(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_649_(Stack),
 yeccgoto_hint_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_,_,_|Nss] = Ss,
 NewStack = yeccpars2_650_(Stack),
 yeccgoto_match(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_651_(Stack),
 yeccgoto_bulk_import_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_652(S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'DETACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'FOREACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'MERGE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'OPTIONAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'REMOVE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'RETURN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'START', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'UNWIND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_652(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_652_$end'(Stack),
 yeccpars2_653(_S, '$end', [652 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_652(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 NewStack = 'yeccpars2_652_;'(Stack),
 yeccpars2_653(_S, ';', [652 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_652(_S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_652_DELETE(Stack),
 yeccpars2_42(42, 'DELETE', [652 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_652(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_652_(Stack),
 yeccpars2_33(33, Cat, [652 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_653_(Stack),
 yeccgoto_load_csv_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_654(S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'DETACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'FOREACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'MERGE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'OPTIONAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'REMOVE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'RETURN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'START', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'UNWIND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_654(_S, '$end', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_clause_list_opt(hd(Ss), '$end', Ss, Stack, T, Ts, Tzr);
yeccpars2_654(_S, ';', Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_clause_list_opt(hd(Ss), ';', Ss, Stack, T, Ts, Tzr);
yeccpars2_654(_S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_654_DELETE(Stack),
 yeccpars2_42(42, 'DELETE', [654 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_654(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_654_(Stack),
 yeccpars2_33(33, Cat, [654 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_655_(Stack),
 yeccgoto_regular_query(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_656(S, 'UNION', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 658, Ss, Stack, T, Ts, Tzr);
yeccpars2_656(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccgoto_union_list_opt(hd(Ss), Cat, Ss, Stack, T, Ts, Tzr).

yeccpars2_657(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_657_(Stack),
 yeccgoto_union_list(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_658(S, 'ALL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 660, Ss, Stack, T, Ts, Tzr);
yeccpars2_658(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_658_(Stack),
 yeccpars2_659(659, Cat, [658 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_659(S, 'CREATE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 559, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'DETACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 54, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'FOREACH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'LOAD', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'MERGE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 58, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'OPTIONAL', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'REMOVE', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'RETURN', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'SET', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'START', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 63, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'UNWIND', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 64, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(S, 'WITH', Ss, Stack, T, Ts, Tzr) ->
 yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_659(_S, 'DELETE', Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_659_DELETE(Stack),
 yeccpars2_42(42, 'DELETE', [659 | Ss], NewStack, T, Ts, Tzr);
yeccpars2_659(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_659_(Stack),
 yeccpars2_33(33, Cat, [659 | Ss], NewStack, T, Ts, Tzr).

yeccpars2_660(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_660_(Stack),
 yeccgoto_all_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccpars2_661(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_661_(Stack),
 yeccgoto_union(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_|Nss] = Ss,
 NewStack = yeccpars2_662_(Stack),
 yeccgoto_union_list(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 [_,_|Nss] = Ss,
 NewStack = yeccpars2_663_(Stack),
 yeccgoto_cypher(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

yeccpars2_664(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 NewStack = yeccpars2_664_(Stack),
 yeccgoto_char_semicolon_opt(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

yeccgoto_all_opt(658, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_659(659, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_anonymous_pattern_part(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_anonymous_pattern_part(540=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_541(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_anonymous_pattern_part(560=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_anonymous_pattern_part(565=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_anonymous_pattern_part(628=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_530(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_any_cypher_option(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_any_cypher_option(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_any_cypher_option_list(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_4(4, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_atom(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(504, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(504, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(361, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(363, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(373, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(379, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_145(145, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(510, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(504, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(523, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(504, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(609, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(504, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(612, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(504, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_atom(619, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_504(504, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_bulk_import_query(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_case_alternatives(321=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_324(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_alternatives(323=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_331(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_case_alternatives_list(321, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_323(323, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_case_expression(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_case_expression(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_144(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_char_opt(352=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_353(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_char_question_mark_opt(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_253(253, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_char_question_mark_opt(252, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_274(274, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_char_semicolon_opt(24=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_663(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_char_vertical_bar_expression(190=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_197(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_char_vertical_bar_expression_opt(190, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_196(196, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_clause(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_567(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_567(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_567(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_51(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_clause_list(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_list(557, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_558(558, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_list(652, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_654(654, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_clause_list(659, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_50(50, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_clause_list_opt(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_653(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_command(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_49(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_comparison(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(411, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_comparison(409, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_411(411, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_configuration_option(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_configuration_option(14=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_configuration_option_list(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_14(14, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_configuration_option_list_opt(9=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_create(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_create(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_create(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_create(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_create(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_create(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_create(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_create_index(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_create_node_property_existence_constraint(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_create_relationship_property_existence_constraint(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_create_unique_constraint(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_cypher(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_3(3, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_cypher_option(0=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_cypher_option(4=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_2(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_decimal_integer(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_decimal_integer(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_143(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_delete(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_delete(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_delete(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_delete(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_delete(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_delete(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_delete(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_detach_opt(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_detach_opt(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_detach_opt(557, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_detach_opt(558, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_detach_opt(652, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_detach_opt(654, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_detach_opt(659, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_42(42, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_distinct_opt(61, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_517(517, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_distinct_opt(66, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_67(67, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_distinct_opt(310, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(311, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_distinct_opt(315, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_316(316, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_distinct_opt(384, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_385(385, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_distinct_opt(612, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_311(311, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_double_literal(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_double_literal(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_142(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_drop_index(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_41(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_drop_node_property_existence_constraint(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_40(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_drop_relationship_property_existence_constraint(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_39(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_drop_unique_constraint(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_else_expression(323=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_330(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_else_expression_opt(323, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_329(329, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_449(449, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_344(344, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_182(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_199(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_204(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_206(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_292(292, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_296(296, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_308(308, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_326(326, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_328(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(332=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_333(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(366, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_368(368, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_322(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(430, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(435, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_430(430, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(439=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_440(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(443=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_444(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(508=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_509(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(513=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_516(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(514=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_515(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(517, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_80(80, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(546, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_547(547, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(555, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_556(556, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression(626, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_192(192, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_10(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_92(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(327, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(366, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(435, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(439, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(443, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(508, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(513, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(514, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(517, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(546, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(555, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10(626, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_10_addon(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_10_addon(101=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_105(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_10_addon_list(76, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_101(101, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_10_addon_list_opt(76=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_100(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_11(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_98(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(327, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(366, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(435, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(439, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(443, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(508, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(513, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(514, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(517, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(546, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(555, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11(626, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_78(78, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_11_addon(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_90(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_11_addon(89=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_93(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_11_addon_list(79, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_89(89, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_11_addon_list_opt(79=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_88(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_12(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(291=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(295=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(307=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(325=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(332=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(366=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(385=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(422=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(427=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(435=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(439=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(443=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(508=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(513=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(514=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(517=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(546=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(555=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12(626=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_77(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_12_addon(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_12_addon(95=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_12_addon_list(78, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_95(95, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_12_addon_list_opt(78=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_94(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_2(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_141(141, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_2(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_382(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_2(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_381(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_2(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_378(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_2(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_374(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_2(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_380(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_2_addon(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_350(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_2_addon(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_356(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_2_addon_list(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_349(349, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_2_addon_list_opt(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_348(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_3(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_140(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_3_addon(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_359(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_3_addon(358=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_383(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_3_addon_list(141, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_358(358, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_3_addon_list_opt(141=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_357(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_4(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_121(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4(392, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4(393, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4(402, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4(403, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4(411, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_114(114, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_4_addon(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon(112=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_123(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon(120=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon(392=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon(393=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon(403=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon(411=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_113(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_4_addon_list(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list(392, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list(393, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list(402, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list(403, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list(411, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_112(112, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_4_addon_list_opt(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list_opt(120, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list_opt(392, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list_opt(393, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list_opt(394, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list_opt(402, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list_opt(403, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(111, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_4_addon_list_opt(411, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_60(111, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_5(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_5(392=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_397(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_5(393=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_396(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_5(394=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_395(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_5(402, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_5(403, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_5(411, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_110(110, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_5_addon(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_119(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_5_addon(118=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_122(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_5_addon_list(114, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_118(118, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_5_addon_list_opt(114=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_117(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_6(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_6(402=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_405(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_6(403=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_404(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_6(411, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_109(109, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_6_addon(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_391(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_6_addon(390=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_398(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_6_addon_list(110, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_390(390, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_6_addon_list_opt(110=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_389(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_7(73, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_108(108, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_7(411=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_419(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_7_addon(109=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_401(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_7_addon(400=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_406(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_7_addon_list(109, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_400(400, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_7_addon_list_opt(109=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_399(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_8(73=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_8_addon(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_410(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_8_addon(409=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_420(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_8_addon_list(108, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_409(409, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_8_addon_list_opt(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_408(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_9(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(327, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(366, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(435, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(439, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(443, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(508, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(513, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(514, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(517, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(546, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(555, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9(626, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_9_addon(64=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(74=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(91=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(97=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(103=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(150=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(181=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(198=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(203=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(205=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(291=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(295=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(307=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(325=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(327=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(332=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(366=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(370=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(385=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(422=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(427=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(435=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(439=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(443=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(508=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(513=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(514=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(517=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(546=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(555=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon(626=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_75(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_9_addon_list(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(327, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(366, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(435, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(439, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(443, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(508, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(513, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(514, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(517, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(546, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(555, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list(626, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_74(74, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_9_addon_list_opt(64, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(91, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(97, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(103, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(181, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(193, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(198, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(203, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(205, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(291, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(295, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(307, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(325, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(327, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(332, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(366, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(435, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(439, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(443, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(508, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(513, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(514, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(517, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(546, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(555, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_9_addon_list_opt(626, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_commalist(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_191(191, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_commalist(193=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_194(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_commalist(311=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_commalist(316=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_commalist(385=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_313(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_commalist(626=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_627(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_commalist_opt(311, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_312(312, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_commalist_opt(316, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_319(319, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_commalist_opt(385, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_386(386, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_expression_opt(150, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_321(321, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_opt(366, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_367(367, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_expression_opt(370, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_371(371, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_field_terminator_opt(549=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_550(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_filter_expression(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_190(190, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_filter_expression(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_208(208, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_filter_expression(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_299(299, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_filter_expression(301, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_302(302, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_filter_expression(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_305(305, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_filter_expression(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_336(336, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_filter_expression(341, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_342(342, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_for_each(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_for_each(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_for_each(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_for_each(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_for_each(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_for_each(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_for_each(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_function_invocation(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_invocation(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_139(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_function_name(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(361, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(363, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(373, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(379, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(510, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(523, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(609, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(612, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_function_name(619, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_138(138, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_hex_integer(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hex_integer(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_137(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_hint(629=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_632(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_hint(631=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_649(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_hint_list(629, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_631(631, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_hint_list_opt(629, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_630(630, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_id_in_coll(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_in_coll(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_in_coll(293, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_294(294, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_in_coll(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_in_coll(301, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_in_coll(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_in_coll(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_in_coll(341, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_189(189, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_id_lookup(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_499(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_lookup(463=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_496(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_id_lookup(464=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_467(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_identified_index_lookup(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_498(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identified_index_lookup(463=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_495(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_identified_index_lookup(464=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_466(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_index(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_625(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_index(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_572(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_index_query(462=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_497(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_index_query(463=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_494(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_index_query(464=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_465(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_label_name(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_224(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_limit(437=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_442(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_limit_opt(437=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_441(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_list_comprehension(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_list_comprehension(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_136(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_literal_ids(468, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_487(487, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_load_csv(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_load_csv(32, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_652(652, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_load_csv(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_load_csv(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_load_csv(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_load_csv(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_load_csv(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_load_csv(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_load_csv_query(32=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_651(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_lookup(458=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_461(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_map_literal(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_220(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_map_literal(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_135(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_match(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_match(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_merge(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_merge(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_merge(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_merge(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_merge(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_merge(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_merge(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_merge_action(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_533(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_merge_action(532=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_539(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_merge_action_list(528, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_532(532, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_merge_action_list_opt(528=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_531(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_node_label(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(347=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(500=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(512=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(519=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_219(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(525=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_226(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(575, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_576(576, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(583, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_607(607, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(637=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_638(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_label(644, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_645(645, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_node_labels(145, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(347, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_labels(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_labels(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_labels(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_232(232, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_labels(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_218(218, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_labels(349, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_347(347, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_labels(500, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_512(512, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_labels(519, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_525(525, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_node_lookup(458=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_460(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_node_pattern(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(111, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(238=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_286(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(338, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(360, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(361, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(363, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(373, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(379, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(510, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(523, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(540, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(560, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(565, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(609, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(612, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(619, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_134(134, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_pattern(628, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_212(212, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_node_property_existence_constraint(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_624(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_node_property_existence_constraint(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_571(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_number_literal(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_number_literal(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_133(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_octal_integer(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_octal_integer(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_132(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_optional_opt(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optional_opt(50, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optional_opt(557, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optional_opt(558, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optional_opt(652, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optional_opt(654, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_optional_opt(659, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_order(69=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_425(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_order_opt(69, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_424(424, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_parameter(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(214=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(218=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(232=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(262=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(276=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_217(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(468, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_486(486, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_473(473, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(479, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_480(480, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parameter(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_131(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_parenthesized_expression(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_parenthesized_expression(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_130(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_partial_comparison_expression(108=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_407(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_partial_comparison_expression(409=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_407(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pattern(560=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_564(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern(628, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_629(629, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pattern_element(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element(210, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_211(211, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_216(216, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element(338, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_339(339, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element(540=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element(560=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element(565=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element(628=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_529(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pattern_element_chain(134=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element_chain(212=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_241(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element_chain(240=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element_chain(388=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_285(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pattern_element_chain_list(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_388(388, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_element_chain_list(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_240(240, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pattern_element_chain_list_opt(212=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_239(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pattern_part(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_528(528, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_part(560, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_563(563, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_part(565, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_563(563, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_part(628, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_563(563, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_pattern_part_commalist(560=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_562(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_part_commalist(565=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_566(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_pattern_part_commalist(628=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_562(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_periodic_commit_hint(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_32(32, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_properties(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_properties(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_properties(214, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_231(231, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_properties(218, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_225(225, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_properties(232, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_234(234, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_properties(262, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_271(271, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_properties(276, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_277(277, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_properties(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_215(215, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_property_expression(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_522(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_expression(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_503(503, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_expression(510, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_503(503, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_expression(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_522(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_expression(609, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_610(610, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_expression(612, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_613(613, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_expression(619, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_620(620, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_property_key_name(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_key_name(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_178(178, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_key_name(351, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_352(352, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_key_name(577, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_578(578, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_key_name(646, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_647(647, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_property_key_name_expression(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_key_name_expression(183, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_177(177, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_property_key_name_expression_commalist(173=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_176(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_key_name_expression_commalist(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_185(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_property_key_name_expression_commalist_opt(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_175(175, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_property_lookup(145=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_lookup(349=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_346(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_lookup(504=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_506(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_property_lookup(505=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_507(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_property_lookup_list(504, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_505(505, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_query(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_query_options(0, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_1(1, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_range_literal(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_267(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_range_literal_opt(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_266(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_range_opt(255, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_262(262, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_range_opt(275, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_276(276, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_reduce(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_reduce(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_129(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_regular_query(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rel_type(589, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_590(590, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rel_type(598, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_599(599, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_rel_type_name(257, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_259(259, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_rel_type_name(591=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_592(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationship_detail(242=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_detail(248=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_250(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationship_detail_opt(242, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_282(282, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_detail_opt(248, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_249(249, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationship_lookup(458=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_459(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationship_pattern(134, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(238, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_pattern(212, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(238, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_pattern(240, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(238, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_pattern(388, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_238(238, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationship_pattern_syntax(580, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_581(581, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationship_property_existence_constraint(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_623(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_property_existence_constraint(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_570(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationship_types(253=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_types(260=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_261(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_types(274=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_256(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationship_types_opt(253, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_255(255, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationship_types_opt(274, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_275(275, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_relationships_pattern(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_relationships_pattern(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_128(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_remove(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_remove(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_remove(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_remove(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_remove(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_remove(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_remove(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_29(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_remove_item(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_521(521, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_remove_item(523, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_521(521, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_remove_item_commalist(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_520(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_remove_item_commalist(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_524(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_return(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_28(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_return_body(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_72(72, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return_body(517=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_518(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_return_item(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return_item(83, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return_item(422, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return_item(517, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_71(71, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_return_item_commalist(67=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return_item_commalist(83=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return_item_commalist(422=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_423(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return_item_commalist(517=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_return_items(67, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_return_items(517, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_69(69, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_set(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(535=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_538(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(536=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_537(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_27(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_set_item(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_502(502, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_item(510, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_502(502, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_set_item_commalist(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_501(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_set_item_commalist(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_511(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_shortest_path_pattern(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(540=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(560=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(565=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_127(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_shortest_path_pattern(628=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_527(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signed_integer_literal(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_448(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_signed_integer_literal(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_126(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_signed_integer_literal_opt(446=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_447(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_single_query(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_26(26, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_single_query(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_661(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_skip(424=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_438(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_skip_opt(424, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_437(437, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sort_item(427, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(429, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sort_item(435, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_429(429, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_sort_item_commalist(427=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_428(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_sort_item_commalist(435=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_436(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_start(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_start(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_start(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_start(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_start(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_start(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_start(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_25(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_start_point(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(454, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_start_point(455, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_454(454, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_start_point_commalist(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_453(453, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_start_point_commalist(455=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_456(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_statement(1, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_24(24, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_symbolic_name(9, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(14, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_12(12, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(19=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_20(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(58=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(60=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(62=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(63=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(146=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(172=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(173, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_174(174, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(183=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(207=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(213=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(222=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_223(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(251=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(257=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(287=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(289=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(293=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(298=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(301=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(304=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(335=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(341=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(351=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(450=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(455=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(469, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_470(470, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(471, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_472(472, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(475, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_476(476, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(510=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(523=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(548=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(553=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(560=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(565=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(577=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(582=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(588=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(591=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_258(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(597=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_125(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(628=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(634=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(636=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(639=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(642=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_87(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_symbolic_name(646=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_184(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_union(26=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_657(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_union(656=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_662(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_union_list(26, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_656(656, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_union_list_opt(26=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_655(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unique_constraint(53=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_622(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unique_constraint(55=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_569(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unique_opt(53, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(560, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unique_opt(559, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_58(560, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unsigned_integer_literal(263=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unsigned_integer_literal(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_265(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unsigned_integer_literal(468, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_485(485, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unsigned_integer_literal(492, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_485(485, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unsigned_integer_literal_commalist(468=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_484(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unsigned_integer_literal_commalist(492=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_493(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unsigned_integer_literal_opt(263, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_264(264, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unsigned_integer_literal_opt(269=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_270(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_unwind(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unwind(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unwind(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unwind(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unwind(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unwind(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_unwind(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_variable(58, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_526(526, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(60, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_519(519, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(62, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(500, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(63, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(452, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(85=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(111=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(146, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(172, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(207, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(213, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(251, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_252(252, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(287, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_214(214, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(289, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_290(290, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(293, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(298, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(301, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(304, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(335, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(341, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_188(188, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(360=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(361=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(363=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(373=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(379=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(450=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_451(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(455, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_452(452, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(510, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_500(500, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(523, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_519(519, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(548, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_549(549, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(553, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_554(554, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(560, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_526(526, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(565, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_526(526, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(582, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(583, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(588, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_589(589, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(597, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_589(598, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(609=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(612=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(619=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_124(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(628, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_526(526, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(634, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(644, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(636, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_575(637, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(639, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_641(641, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable(642, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_641(641, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_variable_commalist(639=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_640(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_variable_commalist(642=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_643(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_version_number(6=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_version_number_opt(6, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_9(9, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_where(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where(453=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where(630=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_202(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_where_opt(72=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_421(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where_opt(189=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_201(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where_opt(453=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_457(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_where_opt(630=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_650(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_with(1=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_with(50=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_with(557=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_with(558=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_with(652=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_with(654=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_with(659=_S, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_22(_S, Cat, Ss, Stack, T, Ts, Tzr).

yeccgoto_with_headers_opt(542, Cat, Ss, Stack, T, Ts, Tzr) ->
 yeccpars2_543(543, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline,yeccpars2_0_/1}).
-file("src/ocparse_legacy.yrl", 365).
yeccpars2_0_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_1_DELETE/1}).
-file("src/ocparse_legacy.yrl", 583).
yeccpars2_1_DELETE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_1_/1}).
-file("src/ocparse_legacy.yrl", 525).
yeccpars2_1_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_2_/1}).
-file("src/ocparse_legacy.yrl", 380).
yeccpars2_2_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { anyCypherOption , __1 }
  end | __Stack].

-compile({inline,yeccpars2_4_/1}).
-file("src/ocparse_legacy.yrl", 366).
yeccpars2_4_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { queryOptions , __1 }
  end | __Stack].

-compile({inline,yeccpars2_5_/1}).
-file("src/ocparse_legacy.yrl", 375).
yeccpars2_5_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_6_/1}).
-file("src/ocparse_legacy.yrl", 387).
yeccpars2_6_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_7_/1}).
-file("src/ocparse_legacy.yrl", 378).
yeccpars2_7_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { anyCypherOption , explain , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_8_/1}).
-file("src/ocparse_legacy.yrl", 379).
yeccpars2_8_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { anyCypherOption , profile , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_9_/1}).
-file("src/ocparse_legacy.yrl", 390).
yeccpars2_9_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_11_/1}).
-file("src/ocparse_legacy.yrl", 397).
yeccpars2_11_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { versionNumber , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_13_/1}).
-file("src/ocparse_legacy.yrl", 382).
yeccpars2_13_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { cypherOption , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_15_/1}).
-file("src/ocparse_legacy.yrl", 394).
yeccpars2_15_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_16_/1}).
-file("src/ocparse_legacy.yrl", 1160).
yeccpars2_16_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { symbolicName , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_17_/1}).
-file("src/ocparse_legacy.yrl", 1161).
yeccpars2_17_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { symbolicName , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_18_/1}).
-file("src/ocparse_legacy.yrl", 393).
yeccpars2_18_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_20_/1}).
-file("src/ocparse_legacy.yrl", 399).
yeccpars2_20_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { configurationOption , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_21_/1}).
-file("src/ocparse_legacy.yrl", 374).
yeccpars2_21_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_22_/1}).
-file("src/ocparse_legacy.yrl", 463).
yeccpars2_22_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_23_/1}).
-file("src/ocparse_legacy.yrl", 456).
yeccpars2_23_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_24_/1}).
-file("src/ocparse_legacy.yrl", 371).
yeccpars2_24_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_25_/1}).
-file("src/ocparse_legacy.yrl", 454).
yeccpars2_25_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_26_/1}).
-file("src/ocparse_legacy.yrl", 412).
yeccpars2_26_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_27_/1}).
-file("src/ocparse_legacy.yrl", 459).
yeccpars2_27_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_28_/1}).
-file("src/ocparse_legacy.yrl", 464).
yeccpars2_28_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_29_/1}).
-file("src/ocparse_legacy.yrl", 461).
yeccpars2_29_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_30_/1}).
-file("src/ocparse_legacy.yrl", 404).
yeccpars2_30_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { query , __1 }
  end | __Stack].

-compile({inline,yeccpars2_34_/1}).
-file("src/ocparse_legacy.yrl", 457).
yeccpars2_34_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_35_/1}).
-file("src/ocparse_legacy.yrl", 455).
yeccpars2_35_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_36_/1}).
-file("src/ocparse_legacy.yrl", 453).
yeccpars2_36_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_37_/1}).
-file("src/ocparse_legacy.yrl", 462).
yeccpars2_37_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_38_/1}).
-file("src/ocparse_legacy.yrl", 469).
yeccpars2_38_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,yeccpars2_39_/1}).
-file("src/ocparse_legacy.yrl", 473).
yeccpars2_39_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,yeccpars2_40_/1}).
-file("src/ocparse_legacy.yrl", 471).
yeccpars2_40_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,yeccpars2_41_/1}).
-file("src/ocparse_legacy.yrl", 467).
yeccpars2_41_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,yeccpars2_43_/1}).
-file("src/ocparse_legacy.yrl", 460).
yeccpars2_43_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,yeccpars2_44_/1}).
-file("src/ocparse_legacy.yrl", 468).
yeccpars2_44_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,yeccpars2_45_/1}).
-file("src/ocparse_legacy.yrl", 472).
yeccpars2_45_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,yeccpars2_46_/1}).
-file("src/ocparse_legacy.yrl", 470).
yeccpars2_46_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,yeccpars2_47_/1}).
-file("src/ocparse_legacy.yrl", 466).
yeccpars2_47_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { command , __1 }
  end | __Stack].

-compile({inline,yeccpars2_48_/1}).
-file("src/ocparse_legacy.yrl", 458).
yeccpars2_48_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { clause , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_50_$end'/1}).
-file("src/ocparse_legacy.yrl", 421).
'yeccpars2_50_$end'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { singleQuery , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_50_;'/1}).
-file("src/ocparse_legacy.yrl", 421).
'yeccpars2_50_;'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { singleQuery , __1 }
  end | __Stack].

-compile({inline,yeccpars2_50_UNION/1}).
-file("src/ocparse_legacy.yrl", 421).
yeccpars2_50_UNION(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { singleQuery , __1 }
  end | __Stack].

-compile({inline,yeccpars2_50_DELETE/1}).
-file("src/ocparse_legacy.yrl", 583).
yeccpars2_50_DELETE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_50_/1}).
-file("src/ocparse_legacy.yrl", 525).
yeccpars2_50_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_51_/1}).
-file("src/ocparse_legacy.yrl", 427).
yeccpars2_51_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_52_/1}).
-file("src/ocparse_legacy.yrl", 405).
yeccpars2_52_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { query , __1 }
  end | __Stack].

-compile({inline,yeccpars2_53_/1}).
-file("src/ocparse_legacy.yrl", 560).
yeccpars2_53_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_54_/1}).
-file("src/ocparse_legacy.yrl", 584).
yeccpars2_54_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "detach"
  end | __Stack].

-compile({inline,yeccpars2_59_/1}).
-file("src/ocparse_legacy.yrl", 526).
yeccpars2_59_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "optional"
  end | __Stack].

-compile({inline,yeccpars2_61_/1}).
-file("src/ocparse_legacy.yrl", 609).
yeccpars2_61_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_64_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_64_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_66_/1}).
-file("src/ocparse_legacy.yrl", 609).
yeccpars2_66_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_67_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_67_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_68_/1}).
-file("src/ocparse_legacy.yrl", 610).
yeccpars2_68_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "distinct"
  end | __Stack].

-compile({inline,yeccpars2_69_/1}).
-file("src/ocparse_legacy.yrl", 620).
yeccpars2_69_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_70_/1}).
-file("src/ocparse_legacy.yrl", 632).
yeccpars2_70_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { returnItems , [ ] , __1 }
  end | __Stack].

-compile({inline,yeccpars2_71_/1}).
-file("src/ocparse_legacy.yrl", 637).
yeccpars2_71_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_72_/1}).
-file("src/ocparse_legacy.yrl", 534).
yeccpars2_72_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_73_/1}).
-file("src/ocparse_legacy.yrl", 952).
yeccpars2_73_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_75_/1}).
-file("src/ocparse_legacy.yrl", 883).
yeccpars2_75_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_76_/1}).
-file("src/ocparse_legacy.yrl", 865).
yeccpars2_76_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_77_/1}).
-file("src/ocparse_legacy.yrl", 830).
yeccpars2_77_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { expression , __1 }
  end | __Stack].

-compile({inline,yeccpars2_78_/1}).
-file("src/ocparse_legacy.yrl", 837).
yeccpars2_78_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_79_/1}).
-file("src/ocparse_legacy.yrl", 851).
yeccpars2_79_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_80_/1}).
-file("src/ocparse_legacy.yrl", 642).
yeccpars2_80_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { returnItem , __1 }
  end | __Stack].

-compile({inline,yeccpars2_81_/1}).
-file("src/ocparse_legacy.yrl", 630).
yeccpars2_81_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { returnItems , "*" , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_82_/1}).
-file("src/ocparse_legacy.yrl", 885).
yeccpars2_82_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { "not" }
  end | __Stack].

-compile({inline,yeccpars2_83_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_83_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_84_/1}).
-file("src/ocparse_legacy.yrl", 631).
yeccpars2_84_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { returnItems , "*" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_86_/1}).
-file("src/ocparse_legacy.yrl", 641).
yeccpars2_86_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { returnItem , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_87_/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_87_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_88_/1}).
-file("src/ocparse_legacy.yrl", 846).
yeccpars2_88_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression11 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_90_/1}).
-file("src/ocparse_legacy.yrl", 855).
yeccpars2_90_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_91_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_91_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_92_/1}).
-file("src/ocparse_legacy.yrl", 857).
yeccpars2_92_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "xor" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_93_/1}).
-file("src/ocparse_legacy.yrl", 854).
yeccpars2_93_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_94_/1}).
-file("src/ocparse_legacy.yrl", 832).
yeccpars2_94_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression12 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_96_/1}).
-file("src/ocparse_legacy.yrl", 841).
yeccpars2_96_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_97_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_97_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_98_/1}).
-file("src/ocparse_legacy.yrl", 843).
yeccpars2_98_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "or" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_99_/1}).
-file("src/ocparse_legacy.yrl", 840).
yeccpars2_99_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_100_/1}).
-file("src/ocparse_legacy.yrl", 860).
yeccpars2_100_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression10 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_102_/1}).
-file("src/ocparse_legacy.yrl", 869).
yeccpars2_102_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_103_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_103_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_104_/1}).
-file("src/ocparse_legacy.yrl", 871).
yeccpars2_104_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "and" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_105_/1}).
-file("src/ocparse_legacy.yrl", 868).
yeccpars2_105_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_106_/1}).
-file("src/ocparse_legacy.yrl", 882).
yeccpars2_106_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_107_/1}).
-file("src/ocparse_legacy.yrl", 874).
yeccpars2_107_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression9 , __2 , __1 }
  end | __Stack].

-compile({inline,yeccpars2_108_/1}).
-file("src/ocparse_legacy.yrl", 893).
yeccpars2_108_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_109_/1}).
-file("src/ocparse_legacy.yrl", 907).
yeccpars2_109_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_110_/1}).
-file("src/ocparse_legacy.yrl", 922).
yeccpars2_110_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_113_/1}).
-file("src/ocparse_legacy.yrl", 956).
yeccpars2_113_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_114_/1}).
-file("src/ocparse_legacy.yrl", 938).
yeccpars2_114_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_115_/1}).
-file("src/ocparse_legacy.yrl", 958).
yeccpars2_115_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { "+" }
  end | __Stack].

-compile({inline,yeccpars2_116_/1}).
-file("src/ocparse_legacy.yrl", 959).
yeccpars2_116_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { "-" }
  end | __Stack].

-compile({inline,yeccpars2_117_/1}).
-file("src/ocparse_legacy.yrl", 933).
yeccpars2_117_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression5 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_119_/1}).
-file("src/ocparse_legacy.yrl", 942).
yeccpars2_119_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_120_/1}).
-file("src/ocparse_legacy.yrl", 952).
yeccpars2_120_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_121_/1}).
-file("src/ocparse_legacy.yrl", 944).
yeccpars2_121_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "^" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_122_/1}).
-file("src/ocparse_legacy.yrl", 941).
yeccpars2_122_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_123_/1}).
-file("src/ocparse_legacy.yrl", 955).
yeccpars2_123_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_124_/1}).
-file("src/ocparse_legacy.yrl", 1025).
yeccpars2_124_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_!='/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_!='(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_$end'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_$end'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_%'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_%'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_)'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_)'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_*'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_*'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_+'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_+'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_+='/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_+='(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_,'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_,'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_-'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_-'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_.'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_.'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_..'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_..'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_/'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_/'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_:'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_:'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_;'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_;'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_<'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_<'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_<='/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_<='(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_<>'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_<>'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_='/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_='(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_=~'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_=~'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_>'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_>'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_>='/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_>='(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_AND/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_AND(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_AS/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_AS(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_ASC/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_ASC(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_ASCENDING/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_ASCENDING(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_CONTAINS/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_CONTAINS(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_CREATE/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_CREATE(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_DELETE/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_DELETE(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_DESC/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_DESC(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_DESCENDING/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_DESCENDING(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_DETACH/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_DETACH(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_ELSE/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_ELSE(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_END/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_END(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_ENDS/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_ENDS(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_FOREACH/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_FOREACH(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_IN/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_IN(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_IS/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_IS(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_LIMIT/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_LIMIT(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_LOAD/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_LOAD(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_MATCH/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_MATCH(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_MERGE/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_MERGE(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_ON/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_ON(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_OPTIONAL/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_OPTIONAL(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_OR/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_OR(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_ORDER/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_ORDER(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_REMOVE/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_REMOVE(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_RETURN/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_RETURN(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_SET/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_SET(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_SKIP/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_SKIP(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_START/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_START(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_STARTS/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_STARTS(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_THEN/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_THEN(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_UNION/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_UNION(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_UNWIND/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_UNWIND(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_WHEN/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_WHEN(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_WHERE/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_WHERE(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_WITH/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_WITH(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_XOR/1}).
-file("src/ocparse_legacy.yrl", 1102).
yeccpars2_125_XOR(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_['/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_['(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_]'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_]'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_^'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_^'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_|'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_|'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,'yeccpars2_125_}'/1}).
-file("src/ocparse_legacy.yrl", 1102).
'yeccpars2_125_}'(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { variable , __1 }
  end | __Stack].

-compile({inline,yeccpars2_125_/1}).
-file("src/ocparse_legacy.yrl", 1063).
yeccpars2_125_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { functionName , __1 }
  end | __Stack].

-compile({inline,yeccpars2_126_/1}).
-file("src/ocparse_legacy.yrl", 1105).
yeccpars2_126_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { numberLiteral , __1 }
  end | __Stack].

-compile({inline,yeccpars2_127_/1}).
-file("src/ocparse_legacy.yrl", 1022).
yeccpars2_127_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_128_/1}).
-file("src/ocparse_legacy.yrl", 1021).
yeccpars2_128_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_129_/1}).
-file("src/ocparse_legacy.yrl", 1016).
yeccpars2_129_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_130_/1}).
-file("src/ocparse_legacy.yrl", 1023).
yeccpars2_130_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_131_/1}).
-file("src/ocparse_legacy.yrl", 1004).
yeccpars2_131_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_132_/1}).
-file("src/ocparse_legacy.yrl", 1137).
yeccpars2_132_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signedIntegerLiteral , __1 }
  end | __Stack].

-compile({inline,yeccpars2_133_/1}).
-file("src/ocparse_legacy.yrl", 1002).
yeccpars2_133_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_135_/1}).
-file("src/ocparse_legacy.yrl", 1010).
yeccpars2_135_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_136_/1}).
-file("src/ocparse_legacy.yrl", 1011).
yeccpars2_136_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_137_/1}).
-file("src/ocparse_legacy.yrl", 1136).
yeccpars2_137_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signedIntegerLiteral , __1 }
  end | __Stack].

-compile({inline,yeccpars2_139_/1}).
-file("src/ocparse_legacy.yrl", 1024).
yeccpars2_139_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_140_/1}).
-file("src/ocparse_legacy.yrl", 947).
yeccpars2_140_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression4 , __2 , __1 }
  end | __Stack].

-compile({inline,yeccpars2_141_/1}).
-file("src/ocparse_legacy.yrl", 967).
yeccpars2_141_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_142_/1}).
-file("src/ocparse_legacy.yrl", 1104).
yeccpars2_142_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { numberLiteral , __1 }
  end | __Stack].

-compile({inline,yeccpars2_143_/1}).
-file("src/ocparse_legacy.yrl", 1138).
yeccpars2_143_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { signedIntegerLiteral , __1 }
  end | __Stack].

-compile({inline,yeccpars2_144_/1}).
-file("src/ocparse_legacy.yrl", 1008).
yeccpars2_144_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_145_/1}).
-file("src/ocparse_legacy.yrl", 992).
yeccpars2_145_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_146_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_146_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_150_('/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_150_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_150_+'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_150_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_150_-'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_150_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_ALL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_ALL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_ALLSHORTESTPATHS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_ALLSHORTESTPATHS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_ANY/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_ANY(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_CASE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_CASE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_COUNT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_COUNT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_DIGIT_STRING/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_DIGIT_STRING(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_ESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_ESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_EXISTS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_EXISTS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_EXPONENT_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_EXPONENT_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_EXTRACT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_EXTRACT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_FALSE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_FALSE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_FILTER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_FILTER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_HEX_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_HEX_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_NONE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_NONE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_NULL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_NULL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_REDUCE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_REDUCE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_SHORTESTPATH/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_SHORTESTPATH(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_SIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_SIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_SIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_SIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_SIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_SIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_SINGLE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_SINGLE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_STRING_LITERAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_STRING_LITERAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_TRUE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_TRUE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_UNESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_UNESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_UNSIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_UNSIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_UNSIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_UNSIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_UNSIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_150_UNSIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_150_['/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_150_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_150_{'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_150_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_150_/1}).
-file("src/ocparse_legacy.yrl", 983).
yeccpars2_150_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_152_/1}).
-file("src/ocparse_legacy.yrl", 1153).
yeccpars2_152_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __1 )
  end | __Stack].

-compile({inline,yeccpars2_154_/1}).
-file("src/ocparse_legacy.yrl", 1156).
yeccpars2_154_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { doubleLiteral , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_156_/1}).
-file("src/ocparse_legacy.yrl", 1006).
yeccpars2_156_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , { terminal , "false" } }
  end | __Stack].

-compile({inline,yeccpars2_158_/1}).
-file("src/ocparse_legacy.yrl", 1142).
yeccpars2_158_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { hexInteger , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_160_/1}).
-file("src/ocparse_legacy.yrl", 1007).
yeccpars2_160_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , { terminal , "null" } }
  end | __Stack].

-compile({inline,yeccpars2_163_/1}).
-file("src/ocparse_legacy.yrl", 1144).
yeccpars2_163_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { decimalInteger , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_164_/1}).
-file("src/ocparse_legacy.yrl", 1147).
yeccpars2_164_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { octalInteger , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_165_/1}).
-file("src/ocparse_legacy.yrl", 1157).
yeccpars2_165_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { doubleLiteral , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_167_/1}).
-file("src/ocparse_legacy.yrl", 1003).
yeccpars2_167_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , { stringLiteral , unwrap ( __1 ) } }
  end | __Stack].

-compile({inline,yeccpars2_168_/1}).
-file("src/ocparse_legacy.yrl", 1005).
yeccpars2_168_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , { terminal , "true" } }
  end | __Stack].

-compile({inline,yeccpars2_169_/1}).
-file("src/ocparse_legacy.yrl", 1145).
yeccpars2_169_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { decimalInteger , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_170_/1}).
-file("src/ocparse_legacy.yrl", 1148).
yeccpars2_170_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { octalInteger , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_171_/1}).
-file("src/ocparse_legacy.yrl", 1158).
yeccpars2_171_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { doubleLiteral , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_172_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_172_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_173_/1}).
-file("src/ocparse_legacy.yrl", 1112).
yeccpars2_173_(__Stack0) ->
 [begin
   "{}"
  end | __Stack0].

-compile({inline,yeccpars2_174_/1}).
-file("src/ocparse_legacy.yrl", 1134).
yeccpars2_174_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { propertyKeyName , __1 }
  end | __Stack].

-compile({inline,yeccpars2_177_/1}).
-file("src/ocparse_legacy.yrl", 1115).
yeccpars2_177_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_180_/1}).
-file("src/ocparse_legacy.yrl", 1123).
yeccpars2_180_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { parameter , unwrap ( __2 ) }
  end | __Stack].

-compile({inline,yeccpars2_181_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_181_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_182_/1}).
-file("src/ocparse_legacy.yrl", 1119).
yeccpars2_182_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_184_/1}).
-file("src/ocparse_legacy.yrl", 1134).
yeccpars2_184_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { propertyKeyName , __1 }
  end | __Stack].

-compile({inline,yeccpars2_185_/1}).
-file("src/ocparse_legacy.yrl", 1117).
yeccpars2_185_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_186_/1}).
-file("src/ocparse_legacy.yrl", 1107).
yeccpars2_186_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mapLiteral , __2 }
  end | __Stack].

-compile({inline,yeccpars2_187_/1}).
-file("src/ocparse_legacy.yrl", 1122).
yeccpars2_187_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { parameter , __2 }
  end | __Stack].

-compile({inline,yeccpars2_189_/1}).
-file("src/ocparse_legacy.yrl", 534).
yeccpars2_189_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_190_/1}).
-file("src/ocparse_legacy.yrl", 1070).
yeccpars2_190_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_192_/1}).
-file("src/ocparse_legacy.yrl", 586).
yeccpars2_192_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_193_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_193_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_194_/1}).
-file("src/ocparse_legacy.yrl", 587).
yeccpars2_194_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_195_/1}).
-file("src/ocparse_legacy.yrl", 1012).
yeccpars2_195_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , __2 , "]" }
  end | __Stack].

-compile({inline,yeccpars2_198_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_198_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_199_/1}).
-file("src/ocparse_legacy.yrl", 1073).
yeccpars2_199_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_200_/1}).
-file("src/ocparse_legacy.yrl", 1065).
yeccpars2_200_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { listComprehension , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_201_/1}).
-file("src/ocparse_legacy.yrl", 1047).
yeccpars2_201_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { filterExpression , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_203_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_203_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_204_/1}).
-file("src/ocparse_legacy.yrl", 719).
yeccpars2_204_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { where , __2 }
  end | __Stack].

-compile({inline,yeccpars2_205_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_205_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_206_/1}).
-file("src/ocparse_legacy.yrl", 1049).
yeccpars2_206_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { idInColl , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_209_/1}).
-file("src/ocparse_legacy.yrl", 1020).
yeccpars2_209_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , { single , __3 } }
  end | __Stack].

-compile({inline,yeccpars2_212_/1}).
-file("src/ocparse_legacy.yrl", 745).
yeccpars2_212_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_217_/1}).
-file("src/ocparse_legacy.yrl", 805).
yeccpars2_217_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { properties , __1 }
  end | __Stack].

-compile({inline,yeccpars2_219_/1}).
-file("src/ocparse_legacy.yrl", 813).
yeccpars2_219_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_220_/1}).
-file("src/ocparse_legacy.yrl", 804).
yeccpars2_220_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { properties , __1 }
  end | __Stack].

-compile({inline,yeccpars2_221_/1}).
-file("src/ocparse_legacy.yrl", 759).
yeccpars2_221_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePattern , { } , { } , { } }
  end | __Stack].

-compile({inline,yeccpars2_223_/1}).
-file("src/ocparse_legacy.yrl", 826).
yeccpars2_223_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { labelName , __1 }
  end | __Stack].

-compile({inline,yeccpars2_224_/1}).
-file("src/ocparse_legacy.yrl", 815).
yeccpars2_224_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { nodeLabel , __2 }
  end | __Stack].

-compile({inline,yeccpars2_226_/1}).
-file("src/ocparse_legacy.yrl", 812).
yeccpars2_226_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_227_/1}).
-file("src/ocparse_legacy.yrl", 757).
yeccpars2_227_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePattern , { } , { nodeLabels , __2 } , { } }
  end | __Stack].

-compile({inline,yeccpars2_228_/1}).
-file("src/ocparse_legacy.yrl", 756).
yeccpars2_228_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePattern , { } , { nodeLabels , __2 } , __3 }
  end | __Stack].

-compile({inline,yeccpars2_229_/1}).
-file("src/ocparse_legacy.yrl", 740).
yeccpars2_229_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { patternElement , __2 }
  end | __Stack].

-compile({inline,yeccpars2_230_/1}).
-file("src/ocparse_legacy.yrl", 758).
yeccpars2_230_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePattern , { } , { } , __2 }
  end | __Stack].

-compile({inline,yeccpars2_233_/1}).
-file("src/ocparse_legacy.yrl", 755).
yeccpars2_233_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePattern , __2 , { } , { } }
  end | __Stack].

-compile({inline,yeccpars2_235_/1}).
-file("src/ocparse_legacy.yrl", 753).
yeccpars2_235_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePattern , __2 , { nodeLabels , __3 } , { } }
  end | __Stack].

-compile({inline,yeccpars2_236_/1}).
-file("src/ocparse_legacy.yrl", 752).
yeccpars2_236_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePattern , __2 , { nodeLabels , __3 } , __4 }
  end | __Stack].

-compile({inline,yeccpars2_237_/1}).
-file("src/ocparse_legacy.yrl", 754).
yeccpars2_237_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePattern , __2 , { } , __3 }
  end | __Stack].

-compile({inline,yeccpars2_239_/1}).
-file("src/ocparse_legacy.yrl", 739).
yeccpars2_239_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { patternElement , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_241_/1}).
-file("src/ocparse_legacy.yrl", 749).
yeccpars2_241_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_242_/1}).
-file("src/ocparse_legacy.yrl", 775).
yeccpars2_242_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_243_/1}).
-file("src/ocparse_legacy.yrl", 769).
yeccpars2_243_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { relationshipPattern , "--" }
  end | __Stack].

-compile({inline,yeccpars2_244_/1}).
-file("src/ocparse_legacy.yrl", 767).
yeccpars2_244_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { relationshipPattern , "-->" }
  end | __Stack].

-compile({inline,yeccpars2_246_/1}).
-file("src/ocparse_legacy.yrl", 765).
yeccpars2_246_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { relationshipPattern , "<--" }
  end | __Stack].

-compile({inline,yeccpars2_247_/1}).
-file("src/ocparse_legacy.yrl", 763).
yeccpars2_247_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { relationshipPattern , "<-->" }
  end | __Stack].

-compile({inline,yeccpars2_248_/1}).
-file("src/ocparse_legacy.yrl", 775).
yeccpars2_248_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_251_/1}).
-file("src/ocparse_legacy.yrl", 791).
yeccpars2_251_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_252_/1}).
-file("src/ocparse_legacy.yrl", 791).
yeccpars2_252_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_253_/1}).
-file("src/ocparse_legacy.yrl", 794).
yeccpars2_253_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_254_/1}).
-file("src/ocparse_legacy.yrl", 792).
yeccpars2_254_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "?"
  end | __Stack].

-compile({inline,yeccpars2_255_/1}).
-file("src/ocparse_legacy.yrl", 797).
yeccpars2_255_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_256_/1}).
-file("src/ocparse_legacy.yrl", 795).
yeccpars2_256_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { relationshipTypes , __1 }
  end | __Stack].

-compile({inline,yeccpars2_258_/1}).
-file("src/ocparse_legacy.yrl", 828).
yeccpars2_258_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { relTypeName , __1 }
  end | __Stack].

-compile({inline,yeccpars2_259_/1}).
-file("src/ocparse_legacy.yrl", 809).
yeccpars2_259_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_261_/1}).
-file("src/ocparse_legacy.yrl", 810).
yeccpars2_261_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __2 | __4 ]
  end | __Stack].

-compile({inline,'yeccpars2_263_..'/1}).
-file("src/ocparse_legacy.yrl", 822).
'yeccpars2_263_..'(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_263_/1}).
-file("src/ocparse_legacy.yrl", 800).
yeccpars2_263_(__Stack0) ->
 [begin
   "*"
  end | __Stack0].

-compile({inline,yeccpars2_266_/1}).
-file("src/ocparse_legacy.yrl", 798).
yeccpars2_266_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "*" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_268_/1}).
-file("src/ocparse_legacy.yrl", 1140).
yeccpars2_268_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { unsignedIntegerLiteral , unwrap ( __1 ) }
  end | __Stack].

-compile({inline,yeccpars2_269_/1}).
-file("src/ocparse_legacy.yrl", 822).
yeccpars2_269_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_270_/1}).
-file("src/ocparse_legacy.yrl", 817).
yeccpars2_270_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { rangeLiteral , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_272_/1}).
-file("src/ocparse_legacy.yrl", 786).
yeccpars2_272_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipDetail , { } , __2 , __3 , __4 , { } }
  end | __Stack].

-compile({inline,yeccpars2_273_/1}).
-file("src/ocparse_legacy.yrl", 785).
yeccpars2_273_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipDetail , { } , __2 , __3 , __4 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_274_/1}).
-file("src/ocparse_legacy.yrl", 794).
yeccpars2_274_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_275_/1}).
-file("src/ocparse_legacy.yrl", 797).
yeccpars2_275_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_278_/1}).
-file("src/ocparse_legacy.yrl", 783).
yeccpars2_278_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipDetail , __2 , __3 , __4 , __5 , { } }
  end | __Stack].

-compile({inline,yeccpars2_279_/1}).
-file("src/ocparse_legacy.yrl", 780).
yeccpars2_279_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipDetail , __2 , __3 , __4 , __5 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_280_/1}).
-file("src/ocparse_legacy.yrl", 766).
yeccpars2_280_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipPattern , "<" , "-" , __3 , "-" , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_281_/1}).
-file("src/ocparse_legacy.yrl", 764).
yeccpars2_281_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipPattern , "<" , "-" , __3 , "-" , ">" }
  end | __Stack].

-compile({inline,yeccpars2_283_/1}).
-file("src/ocparse_legacy.yrl", 770).
yeccpars2_283_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipPattern , [ ] , "-" , __2 , "-" , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_284_/1}).
-file("src/ocparse_legacy.yrl", 768).
yeccpars2_284_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipPattern , [ ] , "-" , __2 , "-" , ">" }
  end | __Stack].

-compile({inline,yeccpars2_285_/1}).
-file("src/ocparse_legacy.yrl", 748).
yeccpars2_285_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_286_/1}).
-file("src/ocparse_legacy.yrl", 761).
yeccpars2_286_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { patternElementChain , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_288_/1}).
-file("src/ocparse_legacy.yrl", 736).
yeccpars2_288_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { shortestPathPattern , "shortestpath" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_291_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_291_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_295_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_295_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_297_/1}).
-file("src/ocparse_legacy.yrl", 1027).
yeccpars2_297_(__Stack0) ->
 [__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { reduce , __3 , __5 , __7 , __9 }
  end | __Stack].

-compile({inline,yeccpars2_300_/1}).
-file("src/ocparse_legacy.yrl", 1019).
yeccpars2_300_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , { none , __3 } }
  end | __Stack].

-compile({inline,yeccpars2_303_/1}).
-file("src/ocparse_legacy.yrl", 1013).
yeccpars2_303_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , { filter , __3 } }
  end | __Stack].

-compile({inline,yeccpars2_306_/1}).
-file("src/ocparse_legacy.yrl", 1015).
yeccpars2_306_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , { extract , __3 } }
  end | __Stack].

-compile({inline,yeccpars2_307_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_307_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_309_/1}).
-file("src/ocparse_legacy.yrl", 1014).
yeccpars2_309_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , { extract , __3 , __5 } }
  end | __Stack].

-compile({inline,yeccpars2_310_/1}).
-file("src/ocparse_legacy.yrl", 609).
yeccpars2_310_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_311_('/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_311_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_311_+'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_311_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_311_-'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_311_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_ALL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_ALL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_ALLSHORTESTPATHS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_ALLSHORTESTPATHS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_ANY/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_ANY(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_CASE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_CASE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_COUNT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_COUNT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_DIGIT_STRING/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_DIGIT_STRING(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_ESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_ESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_EXISTS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_EXISTS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_EXPONENT_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_EXPONENT_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_EXTRACT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_EXTRACT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_FALSE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_FALSE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_FILTER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_FILTER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_HEX_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_HEX_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_NONE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_NONE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_NULL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_NULL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_REDUCE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_REDUCE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_SHORTESTPATH/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_SHORTESTPATH(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_SIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_SIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_SIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_SIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_SIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_SIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_SINGLE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_SINGLE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_STRING_LITERAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_STRING_LITERAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_TRUE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_TRUE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_UNESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_UNESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_UNSIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_UNSIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_UNSIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_UNSIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_UNSIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_311_UNSIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_311_['/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_311_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_311_{'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_311_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_311_/1}).
-file("src/ocparse_legacy.yrl", 1059).
yeccpars2_311_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_314_/1}).
-file("src/ocparse_legacy.yrl", 1057).
yeccpars2_314_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { functionInvocation , { functionName , { symbolicName , "exists" } } , __4 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_315_/1}).
-file("src/ocparse_legacy.yrl", 609).
yeccpars2_315_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_316_('/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_316_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_316_+'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_316_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_316_-'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_316_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_ALL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_ALL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_ALLSHORTESTPATHS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_ALLSHORTESTPATHS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_ANY/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_ANY(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_CASE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_CASE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_COUNT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_COUNT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_DIGIT_STRING/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_DIGIT_STRING(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_ESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_ESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_EXISTS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_EXISTS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_EXPONENT_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_EXPONENT_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_EXTRACT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_EXTRACT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_FALSE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_FALSE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_FILTER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_FILTER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_HEX_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_HEX_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_NONE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_NONE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_NULL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_NULL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_REDUCE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_REDUCE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_SHORTESTPATH/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_SHORTESTPATH(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_SIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_SIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_SIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_SIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_SIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_SIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_SINGLE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_SINGLE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_STRING_LITERAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_STRING_LITERAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_TRUE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_TRUE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_UNESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_UNESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_UNSIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_UNSIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_UNSIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_UNSIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_UNSIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_316_UNSIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_316_['/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_316_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_316_{'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_316_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_316_/1}).
-file("src/ocparse_legacy.yrl", 1059).
yeccpars2_316_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_318_/1}).
-file("src/ocparse_legacy.yrl", 1009).
yeccpars2_318_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , { terminal , "count(*)" } }
  end | __Stack].

-compile({inline,yeccpars2_320_/1}).
-file("src/ocparse_legacy.yrl", 1056).
yeccpars2_320_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { functionInvocation , { functionName , { symbolicName , "count" } } , __4 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_323_/1}).
-file("src/ocparse_legacy.yrl", 1091).
yeccpars2_323_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_324_/1}).
-file("src/ocparse_legacy.yrl", 1097).
yeccpars2_324_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_325_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_325_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_327_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_327_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_328_/1}).
-file("src/ocparse_legacy.yrl", 1100).
yeccpars2_328_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { caseAlternatives , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_331_/1}).
-file("src/ocparse_legacy.yrl", 1096).
yeccpars2_331_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_332_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_332_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_333_/1}).
-file("src/ocparse_legacy.yrl", 1094).
yeccpars2_333_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __2
  end | __Stack].

-compile({inline,yeccpars2_334_/1}).
-file("src/ocparse_legacy.yrl", 1086).
yeccpars2_334_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { caseExpression , __2 , __3 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_337_/1}).
-file("src/ocparse_legacy.yrl", 1018).
yeccpars2_337_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , { any , __3 } }
  end | __Stack].

-compile({inline,yeccpars2_340_/1}).
-file("src/ocparse_legacy.yrl", 737).
yeccpars2_340_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { shortestPathPattern , "allshortestpaths" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_343_/1}).
-file("src/ocparse_legacy.yrl", 1017).
yeccpars2_343_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { atom , { all , __3 } }
  end | __Stack].

-compile({inline,yeccpars2_345_/1}).
-file("src/ocparse_legacy.yrl", 1043).
yeccpars2_345_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { parenthesizedExpression , __2 }
  end | __Stack].

-compile({inline,yeccpars2_347_/1}).
-file("src/ocparse_legacy.yrl", 998).
yeccpars2_347_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { nodeLabels , __1 }
  end | __Stack].

-compile({inline,yeccpars2_348_/1}).
-file("src/ocparse_legacy.yrl", 987).
yeccpars2_348_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression2 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_350_/1}).
-file("src/ocparse_legacy.yrl", 996).
yeccpars2_350_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_352_/1}).
-file("src/ocparse_legacy.yrl", 1081).
yeccpars2_352_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_353_/1}).
-file("src/ocparse_legacy.yrl", 1076).
yeccpars2_353_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { propertyLookup , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_354_/1}).
-file("src/ocparse_legacy.yrl", 1083).
yeccpars2_354_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "!"
  end | __Stack].

-compile({inline,yeccpars2_355_/1}).
-file("src/ocparse_legacy.yrl", 1082).
yeccpars2_355_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "?"
  end | __Stack].

-compile({inline,yeccpars2_356_/1}).
-file("src/ocparse_legacy.yrl", 995).
yeccpars2_356_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_357_/1}).
-file("src/ocparse_legacy.yrl", 962).
yeccpars2_357_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression3 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_359_/1}).
-file("src/ocparse_legacy.yrl", 971).
yeccpars2_359_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,'yeccpars2_366_('/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_366_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_366_+'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_366_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_366_-'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_366_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_ALL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_ALL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_ALLSHORTESTPATHS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_ALLSHORTESTPATHS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_ANY/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_ANY(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_CASE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_CASE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_COUNT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_COUNT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_DIGIT_STRING/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_DIGIT_STRING(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_ESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_ESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_EXISTS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_EXISTS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_EXPONENT_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_EXPONENT_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_EXTRACT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_EXTRACT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_FALSE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_FALSE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_FILTER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_FILTER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_HEX_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_HEX_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_NONE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_NONE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_NULL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_NULL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_REDUCE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_REDUCE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_SHORTESTPATH/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_SHORTESTPATH(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_SIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_SIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_SIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_SIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_SIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_SIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_SINGLE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_SINGLE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_STRING_LITERAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_STRING_LITERAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_TRUE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_TRUE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_UNESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_UNESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_UNSIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_UNSIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_UNSIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_UNSIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_UNSIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_366_UNSIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_366_['/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_366_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_366_{'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_366_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_366_/1}).
-file("src/ocparse_legacy.yrl", 983).
yeccpars2_366_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_369_/1}).
-file("src/ocparse_legacy.yrl", 974).
yeccpars2_369_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { "[" , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_370_('/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_370_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_370_+'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_370_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_370_-'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_370_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_ALL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_ALL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_ALLSHORTESTPATHS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_ALLSHORTESTPATHS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_ANY/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_ANY(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_CASE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_CASE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_COUNT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_COUNT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_DIGIT_STRING/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_DIGIT_STRING(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_ESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_ESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_EXISTS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_EXISTS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_EXPONENT_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_EXPONENT_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_EXTRACT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_EXTRACT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_FALSE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_FALSE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_FILTER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_FILTER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_HEX_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_HEX_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_NONE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_NONE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_NULL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_NULL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_REDUCE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_REDUCE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_SHORTESTPATH/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_SHORTESTPATH(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_SIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_SIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_SIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_SIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_SIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_SIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_SINGLE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_SINGLE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_STRING_LITERAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_STRING_LITERAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_TRUE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_TRUE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_UNESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_UNESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_UNSIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_UNSIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_UNSIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_UNSIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_UNSIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_370_UNSIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_370_['/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_370_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_370_{'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_370_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_370_/1}).
-file("src/ocparse_legacy.yrl", 983).
yeccpars2_370_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_372_/1}).
-file("src/ocparse_legacy.yrl", 973).
yeccpars2_372_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { "[" , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_374_/1}).
-file("src/ocparse_legacy.yrl", 977).
yeccpars2_374_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { "starts with" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_376_/1}).
-file("src/ocparse_legacy.yrl", 981).
yeccpars2_376_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "is null" }
  end | __Stack].

-compile({inline,yeccpars2_377_/1}).
-file("src/ocparse_legacy.yrl", 980).
yeccpars2_377_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { "is not null" }
  end | __Stack].

-compile({inline,yeccpars2_378_/1}).
-file("src/ocparse_legacy.yrl", 976).
yeccpars2_378_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "in" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_380_/1}).
-file("src/ocparse_legacy.yrl", 978).
yeccpars2_380_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { "ends with" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_381_/1}).
-file("src/ocparse_legacy.yrl", 979).
yeccpars2_381_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "contains" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_382_/1}).
-file("src/ocparse_legacy.yrl", 975).
yeccpars2_382_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "=~" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_383_/1}).
-file("src/ocparse_legacy.yrl", 970).
yeccpars2_383_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_384_/1}).
-file("src/ocparse_legacy.yrl", 609).
yeccpars2_384_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_385_('/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_385_('(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_385_+'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_385_+'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_385_-'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_385_-'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_ALL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_ALL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_ALLSHORTESTPATHS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_ALLSHORTESTPATHS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_ANY/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_ANY(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_CASE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_CASE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_COUNT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_COUNT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_DIGIT_STRING/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_DIGIT_STRING(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_ESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_ESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_EXISTS/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_EXISTS(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_EXPONENT_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_EXPONENT_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_EXTRACT/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_EXTRACT(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_FALSE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_FALSE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_FILTER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_FILTER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_HEX_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_HEX_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_NONE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_NONE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_NULL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_NULL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_REDUCE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_REDUCE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_SHORTESTPATH/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_SHORTESTPATH(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_SIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_SIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_SIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_SIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_SIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_SIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_SINGLE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_SINGLE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_STRING_LITERAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_STRING_LITERAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_TRUE/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_TRUE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_UNESCAPED_SYMBOLIC_NAME/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_UNESCAPED_SYMBOLIC_NAME(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_UNSIGNED_DECIMAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_UNSIGNED_DECIMAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_UNSIGNED_OCTAL_INTEGER/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_UNSIGNED_OCTAL_INTEGER(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_UNSIGNED_REGULAR_DECIMAL_REAL/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_385_UNSIGNED_REGULAR_DECIMAL_REAL(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_385_['/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_385_['(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_385_{'/1}).
-file("src/ocparse_legacy.yrl", 879).
'yeccpars2_385_{'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_385_/1}).
-file("src/ocparse_legacy.yrl", 1059).
yeccpars2_385_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_387_/1}).
-file("src/ocparse_legacy.yrl", 1051).
yeccpars2_387_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { functionInvocation , __1 , __4 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_388_/1}).
-file("src/ocparse_legacy.yrl", 1045).
yeccpars2_388_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipsPattern , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_389_/1}).
-file("src/ocparse_legacy.yrl", 917).
yeccpars2_389_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression6 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_391_/1}).
-file("src/ocparse_legacy.yrl", 926).
yeccpars2_391_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_392_/1}).
-file("src/ocparse_legacy.yrl", 952).
yeccpars2_392_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_393_/1}).
-file("src/ocparse_legacy.yrl", 952).
yeccpars2_393_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_394_/1}).
-file("src/ocparse_legacy.yrl", 952).
yeccpars2_394_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_395_/1}).
-file("src/ocparse_legacy.yrl", 929).
yeccpars2_395_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "/" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_396_/1}).
-file("src/ocparse_legacy.yrl", 928).
yeccpars2_396_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "*" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_397_/1}).
-file("src/ocparse_legacy.yrl", 930).
yeccpars2_397_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "%" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_398_/1}).
-file("src/ocparse_legacy.yrl", 925).
yeccpars2_398_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_399_/1}).
-file("src/ocparse_legacy.yrl", 902).
yeccpars2_399_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression7 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_401_/1}).
-file("src/ocparse_legacy.yrl", 911).
yeccpars2_401_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_402_/1}).
-file("src/ocparse_legacy.yrl", 952).
yeccpars2_402_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_403_/1}).
-file("src/ocparse_legacy.yrl", 952).
yeccpars2_403_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_404_/1}).
-file("src/ocparse_legacy.yrl", 914).
yeccpars2_404_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "-" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_405_/1}).
-file("src/ocparse_legacy.yrl", 913).
yeccpars2_405_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { "+" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_406_/1}).
-file("src/ocparse_legacy.yrl", 910).
yeccpars2_406_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_408_/1}).
-file("src/ocparse_legacy.yrl", 888).
yeccpars2_408_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { expression8 , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_410_/1}).
-file("src/ocparse_legacy.yrl", 897).
yeccpars2_410_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_411_/1}).
-file("src/ocparse_legacy.yrl", 952).
yeccpars2_411_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_412_/1}).
-file("src/ocparse_legacy.yrl", 1036).
yeccpars2_412_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "!="
  end | __Stack].

-compile({inline,yeccpars2_413_/1}).
-file("src/ocparse_legacy.yrl", 1037).
yeccpars2_413_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "<"
  end | __Stack].

-compile({inline,yeccpars2_414_/1}).
-file("src/ocparse_legacy.yrl", 1039).
yeccpars2_414_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "<="
  end | __Stack].

-compile({inline,yeccpars2_415_/1}).
-file("src/ocparse_legacy.yrl", 1035).
yeccpars2_415_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "<>"
  end | __Stack].

-compile({inline,yeccpars2_416_/1}).
-file("src/ocparse_legacy.yrl", 1034).
yeccpars2_416_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "="
  end | __Stack].

-compile({inline,yeccpars2_417_/1}).
-file("src/ocparse_legacy.yrl", 1038).
yeccpars2_417_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ">"
  end | __Stack].

-compile({inline,yeccpars2_418_/1}).
-file("src/ocparse_legacy.yrl", 1040).
yeccpars2_418_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ">="
  end | __Stack].

-compile({inline,yeccpars2_419_/1}).
-file("src/ocparse_legacy.yrl", 1029).
yeccpars2_419_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { partialComparisonExpression , __2 , __1 }
  end | __Stack].

-compile({inline,yeccpars2_420_/1}).
-file("src/ocparse_legacy.yrl", 896).
yeccpars2_420_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_421_/1}).
-file("src/ocparse_legacy.yrl", 604).
yeccpars2_421_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { with , __2 , __3 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_422_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_422_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_423_/1}).
-file("src/ocparse_legacy.yrl", 638).
yeccpars2_423_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_424_/1}).
-file("src/ocparse_legacy.yrl", 623).
yeccpars2_424_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_427_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_427_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_428_/1}).
-file("src/ocparse_legacy.yrl", 644).
yeccpars2_428_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { order , __3 }
  end | __Stack].

-compile({inline,yeccpars2_429_/1}).
-file("src/ocparse_legacy.yrl", 649).
yeccpars2_429_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_430_/1}).
-file("src/ocparse_legacy.yrl", 661).
yeccpars2_430_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { sortItem , __1 }
  end | __Stack].

-compile({inline,yeccpars2_431_/1}).
-file("src/ocparse_legacy.yrl", 660).
yeccpars2_431_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { sortItem , __1 , "asc" }
  end | __Stack].

-compile({inline,yeccpars2_432_/1}).
-file("src/ocparse_legacy.yrl", 659).
yeccpars2_432_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { sortItem , __1 , "ascending" }
  end | __Stack].

-compile({inline,yeccpars2_433_/1}).
-file("src/ocparse_legacy.yrl", 658).
yeccpars2_433_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { sortItem , __1 , "desc" }
  end | __Stack].

-compile({inline,yeccpars2_434_/1}).
-file("src/ocparse_legacy.yrl", 657).
yeccpars2_434_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { sortItem , __1 , "descending" }
  end | __Stack].

-compile({inline,yeccpars2_435_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_435_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_436_/1}).
-file("src/ocparse_legacy.yrl", 650).
yeccpars2_436_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_437_/1}).
-file("src/ocparse_legacy.yrl", 626).
yeccpars2_437_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_439_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_439_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_440_/1}).
-file("src/ocparse_legacy.yrl", 653).
yeccpars2_440_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { skip , __2 }
  end | __Stack].

-compile({inline,yeccpars2_441_/1}).
-file("src/ocparse_legacy.yrl", 615).
yeccpars2_441_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { returnBody , __1 , __2 , __3 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_443_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_443_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_444_/1}).
-file("src/ocparse_legacy.yrl", 655).
yeccpars2_444_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { limit , __2 }
  end | __Stack].

-compile({inline,yeccpars2_446_/1}).
-file("src/ocparse_legacy.yrl", 437).
yeccpars2_446_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_447_/1}).
-file("src/ocparse_legacy.yrl", 430).
yeccpars2_447_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { periodicCommitHint , __4 }
  end | __Stack].

-compile({inline,yeccpars2_451_/1}).
-file("src/ocparse_legacy.yrl", 538).
yeccpars2_451_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { unwind , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_453_/1}).
-file("src/ocparse_legacy.yrl", 534).
yeccpars2_453_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_454_/1}).
-file("src/ocparse_legacy.yrl", 679).
yeccpars2_454_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_456_/1}).
-file("src/ocparse_legacy.yrl", 680).
yeccpars2_456_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_457_/1}).
-file("src/ocparse_legacy.yrl", 674).
yeccpars2_457_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { start , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_459_/1}).
-file("src/ocparse_legacy.yrl", 686).
yeccpars2_459_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { lookup , __1 }
  end | __Stack].

-compile({inline,yeccpars2_460_/1}).
-file("src/ocparse_legacy.yrl", 685).
yeccpars2_460_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { lookup , __1 }
  end | __Stack].

-compile({inline,yeccpars2_461_/1}).
-file("src/ocparse_legacy.yrl", 683).
yeccpars2_461_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { startPoint , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_465_/1}).
-file("src/ocparse_legacy.yrl", 696).
yeccpars2_465_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipLookup , "relationship" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_466_/1}).
-file("src/ocparse_legacy.yrl", 695).
yeccpars2_466_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipLookup , "relationship" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_467_/1}).
-file("src/ocparse_legacy.yrl", 697).
yeccpars2_467_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipLookup , "relationship" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_477_/1}).
-file("src/ocparse_legacy.yrl", 702).
yeccpars2_477_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { indexQuery , __2 , unwrap ( __4 ) }
  end | __Stack].

-compile({inline,yeccpars2_478_/1}).
-file("src/ocparse_legacy.yrl", 703).
yeccpars2_478_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { indexQuery , __2 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_482_/1}).
-file("src/ocparse_legacy.yrl", 699).
yeccpars2_482_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { identifiedIndexLookup , __2 , __4 , unwrap ( __6 ) }
  end | __Stack].

-compile({inline,yeccpars2_483_/1}).
-file("src/ocparse_legacy.yrl", 700).
yeccpars2_483_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { identifiedIndexLookup , __2 , __4 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_484_/1}).
-file("src/ocparse_legacy.yrl", 709).
yeccpars2_484_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { literalIds , __1 }
  end | __Stack].

-compile({inline,yeccpars2_485_/1}).
-file("src/ocparse_legacy.yrl", 714).
yeccpars2_485_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_489_/1}).
-file("src/ocparse_legacy.yrl", 707).
yeccpars2_489_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { idLookup , "(*)" }
  end | __Stack].

-compile({inline,yeccpars2_490_/1}).
-file("src/ocparse_legacy.yrl", 705).
yeccpars2_490_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { idLookup , __2 }
  end | __Stack].

-compile({inline,yeccpars2_491_/1}).
-file("src/ocparse_legacy.yrl", 706).
yeccpars2_491_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { idLookup , __2 }
  end | __Stack].

-compile({inline,yeccpars2_493_/1}).
-file("src/ocparse_legacy.yrl", 716).
yeccpars2_493_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_494_/1}).
-file("src/ocparse_legacy.yrl", 693).
yeccpars2_494_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipLookup , "rel" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_495_/1}).
-file("src/ocparse_legacy.yrl", 692).
yeccpars2_495_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipLookup , "rel" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_496_/1}).
-file("src/ocparse_legacy.yrl", 694).
yeccpars2_496_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipLookup , "rel" , __2 }
  end | __Stack].

-compile({inline,yeccpars2_497_/1}).
-file("src/ocparse_legacy.yrl", 689).
yeccpars2_497_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { nodeLookup , __2 }
  end | __Stack].

-compile({inline,yeccpars2_498_/1}).
-file("src/ocparse_legacy.yrl", 688).
yeccpars2_498_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { nodeLookup , __2 }
  end | __Stack].

-compile({inline,yeccpars2_499_/1}).
-file("src/ocparse_legacy.yrl", 690).
yeccpars2_499_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { nodeLookup , __2 }
  end | __Stack].

-compile({inline,yeccpars2_500_/1}).
-file("src/ocparse_legacy.yrl", 1025).
yeccpars2_500_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_501_/1}).
-file("src/ocparse_legacy.yrl", 564).
yeccpars2_501_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { set , __2 }
  end | __Stack].

-compile({inline,yeccpars2_502_/1}).
-file("src/ocparse_legacy.yrl", 569).
yeccpars2_502_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_505_/1}).
-file("src/ocparse_legacy.yrl", 1125).
yeccpars2_505_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { propertyExpression , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_506_/1}).
-file("src/ocparse_legacy.yrl", 1131).
yeccpars2_506_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_507_/1}).
-file("src/ocparse_legacy.yrl", 1130).
yeccpars2_507_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_508_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_508_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_509_/1}).
-file("src/ocparse_legacy.yrl", 573).
yeccpars2_509_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { setItem , __1 , "=" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_511_/1}).
-file("src/ocparse_legacy.yrl", 570).
yeccpars2_511_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_512_/1}).
-file("src/ocparse_legacy.yrl", 576).
yeccpars2_512_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { setItem , __1 , { nodeLabels , __2 } }
  end | __Stack].

-compile({inline,yeccpars2_513_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_513_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_514_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_514_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_515_/1}).
-file("src/ocparse_legacy.yrl", 574).
yeccpars2_515_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { setItem , __1 , "=" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_516_/1}).
-file("src/ocparse_legacy.yrl", 575).
yeccpars2_516_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { setItem , __1 , "+=" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_517_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_517_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_518_/1}).
-file("src/ocparse_legacy.yrl", 613).
yeccpars2_518_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { return , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_519_/1}).
-file("src/ocparse_legacy.yrl", 1025).
yeccpars2_519_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { atom , __1 }
  end | __Stack].

-compile({inline,yeccpars2_520_/1}).
-file("src/ocparse_legacy.yrl", 590).
yeccpars2_520_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { remove , __2 }
  end | __Stack].

-compile({inline,yeccpars2_521_/1}).
-file("src/ocparse_legacy.yrl", 595).
yeccpars2_521_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_522_/1}).
-file("src/ocparse_legacy.yrl", 600).
yeccpars2_522_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { removeItem , __1 }
  end | __Stack].

-compile({inline,yeccpars2_524_/1}).
-file("src/ocparse_legacy.yrl", 596).
yeccpars2_524_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_525_/1}).
-file("src/ocparse_legacy.yrl", 599).
yeccpars2_525_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { removeItem , __1 , { nodeLabels , __2 } }
  end | __Stack].

-compile({inline,yeccpars2_527_/1}).
-file("src/ocparse_legacy.yrl", 733).
yeccpars2_527_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { anonymousPatternPart , __1 }
  end | __Stack].

-compile({inline,yeccpars2_528_/1}).
-file("src/ocparse_legacy.yrl", 545).
yeccpars2_528_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_529_/1}).
-file("src/ocparse_legacy.yrl", 734).
yeccpars2_529_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { anonymousPatternPart , __1 }
  end | __Stack].

-compile({inline,yeccpars2_530_/1}).
-file("src/ocparse_legacy.yrl", 731).
yeccpars2_530_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { patternPart , __1 }
  end | __Stack].

-compile({inline,yeccpars2_531_/1}).
-file("src/ocparse_legacy.yrl", 540).
yeccpars2_531_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { merge , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_533_/1}).
-file("src/ocparse_legacy.yrl", 549).
yeccpars2_533_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_537_/1}).
-file("src/ocparse_legacy.yrl", 552).
yeccpars2_537_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mergeAction , "match" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_538_/1}).
-file("src/ocparse_legacy.yrl", 553).
yeccpars2_538_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { mergeAction , "create" , __3 }
  end | __Stack].

-compile({inline,yeccpars2_539_/1}).
-file("src/ocparse_legacy.yrl", 548).
yeccpars2_539_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_541_/1}).
-file("src/ocparse_legacy.yrl", 730).
yeccpars2_541_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { patternPart , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_542_/1}).
-file("src/ocparse_legacy.yrl", 513).
yeccpars2_542_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_545_/1}).
-file("src/ocparse_legacy.yrl", 514).
yeccpars2_545_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   "with headers"
  end | __Stack].

-compile({inline,yeccpars2_546_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_546_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_549_/1}).
-file("src/ocparse_legacy.yrl", 516).
yeccpars2_549_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_550_/1}).
-file("src/ocparse_legacy.yrl", 508).
yeccpars2_550_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { loadCSV , __3 , __5 , __7 , __8 }
  end | __Stack].

-compile({inline,yeccpars2_552_/1}).
-file("src/ocparse_legacy.yrl", 517).
yeccpars2_552_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   unwrap ( __2 )
  end | __Stack].

-compile({inline,yeccpars2_555_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_555_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_557_DELETE/1}).
-file("src/ocparse_legacy.yrl", 583).
yeccpars2_557_DELETE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_557_/1}).
-file("src/ocparse_legacy.yrl", 525).
yeccpars2_557_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_558_DELETE/1}).
-file("src/ocparse_legacy.yrl", 583).
yeccpars2_558_DELETE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_558_/1}).
-file("src/ocparse_legacy.yrl", 525).
yeccpars2_558_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_559_/1}).
-file("src/ocparse_legacy.yrl", 560).
yeccpars2_559_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_561_/1}).
-file("src/ocparse_legacy.yrl", 561).
yeccpars2_561_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "unique"
  end | __Stack].

-compile({inline,yeccpars2_562_/1}).
-file("src/ocparse_legacy.yrl", 721).
yeccpars2_562_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   { pattern , __1 }
  end | __Stack].

-compile({inline,yeccpars2_563_/1}).
-file("src/ocparse_legacy.yrl", 726).
yeccpars2_563_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_564_/1}).
-file("src/ocparse_legacy.yrl", 555).
yeccpars2_564_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { create , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_566_/1}).
-file("src/ocparse_legacy.yrl", 727).
yeccpars2_566_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_567_/1}).
-file("src/ocparse_legacy.yrl", 426).
yeccpars2_567_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_568_/1}).
-file("src/ocparse_legacy.yrl", 602).
yeccpars2_568_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { forEach , __3 , __5 , __7 }
  end | __Stack].

-compile({inline,yeccpars2_569_/1}).
-file("src/ocparse_legacy.yrl", 484).
yeccpars2_569_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { drop , __2 }
  end | __Stack].

-compile({inline,yeccpars2_570_/1}).
-file("src/ocparse_legacy.yrl", 489).
yeccpars2_570_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { drop , __2 }
  end | __Stack].

-compile({inline,yeccpars2_571_/1}).
-file("src/ocparse_legacy.yrl", 486).
yeccpars2_571_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { drop , __2 }
  end | __Stack].

-compile({inline,yeccpars2_572_/1}).
-file("src/ocparse_legacy.yrl", 491).
yeccpars2_572_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { drop , __2 }
  end | __Stack].

-compile({inline,yeccpars2_579_/1}).
-file("src/ocparse_legacy.yrl", 493).
yeccpars2_579_(__Stack0) ->
 [__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { index , { __3 , __5 } }
  end | __Stack].

-compile({inline,yeccpars2_592_/1}).
-file("src/ocparse_legacy.yrl", 807).
yeccpars2_592_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { relType , __2 }
  end | __Stack].

-compile({inline,yeccpars2_596_/1}).
-file("src/ocparse_legacy.yrl", 504).
yeccpars2_596_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipPatternSyntax , "<" , "-" , __6 , __7 , "-" , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_605_/1}).
-file("src/ocparse_legacy.yrl", 505).
yeccpars2_605_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipPatternSyntax , [ ] , "-" , __5 , __6 , "-" , ">" }
  end | __Stack].

-compile({inline,yeccpars2_606_/1}).
-file("src/ocparse_legacy.yrl", 506).
yeccpars2_606_(__Stack0) ->
 [__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipPatternSyntax , [ ] , "-" , __5 , __6 , "-" , [ ] }
  end | __Stack].

-compile({inline,yeccpars2_612_/1}).
-file("src/ocparse_legacy.yrl", 609).
yeccpars2_612_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_614_/1}).
-file("src/ocparse_legacy.yrl", 499).
yeccpars2_614_(__Stack0) ->
 [__11,__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { nodePropertyExistenceConstraint , __4 , __5 , __10 }
  end | __Stack].

-compile({inline,yeccpars2_616_/1}).
-file("src/ocparse_legacy.yrl", 496).
yeccpars2_616_(__Stack0) ->
 [__10,__9,__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { uniqueConstraint , __4 , __5 , __8 }
  end | __Stack].

-compile({inline,yeccpars2_621_/1}).
-file("src/ocparse_legacy.yrl", 502).
yeccpars2_621_(__Stack0) ->
 [__8,__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { relationshipPropertyExistenceConstraint , __3 , __7 }
  end | __Stack].

-compile({inline,yeccpars2_622_/1}).
-file("src/ocparse_legacy.yrl", 475).
yeccpars2_622_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { create , __2 }
  end | __Stack].

-compile({inline,yeccpars2_623_/1}).
-file("src/ocparse_legacy.yrl", 480).
yeccpars2_623_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { create , __2 }
  end | __Stack].

-compile({inline,yeccpars2_624_/1}).
-file("src/ocparse_legacy.yrl", 477).
yeccpars2_624_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { create , __2 }
  end | __Stack].

-compile({inline,yeccpars2_625_/1}).
-file("src/ocparse_legacy.yrl", 482).
yeccpars2_625_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { create , __2 }
  end | __Stack].

-compile({inline,yeccpars2_626_/1}).
-file("src/ocparse_legacy.yrl", 879).
yeccpars2_626_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_627_/1}).
-file("src/ocparse_legacy.yrl", 578).
yeccpars2_627_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { delete , __1 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_629_/1}).
-file("src/ocparse_legacy.yrl", 528).
yeccpars2_629_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_630_/1}).
-file("src/ocparse_legacy.yrl", 534).
yeccpars2_630_(__Stack0) ->
 [begin
   { }
  end | __Stack0].

-compile({inline,yeccpars2_632_/1}).
-file("src/ocparse_legacy.yrl", 532).
yeccpars2_632_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_638_/1}).
-file("src/ocparse_legacy.yrl", 665).
yeccpars2_638_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { hint , __3 , __4 }
  end | __Stack].

-compile({inline,yeccpars2_640_/1}).
-file("src/ocparse_legacy.yrl", 664).
yeccpars2_640_(__Stack0) ->
 [__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { hint , __4 }
  end | __Stack].

-compile({inline,yeccpars2_641_/1}).
-file("src/ocparse_legacy.yrl", 670).
yeccpars2_641_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_643_/1}).
-file("src/ocparse_legacy.yrl", 671).
yeccpars2_643_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   [ __1 | __3 ]
  end | __Stack].

-compile({inline,yeccpars2_648_/1}).
-file("src/ocparse_legacy.yrl", 663).
yeccpars2_648_(__Stack0) ->
 [__7,__6,__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { hint , __3 , __4 , __6 }
  end | __Stack].

-compile({inline,yeccpars2_649_/1}).
-file("src/ocparse_legacy.yrl", 531).
yeccpars2_649_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_650_/1}).
-file("src/ocparse_legacy.yrl", 520).
yeccpars2_650_(__Stack0) ->
 [__5,__4,__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { match , __1 , __3 , __4 , __5 }
  end | __Stack].

-compile({inline,yeccpars2_651_/1}).
-file("src/ocparse_legacy.yrl", 419).
yeccpars2_651_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { bulkImportQuery , __1 , __2 }
  end | __Stack].

-compile({inline,'yeccpars2_652_$end'/1}).
-file("src/ocparse_legacy.yrl", 440).
'yeccpars2_652_$end'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,'yeccpars2_652_;'/1}).
-file("src/ocparse_legacy.yrl", 440).
'yeccpars2_652_;'(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_652_DELETE/1}).
-file("src/ocparse_legacy.yrl", 583).
yeccpars2_652_DELETE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_652_/1}).
-file("src/ocparse_legacy.yrl", 525).
yeccpars2_652_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_653_/1}).
-file("src/ocparse_legacy.yrl", 432).
yeccpars2_653_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { loadCSVQuery , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_654_DELETE/1}).
-file("src/ocparse_legacy.yrl", 583).
yeccpars2_654_DELETE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_654_/1}).
-file("src/ocparse_legacy.yrl", 525).
yeccpars2_654_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_655_/1}).
-file("src/ocparse_legacy.yrl", 407).
yeccpars2_655_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   { regularQuery , __1 , __2 }
  end | __Stack].

-compile({inline,yeccpars2_657_/1}).
-file("src/ocparse_legacy.yrl", 416).
yeccpars2_657_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   [ __1 ]
  end | __Stack].

-compile({inline,yeccpars2_658_/1}).
-file("src/ocparse_legacy.yrl", 449).
yeccpars2_658_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_659_DELETE/1}).
-file("src/ocparse_legacy.yrl", 583).
yeccpars2_659_DELETE(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_659_/1}).
-file("src/ocparse_legacy.yrl", 525).
yeccpars2_659_(__Stack0) ->
 [begin
   [ ]
  end | __Stack0].

-compile({inline,yeccpars2_660_/1}).
-file("src/ocparse_legacy.yrl", 450).
yeccpars2_660_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   "all"
  end | __Stack].

-compile({inline,yeccpars2_661_/1}).
-file("src/ocparse_legacy.yrl", 444).
yeccpars2_661_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { union , __2 , __3 }
  end | __Stack].

-compile({inline,yeccpars2_662_/1}).
-file("src/ocparse_legacy.yrl", 415).
yeccpars2_662_(__Stack0) ->
 [__2,__1 | __Stack] = __Stack0,
 [begin
   __1 ++ [ __2 ]
  end | __Stack].

-compile({inline,yeccpars2_663_/1}).
-file("src/ocparse_legacy.yrl", 363).
yeccpars2_663_(__Stack0) ->
 [__3,__2,__1 | __Stack] = __Stack0,
 [begin
   { cypher , __1 , { statement , __2 } , __3 }
  end | __Stack].

-compile({inline,yeccpars2_664_/1}).
-file("src/ocparse_legacy.yrl", 372).
yeccpars2_664_(__Stack0) ->
 [__1 | __Stack] = __Stack0,
 [begin
   ";"
  end | __Stack].


-file("src/ocparse_legacy.yrl", 1322).
