%% -----------------------------------------------------------------------------
%%
%% ocparse_test.erl: opencypher - test driver.
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

-module(ocparse_test).

-define(ENV_VAR_FILE_TYPE, ".tst").
-define(ENV_VAR_FILE_WILDCARD, "SOURCEFILES").
-define(ENV_VAR_LOGGING_LEVEL, "LOG").
-define(PARSER_MODULE, ocparse).
-define(TIMEOUT, 60).
-define(WHITESPACE, " \n\r").

-export([common_test_source/1]).
-export([eunit_test_source/3]).

-define(NODEBUG, true).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%------------------------------------------------------------------------------
%% Common Test Driver.
%%------------------------------------------------------------------------------

common_test_source(Source) ->
    %% -------------------------------------------------------------------------
    %% 1. Source ==> ParseTree
    %% -------------------------------------------------------------------------
    case ?PARSER_MODULE:source_to_pt(Source) of
        {ok, {ParseTree, _Tokens}} ->
            %% -----------------------------------------------------------------
            %% Test TopDown
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Source (=NSource_TD)
            %% -----------------------------------------------------------------
            NSource_TD = case ?PARSER_MODULE:pt_to_source_td(ParseTree) of
                             {error, Error_TD} ->
                                 ct:pal("~n[TD] Error ParseTree -> NewSource : Source   ~n > ~p", [Source]),
                                 ct:pal("~n[TD] Error ParseTree -> NewSource : ParseTree~n > ~p", [ParseTree]),
                                 throw({error, Error_TD});
                             NS_TD ->
                                 NS_TD
                         end,
            SourceNet = lists:flatten(remove_whitespace(Source, ?WHITESPACE, [])),
            case string:to_lower(SourceNet) == string:to_lower(lists:flatten(remove_whitespace(binary_to_list(NSource_TD), ?WHITESPACE, []))) of
                true ->
                    ok;
                _ ->
                    ct:pal("~n[TD] Error Source == NewSource : Source      ~n > ~p", [Source]),
                    ct:pal("~n[TD] Error Source == NewSource : SourceNet   ~n > ~p", [SourceNet]),
                    ct:pal("~n[TD] Error Source == NewSource : NewSourceNet~n > ~p", [lists:flatten(remove_whitespace(binary_to_list(NSource_TD), ?WHITESPACE, []))]),
                    throw({error, "[TD] Error Source == NewSource"})
            end,
            %% -----------------------------------------------------------------
            %% 3. Source (=NSource_TD) ==> ParseTree (=NPT_TD)
            %% -----------------------------------------------------------------
            {ok, {NPTree_TD, _NToks_TD}}
                = try
                      {ok, {NPT_TD, NT_TD}} = ?PARSER_MODULE:source_to_pt(NSource_TD),
                      {ok, {NPT_TD, NT_TD}}
                  catch
                      _:_ ->
                          ct:pal("~n[TD] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : Source   ~n > ~p", [Source]),
                          ct:pal("~n[TD] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : NewSource~n > ~p", [NSource_TD]),
                          throw({error, "[TD] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt"})
                  end,
            if ParseTree /= NPTree_TD ->
                ct:pal("~n[TD] Error ParseTree = NPTree : Source      ~n > ~p", [Source]),
                ct:pal("~n[TD] Error ParseTree = NPTree : NewParseTree~n > ~p", [NPTree_TD]),
                ct:pal("~n[TD] Error ParseTree = NPTree : Tokens      ~n > ~p", [_Tokens]),
                ct:pal("~n[TD] Error ParseTree = NPTree : NewTokens   ~n > ~p", [_NToks_TD]);
                true -> ok
            end,
            ?assertEqual(ParseTree, NPTree_TD),
            StringNSource_TD = binary:bin_to_list(NSource_TD),
            StringNSource_TDMultipleSpace = string:str(StringNSource_TD, "  "),
            case StringNSource_TDMultipleSpace of
                0 -> ok;
                _ ->
                    ct:pal("~n[TD] Error redundant whitespace(s) : Source         ~n > ~p", [Source]),
                    ct:pal("~n[TD] Error redundant whitespace(s) : NewSource      ~n > ~p", [StringNSource_TD]),
                    ct:pal("~n[TD] Error redundant whitespace(s) : 1. Redundant WS~n > ~p", [StringNSource_TDMultipleSpace]),
                    throw({error, "[TD] Error redundant whitespace(s)"})
            end,
            %% -----------------------------------------------------------------
            %% Test BottomUp
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Source (=NSource_BU)
            %% -----------------------------------------------------------------
            NSource_BU = case ?PARSER_MODULE:pt_to_source_bu(ParseTree) of
                             {error, Error_BU} ->
                                 ct:pal("~n[BU] Error ParseTree -> NewSource : Source   ~n > ~p", [Source]),
                                 ct:pal("~n[BU] Error ParseTree -> NewSource : ParseTree~n > ~p", [ParseTree]),
                                 throw({error, "[BU] Error ParseTree -> NewSource : " ++ Error_BU});
                             NS_BU ->
                                 NS_BU
                         end,
            case string:to_lower(SourceNet) == string:to_lower(lists:flatten(remove_whitespace(binary_to_list(NSource_BU), ?WHITESPACE, []))) of
                true ->
                    ok;
                _ ->
                    ct:pal("~n[BU] Error Source == NewSource : Source      ~n > ~p", [Source]),
                    ct:pal("~n[BU] Error Source == NewSource : SourceNet   ~n > ~p", [SourceNet]),
                    ct:pal("~n[BU] Error Source == NewSource : NewSourceNet~n > ~p", [lists:flatten(remove_whitespace(binary_to_list(NSource_BU), ?WHITESPACE, []))]),
                    throw({error, "[BU] Error Source == NewSource"})
            end,
            %% -----------------------------------------------------------------
            %% 3. Source (=NSource_BU) ==> ParseTree (=NPT_BU)
            %% -----------------------------------------------------------------
            {ok, {NPTree_BU, _NToks_BU}}
                = try
                      {ok, {NPT_BU, NT_BU}} = ?PARSER_MODULE:source_to_pt(NSource_BU),
                      {ok, {NPT_BU, NT_BU}}
                  catch
                      _:_ ->
                          ct:pal("~n[BU] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : Source   ~n > ~p", [Source]),
                          ct:pal("~n[BU] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : NewSource~n > ~p", [NSource_BU]),
                          throw({error, "[BU] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt"})
                  end,
            if ParseTree /= NPTree_BU ->
                ct:pal("~n[BU] Error ParseTree = NPTree : Source      ~n > ~p", [Source]),
                ct:pal("~n[BU] Error ParseTree = NPTree : NewParseTree~n > ~p", [NPTree_BU]),
                ct:pal("~n[BU] Error ParseTree = NPTree : Tokens      ~n > ~p", [_Tokens]),
                ct:pal("~n[BU] Error ParseTree = NPTree : NewTokens   ~n > ~p", [_NToks_BU]);
                true -> ok
            end,
            ?assertEqual(ParseTree, NPTree_BU),
            StringNSource_BU = binary:bin_to_list(NSource_BU),
            StringNSource_BUMultipleSpace = string:str(StringNSource_BU, "  "),
            case StringNSource_BUMultipleSpace of
                0 -> ok;
                _ ->
                    ct:pal("~n[BU] Error redundant whitespace(s) : Source         ~n > ~p", [Source]),
                    ct:pal("~n[BU] Error redundant whitespace(s) : NewSource      ~n > ~p", [StringNSource_BU]),
                    ct:pal("~n[BU] Error redundant whitespace(s) : 1. Redundant WS~n > ~p", [StringNSource_BUMultipleSpace]),
                    throw({error, "[BU] Error redundant whitespace(s)"})
            end;
        {lex_error, _Error} ->
            ct:pal("~nFailed lex_error : Source~n > ~p", [Source]),
            throw({error, "Failed lex_error : " ++ _Error});
        {parse_error, {_Error, _Tokens}} ->
            ct:pal("~nFailed parse_error : Source~n > ~p", [Source]),
            ct:pal("~nFailed parse_error : Tokens~n > ~p", [_Tokens]),
            throw({error, "Failed parse_error : " ++ _Error})
    end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Determine Files.
%%------------------------------------------------------------------------------

eunit_test_() ->
    WCard = case os:getenv(?ENV_VAR_FILE_WILDCARD) of
                SourceFiles when is_list(SourceFiles) ->
                    SourceFiles;
                _ ->
                    "*"
            end ++ ?ENV_VAR_FILE_TYPE,
    Logs = case os:getenv(?ENV_VAR_LOGGING_LEVEL) of
               V when length(V) > 0 ->
                   case lists:map(
                       fun(VStr) ->
                           list_to_integer(VStr)
                       end, re:split(V, ",", [{return, list}])) of
                       VIs when is_list(VIs) -> VIs;
                       _ -> [0]
                   end;
               _ -> [0]
           end,
    ?debugFmt(atom_to_list(?MODULE) ++ ":eunit_test_ ===>~nFile = ~s, Logs = ~p~n", [WCard, Logs]),
    {ok, Cwd} = file:get_cwd(),
    ?debugFmt(atom_to_list(?MODULE) ++ ":eunit_test_ ===>~nCwd: ~p~n", [Cwd]),
    RootPath = lists:reverse(filename:split(Cwd)),
    ?debugFmt(atom_to_list(?MODULE) ++ ":eunit_test_ ===>~nRootPath: ~p~n", [RootPath]),
    TestDir = filename:join(lists:reverse(["test" | RootPath])),
    ?debugFmt(atom_to_list(?MODULE) ++ ":eunit_test_ ===>~nTestDir: ~p~n", [TestDir]),
    TestFiles = lists:sort([filename:join(TestDir, T)
        || T <- filelib:wildcard(WCard, TestDir)]),
    ?debugFmt(atom_to_list(?MODULE) ++ ":eunit_test_ ===>~nTestFiles: ~p~n", [TestFiles]),
    group_gen(TestFiles, Logs).

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Test Cases.
%%------------------------------------------------------------------------------

-define(D(__Lvl, __Fmt, __Args),
    case lists:member(__Lvl, Logs) of
        true -> io:format(standard_io, __Fmt, __Args);
        _ -> ok
    end).
-define(D(__Lvl, __Msg),
    case lists:member(__Lvl, Logs) of
        true -> io:format(standard_io, __Msg, []);
        _ -> ok
    end).

-define(D_(__Msg), ?D(0, __Msg)).
-define(D_(__Fmt, __Args), ?D(0, __Fmt, __Args)).

-define(D1(__Msg), ?D(1, __Msg)).
-define(D1(__Fmt, __Args), ?D(1, __Fmt, __Args)).
-define(D2(__Msg), ?D(2, __Msg)).
-define(D2(__Fmt, __Args), ?D(2, __Fmt, __Args)).
-define(D3(__Msg), ?D(3, __Msg)).
-define(D3(__Fmt, __Args), ?D(3, __Fmt, __Args)).
-define(D4(__Msg), ?D(4, __Msg)).
-define(D4(__Fmt, __Args), ?D(4, __Fmt, __Args)).
-define(D5(__Msg), ?D(5, __Msg)).
-define(D5(__Fmt, __Args), ?D(5, __Fmt, __Args)).

eunit_test_source(_TestGroup, Source, Logs) ->
    ?debugFmt(atom_to_list(?MODULE) ++ ":eunit_test_source ===>~nTestGroup = ~p~n, Source = ~p~n, Logs = ~p~n", [_TestGroup, Source, Logs]),
    ?D1("~n[TD] Source~n~s", [Source]),
    %% -------------------------------------------------------------------------
    %% 1. Source ==> ParseTree
    %% -------------------------------------------------------------------------
    case ?PARSER_MODULE:source_to_pt(Source) of
        {ok, {ParseTree, _Tokens}} ->
            ?D2("~n[TD] ParseTree~n~p Tokens~n~p", [ParseTree, _Tokens]),
            %% -----------------------------------------------------------------
            %% Test TopDown
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Source (=NSource_TD)
            %% -----------------------------------------------------------------
            NSource_TD = case ?PARSER_MODULE:pt_to_source_td(ParseTree) of
                             {error, Error_TD} ->
                                 ?D_("~n[TD] Error ParseTree -> NewSource : ParseTree~n > ~p", [ParseTree]),
                                 throw({error, Error_TD});
                             NS_TD ->
                                 NS_TD
                         end,
            ?D3("~n[TD] NewSource~n~s", [NSource_TD]),
            SourceNet = lists:flatten(remove_whitespace(Source, ?WHITESPACE, [])),
            case string:to_lower(SourceNet) == string:to_lower(lists:flatten(remove_whitespace(binary_to_list(NSource_TD), ?WHITESPACE, []))) of
                true ->
                    ok;
                _ ->
                    ?D_("~n[TD] Error Source == NewSource : Source      ~n > ~p", [Source]),
                    ?D_("~n[TD] Error Source == NewSource : SourceNet   ~n > ~p", [SourceNet]),
                    ?D_("~n[TD] Error Source == NewSource : NewSourceNet~n > ~p", [lists:flatten(remove_whitespace(binary_to_list(NSource_TD), ?WHITESPACE, []))]),
                    throw({error, "[TD] Error Source == NewSource"})
            end,
            %% -----------------------------------------------------------------
            %% 3. Source (=NSource_TD) ==> ParseTree (=NPT_TD)
            %% -----------------------------------------------------------------
            {ok, {NPTree_TD, _NToks_TD}}
                = try
                      {ok, {NPT_TD, NT_TD}} = ?PARSER_MODULE:source_to_pt(NSource_TD),
                      {ok, {NPT_TD, NT_TD}}
                  catch
                      _:_ ->
                          ?D_("~n[TD] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : Source   ~n > ~p", [Source]),
                          ?D_("~n[TD] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : TestGroup~n > ~p", [_TestGroup]),
                          ?D_("~n[TD] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : NewSource~n > ~p", [NSource_TD]),
                          throw({error, "[TD] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt"})
                  end,
            if ParseTree /= NPTree_TD ->
                ?D_("~n[TD] Error ParseTree = NPTree : Source      ~n > ~p", [Source]),
                ?D_("~n[TD] Error ParseTree = NPTree : NewParseTree~n > ~p", [NPTree_TD]),
                ?D_("~n[TD] Error ParseTree = NPTree : Tokens      ~n > ~p", [_Tokens]),
                ?D_("~n[TD] Error ParseTree = NPTree : NewTokens   ~n > ~p", [_NToks_TD]);
                true -> ok
            end,
            ?assertEqual(ParseTree, NPTree_TD),
            StringNSource_TD = binary:bin_to_list(NSource_TD),
            StringNSource_TDMultipleSpace = string:str(StringNSource_TD, "  "),
            case StringNSource_TDMultipleSpace of
                0 -> ok;
                _ ->
                    ?D_("~n[TD] Error redundant whitespace(s) : Source         ~n > ~p", [Source]),
                    ?D_("~n[TD] Error redundant whitespace(s) : NewSource      ~n > ~p", [StringNSource_TD]),
                    ?D_("~n[TD] Error redundant whitespace(s) : 1. Redundant WS~n > ~p", [StringNSource_TDMultipleSpace]),
                    throw({error, "[TD] Error redundant whitespace(s)"})
            end,
            ?D4("~n[TD] ParseTree~n~p", [ParseTree]),
            %% -----------------------------------------------------------------
            %% Test BottomUp
            %% -----------------------------------------------------------------
            %% 2. ParseTree ==> Source (=NSource_BU)
            %% -----------------------------------------------------------------
            NSource_BU = case ?PARSER_MODULE:pt_to_source_bu(ParseTree) of
                             {error, Error_BU} ->
                                 ?D_("~n[BU] Error ParseTree -> NewSource : ParseTree~n > ~p", [ParseTree]),
                                 throw({error, "[BU] Error ParseTree -> NewSource : " ++ Error_BU});
                             NS_BU ->
                                 NS_BU
                         end,
            ?D3("~n[BU] NewSource~n~s", [NSource_BU]),
            case string:to_lower(SourceNet) == string:to_lower(lists:flatten(remove_whitespace(binary_to_list(NSource_BU), ?WHITESPACE, []))) of
                true ->
                    ok;
                _ ->
                    ?D_("~n[BU] Error Source == NewSource : Source      ~n > ~p", [Source]),
                    ?D_("~n[BU] Error Source == NewSource : SourceNet   ~n > ~p", [SourceNet]),
                    ?D_("~n[BU] Error Source == NewSource : NewSourceNet~n > ~p", [lists:flatten(remove_whitespace(binary_to_list(NSource_BU), ?WHITESPACE, []))]),
                    throw({error, "[BU] Error Source == NewSource"})
            end,
            %% -----------------------------------------------------------------
            %% 3. Source (=NSource_BU) ==> ParseTree (=NPT_BU)
            %% -----------------------------------------------------------------
            {ok, {NPTree_BU, _NToks_BU}}
                = try
                      {ok, {NPT_BU, NT_BU}} = ?PARSER_MODULE:source_to_pt(NSource_BU),
                      {ok, {NPT_BU, NT_BU}}
                  catch
                      _:_ ->
                          ?D_("~n[BU] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : Source   ~n > ~p", [Source]),
                          ?D_("~n[BU] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : TestGroup~n > ~p", [_TestGroup]),
                          ?D_("~n[BU] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt : NewSource~n > ~p", [NSource_BU]),
                          throw({error, "[BU] Error " ++ atom_to_list(?MODULE) ++ ":source_to_pt"})
                  end,
            if ParseTree /= NPTree_BU ->
                ?D_("~n[BU] Error ParseTree = NPTree : Source      ~n > ~p", [Source]),
                ?D_("~n[BU] Error ParseTree = NPTree : NewParseTree~n > ~p", [NPTree_BU]),
                ?D_("~n[BU] Error ParseTree = NPTree : Tokens      ~n > ~p", [_Tokens]),
                ?D_("~n[BU] Error ParseTree = NPTree : NewTokens   ~n > ~p", [_NToks_BU]);
                true -> ok
            end,
            ?assertEqual(ParseTree, NPTree_BU),
            StringNSource_BU = binary:bin_to_list(NSource_BU),
            StringNSource_BUMultipleSpace = string:str(StringNSource_BU, "  "),
            case StringNSource_BUMultipleSpace of
                0 -> ok;
                _ ->
                    ?D_("~n[BU] Error redundant whitespace(s) : Source         ~n > ~p", [Source]),
                    ?D_("~n[BU] Error redundant whitespace(s) : NewSource      ~n > ~p", [StringNSource_BU]),
                    ?D_("~n[BU] Error redundant whitespace(s) : 1. Redundant WS~n > ~p", [StringNSource_BUMultipleSpace]),
                    throw({error, "[BU] Error redundant whitespace(s)"})
            end,
            ?D4("~n[BU] ParseTree~n~p", [ParseTree]);
        {lex_error, _Error} ->
            ?D_("~nFailed lex_error : Source~n > ~p", [Source]),
            throw({error, "Failed lex_error : " ++ _Error});
        {parse_error, {_Error, _Tokens}} ->
            ?D_("~nFailed parse_error : Source~n > ~p", [Source]),
            ?D_("~nFailed parse_error : Tokens~n > ~p", [_Tokens]),
            throw({error, "Failed parse_error : " ++ _Error})
    end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Files.
%%------------------------------------------------------------------------------

group_gen(TestFiles, Logs) ->
    ?debugFmt(atom_to_list(?MODULE) ++ ":group_gen ===>~nTestFiles = ~s, Logs = ~p~n", [TestFiles, Logs]),
    {generator,
        fun() ->
            case TestFiles of
                [] ->
                    [];
                [TestFile | RestTestFiles] ->
                    {ok, [Opts | Tests]} = file:consult(TestFile),
                    {ok, TestFileBin} = file:read_file(TestFile),
                    TestLines = [begin
                                     TRe = re:replace(T, "(.*)(\")(.*)", "\\1\\\\\"\\3"
                                         , [{return, binary}, ungreedy, global, dotall]),
                                     case binary:match(TestFileBin, TRe) of
                                         {I1, _} ->
                                             <<Head:I1/binary, _/binary>> = TestFileBin,
                                             {match, Matches} = re:run(Head, ".*[\r\n]", [global]),
                                             length(Matches) + 1;
                                         nomatch ->
                                             %io:format(user, "~n~nNOMATCH -------------------------~n"
                                             %                "File : ~s~n~n"
                                             %                "Target ---~n~n~s~n~n"
                                             %                "With ---~n~n~s~n~n"
                                             %                "File Content ---~n~n~s~n"
                                             %                "---------------------------------~n~n"
                                             %          , [TestFile, T, TRe, TestFileBin]),
                                             I
                                     end
                                 end
                        || {I, T} <- lists:zip(lists:seq(1, length(Tests)), Tests)],
                    AugTests = lists:zip(TestLines, Tests),
                    TestGroup = filename:rootname(
                        filename:basename(TestFile)),
                    [tests_gen(TestGroup, AugTests, Opts, Logs)
                        | group_gen(RestTestFiles, Logs)]
            end
        end}.

%%------------------------------------------------------------------------------
%% Remove the whitespace characters from a string.
%%------------------------------------------------------------------------------

remove_whitespace([], _WhiteSpace, Acc) ->
    lists:reverse(Acc);
remove_whitespace([CharIn | Tail], WhiteSpace, Acc) ->
    CharOut = case is_whitespace(WhiteSpace, CharIn) of
                  true ->
                      "";
                  _ ->
                      CharIn
              end,
    remove_whitespace(Tail, WhiteSpace, [CharOut | Acc]).

is_whitespace([], _Char) ->
    false;
is_whitespace([Curr | Tail], Char) ->
    case Curr == Char of
        true ->
            true;
        _ ->
            is_whitespace(Tail, Char)
    end.

%%------------------------------------------------------------------------------
%% EUnit Test Driver - Processing Groups.
%%------------------------------------------------------------------------------

tests_gen(TestGroup, Tests, Opts, Logs) ->
    ?debugFmt(atom_to_list(?MODULE) ++ ":tests_gen ===>~nTestGroup = ~p~n, Tests = ~p~n, Opts = ~p~n, Logs = ~p~n", [TestGroup, Tests, Opts, Logs]),
    SelTests = case proplists:get_value(tests, Opts) of
                   St when St =:= undefined; St =:= [] ->
                       {Indices, _} = lists:unzip(Tests),
                       Indices;
                   St -> St
               end,
    tests_gen(TestGroup, Tests, Logs, SelTests, []).

tests_gen(_TestGroup, [], _Logs, _SelTests, Acc) ->
    {inorder, lists:reverse(Acc)};
tests_gen(TestGroup, [{I, T} | Tests], Logs, SelTests, Acc) ->
    case lists:member(I, SelTests) of
        true ->
            tests_gen(TestGroup, Tests, Logs, SelTests,
                [{TestGroup, I,
                    fun() ->
                        {timeout, ?TIMEOUT, ?MODULE:eunit_test_source(TestGroup, T, Logs)}
                    end} | Acc]);
        _ -> Acc
    end.
