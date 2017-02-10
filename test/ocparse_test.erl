-module(ocparse_test).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

-export([test_cypher/3]).

-define(TIMEOUT, 60).

cypher_test_() ->
    WCard = case os:getenv("CYPHER") of
                Cypher when is_list(Cypher) -> Cypher;
                _ -> "*"
            end ++ ".tst",
    ?debugFmt("wwe debugging cypher_test_ ===> ~n WCard: ~p~n", [WCard]),
    Logs = case os:getenv("LOG") of
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
    io:format(user, "File = ~s, Logs = ~p~n", [WCard, Logs]),
    {ok, Cwd} = file:get_cwd(),
    ?debugFmt("wwe debugging cypher_test_ ===> ~n Cwd: ~p~n", [Cwd]),
    RootPath = lists:reverse(filename:split(Cwd)),
    ?debugFmt("wwe debugging cypher_test_ ===> ~n RootPath: ~p~n", [RootPath]),
    TestDir = filename:join(lists:reverse(["test" | RootPath])),
    ?debugFmt("wwe debugging cypher_test_ ===> ~n TestDir: ~p~n", [TestDir]),
    TestFiles = lists:sort([filename:join(TestDir, T)
        || T <- filelib:wildcard(WCard, TestDir)]),
    ?debugFmt("wwe debugging cypher_test_ ===> ~n TestFiles: ~p~n", [TestFiles]),
    group_gen(TestFiles, Logs).

group_gen(TestFiles, Logs) ->
    {generator,
        fun() ->
            case TestFiles of
                [] -> [];
                [TestFile | RestTestFiles] ->
                    ?debugFmt("wwe debugging group_gen ===> ~n TestFile: ~p~n RestTestFiles: ~p~n", [TestFile, RestTestFiles]),
                    {ok, [Opts | Tests]} = file:consult(TestFile),
                    ?debugFmt("wwe debugging group_gen ===> ~n Opts: ~p~n Tests: ~p~n", [Opts, Tests]),
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

tests_gen(TestGroup, Tests, Opts, Logs) ->
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
                        {timeout, ?TIMEOUT, ?MODULE:test_cypher(TestGroup, T, Logs)}
                    end} | Acc]);
        _ -> Acc
    end.

-define(D(__Lvl, __Fmt, __Args),
    case lists:member(__Lvl, Logs) of
        true -> ?debugFmt(__Fmt, __Args);
        _ -> ok
    end).
-define(D(__Lvl, __Msg),
    case lists:member(__Lvl, Logs) of
        true -> ?debugMsg(__Msg);
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

test_cypher(_TestGroup, Test, Logs) ->
    ?D1("~n ~s", [Test]),
    ?debugFmt("~n", []),
    ?debugFmt("wwe debugging test_cypher ===> ~n Test: ~p~n", [Test]),
    case ocparse:parsetree_with_tokens(Test) of
        {ok, {ParseTree, _Tokens}} ->
            ?debugFmt("wwe debugging test_cypher ===> ~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, _Tokens]),
            ?D2("~n~p", [ParseTree]),
            NCypher = case ocparse:parsetree_to_string_td(ParseTree) of
                          {error, Error} ->
                              throw({error, Error});
                          NS ->
                              ?debugFmt("wwe debugging test_cypher ===> ~n NS: ~p~n", [NS]),
                              NS
                      end,
            ?D3("~n ~ts~n", [NCypher]),
            {ok, {NPTree, _NToks}}
                = try
                      {ok, {NPT, NT}} = ocparse:parsetree_with_tokens(NCypher),
                      {ok, {NPT, NT}}
                  catch
                      _:_ ->
                          ?D_("Error ocparse:parsetree_with_tokens : Test     ~n > ~p", [Test]),
                          ?D_("Error ocparse:parsetree_with_tokens : TestGroup~n > ~p", [_TestGroup]),
                          ?D_("Error ocparse:parsetree_with_tokens : NCypher  ~n > ~p", [NCypher]),
                          throw({error, "Error ocparse:parsetree_with_tokens"})
                  end,
            try
                ParseTree = NPTree
            catch
                _:_ ->
                    ?D_("[catch ParseTree = NPTree] : Test  ~n > ~p", [Test]),
                    ?D_("[catch ParseTree = NPTree] : NPTree~n > ~p", [NPTree]),
                    ?D_("[catch ParseTree = NPTree] : Tokens~n > ~p", [_Tokens]),
                    ?D_("[catch ParseTree = NPTree] : NToks ~n > ~p", [_NToks]),
                    throw({error, "Error catch ParseTree = NPTree"})
            end,
            try
                ?assertEqual(ParseTree, NPTree)
            catch
                _:_ ->
                    ?D_("[catch ?assertEqual(ParseTree, NPTree)] : Test     ~n > ~p", [Test]),
                    ?D_("[catch ?assertEqual(ParseTree, NPTree)] : ParseTree~n > ~p", [ParseTree]),
                    ?D_("[catch ?assertEqual(ParseTree, NPTree)] : NPTree   ~n > ~p", [NPTree]),
                    throw({error, "Error catch ?assertEqual(ParseTree, NPTree)"})
            end,
            ?D4("~n ~p~n", [ParseTree]);
        {lex_error, _Error} ->
            ?D_("Failed lex_error : Test ~n > ~p", [Test]),
            ?D_("Failed lex_error : Error~n > ~p", [_Error]),
            ?assertEqual(ok, _Error);
        {parse_error, {_Error, _Tokens}} ->
            ?D_("Failed parse_error : Test  ~n > ~p", [Test]),
            ?D_("Failed parse_error : Error ~n > ~p", [_Error]),
            ?D_("Failed parse_error : Tokens~n > ~p", [_Tokens]),
            ?assertEqual(ok, _Error)
    end.
