-module(ocparse_test).

-include_lib("eunit/include/eunit.hrl").

-export([test_cypher/3]).

cypher_test_() ->
    WCard = case os:getenv("CYPHER") of
                Cypher when is_list(Cypher) -> Cypher;
                _ -> "*"
            end ++ ".tst",
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
    [_ | RootPath] = lists:reverse(filename:split(Cwd)),
    TestDir = filename:join(lists:reverse(["test" | RootPath])),
    TestFiles = lists:sort([filename:join(TestDir, T)
        || T <- filelib:wildcard(WCard, TestDir)]),
    group_gen(TestFiles, Logs).

group_gen(TestFiles, Logs) ->
    {generator,
        fun() ->
            case TestFiles of
                [] -> [];
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

tests_gen(TestGroup, Tests, Opts, Logs) ->
    SelTests = case proplists:get_value(tests, Opts) of
                   St when St =:= undefined; St =:= [] ->
                       {Indices, _} = lists:unzip(Tests),
                       Indices;
                   St -> St
               end,
    tests_gen(TestGroup, Tests, Logs, SelTests, []).

tests_gen(_TestGroup, [], _Logs, _SelTests, Acc) ->
    {inparallel, lists:reverse(Acc)};
tests_gen(TestGroup, [{I, T} | Tests], Logs, SelTests, Acc) ->
    case lists:member(I, SelTests) of
        true ->
            tests_gen(TestGroup, Tests, Logs, SelTests,
                [{TestGroup, I,
                    fun() ->
                        {timeout, 60, ?MODULE:test_cypher(TestGroup, T, Logs)}
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

test_cypher(TestGroup, Test, Logs) ->
    ?D1("~n ~s", [Test]),
    %?debugFmt("~n", []),
    %?debugFmt("wwe debugging test_cypher/3 ===> ~n Test: ~p~n", [Test]),
    case ocparse:parsetree_with_tokens(Test) of
        {ok, {ParseTree, Tokens}} ->
            %?debugFmt("wwe debugging test_cypher/3 ===> ~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, Tokens]),
            ?D2("~n~p", [ParseTree]),
            NCypher = case ocparse:parsetree_to_string_td(ParseTree) of
                          {error, Error} ->
                              throw({error, Error});
                          NS ->
                              %?debugFmt("wwe debugging test_cypher/3 ===> ~n NS: ~p~n", [NS]),
                              NS
                      end,
            ?D3("~n ~ts~n", [NCypher]),
            {ok, {NPTree, NToks}}
                = try
                      {ok, {NPT, NT}} = ocparse:parsetree_with_tokens(NCypher),
                      {ok, {NPT, NT}}
                  catch _:_ -> ?D_("Error : ~p ocparse:parsetree_with_tokens(~s)", [TestGroup, NCypher])
                  end,
            try
                ParseTree = NPTree
            catch
                _:_ ->
                    ?debugFmt("wwe debugging test_cypher/3 ===> ~n NPTree: ~p~n NToks: ~p~n", [NPTree, NToks]),
                    ?D_("~n > ~p", [NPTree]),
                    ?D_("~n > ~p", [Tokens]),
                    ?D_("~n > ~p", [NToks])
            end,
            try
                ?assertEqual(ParseTree, NPTree)
            catch
                _:_ ->
                    ?debugFmt("wwe debugging test_cypher/3 ===> not equal~n ParseTree: ~p~n NPTree: ~p~n", [ParseTree, NPTree])
            end,
            ?D4("~n ~p~n", [ParseTree]);
        {lex_error, Error} ->
            ?debugFmt("wwe debugging test_cypher/3 ===> Failed lexer~n Error: ~p~n", [Error]),
            ?D_("Failed lexer ~p", [Error]),
            ?assertEqual(ok, Error);
        {parse_error, {Error, _Tokens}} ->
            ?debugFmt("wwe debugging test_cypher/3 parser ===> ~n Error: ~p~n Tokens: ~p~n", [Error, _Tokens]),
            ?D_("Failed parser ~p", [Error]),
            % ?D_("~nFailed: ~p~nTest: ~s~nTokens ~p", [Error, Test, Tokens]),
            ?assertEqual(ok, Error)
    end.
