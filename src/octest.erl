%%%-------------------------------------------------------------------
%%% File        : octest.erl
%%% Description : Test helper functions.
%%%
%%% Created     : 06.2016
%%%-------------------------------------------------------------------
-module(octest).

-compile(export_all).

%-define(NODEBUG, true).
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ct_string/1]).

%%--------------------------------------------------------------------
%% Common Test Helper Function.
%%--------------------------------------------------------------------

ct_string(Test) ->
    % ?debugFmt("wwe debugging test_cypher/1 ===> ~n Test: ~p~n", [Test]),
    case ocparse:parsetree_with_tokens(Test) of
        {ok, {ParseTree, _Tokens}} ->
            % ?debugFmt("wwe debugging test_cypher/1 ===> ~n ParseTree: ~p~n Tokens: ~p~n", [ParseTree, _Tokens]),
            %% --------------------------------------------------------
            %% Test TopDown
            %% --------------------------------------------------------
            NCypherTD = case ocparse:parsetree_to_string_td(ParseTree) of
                            {error, ErrorTD} ->
                                throw({error, ErrorTD});
                            NSTD ->
                                % ?debugFmt("wwe debugging test_cypher/1 ===> top down~n Test: ~p~n NS: ~p~n", [NSTD]),
                                NSTD
                        end,
            {ok, {NPTreeTD, _NToksTD}}
                = try
                      {ok, {NPTTD, NTTD}} = ocparse:parsetree_with_tokens(NCypherTD),
                      {ok, {NPTTD, NTTD}}
                  catch _:_ ->
                ?debugFmt("wwe debugging test_cypher/1 ===> top down~n Test: ~p~n NCypherTD: ~p~n", [Test, NCypherTD])
                  end,
            try
                ParseTree = NPTreeTD
            catch
                _:_ ->
                    ?debugFmt("wwe debugging test_cypher/1 ===> top down~n Test: ~p~n ParseTree: ~p~n NPTree: ~p~n Tokens: ~p~n NToks: ~p~n", [Test, ParseTree, NPTreeTD, _Tokens, _NToksTD])
            end,
            ?assertEqual(ParseTree, NPTreeTD),
            StringNCypherTD = binary:bin_to_list(NCypherTD),
            StringNCypherTDMultipleSpace = string:str(StringNCypherTD, "  "),
            _ = case StringNCypherTDMultipleSpace of
                    0 -> ok;
                    _ ->
                        ?debugFmt("wwe debugging test_cypher/1 ===> top down~n Test: ~p~n NCypher: ~p~n Position space problem: ~p~n", [Test, StringNCypherTD, StringNCypherTDMultipleSpace]),
                        not_ok
                end,
            %% --------------------------------------------------------
            %% Test BottomUp
            %% --------------------------------------------------------
            NCypherBU = case ocparse:parsetree_to_string_bu(ParseTree) of
                            {error, ErrorBU} ->
                                throw({error, ErrorBU});
                            NSBU ->
                                % ?debugFmt("wwe debugging test_cypher/1 ===> bottom up~n Test: ~p~n NS: ~p~n", [NSBU]),
                                NSBU
                        end,
            {ok, {NPTreeBU, _NToksBU}}
                = try
                      {ok, {NPTBU, NTBU}} = ocparse:parsetree_with_tokens(NCypherBU),
                      {ok, {NPTBU, NTBU}}
                  catch _:_ ->
                ?debugFmt("wwe debugging test_cypher/1 ===> bottom up~n Test: ~p~n NCypher: ~p~n", [Test, NCypherBU])
                  end,
            try
                ParseTree = NPTreeBU
            catch
                _:_ ->
                    ?debugFmt("wwe debugging test_cypher/1 ===> bottom up~n Test: ~p~n ParseTree: ~p~n NPTree: ~p~n Tokens: ~p~n NToks: ~p~n", [Test, ParseTree, NPTreeBU, _Tokens, _NToksBU])
            end,
            StringNCypherBU = binary:bin_to_list(NCypherBU),
            StringNCypherBUMultipleSpace = string:str(StringNCypherBU, "  "),
            _ = case StringNCypherBUMultipleSpace of
                    0 -> ok;
                    _ ->
                        ?debugFmt("wwe debugging test_cypher/1 ===> bottom up~n Test: ~p~n NCypher: ~p~n Position space problem: ~p~n", [Test, StringNCypherBU, StringNCypherBUMultipleSpace]),
                        not_ok
                end;
        {lex_error, _Error} ->
            ?debugFmt("wwe debugging test_cypher/1 ===> Failed lexer~n Test: ~p~n Error: ~p~n", [Test, _Error]),
            ?assertEqual(ok, _Error);
        {parse_error, {_Error, _Tokens}} ->
            ?debugFmt("wwe debugging test_cypher/1 ===> Failed parser~n Test: ~p~n Error: ~p~n Tokens: ~p~n", [Test, _Error, _Tokens]),
            ?assertEqual(ok, _Error)
    end.
