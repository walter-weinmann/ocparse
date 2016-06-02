-module(ocparse_fold).

-export([fold/5]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cypher Statement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {cypher_statement, {query_options, _} = QueryOptions, {statement, _} = Statement} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {QueryOptionsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, QueryOptions),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {StatementNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Statement),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {QueryOptionsNew ++ StatementNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {cypher_statement, {statement, _} = Statement} = ST)
    when is_tuple(Statement) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {StatementNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Statement),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {StatementNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cypher Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {{version, _} = Version, {options, _} = Options} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VersionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Version),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {OptionsNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Options),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {VersionNew ++ OptionsNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {{version, _} = Version} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VersionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Version),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {VersionNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {{options, _} = Options} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptionsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Options),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {OptionsNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {options, CypherOptions} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptionStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {option, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ [SubAcc], CtxAcc1};
            Other ->
                ?debugFmt("wwe debugging fold/5 ===> ~n Other: ~p~n", [Other]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, Other),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                       end,
        {[], NewCtx},
        CypherOptions),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {OptionStr, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {option, {_, _, LeftSide}, {_, _, RightSide}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {LeftSide ++ " = " ++ RightSide ++ " ", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Query Options
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {query_options, QueryOptions} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptionStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {Option, []} when Option == cypher;  Option == explain;  Option == profile ->
                {Acc ++ atom_to_list(Option) ++ " ", Fun(F, CtxAcc)};
            {cypher, CypherOptions} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n CypherOptions: ~p~n", [CypherOptions]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, CypherOptions),
                {Acc ++ atom_to_list(cypher) ++ " " ++ [SubAcc], CtxAcc1}
        end
                                       end,
        {[], NewCtx},
        QueryOptions),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {OptionStr, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Statement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {statement, StatementDetails} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {StatementDetailsStr, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, StatementDetails),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {StatementDetailsStr, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Version Number
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {version, VersionNumber} = ST)
    when is_binary(VersionNumber) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {binary_to_list(VersionNumber) ++ " ", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reserved Word
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, ST)
    when is_atom(ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {atom_to_list(ST) ++ " ", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Not Yet Supported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, PTree) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n PTree: ~p~n", [_Lvl, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).
