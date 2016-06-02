-module(ocparse_fold).

-export([fold/5]).

-include("ocparse.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Cypher specifics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 
% List of parse trees
%
fold(FType, Fun, Ctx, Lvl, STs) when is_list(STs) ->
    ?LogDebug("wwe debugging fold/5 ===> Start ~p~n STs: ~p~n", [Lvl, STs]),
    NewCtx = case FType of
                 top_down -> Fun(STs, Ctx);
                 bottom_up -> Ctx
             end,
    {CypherStr, NewCtx1}
        = lists:foldl(
        fun(ST, {Cypher, AccCtx}) ->
            {NewCypher, NewAccCtx} = fold(FType, Fun, AccCtx, Lvl, ST),
            {Cypher ++
                if length(Cypher) > 0 -> "; " ++ NewCypher; true -> NewCypher end,
                NewAccCtx}
        end, {"", NewCtx}, STs),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(STs, NewCtx1)
              end,
    RT = {CypherStr, NewCtx2},
    ?LogDebug("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

% Cypher
fold(FType, Fun, Ctx, Lvl, {cypher, QueryOptions, Statement} = ST)
    when is_tuple(QueryOptions), is_tuple(Statement) ->
    ?LogDebug("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
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
    RT = {QueryOptionsNew ++ " " ++ StatementNew, NewCtx4},
    ?LogDebug("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {cypher, Statement} = ST)
    when is_tuple(Statement) ->
    ?LogDebug("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
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
    ?LogDebug("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%
% Query Options
%
fold(FType, Fun, Ctx, Lvl, {'query options', QueryOptions} = ST) ->
    ?LogDebug("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {OptionStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {Option, []} when Option == cypher;  Option == explain;  Option == profile ->
                {Acc ++ atom_to_list(Option) ++ " ", Fun(F, CtxAcc)};
            Other ->
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, Other),
                {Acc ++ [SubAcc], CtxAcc1}
        end
                                       end,
        {[], NewCtx},
        QueryOptions),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {OptionStr, NewCtx2},
    ?LogDebug("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%
% Statement
%
fold(FType, Fun, Ctx, Lvl, {statement, StatementDetails} = ST) ->
    ?LogDebug("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
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
    ?LogDebug("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

% Reserved words
fold(FType, Fun, Ctx, _Lvl, ST) 
    when is_atom(ST) ->
    ?LogDebug("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {atom_to_list(ST) ++ " ", NewCtx1},
    ?LogDebug("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%
% UNSUPPORTED
%
fold(_FType, Fun, Ctx, _Lvl, PTree) ->
    ?LogDebug("wwe debugging fold/5 ===> Start ~p~n PTree: ~p~n", [_Lvl, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).
