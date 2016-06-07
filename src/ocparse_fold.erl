-module(ocparse_fold).

-export([fold/5]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {doubleLiteral, {DoubleType, Double}} = ST)
    when DoubleType == exponentDecimalReal; DoubleType == regularDecimalReal ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Double, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {parameter, {unsignedDecimalInteger, "0"}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"{0} ", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {signedIntegerLiteral, {IntegerType, Integer}} = ST)
    when IntegerType == decimalInteger; IntegerType == hexInteger; IntegerType == octalInteger ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Integer, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {symbolicName, SymbolicName} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {SymbolicName, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {versionNumber, VersionNumber} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {VersionNumber, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {Type, Value} = ST)
    when Type == stringLiteral;  Type == unsignedDecimalInteger ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Value, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {terminal, count} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {atom_to_list(count) ++ "(*)", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {Type, Value} = ST)
    when Type == terminal ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {atom_to_list(Value), NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% anyCypherOptions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {anyCypherOptions, AnyCypherOptions} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AnyCypherOptionsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {anyCypherOption, Option, []} when Option == explain;  Option == profile ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {Acc ++ atom_to_list(Option) ++ " ", Fun(F, CtxAcc)};
            {anyCypherOption, {cypherOption, {versionNumber, _} = VersionNumber}}
                when is_tuple(VersionNumber) ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, VersionNumber),
                {Acc ++ atom_to_list(cypher) ++ " " ++ [SubAcc] ++ " ", CtxAcc1};
            {anyCypherOption, {cypherOption, Option}} when Option == cypher ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {Acc ++ atom_to_list(Option) ++ " ", Fun(F, CtxAcc)};
            {anyCypherOption, {cypherOption, ConfigurationOptions}}
                when is_list(ConfigurationOptions) ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, {configurationOptions, ConfigurationOptions}),
                {Acc ++ atom_to_list(cypher) ++ " " ++ [SubAcc], CtxAcc1};
            {anyCypherOption, {cypherOption, {{versionNumber, _} = VersionNumber, ConfigurationOptions}}}
                when is_tuple(VersionNumber), is_list(ConfigurationOptions) ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc1, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, VersionNumber),
                CtxAcc2 = case FType of
                              top_down -> Fun(ST, CtxAcc1);
                              bottom_up -> CtxAcc1
                          end,
                {SubAcc2, CtxAcc3} = fold(FType, Fun, CtxAcc2, Lvl + 1, {configurationOptions, ConfigurationOptions}),
                {Acc ++ atom_to_list(cypher) ++ " " ++ [SubAcc1] ++ " " ++ [SubAcc2], CtxAcc3}
        end
                                                 end,
        {[], NewCtx},
        AnyCypherOptions),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {AnyCypherOptionsStr, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% atom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {atom, {Type, _} = Value} = ST)
    when Type == numberLiteral; Type == parameter; Type == stringLiteral; Type == terminal; Type == variable ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {command, {CommandType, _} = Command} = ST)
    when CommandType == createIndex;  CommandType == dropIndex ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {CommandNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Command),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {CommandNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {Command, {{node_label, {'NAME', _, NodeLabel}}, {'NAME', _, PropertyKeyName}}} = ST)
    when Command == 'create constraint on';  Command == 'drop constraint on' ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {atom_to_list(Command) ++ " :" ++ NodeLabel ++ " (" ++ PropertyKeyName ++ ")", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% configurationOptions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {configurationOptions, ConfigurationOptions} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ConfigurationOptionsStr, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {configurationOption, {symbolicName, _} = Left, {symbolicName, _} = Right} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc1, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, Left),
                CtxAcc2 = case FType of
                              top_down -> Fun(ST, CtxAcc1);
                              bottom_up -> CtxAcc1
                          end,
                {SubAcc2, CtxAcc3} = fold(FType, Fun, CtxAcc2, Lvl + 1, Right),
                {Acc ++ [SubAcc1] ++ "=" ++ [SubAcc2] ++ " ", CtxAcc3}
        end
                                                     end,
        {[], NewCtx},
        ConfigurationOptions),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ConfigurationOptionsStr, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% createIndex / dropIndex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {createIndex, {index, _} = Index} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IndexNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Index),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"create " ++ IndexNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {dropIndex, {index, _} = Index} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IndexNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Index),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"drop " ++ IndexNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cypher
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {cypher, {queryOptions, _} = QueryOptions, {statement, _} = Statement} = ST) ->
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
    RT = {lists:flatten(QueryOptionsNew ++ StatementNew), NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {cypher, {statement, _} = Statement} = ST)
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
    RT = {lists:flatten(StatementNew), NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression2, Atom, Addons} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AtomNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Atom),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {AddonStr, NewCtx3} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {nodeLabel, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ [SubAcc] ++ " ", CtxAcc1};
            {propertyLookup, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ [SubAcc] ++ " ", CtxAcc1}
        end
                                      end,
        {[], NewCtx2},
        Addons),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:flatten(AtomNew ++ " " ++ AddonStr), NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression3, Expression2, Terminal} = ST)
    when Terminal == 'is null'; Terminal == 'is not null' ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Expression2New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression2),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {Expression2New ++ atom_to_list(Terminal) ++ " ", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {expression3, Expression2_1, Terminal, Expression2_2} = ST)
    when Terminal == "=~" ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Expression2_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression2_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Expression2_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression2_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Expression2_1New ++ Terminal ++ " " ++ Expression2_2New ++ " ", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {expression3, Expression2_1, Terminal, Expression2_2} = ST)
    when Terminal == contains; Terminal == 'ends with'; Terminal == in; Terminal == 'starts with' ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Expression2_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression2_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Expression2_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression2_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Expression2_1New ++ atom_to_list(Terminal) ++ " " ++ Expression2_2New ++ " ", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression4, Expression3, Terminal} = ST)
    when Terminal == "+"; Terminal == "-" ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Expression3New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression3),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {Terminal ++ Expression3New ++ " ", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {ExpressionType, ExpressionPrev} = ST) 
    when ExpressionType == expression4; ExpressionType == expression5; ExpressionType == expression6; ExpressionType == expression7; ExpressionType == expression8 ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionPrevNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ExpressionPrev),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ExpressionPrevNew ++ " ", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression5, Expression4_1, Terminal, Expression4_2} = ST)
    when Terminal == "^" ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Expression4_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression4_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Expression4_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression4_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Expression4_1New ++ Terminal ++ " " ++ Expression4_2New ++ " ", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression6, Expression5_1, Terminal, Expression5_2} = ST)
    when Terminal == "*"; Terminal == "/"; Terminal == "%" ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Expression5_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression5_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Expression5_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression5_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Expression5_1New ++ Terminal ++ " " ++ Expression5_2New ++ " ", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression7, Expression6_1, Terminal, Expression6_2} = ST)
    when Terminal == "+"; Terminal == "-" ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Expression6_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression6_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Expression6_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression6_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Expression6_1New ++ Terminal ++ " " ++ Expression6_2New ++ " ", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% index
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {index, {{nodeLabel, _} = NodeLabel, {propertyKeyName, _} = PropertyKeyName}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NodeLabelNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, NodeLabel),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {PropertyKeyNameNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, PropertyKeyName),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"index on " ++ NodeLabelNew ++ "(" ++ PropertyKeyNameNew ++ ")", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% labelName
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {labelName, LabelName} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {LabelNameNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, LabelName),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {LabelNameNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nodeLabel
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {nodeLabel, NodeLabel} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NodeLabelNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, NodeLabel),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {":" ++ NodeLabelNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% numberLiteral
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {numberLiteral, {Type, _} = NumberLiteral} = ST)
    when Type == doubleLiteral; Type == unsignedDecimalInteger; Type == signedIntegerLiteral ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NumberLiteralNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, NumberLiteral),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {NumberLiteralNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {parameter, {unsignedDecimalInteger, _} = Parameter} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ParameterNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Parameter),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"{" ++ ParameterNew ++ "} ", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {parameter, Parameter} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ParameterNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Parameter),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"{" ++ ParameterNew ++ "} ", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyKeyName
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyKeyName, PropertyKeyName} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PropertyKeyNameNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, PropertyKeyName),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {PropertyKeyNameNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyLookup, PropertyLookup, Addon} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PropertyLookupNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, PropertyLookup),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"." ++ PropertyLookupNew ++ Addon, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% queryOptions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {queryOptions, QueryOptions} = ST)
    when is_list(QueryOptions) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {QueryOptionsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {anyCypherOptions, QueryOptions}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {QueryOptionsNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Statement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {statement, Statement} = ST) ->
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
% variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {variable, Variable} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Variable),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {VariableNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Not yet supported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, PTree) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n PTree: ~p~n", [_Lvl, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).
