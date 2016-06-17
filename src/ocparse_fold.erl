-module(ocparse_fold).

-export([fold/5]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {Type, {Value, _}} = ST)
  when Type == dash; Type == leftArrowHead; Type == rightArrowHead ->
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
fold(FType, Fun, Ctx, _Lvl, {doubleLiteral, {DoubleType, Value}} = ST)
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
  RT = {Value, NewCtx1},
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
fold(FType, Fun, Ctx, _Lvl, {signedIntegerLiteral, {IntegerType, Value}} = ST)
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
  RT = {Value, NewCtx1},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, _Lvl, {Type, Value} = ST)
  when Type == stringLiteral; Type == symbolicName;  Type == unsignedDecimalInteger; Type == versionNumber ->
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
fold(FType, Fun, Ctx, _Lvl, {terminal, Value} = ST) ->
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
% atom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {atom, {Type, _} = Value} = ST)
  when Type == caseExpression; Type == functionInvocation; Type == listComprehension;
  Type == mapLiteral; Type == numberLiteral; Type == parameter; Type == parenthesizedExpression;
  Type == stringLiteral; Type == terminal; Type == variable ->
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
fold(FType, Fun, Ctx, Lvl, {atom, {Type, Value}} = ST)
  when Type == all; Type == any; Type == extract; Type == filter; Type == none; Type == single ->
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
  RT = {atom_to_list(Type) ++ "(" ++ ValueNew ++ ")", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {atom, {extract = Type, FilterExpression, Expression}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {FilterExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, FilterExpression),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {ExpressionNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {atom_to_list(Type) ++ "(" ++ FilterExpressionNew ++ "|" ++ ExpressionNew ++ ")", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {atom, Value, "]"} = ST)
  when is_list(Value) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {expressionCommaList, Value}),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {"[" ++ ValueNew ++ "]", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {atom, {Type, _, _} = Value} = ST)
  when Type == caseExpression; Type == functionInvocation; Type == listComprehension;
  Type == relationshipsPattern ->
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
fold(FType, Fun, Ctx, Lvl, {atom, {Type, _, _, _} = Value} = ST)
  when Type == caseExpression; Type == functionInvocation ->
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
fold(FType, Fun, Ctx, Lvl, {atom, {reduce, _, _, _, _} = Value} = ST) ->
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
% caseAlternatives
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {caseAlternatives, Values} = ST)
  when is_list(Values) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
    case F of
      {caseAlternatives, When, Then} ->
        ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
        {SubAcc1, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, When),
        CtxAcc2 = case FType of
                    top_down -> Fun(ST, CtxAcc1);
                    bottom_up -> CtxAcc1
                  end,
        {SubAcc2, CtxAcc3} = fold(FType, Fun, CtxAcc2, Lvl + 1, Then),
        {Acc ++ "when " ++ [SubAcc1] ++ " then " ++ [SubAcc2] ++ " ", CtxAcc3}
    end
                                    end,
    {[], NewCtx},
    Values),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caseExpression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {caseExpression, {}, CaseAlternatives, {}} = ST)
  when is_list(CaseAlternatives) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {CaseAlternativesNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {caseAlternatives, CaseAlternatives}),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {"case " ++ CaseAlternativesNew ++ " end", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {caseExpression, {}, CaseAlternatives, Expression_2} = ST)
  when is_list(CaseAlternatives) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {CaseAlternativesNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {caseAlternatives, CaseAlternatives}),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Expression_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression_2),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"case " ++ CaseAlternativesNew ++ "else " ++ Expression_2New ++ " end", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {caseExpression, Expression_1, CaseAlternatives, {}} = ST)
  when is_list(CaseAlternatives) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Expression_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression_1),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {CaseAlternativesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {caseAlternatives, CaseAlternatives}),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"case " ++ Expression_1New ++ " " ++ CaseAlternativesNew ++ " end", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {caseExpression, Expression_1, CaseAlternatives, Expression_2} = ST)
  when is_list(CaseAlternatives) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Expression_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression_1),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {CaseAlternativesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {caseAlternatives, CaseAlternatives}),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {Expression_2New, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Expression_2),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {"case " ++ Expression_1New ++ " " ++ CaseAlternativesNew ++ "else " ++ Expression_2New ++ " end", NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {command, {CommandType, _} = Value} = ST)
  when CommandType == createIndex;  CommandType == dropIndex ->
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

fold(FType, Fun, Ctx, Lvl, {configurationOptions, Values} = ST)
  when is_list(Values) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
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
    Values),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% createIndex / dropIndex
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {createIndex, {index, _} = Value} = ST) ->
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
  RT = {"create " ++ ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {dropIndex, {index, _} = Value} = ST) ->
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
  RT = {"drop " ++ ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cypher
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {cypher, {}, {statement, _} = Statement, Semicolon} = ST) ->
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
  RT = {lists:flatten(StatementNew ++ Semicolon), NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {cypher, {queryOptions, _} = QueryOptions, {statement, _} = Statement, Semicolon} = ST) ->
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
  RT = {lists:flatten(QueryOptionsNew ++ StatementNew ++ Semicolon), NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expressionCommaList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expressionCommaList, Values} = ST)
  when is_list(Values) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
    case F of
      {expression, _} ->
        ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ case length(Acc) of
                  0 -> [];
                  _ -> ","
                end ++ [SubAcc], CtxAcc1}
    end
                                    end,
    {[], NewCtx},
    Values),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression10 .. expression12
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {ExpressionType, Expression9_1, {Terminal, _}, Expression9_2} = ST)
  when ExpressionType == expression10 andalso Terminal == 'AND';
  ExpressionType == expression12 andalso Terminal == 'OR';
  ExpressionType == expression11 andalso Terminal == 'XOR' ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Expression9_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression9_1),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Expression9_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression9_2),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {Expression9_1New ++ " " ++ atom_to_list(Terminal) ++ " " ++ Expression9_2New, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression2, Atom, Addons} = ST)
  when is_list(Addons) ->
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
  {AddonsNew, NewCtx3} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
    case F of
      {nodeLabels, _} ->
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
  RT = {lists:flatten(AtomNew ++ " " ++ AddonsNew), NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression3, Value, Terminal} = ST)
  when Terminal == 'is null'; Terminal == 'is not null' ->
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
  RT = {ValueNew ++ atom_to_list(Terminal), NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {expression3, Expression2_1, "=~", Expression2_2} = ST) ->
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
  RT = {Expression2_1New ++ "=~" ++ " " ++ Expression2_2New, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {expression3, Expression2, "[", Expression} = ST) ->
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
  {ExpressionNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {Expression2New ++ "[" ++ ExpressionNew ++ "]", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {expression3, Expression2, "[", Expression_1, Expression_2} = ST) ->
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
  {Expression_1New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression_1),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {Expression_2New, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Expression_2),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {Expression2New ++ "[" ++ Expression_1New ++ ".." ++ Expression_2New ++ "]", NewCtx6},
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
  RT = {Expression2_1New ++ atom_to_list(Terminal) ++ " " ++ Expression2_2New, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {ExpressionType, Value} = ST)
  when ExpressionType == expression3; ExpressionType == expression4; ExpressionType == expression5;
  ExpressionType == expression6; ExpressionType == expression7; ExpressionType == expression8;
  ExpressionType == expression9; ExpressionType == expression10; ExpressionType == expression11;
  ExpressionType == expression12 ->
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
fold(FType, Fun, Ctx, Lvl, {expression, Value} = ST) ->
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
% expression4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression4, Value, Terminal} = ST)
  when Terminal == "+"; Terminal == "-" ->
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
  RT = {Terminal ++ ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression5, Expression4_1, "^" = Terminal, Expression4_2} = ST) ->
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
  RT = {Expression4_1New ++ Terminal ++ " " ++ Expression4_2New, NewCtx4},
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
  RT = {Expression5_1New ++ Terminal ++ " " ++ Expression5_2New, NewCtx4},
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
  RT = {Expression6_1New ++ Terminal ++ " " ++ Expression6_2New, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression8, Expression7, PartialCompraisonExpression} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Expression7New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression7),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {PartialCompraisonExpressionNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, PartialCompraisonExpression),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {Expression7New ++ " " ++ PartialCompraisonExpressionNew, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression9, Value, {'NOT', _}} = ST) ->
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
  RT = {"not " ++ ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filterExpression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {filterExpression, IdInColl, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {IdInCollNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, IdInColl),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {IdInCollNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {filterExpression, IdInColl, Where} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {IdInCollNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, IdInColl),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {WhereNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Where),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {IdInCollNew ++ " " ++ WhereNew, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionInvocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, [], []} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {FunctionNameNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, FunctionName),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {FunctionNameNew ++ "()", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, [], "distinct"} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {FunctionNameNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, FunctionName),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {FunctionNameNew ++ "(distinct)", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, ExpressionCommaList, []} = ST)
  when is_list(ExpressionCommaList) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {FunctionNameNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, FunctionName),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {ExpressionCommaListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {expressionCommaList, ExpressionCommaList}),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {FunctionNameNew ++ "(" ++ ExpressionCommaListNew ++ ")", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, ExpressionCommaList, "distinct"} = ST)
  when is_list(ExpressionCommaList) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {FunctionNameNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, FunctionName),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {ExpressionCommaListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {expressionCommaList, ExpressionCommaList}),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {FunctionNameNew ++ "(distinct " ++ ExpressionCommaListNew ++ ")", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% idInColl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {idInColl, Variable, Expression} = ST) ->
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
  {ExpressionNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {VariableNew ++ " in " ++ ExpressionNew, NewCtx4},
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
% listComprehension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {listComprehension, FilterExpression, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {FilterExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, FilterExpression),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {"[" ++ FilterExpressionNew ++ "]", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {listComprehension, FilterExpression, Expression} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {FilterExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, FilterExpression),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {ExpressionNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"[" ++ FilterExpressionNew ++ "|" ++ ExpressionNew ++ "]", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mapLiteral
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {mapLiteral, []} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  NewCtx1 = case FType of
              top_down -> NewCtx;
              bottom_up -> Fun(ST, NewCtx)
            end,
  RT = {"{}", NewCtx1},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {mapLiteral, Value} = ST)
  when is_list(Value) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {propertyKeyNameExpressionCommaList, Value}),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {"{" ++ ValueNew ++ "}", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nodeLabel, properties, unsignedIntegerLiteral, where
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
  when Type == nodeLabel; Type == properties; Type == unsignedIntegerLiteral; Type == where ->
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
  RT = {case Type of
          nodeLabel -> ":";
          where -> "where ";
          _ -> []
        end ++ ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nodeLabels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {nodeLabels, Values} = ST)
  when is_list(Values) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
    case F of
      {nodeLabel, _} ->
        ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ [SubAcc] ++ " ", CtxAcc1}
    end
                                    end,
    {[], NewCtx},
    Values),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nodePattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {nodePattern, {}, [], {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  NewCtx1 = case FType of
              top_down -> NewCtx;
              bottom_up -> Fun(ST, NewCtx)
            end,
  RT = {"()", NewCtx1},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {nodePattern, {}, NodeLabels, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {NodeLabelsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, NodeLabels),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {"(" ++ NodeLabelsNew ++ ")", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {nodePattern, {}, NodeLabels, Properties} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {NodeLabelsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, NodeLabels),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {PropertiesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Properties),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"(" ++ NodeLabelsNew ++ PropertiesNew ++ ")", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {nodePattern, Variable, [], {}} = ST) ->
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
  RT = {"(" ++ VariableNew ++ ")", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {nodePattern, Variable, [], Properties} = ST) ->
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
  {PropertiesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Properties),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"(" ++ VariableNew ++ PropertiesNew ++ ")", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {nodePattern, Variable, NodeLabels, {}} = ST) ->
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
  {NodeLabelsNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, NodeLabels),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"(" ++ VariableNew ++ NodeLabelsNew ++ ")", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {nodePattern, Variable, NodeLabels, Properties} = ST) ->
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
  {NodeLabelsNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, NodeLabels),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {PropertiesNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Properties),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {"(" ++ VariableNew ++ NodeLabelsNew ++ PropertiesNew ++ ")", NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% numberLiteral
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {numberLiteral, {Type, _} = Value} = ST)
  when Type == doubleLiteral; Type == unsignedDecimalInteger; Type == signedIntegerLiteral ->
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
% parameter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {parameter, {unsignedDecimalInteger, _} = Value} = ST) ->
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
  RT = {"{" ++ ValueNew ++ "} ", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {parameter, Value} = ST) ->
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
  RT = {"{" ++ ValueNew ++ "} ", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parenthesizedExpression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {parenthesizedExpression, Value} = ST) ->
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
  RT = {"(" ++ ValueNew ++ ") ", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partialComparisonExpression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {partialComparisonExpression, Value, Terminal} = ST) ->
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
  RT = {Terminal ++ ValueNew ++ " ", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternElementChain
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {patternElementChain, RelationshipPattern, NodePattern} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {RelationshipPatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipPattern),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {NodePatternNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, NodePattern),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {RelationshipPatternNew ++ " " ++ NodePatternNew, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternElementChainList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {patternElementChainList, Values} = ST)
  when is_list(Values) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
    case F of
      {patternElementChain, _, _} ->
        ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ [SubAcc], CtxAcc1}
    end
                                    end,
    {[], NewCtx},
    Values),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyKeyName
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyKeyName, Value} = ST) ->
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
% propertyKeyNameExpression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyKeyNameExpression, {propertyKeyName, _} = PropertyKeyName, {expression, _} = Expression} = ST) ->
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
  {ExpressionNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {PropertyKeyNameNew ++ ":" ++ ExpressionNew, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyKeyNameExpressionCommaList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyKeyNameExpressionCommaList, Values} = ST)
  when is_list(Values) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
    case F of
      {propertyKeyNameExpression, _, _} ->
        ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ case length(Acc) of
                  0 -> [];
                  _ -> ","
                end ++ [SubAcc], CtxAcc1}
    end
                                    end,
    {[], NewCtx},
    Values),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyLookup, Value, []} = ST) ->
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
  RT = {"." ++ ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {propertyLookup, Value, Addon} = ST) ->
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
  RT = {"." ++ ValueNew ++ Addon, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% queryOptions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {queryOptions, Values} = ST)
  when is_list(Values) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
    case F of
      {anyCypherOption, Option, []} when Option == explain;  Option == profile ->
        ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
        {Acc ++ atom_to_list(Option) ++ " ", Fun(F, CtxAcc)};
      {anyCypherOption, {cypherOption, {}, ConfigurationOptions}}
        when is_list(ConfigurationOptions) ->
        ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, {configurationOptions, ConfigurationOptions}),
        {Acc ++ atom_to_list(cypher) ++ " " ++ [SubAcc], CtxAcc1};
      {anyCypherOption, {cypherOption, {versionNumber, _} = VersionNumber, ConfigurationOptions}}
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
    Values),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% range
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {"*", {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  NewCtx1 = case FType of
              top_down -> NewCtx;
              bottom_up -> Fun(ST, NewCtx)
            end,
  RT = {"*", NewCtx1},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {"*", Value} = ST) ->
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
  RT = {"*" ++ ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rangeLiteral
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {rangeLiteral, {}, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  NewCtx1 = case FType of
              top_down -> NewCtx;
              bottom_up -> Fun(ST, NewCtx)
            end,
  RT = {" .. ", NewCtx1},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {rangeLiteral, {}, Right} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {RightNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Right),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {" .. " ++ RightNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {rangeLiteral, Left, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {LeftNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Left),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {LeftNew ++ " .. ", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {rangeLiteral, Left, Right} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {LeftNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Left),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {RightNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Right),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {LeftNew ++ " .. " ++ RightNew, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% reduce
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {reduce, Variable_1, Expression_1, IdInColl, Expression_2} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Variable_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Variable_1),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Expression_1New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Expression_1),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {IdInCollNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, IdInColl),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  {Expression_2New, NewCtx7} = fold(FType, Fun, NewCtx6, Lvl + 1, Expression_2),
  NewCtx8 = case FType of
              top_down -> NewCtx7;
              bottom_up -> Fun(ST, NewCtx7)
            end,
  RT = {"reduce (" ++ Variable_1New ++ "=" ++ Expression_1New ++ "," ++ IdInCollNew ++ "|" ++ Expression_2New ++ ")", NewCtx8},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipDetail
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {relationshipDetail, {}, CharQuestionMark, [], {}, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  NewCtx1 = case FType of
              top_down -> NewCtx;
              bottom_up -> Fun(ST, NewCtx)
            end,
  RT = {"[" ++ CharQuestionMark ++ "]", NewCtx1},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, {}, CharQuestionMark, [], {}, Properties} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {PropertiesNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Properties),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {"[" ++ CharQuestionMark ++ PropertiesNew ++ "]", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, {}, CharQuestionMark, [], RangeLiteral, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {RangeLiteralNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RangeLiteral),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {"[" ++ CharQuestionMark ++ RangeLiteralNew ++ "]", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, {}, CharQuestionMark, [], RangeLiteral, Properties} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {RangeLiteralNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RangeLiteral),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {PropertiesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Properties),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"[" ++ CharQuestionMark ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, {}, CharQuestionMark, RelationshipTypes, {}, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {RelationshipTypesNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipTypes),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {"[" ++ CharQuestionMark ++ RelationshipTypesNew ++ "]", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, {}, CharQuestionMark, RelationshipTypes, {}, Properties} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {RelationshipTypesNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipTypes),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {PropertiesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Properties),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"[" ++ CharQuestionMark ++ RelationshipTypesNew ++ PropertiesNew ++ "]", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, {}, CharQuestionMark, RelationshipTypes, RangeLiteral, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {RelationshipTypesNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipTypes),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {RangeLiteralNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"[" ++ CharQuestionMark ++ RelationshipTypesNew ++ RangeLiteralNew ++ "]", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, {}, CharQuestionMark, RelationshipTypes, RangeLiteral, Properties} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {RelationshipTypesNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipTypes),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {RangeLiteralNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {PropertiesNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Properties),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {"[" ++ CharQuestionMark ++ RelationshipTypesNew ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, [], {}, {}} = ST) ->
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
  RT = {"[" ++ VariableNew ++ CharQuestionMark ++ "]", NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, [], {}, Properties} = ST) ->
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
  {PropertiesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Properties),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"[" ++ VariableNew ++ CharQuestionMark ++ PropertiesNew ++ "]", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, [], RangeLiteral, {}} = ST) ->
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
  {RangeLiteralNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"[" ++ VariableNew ++ CharQuestionMark ++ RangeLiteralNew ++ "]", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, [], RangeLiteral, Properties} = ST) ->
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
  {RangeLiteralNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {PropertiesNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Properties),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {"[" ++ VariableNew ++ CharQuestionMark ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, RelationshipTypes, {}, {}} = ST) ->
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
  {RelationshipTypesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelationshipTypes),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {"[" ++ VariableNew ++ CharQuestionMark ++ RelationshipTypesNew ++ "]", NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, RelationshipTypes, {}, Properties} = ST) ->
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
  {RelationshipTypesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelationshipTypes),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {PropertiesNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Properties),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {"[" ++ VariableNew ++ CharQuestionMark ++ RelationshipTypesNew ++ PropertiesNew ++ "]", NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, RelationshipTypes, RangeLiteral, {}} = ST) ->
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
  {RelationshipTypesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelationshipTypes),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {RangeLiteralNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, RangeLiteral),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {"[" ++ VariableNew ++ CharQuestionMark ++ RelationshipTypesNew ++ RangeLiteralNew ++ "]", NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, RelationshipTypes, RangeLiteral, Properties} = ST) ->
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
  {RelationshipTypesNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelationshipTypes),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {RangeLiteralNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, RangeLiteral),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  {PropertiesNew, NewCtx7} = fold(FType, Fun, NewCtx6, Lvl + 1, Properties),
  NewCtx8 = case FType of
              top_down -> NewCtx7;
              bottom_up -> Fun(ST, NewCtx7)
            end,
  RT = {"[" ++ VariableNew ++ CharQuestionMark ++ RelationshipTypesNew ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx8},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipPattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {relationshipPattern, {}, Dash_1, {}, Dash_2, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Dash_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Dash_1),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Dash_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Dash_2),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {Dash_1New ++ " " ++ Dash_2New, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, {}, Dash_1, {}, Dash_2, RightArrowHead} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Dash_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Dash_1),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Dash_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Dash_2),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {RightArrowHeadNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, RightArrowHead),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {Dash_1New ++ " " ++ Dash_2New ++ RightArrowHeadNew, NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, {}, Dash_1, RelationshipDetail, Dash_2, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Dash_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Dash_1),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {RelationshipDetailNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelationshipDetail),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {Dash_2New, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Dash_2),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {Dash_1New ++ RelationshipDetailNew ++ Dash_2New, NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, {}, Dash_1, RelationshipDetail, Dash_2, RightArrowHead} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {Dash_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Dash_1),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {RelationshipDetailNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelationshipDetail),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {Dash_2New, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Dash_2),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  {RightArrowHeadNew, NewCtx7} = fold(FType, Fun, NewCtx6, Lvl + 1, RightArrowHead),
  NewCtx8 = case FType of
              top_down -> NewCtx7;
              bottom_up -> Fun(ST, NewCtx7)
            end,
  RT = {Dash_1New ++ RelationshipDetailNew ++ Dash_2New ++ RightArrowHeadNew, NewCtx8},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, LeftArrowHead, Dash_1, {}, Dash_2, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {LeftArrowHeadNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, LeftArrowHead),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Dash_1New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Dash_1),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {Dash_2New, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Dash_2),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  RT = {LeftArrowHeadNew ++ Dash_1New ++ " " ++ Dash_2New, NewCtx6},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, LeftArrowHead, Dash_1, {}, Dash_2, RightArrowHead} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {LeftArrowHeadNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, LeftArrowHead),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Dash_1New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Dash_1),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {Dash_2New, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Dash_2),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  {RightArrowHeadNew, NewCtx7} = fold(FType, Fun, NewCtx6, Lvl + 1, RightArrowHead),
  NewCtx8 = case FType of
              top_down -> NewCtx7;
              bottom_up -> Fun(ST, NewCtx7)
            end,
  RT = {LeftArrowHeadNew ++ Dash_1New ++ " " ++ Dash_2New ++ RightArrowHeadNew, NewCtx8},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, LeftArrowHead, Dash_1, RelationshipDetail, Dash_2, {}} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {LeftArrowHeadNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, LeftArrowHead),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Dash_1New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Dash_1),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {RelationshipDetailNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, RelationshipDetail),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  {Dash_2New, NewCtx7} = fold(FType, Fun, NewCtx6, Lvl + 1, Dash_2),
  NewCtx8 = case FType of
              top_down -> NewCtx7;
              bottom_up -> Fun(ST, NewCtx7)
            end,
  RT = {LeftArrowHeadNew ++ Dash_1New ++ RelationshipDetailNew ++ Dash_2New, NewCtx8},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, LeftArrowHead, Dash_1, RelationshipDetail, Dash_2, RightArrowHead} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {LeftArrowHeadNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, LeftArrowHead),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {Dash_1New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Dash_1),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  {RelationshipDetailNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, RelationshipDetail),
  NewCtx6 = case FType of
              top_down -> NewCtx5;
              bottom_up -> Fun(ST, NewCtx5)
            end,
  {Dash_2New, NewCtx7} = fold(FType, Fun, NewCtx6, Lvl + 1, Dash_2),
  NewCtx8 = case FType of
              top_down -> NewCtx7;
              bottom_up -> Fun(ST, NewCtx7)
            end,
  {RightArrowHeadNew, NewCtx9} = fold(FType, Fun, NewCtx8, Lvl + 1, RightArrowHead),
  NewCtx10 = case FType of
               top_down -> NewCtx9;
               bottom_up -> Fun(ST, NewCtx9)
             end,
  RT = {LeftArrowHeadNew ++ Dash_1New ++ RelationshipDetailNew ++ Dash_2New ++ RightArrowHeadNew, NewCtx10},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipsPattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {relationshipsPattern, NodePattern, []} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {NodePatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, NodePattern),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {NodePatternNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;
fold(FType, Fun, Ctx, Lvl, {relationshipsPattern, NodePattern, PatternElementChainList} = ST) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {NodePatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, NodePattern),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  {PatternElementChainListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {patternElementChainList, PatternElementChainList}),
  NewCtx4 = case FType of
              top_down -> NewCtx3;
              bottom_up -> Fun(ST, NewCtx3)
            end,
  RT = {NodePatternNew ++ PatternElementChainListNew, NewCtx4},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipTypes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {relationshipTypes, Values} = ST)
  when is_list(Values) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
  NewCtx = case FType of
             top_down -> Fun(ST, Ctx);
             bottom_up -> Ctx
           end,
  {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
    case F of
      {relTypeName, _} ->
        ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
        {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
        {Acc ++ case length(Acc) of
                  0 -> ":";
                  _ -> "|:" end ++ [SubAcc], CtxAcc1}
    end
                                    end,
    {[], NewCtx},
    Values),
  NewCtx2 = case FType of
              top_down -> NewCtx1;
              bottom_up -> Fun(ST, NewCtx1)
            end,
  RT = {ValueNew, NewCtx2},
  ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
  RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% statement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {statement, Value} = ST) ->
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
% symbolicName
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
  when Type == functionName; Type == labelName; Type == relTypeName; Type == variable ->
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
% Not yet supported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, PTree) ->
  ?debugFmt("wwe debugging fold/5 ===> Start ~p~n PTree: ~p~n", [_Lvl, PTree]),
  Fun(PTree, Ctx),
  throw({"Parse tree not supported", PTree}).
