-module(ocparse_fold).

-export([fold/5]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Terminals.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
% anonymousPatternPart / expression... / functionName / labelName / lookup / patternPart /  
% propertyKeyName / relTypeName / statement / variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == anonymousPatternPart; Type == expression; Type == expression3; Type == expression4;
    Type == expression5; Type == expression6; Type == expression7; Type == expression8;
    Type == expression9; Type == expression10; Type == expression11; Type == expression12;
    Type == functionName; Type == labelName; Type == lookup; Type == patternPart;
    Type == propertyKeyName; Type == query; Type == relTypeName; Type == removeItem; Type == sortItem;
    Type == statement; Type == variable ->
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
% atom
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {atom, {Type, _} = Value} = ST)
    when Type == caseExpression; Type == functionInvocation; Type == listComprehension;
    Type == numberLiteral; Type == parenthesizedExpression; Type == properties;
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
    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {expressionCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"[" ++ ValueNew ++ "]", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {atom, {Type, _, _} = Value} = ST)
    when Type == caseExpression; Type == functionInvocation; Type == listComprehension;
    Type == relationshipsPattern; Type == shortestPathPattern ->
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
% clause
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold(FType, Fun, Ctx, Lvl, {clause, {Type, _} = Value} = ST)
    when Type == remove; Type == set ->
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

fold(FType, Fun, Ctx, Lvl, {clause, {Type, _, _} = Value} = ST)
    when Type == create; Type == delete; Type == merge; Type == return; Type == start; Type == unwind ->
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
fold(FType, Fun, Ctx, Lvl, {clause, {Type, _, _, _} = Value} = ST)
    when Type == forEach; Type == with ->
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
fold(FType, Fun, Ctx, Lvl, {clause, {Type, _, _, _, _} = Value} = ST)
    when Type == loadCSV; Type == match ->
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
% clauseList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {clauseList, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {clause, _} ->
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
% command
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {command, {Type, _} = Value} = ST)
    when Type == createIndex; Type == createNodePropertyExistenceConstraint;
    Type == createRelationshipPropertyExistenceConstraint; Type == createUniqueConstraint;
    Type == dropIndex; Type == dropNodePropertyExistenceConstraint;
    Type == dropRelationshipPropertyExistenceConstraint; Type == dropUniqueConstraint ->
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
% create
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {create, [], Pattern} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"create " ++ PatternNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {create, _Unique, Pattern} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"create unique " ++ PatternNew, NewCtx2},
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
% createNodePropertyExistenceConstraint / createRelationshipPropertyExistenceConstraint / createUniqueConstraint 
% dropNodePropertyExistenceConstraint / dropRelationshipPropertyExistenceConstraint / dropUniqueConstraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == createNodePropertyExistenceConstraint; Type == createRelationshipPropertyExistenceConstraint;
    Type == createUniqueConstraint ->
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
fold(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == dropNodePropertyExistenceConstraint; Type == dropRelationshipPropertyExistenceConstraint;
    Type == dropUniqueConstraint ->
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
% delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {delete, [], ExpressionCommalist} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionCommalistNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"delete " ++ ExpressionCommalistNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {delete, _Detach, ExpressionCommalist} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionCommalistNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"detach delete " ++ ExpressionCommalistNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expressionCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expressionCommalist, Values} = ST)
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
% forEach
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {forEach, Variable, Expression, []} = ST) ->
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
    RT = {"foreach (" ++ VariableNew ++ " in " ++ ExpressionNew ++ " |)", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {forEach, Variable, Expression, ClauseList} = ST) ->
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
    {ClauseListNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, {clauseList, ClauseList}),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"foreach (" ++ VariableNew ++ " in " ++ ExpressionNew ++ " | " ++ ClauseListNew ++ ")", NewCtx6},
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
fold(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, ExpressionCommalist, []} = ST)
    when is_list(ExpressionCommalist) ->
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
    {ExpressionCommalistNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {FunctionNameNew ++ "(" ++ ExpressionCommalistNew ++ ")", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, ExpressionCommalist, "distinct"} = ST)
    when is_list(ExpressionCommalist) ->
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
    {ExpressionCommalistNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {FunctionNameNew ++ "(distinct " ++ ExpressionCommalistNew ++ ")", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {hint, Value} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {variableCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"using join on " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {hint, Variable, NodeLabel} = ST) ->
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
    {NodeLabelNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, NodeLabel),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"using scan " ++ VariableNew ++ NodeLabelNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {hint, Variable, NodeLabel, PropertyKeyName} = ST) ->
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
    {NodeLabelNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, NodeLabel),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {PropertyKeyNameNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, PropertyKeyName),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"using index " ++ VariableNew ++ NodeLabelNew ++ "(" ++ PropertyKeyNameNew ++ ")", NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% hintList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {hintList, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {hint, _, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " " end ++ [SubAcc], CtxAcc1};
            {hint, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " " end ++ [SubAcc], CtxAcc1};
            {hint, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " " end ++ [SubAcc], CtxAcc1}
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
% identifiedIndexLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {identifiedIndexLookup, SymbolicName_1, SymbolicName_2, Value} = ST)
    when is_list(Value) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SymbolicName_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, SymbolicName_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {SymbolicName_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, SymbolicName_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {":" ++ SymbolicName_1New ++ "(" ++ SymbolicName_2New ++ " = " ++ Value ++ ")", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {identifiedIndexLookup, SymbolicName_1, SymbolicName_2, Value} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SymbolicName_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, SymbolicName_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {SymbolicName_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, SymbolicName_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {ValueNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Value),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {":" ++ SymbolicName_1New ++ "(" ++ SymbolicName_2New ++ " = " ++ ValueNew ++ ")", NewCtx6},
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
% idLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {idLookup, "*"} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"(*)", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {idLookup, Value} = ST) ->
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
    RT = {"(" ++ ValueNew ++ ")", NewCtx2},
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
% indexQuery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {indexQuery, SymbolicName, Value} = ST)
    when is_list(Value) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SymbolicNameNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, SymbolicName),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {":" ++ SymbolicNameNew ++ "(" ++ Value ++ ")", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {indexQuery, SymbolicName, Value} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SymbolicNameNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, SymbolicName),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {ValueNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Value),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {":" ++ SymbolicNameNew ++ "(" ++ ValueNew ++ ")", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% limit
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {limit, Value} = ST) ->
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
    RT = {"limit " ++ ValueNew, NewCtx2},
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
% literalIds
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {literalIds, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {unsignedIntegerLiteral, _} ->
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
% loadCSV
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {loadCSV, [], Expression, Variable, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {VariableNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Variable),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"load csv from " ++ ExpressionNew ++ " as " ++ VariableNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {loadCSV, [], Expression, Variable, FieldTerminator} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {VariableNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Variable),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"load csv from " ++ ExpressionNew ++ " as " ++ VariableNew ++ " fieldterminator " ++ FieldTerminator, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {loadCSV, _WithHeaders, Expression, Variable, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {VariableNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Variable),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"load csv with headers from " ++ ExpressionNew ++ " as " ++ VariableNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {loadCSV, _WithHeaders, Expression, Variable, FieldTerminator} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {VariableNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Variable),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"load csv with headers from " ++ ExpressionNew ++ " as " ++ VariableNew ++ " fieldterminator " ++ FieldTerminator, NewCtx4},
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
    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {propertyKeyNameExpressionCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"{" ++ ValueNew ++ "}", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {match, [], Pattern, [], {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"match " ++ PatternNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {match, [], Pattern, [], Where} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {WhereNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Where),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"match " ++ PatternNew ++ " " ++ WhereNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {match, [], Pattern, HintList, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {HintListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {hintList, HintList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"match " ++ PatternNew ++ " " ++ HintListNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {match, [], Pattern, HintList, Where} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {HintListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {hintList, HintList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {WhereNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Where),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"match " ++ PatternNew ++ " " ++ HintListNew ++ " " ++ WhereNew, NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {match, _Optional, Pattern, [], {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"optional match " ++ PatternNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {match, _Optional, Pattern, [], Where} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {WhereNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Where),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"optional match " ++ PatternNew ++ " " ++ WhereNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {match, _Optional, Pattern, HintList, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {HintListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {hintList, HintList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"optional match " ++ PatternNew ++ " " ++ HintListNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {match, _Optional, Pattern, HintList, Where} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {HintListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {hintList, HintList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {WhereNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Where),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"optional match " ++ PatternNew ++ " " ++ HintListNew ++ " " ++ WhereNew, NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merge
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {merge, PatternPart, MergeActionList} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternPartNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, PatternPart),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {MergeActionListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {mergeActionList, MergeActionList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"merge " ++ PatternPartNew ++ " " ++ MergeActionListNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mergeAction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {mergeAction, Type, Value} = ST) ->
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
    RT = {"on " ++ Type ++ " " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mergeActionList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {mergeActionList, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {mergeAction, _, _} ->
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
% nodeLabel, properties, unsignedIntegerLiteral, where
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == nodeLabel; Type == properties; Type == relType; Type == unsignedIntegerLiteral; Type == where ->
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
              relType -> ":";
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
% nodeLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {nodeLookup, Value} = ST) ->
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
    RT = {"node " ++ ValueNew, NewCtx2},
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
fold(FType, Fun, Ctx, Lvl, {nodePattern, {}, [], Properties} = ST) ->
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
    RT = {"(" ++ PropertiesNew ++ ")", NewCtx2},
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
% nodePropertyExistenceConstraint / uniqueConstraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Variable, NodeLabel, PropertyExpression} = ST)
    when Type == nodePropertyExistenceConstraint; Type == uniqueConstraint ->
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
    {NodeLabelNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, NodeLabel),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {PropertyExpressionNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, PropertyExpression),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"constraint on (" ++ VariableNew ++ " " ++ NodeLabelNew ++ case Type of
                                                                         nodePropertyExistenceConstraint ->
                                                                             ") assert exists (";
                                                                         uniqueConstraint ->
                                                                             ") assert " end ++ PropertyExpressionNew ++ case Type of
                                                                                                                             nodePropertyExistenceConstraint ->
                                                                                                                                 ")";
                                                                                                                             uniqueConstraint ->
                                                                                                                                 " is unique" end, NewCtx6},
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
% order
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {order, SortItemCommalist} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SortItemCommalistNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {sortItemCommalist, SortItemCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"order by " ++ SortItemCommalistNew, NewCtx2},
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
    RT = {"{" ++ ValueNew ++ "}", NewCtx2},
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
    RT = {"{" ++ ValueNew ++ "}", NewCtx2},
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
% pattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {pattern, Value} = ST)
    when is_list(Value) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {patternPartCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternElement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {patternElement, PatternElement} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternElementNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, PatternElement),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"(" ++ PatternElementNew ++ ")", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {patternElement, {nodePattern, _, _, _} = NodePattern, {}} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {patternElement, {nodePattern, _, _, _} = NodePattern, {patternElementChain, _, _} = PatternElementChain} = ST) ->
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
    {PatternElementChainNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, PatternElementChain),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {NodePatternNew ++ " " ++ PatternElementChainNew, NewCtx4},
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
% patternPart
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {patternPart, Variable, AnonymousPatternPart} = ST) ->
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
    {AnonymousPatternPartNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, AnonymousPatternPart),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {VariableNew ++ "=" ++ AnonymousPatternPartNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternPartCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {patternPartCommalist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {patternPart, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ [SubAcc], CtxAcc1};
            {patternPart, _} ->
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
% propertyExpression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyExpression, Atom, []} = ST) ->
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
    RT = {AtomNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {propertyExpression, Atom, PropertyLookUpList} = ST) ->
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
    {PropertyLookUpListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {propertyLookupList, PropertyLookUpList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {AtomNew ++ " " ++ PropertyLookUpListNew, NewCtx4},
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
% propertyKeyNameExpressionCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyKeyNameExpressionCommalist, Values} = ST)
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
    RT = {"." ++ ValueNew ++ " ", NewCtx2},
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
    RT = {"." ++ ValueNew ++ Addon ++ " ", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyLookupList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {propertyLookupList, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {propertyLookup, _, _} ->
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
% regularQuery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {regularQuery, SingularQuery, []} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SingularQueryNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, SingularQuery),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {SingularQueryNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {regularQuery, SingularQuery, UnionList} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SingularQueryNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, SingularQuery),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {UnionListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {unionList, UnionList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {SingularQueryNew ++ " " ++ UnionListNew, NewCtx4},
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
% relationshipLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {relationshipLookup, Type, Value} = ST) ->
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
    RT = {Type ++ " " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipPattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {relationshipPattern, {}, Dash_1, {}, Dash_2, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Dash_1 ++ " " ++ Dash_2, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {relationshipPattern, {}, Dash_1, {}, Dash_2, RightArrowHead} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Dash_1 ++ " " ++ Dash_2 ++ RightArrowHead, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, {}, Dash_1, RelationshipDetail, Dash_2, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipDetailNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipDetail),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {Dash_1 ++ RelationshipDetailNew ++ Dash_2, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, {}, Dash_1, RelationshipDetail, Dash_2, RightArrowHead} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipDetailNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipDetail),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {Dash_1 ++ RelationshipDetailNew ++ Dash_2 ++ RightArrowHead, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {relationshipPattern, LeftArrowHead, Dash_1, {}, Dash_2, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {LeftArrowHead ++ Dash_1 ++ " " ++ Dash_2, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {relationshipPattern, LeftArrowHead, Dash_1, {}, Dash_2, RightArrowHead} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {LeftArrowHead ++ Dash_1 ++ " " ++ Dash_2 ++ RightArrowHead, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, LeftArrowHead, Dash_1, RelationshipDetail, Dash_2, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipDetailNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipDetail),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {LeftArrowHead ++ Dash_1 ++ RelationshipDetailNew ++ Dash_2, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPattern, LeftArrowHead, Dash_1, RelationshipDetail, Dash_2, RightArrowHead} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipDetailNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipDetail),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {LeftArrowHead ++ Dash_1 ++ RelationshipDetailNew ++ Dash_2 ++ RightArrowHead, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipPatternSyntax
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {relationshipPatternSyntax, {}, Dash_1, Variable, RelType, Dash_2, {}} = ST) ->
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
    {RelTypeNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelType),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"()" ++ Dash_1 ++ "[" ++ VariableNew ++ RelTypeNew ++ "]" ++ Dash_2 ++ "()", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPatternSyntax, LeftArrowHead, Dash_1, Variable, RelType, Dash_2, {}} = ST) ->
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
    {RelTypeNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelType),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"()" ++ LeftArrowHead ++ Dash_1 ++ "[" ++ VariableNew ++ RelTypeNew ++ "]" ++ Dash_2 ++ "()", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPatternSyntax, {}, Dash_1, Variable, RelType, Dash_2, RightArrowHead} = ST) ->
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
    {RelTypeNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelType),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"()" ++ Dash_1 ++ "[" ++ VariableNew ++ RelTypeNew ++ "]" ++ Dash_2 ++ RightArrowHead ++ "()", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipPatternSyntax, LeftArrowHead, Dash_1, Variable, RelType, Dash_2, RightArrowHead} = ST) ->
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
    {RelTypeNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, RelType),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"()" ++ LeftArrowHead ++ Dash_1 ++ "[" ++ VariableNew ++ RelTypeNew ++ "]" ++ Dash_2 ++ RightArrowHead ++ "()", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipPropertyExistenceConstraint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {relationshipPropertyExistenceConstraint, RelationshipPropertySyntax, PropertyExpression} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipPropertySyntaxNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelationshipPropertySyntax),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {PropertyExpressionNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, PropertyExpression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"constraint on " ++ RelationshipPropertySyntaxNew ++ " assert exists (" ++ PropertyExpressionNew ++ ")", NewCtx4},
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
% remove
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {remove, Value} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {removeItemCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"remove " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% removeItem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {removeItem, Value_1, Value_2} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Value_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value_1New ++ " " ++ Value_2New, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% removeItemCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {removeItemCommalist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {removeItem, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ [SubAcc], CtxAcc1};
            {removeItem, _} ->
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
% return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {return, [], ReturnBody} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"return " ++ ReturnBodyNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {return, Distinct, ReturnBody} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"return " ++ Distinct ++ " " ++ ReturnBodyNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% returnBody
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, {}, {}, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ReturnItemsNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, {}, {}, Limit} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {LimitNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Limit),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ReturnItemsNew ++ " " ++ LimitNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, {}, Skip, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {SkipNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Skip),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ReturnItemsNew ++ " " ++ SkipNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, {}, Skip, Limit} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {SkipNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Skip),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {LimitNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Limit),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {ReturnItemsNew ++ " " ++ SkipNew ++ " " ++ LimitNew, NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, {}, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {OrderNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Order),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ReturnItemsNew ++ " " ++ OrderNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, {}, Limit} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {OrderNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Order),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {LimitNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Limit),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {ReturnItemsNew ++ " " ++ OrderNew ++ " " ++ LimitNew, NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, Skip, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {OrderNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Order),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {SkipNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Skip),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {ReturnItemsNew ++ " " ++ OrderNew ++ " " ++ SkipNew, NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, Skip, Limit} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {OrderNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Order),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {SkipNew, NewCtx5} = fold(FType, Fun, NewCtx4, Lvl + 1, Skip),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    {LimitNew, NewCtx7} = fold(FType, Fun, NewCtx6, Lvl + 1, Limit),
    NewCtx8 = case FType of
                  top_down -> NewCtx7;
                  bottom_up -> Fun(ST, NewCtx7)
              end,
    RT = {ReturnItemsNew ++ " " ++ OrderNew ++ " " ++ SkipNew ++ " " ++ LimitNew, NewCtx8},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% returnItem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {returnItem, Expression} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ExpressionNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnItem, Expression, Variable} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {VariableNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Variable),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ExpressionNew ++ " as " ++ VariableNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% returnItemCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {returnItemCommalist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {returnItem, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ [SubAcc], CtxAcc1};
            {returnItem, _} ->
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
% returnItems
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {returnItems, [], ReturnItemCommalist} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemCommalistNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {returnItemCommalist, ReturnItemCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ReturnItemCommalistNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {returnItems, Asterik, []} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Asterik, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnItems, Asterik, ReturnItemCommalist} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemCommalistNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {returnItemCommalist, ReturnItemCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {Asterik ++ "," ++ ReturnItemCommalistNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {set, Value} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {setItemCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"set " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setItem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {setItem, Value_1, Value_2} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Value_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value_1New ++ " " ++ Value_2New, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {setItem, Value_1, Operation, Value_2} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value_2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Value_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value_1New ++ Operation ++ Value_2New, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setItemCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {setItemCommalist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {setItem, _, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ [SubAcc], CtxAcc1};
            {setItem, _, _} ->
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
% shortestPathPattern
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {shortestPathPattern, Type, PatternElement} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternElementNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, PatternElement),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {Type ++ "(" ++ PatternElementNew ++ ")", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% singleQuery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {singleQuery, Value} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {clauseList, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% skip
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {skip, Value} = ST) ->
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
    RT = {"skip " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sortItem
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {sortItem, Value, Type} = ST) ->
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
    RT = {ValueNew ++ " " ++ Type, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sortItemCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {sortItemCommalist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {sortItem, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ [SubAcc], CtxAcc1};
            {sortItem, _} ->
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
% start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {start, StartPointCommalist, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {StartPointCommalistNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {startPointCommalist, StartPointCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"start " ++ StartPointCommalistNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {start, StartPointCommalist, Where} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {StartPointCommalistNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, {startPointCommalist, StartPointCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {WhereNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Where),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"start " ++ StartPointCommalistNew ++ " " ++ WhereNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% startPoint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {startPoint, Variable, Lookup} = ST) ->
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
    {LookupNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Lookup),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {VariableNew ++ " = " ++ LookupNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% startPointCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {startPointCommalist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {startPoint, _, _} ->
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
% union
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {union, [], SingleQuery} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SingleQueryNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, SingleQuery),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"union " ++ SingleQueryNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {union, All, SingleQuery} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SingleQueryNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, SingleQuery),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"union " ++ All ++ " " ++ SingleQueryNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unionList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {unionList, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {union, _, _} ->
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
% unwind
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {unwind, Expression, Variable} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {VariableNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Variable),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"unwind " ++ ExpressionNew ++ " as " ++ VariableNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% variableCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {variableCommalist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {variable, _} ->
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
% with
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {with, [], ReturnBody, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"with " ++ ReturnBodyNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {with, [], ReturnBody, Where} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {WhereNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Where),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"with " ++ ReturnBodyNew ++ " " ++ WhereNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {with, Distinct, ReturnBody, {}} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"with " ++ Distinct ++ " " ++ ReturnBodyNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {with, Distinct, ReturnBody, Where} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {WhereNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Where),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"with " ++ Distinct ++ " " ++ ReturnBodyNew ++ " " ++ WhereNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Not yet supported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, PTree) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n PTree: ~p~n", [_Lvl, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).
