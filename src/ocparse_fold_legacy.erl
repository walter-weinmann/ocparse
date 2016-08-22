-module(ocparse_fold_legacy).

-export([fold/5]).

-define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% and, contains, ends with, in, or, starts with, xor, +, -, *, /, %, ^, [, ..
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {"[" = Type, {expression, _} = Value} = ST) ->
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
    RT = {Type ++ ValueNew ++ "]", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {"[", [], {expression, _} = Value} = ST) ->
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
    RT = {"[.." ++ ValueNew ++ "]", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {"[", {expression, _} = Value, []} = ST) ->
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
    RT = {"[" ++ ValueNew ++ "..]", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {"[", {expression, _} = Value1, {expression, _} = Value2} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value2New, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, Value2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ Value1New ++ ".." ++ Value2New ++ "]", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Type, {Expression, _, _} = Value} = ST)
    when Expression == expression2 andalso (
    Type == "=~" orelse
        Type == "in" orelse
        Type == "starts with" orelse
        Type == "ends with" orelse
        Type == "contains" orelse
        Type == "ends with") orelse
    Expression == expression4 andalso Type == "^" orelse
    Expression == expression5 andalso (
        Type == "*" orelse
            Type == "/" orelse
            Type == "%") orelse
    Expression == expression6 andalso (
        Type == "+" orelse
            Type == "-") orelse
    Expression == expression9 andalso Type == "and" orelse
    Expression == expression10 andalso Type == "xor" orelse
    Expression == expression11 andalso Type == "or" ->
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
% anonymousPatternPart / expression / functionName / labelName / lookup / patternPart /
% propertyKeyName / query / relTypeName / removeItem / sortItem / statement / variable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == anonymousPatternPart;
    Type == expression;
    Type == functionName;
    Type == labelName;
    Type == lookup;
    Type == patternPart;
    Type == propertyKeyName;
    Type == query;
    Type == relTypeName;
    Type == removeItem;
    Type == sortItem;
    Type == statement;
    Type == variable ->
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
    when Type == caseExpression;
    Type == functionInvocation;
    Type == legacyParameter;
    Type == listComprehension;
    Type == mapLiteral;
    Type == numberLiteral;
    Type == parameter;
    Type == parenthesizedExpression;
    Type == stringLiteral;
    Type == terminal;
    Type == variable ->
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
    when Type == all;
    Type == any;
    Type == filter;
    Type == none;
    Type == single ->
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
fold(FType, Fun, Ctx, Lvl, {atom, {extract = Type, FilterExpression, []}} = ST) ->
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
    RT = {atom_to_list(Type) ++ "(" ++ FilterExpressionNew ++ ")", NewCtx2},
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
    when Type == caseExpression;
    Type == functionInvocation;
    Type == listComprehension;
    Type == relationshipsPattern;
    Type == shortestPathPattern ->
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
    when Type == caseExpression;
    Type == functionInvocation ->
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
% bulkImportQuery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {bulkImportQuery, Value_1, Value_2} = ST) ->
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
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ "when " ++ SubAcc1 ++ " then " ++ SubAcc2, CtxAcc3}
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

fold(FType, Fun, Ctx, Lvl, {caseExpression, [], CaseAlternatives, []} = ST)
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
fold(FType, Fun, Ctx, Lvl, {caseExpression, [], CaseAlternatives, Expression_2} = ST)
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
    RT = {"case " ++ CaseAlternativesNew ++ " else " ++ Expression_2New ++ " end", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {caseExpression, Expression_1, CaseAlternatives, []} = ST)
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
    RT = {"case " ++ Expression_1New ++ " " ++ CaseAlternativesNew ++ " else " ++ Expression_2New ++ " end", NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% clause
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
fold(FType, Fun, Ctx, Lvl, {Type, {SubType, _} = Value} = ST)
    when Type == clause andalso (
    SubType == create orelse
        SubType == createUnique orelse
        SubType == remove orelse
        SubType == set) orelse
    Type == command andalso (
        SubType == create orelse
            SubType == drop) orelse
    Type == numberLiteral andalso (
        SubType == doubleLiteral orelse
            SubType == integerLiteral) ->
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
    when Type == delete;
    Type == merge;
    Type == return;
    Type == start;
    Type == unwind ->
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
    when Type == forEach;
    Type == with ->
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
    when Type == loadCSV;
    Type == match ->
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
% clauseList / hintList / mergeActionList / nodeLabels / unionList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when Type == clauseList;
    Type == hintList;
    Type == mergeActionList;
    Type == nodeLabels;
    Type == unionList,
    is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {ListType, _}
                when Type == clauseList, ListType == clause;
                Type == hintList, ListType == hint;
                Type == nodeLabels, ListType == nodeLabel ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc, CtxAcc1};
            {ListType, _, _}
                when Type == hintList, ListType == hint;
                Type == mergeActionList, ListType == mergeAction;
                Type == unionList, ListType == union ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc, CtxAcc1};
            {ListType, _, _, _}
                when Type == hintList, ListType == hint ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc, CtxAcc1}
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
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc1 ++ "=" ++ SubAcc2, CtxAcc3}
        end
                                      end,
        {[], NewCtx},
        Values),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:flatten(ValueNew), NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create / drop / limit / skip
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, {SubType, _} = Value} = ST)
    when (Type == create orelse
    Type == drop) andalso SubType == index orelse
    Type == limit orelse
    Type == skip ->
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
    RT = {atom_to_list(Type) ++ " " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Type, {SubType, _, _} = Value} = ST)
    when (Type == create orelse
    Type == drop) andalso SubType == relationshipPropertyExistenceConstraint ->
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
    RT = {atom_to_list(Type) ++ " " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Type, {SubType, _, _, _} = Value} = ST)
    when (Type == create orelse
    Type == drop) andalso
    (SubType == nodePropertyExistenceConstraint orelse
        SubType == uniqueConstraint) ->
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
    RT = {atom_to_list(Type) ++ " " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {create, {pattern, _} = Value} = ST) ->
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% createUnique
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {createUnique, {pattern, _} = Value} = ST) ->
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
    RT = {"create unique " ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cypher
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {cypher, [], {statement, _} = Statement, Semicolon} = ST) ->
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
    RT = {StatementNew ++ Semicolon, NewCtx2},
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
    RT = {QueryOptionsNew ++ " " ++ StatementNew ++ Semicolon, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doubleLiteral / idLookup / integerLiteral / mapLiteral/ relationshipPattern / stringLiteral /
% symbolicName / terminal / % * (range)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {Type, Value} = ST)
    when Type == doubleLiteral orelse
    Type == idLookup andalso Value == "(*)" orelse
    Type == integerLiteral orelse
    Type == mapLiteral andalso Value == "{}" orelse
    Type == relationshipPattern andalso (
        Value == "<-->" orelse
            Value == "<--" orelse
            Value == "-->" orelse
            Value == "--") orelse
    Type == stringLiteral orelse
    Type == symbolicName orelse
    Type == terminal orelse
    Type == "*" andalso Value == "*" ->
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {delete, Detach, ExpressionCommalist} = ST) ->
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
    RT = {case length(Detach) of
              0 -> [];
              _ -> "detach "
          end ++ "delete " ++ ExpressionCommalistNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expressionCommalist / patternPartCommalist / propertyKeyNameExpressionCommalist /
% removeItemCommalist / returnItemCommalist / setItemCommalist / sortItemCommalist /
% startPointCommalist / variableCommalist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when Type == expressionCommalist;
    Type == patternPartCommalist;
    Type == propertyKeyNameExpressionCommalist;
    Type == removeItemCommalist;
    Type == returnItemCommalist;
    Type == setItemCommalist;
    Type == sortItemCommalist;
    Type == startPointCommalist;
    Type == variableCommalist, is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {ListType, _}
                when Type == expressionCommalist, ListType == expression;
                Type == patternPartCommalist, ListType == patternPart;
                Type == removeItemCommalist, ListType == removeItem;
                Type == returnItemCommalist, ListType == returnItem;
                Type == sortItemCommalist, ListType == sortItem;
                Type == variableCommalist, ListType == variable ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ SubAcc, CtxAcc1};
            {ListType, _, _}
                when Type == patternPartCommalist, ListType == patternPart;
                Type == removeItemCommalist, ListType == removeItem;
                Type == returnItemCommalist, ListType == returnItem;
                Type == setItemCommalist, ListType == setItem;
                Type == sortItemCommalist, ListType == sortItem;
                Type == startPointCommalist, ListType == startPoint ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ SubAcc, CtxAcc1};
            {ListType, _, _, _}
                when Type == setItemCommalist, ListType == setItem ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ SubAcc, CtxAcc1};
            {{propertyKeyName, _}, {expression, _}}
                when Type == propertyKeyNameExpressionCommalist ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ SubAcc, CtxAcc1}
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
% expression2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {expression2, Atom, []} = ST) ->
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
                {Acc ++ SubAcc, CtxAcc1};
            {propertyLookup, _, _} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ SubAcc, CtxAcc1}
        end
                                       end,
        {[], NewCtx2},
        Addons),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {AtomNew ++ " " ++ AddonsNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression3 .. expression12
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Value, []} = ST)
    when Type == expression3;
    Type == expression4;
    Type == expression5;
    Type == expression6;
    Type == expression7;
    Type == expression8;
    Type == expression9;
    Type == expression10;
    Type == expression11;
    Type == expression12 ->
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
fold(FType, Fun, Ctx, Lvl, {Type, Expression, ExpressionAddonList} = ST)
    when Type == expression4;
    Type == expression9 ->
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
    {ExpressionAddonListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {list_to_atom(atom_to_list(Type) ++ "AddonList"), ExpressionAddonList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ExpressionAddonListNew ++ " " ++ ExpressionNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {Type, Expression, ExpressionAddonList} = ST)
    when Type == expression3;
    Type == expression5;
    Type == expression6;
    Type == expression7;
    Type == expression8;
    Type == expression10;
    Type == expression11;
    Type == expression12 ->
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
    {ExpressionAddonListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {list_to_atom(atom_to_list(Type) ++ "AddonList"), ExpressionAddonList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ExpressionNew ++ " " ++ ExpressionAddonListNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expression3AddonList .. expression12AddonList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when (Type == expression3AddonList orelse
    Type == expression4AddonList orelse
    Type == expression5AddonList orelse
    Type == expression6AddonList orelse
    Type == expression7AddonList orelse
    Type == expression8AddonList orelse
    Type == expression9AddonList orelse
    Type == expression10AddonList orelse
    Type == expression11AddonList orelse
    Type == expression12AddonList)
    andalso is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValuesNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {Operator, _, _}
                when Type == expression3AddonList andalso Operator == "[" orelse
                Type == expression8AddonList andalso Operator == partialComparisonExpression ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc, CtxAcc1};
            {Operator, _}
                when Type == expression3AddonList andalso (
                Operator == "[" orelse
                    Operator == "=~" orelse
                    Operator == "in" orelse
                    Operator == "starts with" orelse
                    Operator == "ends with" orelse
                    Operator == "contains") orelse
                Type == expression5AddonList andalso Operator == "^" orelse
                Type == expression6AddonList andalso (
                    Operator == "*" orelse
                        Operator == "/" orelse
                        Operator == "%") orelse
                Type == expression7AddonList andalso (
                    Operator == "+" orelse
                        Operator == "-") orelse
                Type == expression10AddonList andalso Operator == "and" orelse
                Type == expression11AddonList andalso Operator == "xor" orelse
                Type == expression12AddonList andalso Operator == "or" ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc, CtxAcc1};
            {Operator}
                when Type == expression3AddonList andalso Operator == "is null" orelse
                Type == expression3AddonList andalso Operator == "is not null" orelse
                Type == expression9AddonList andalso Operator == "not" ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ Operator, CtxAcc};
            {Operator}
                when Type == expression4AddonList andalso Operator == "+" orelse
                Type == expression4AddonList andalso Operator == "-" ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {Acc ++ Operator, CtxAcc}
        end
                                       end,
        {[], NewCtx},
        Values),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValuesNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filterExpression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {filterExpression, IdInColl, []} = ST) ->
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

fold(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, Distinct, []} = ST) ->
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
    RT = {FunctionNameNew ++ "(" ++ Distinct ++ ")", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, Distinct, ExpressionCommalist} = ST)
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
    RT = {FunctionNameNew ++ "(" ++ case length(Distinct) of
                                        0 -> [];
                                        _ -> Distinct ++ " "
                                    end ++ ExpressionCommalistNew ++ ")", NewCtx4},
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
    RT = {":" ++ SymbolicName_1New ++ "(" ++ SymbolicName_2New ++ "=" ++ Value ++ ")", NewCtx4},
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
    RT = {":" ++ SymbolicName_1New ++ "(" ++ SymbolicName_2New ++ "=" ++ ValueNew ++ ")", NewCtx6},
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
% legacyParameter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {legacyParameter, {symbolicName, _} = Value} = ST) ->
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
fold(FType, Fun, Ctx, _Lvl, {legacyParameter, Value} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"{" ++ Value ++ "}", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% listComprehension
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {listComprehension, FilterExpression, []} = ST) ->
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

fold(FType, Fun, Ctx, _Lvl, {literalIds} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {",", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% loadCSV
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {loadCSV, [], Expression, Variable, []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {loadCSV, _WithHeaders, Expression, Variable, []} = ST) ->
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
% loadCSVQuery
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {loadCSVQuery, LoadCSV, []} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {LoadCSVNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, LoadCSV),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {LoadCSVNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {loadCSVQuery, LoadCSV, ClauseList} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {LoadCSVNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, LoadCSV),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {ClauseListNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {clauseList, ClauseList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {LoadCSVNew ++ " " ++ ClauseListNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mapLiteral
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

fold(FType, Fun, Ctx, Lvl, {match, [], Pattern, [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {match, [], Pattern, HintList, []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {match, _Optional, Pattern, [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {match, _Optional, Pattern, HintList, []} = ST) ->
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

fold(FType, Fun, Ctx, Lvl, {merge, PatternPart, []} = ST) ->
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
    RT = {"merge " ++ PatternPartNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
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
% nodeLabel, properties, relType, where
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == nodeLabel;
    Type == properties;
    Type == relType;
    Type == where ->
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
              where -> "where "
          end ++ ValueNew, NewCtx2},
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

fold(FType, Fun, Ctx, _Lvl, {nodePattern, [], [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {nodePattern, [], [], Properties} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {nodePattern, [], NodeLabels, []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {nodePattern, [], NodeLabels, Properties} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {nodePattern, Variable, [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {nodePattern, Variable, NodeLabels, []} = ST) ->
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
    when Type == nodePropertyExistenceConstraint;
    Type == uniqueConstraint ->
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
    RT = {"constraint on (" ++ VariableNew ++ NodeLabelNew ++ case Type of
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

fold(FType, Fun, Ctx, Lvl, {parameter, {symbolicName, _} = Value} = ST) ->
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
    RT = {"$" ++ ValueNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, _Lvl, {parameter, Value} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"$" ++ Value, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parenthesizedExpression - currently not supported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%fold(FType, Fun, Ctx, Lvl, {parenthesizedExpression, Value} = ST) ->
%%    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
%%    NewCtx = case FType of
%%                 top_down -> Fun(ST, Ctx);
%%                 bottom_up -> Ctx
%%             end,
%%    {ValueNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value),
%%    NewCtx2 = case FType of
%%                  top_down -> NewCtx1;
%%                  bottom_up -> Fun(ST, NewCtx1)
%%              end,
%%    RT = {"(" ++ ValueNew ++ ")", NewCtx2},
%%    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
%%    RT;

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
    RT = {Terminal ++ " " ++ ValueNew, NewCtx2},
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

fold(FType, Fun, Ctx, Lvl, {patternElement, PatternElement, "("} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {patternElement, {nodePattern, _, _, _} = NodePattern, []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {patternElement, {nodePattern, _, _, _} = NodePattern, PatternElementChainList} = ST) ->
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
    RT = {NodePatternNew ++ " " ++ PatternElementChainListNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternElementChain
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {patternElementChain, Value_1, Value_2} = ST) ->
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
    RT = {Value_1New ++ Value_2New, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternElementChainList / propertyLookupList
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when Type == patternElementChainList;
    Type == propertyLookupList, is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {ListType, _, _}
                when Type == patternElementChainList, ListType == patternElementChain;
                Type == propertyLookupList, ListType == propertyLookup ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ SubAcc, CtxAcc1}
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

fold(FType, Fun, Ctx, Lvl, {patternPart, [], Value} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {patternPart, Value_1, Value_2} = ST) ->
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
    RT = {Value_1New ++ "=" ++ Value_2New, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% periodicCommitHint
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {periodicCommitHint} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"using periodic commit", NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyExpression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
% propertyKeyNameExpression (Helper definition)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {{propertyKeyName, _} = PropertyKeyName, {expression, _} = Expression} = ST) ->
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
% propertyLookup
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
            {anyCypherOption, Option, []} when Option == explain; Option == profile ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ atom_to_list(Option), Fun(F, CtxAcc)};
            {anyCypherOption, {cypherOption, [], []}} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ "cypher", Fun(F, CtxAcc)};
            {anyCypherOption, {cypherOption, [], ConfigurationOptions}}
                when is_list(ConfigurationOptions) ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, {configurationOptions, ConfigurationOptions}),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ "cypher " ++ SubAcc, CtxAcc1};
            {anyCypherOption, {cypherOption, {versionNumber, _} = VersionNumber, []}}
                when is_tuple(VersionNumber) ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, VersionNumber),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ "cypher " ++ SubAcc, CtxAcc1};
            {anyCypherOption, {cypherOption, {versionNumber, _} = VersionNumber, ConfigurationOptions}}
                when is_tuple(VersionNumber), is_list(ConfigurationOptions) ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc1, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, VersionNumber),
                CtxAcc2 = case FType of
                              top_down -> Fun(ST, CtxAcc1);
                              bottom_up -> CtxAcc1
                          end,
                {SubAcc2, CtxAcc3} = fold(FType, Fun, CtxAcc2, Lvl + 1, {configurationOptions, ConfigurationOptions}),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ "cypher " ++ SubAcc1 ++ " " ++ SubAcc2, CtxAcc3}
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
% rangeLiteral
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {rangeLiteral, [], ".." = Op, Value_2} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_2New, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, Value_2),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {Op ++ Value_2New, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {rangeLiteral, Value_1, [], []} = ST) ->
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
    RT = {Value_1New, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {rangeLiteral, Value_1, ".." = Op, []} = ST) ->
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
    RT = {Value_1New ++ Op, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {rangeLiteral, Value_1, ".." = Op, Value_2} = ST) ->
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
    RT = {Value_1New ++ Op ++ Value_2New, NewCtx4},
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

fold(FType, Fun, Ctx, _Lvl, {relationshipDetail, [], CharQuestionMark, [], [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, [], CharQuestionMark, [], [], Properties} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, [], CharQuestionMark, [], RangeLiteral, []} = ST) ->
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
    RT = {"[" ++ CharQuestionMark ++ "*" ++ RangeLiteralNew ++ "]", NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, [], CharQuestionMark, [], RangeLiteral, Properties} = ST) ->
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
    RT = {"[" ++ CharQuestionMark ++ "*" ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, [], CharQuestionMark, RelationshipTypes, [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, [], CharQuestionMark, RelationshipTypes, [], Properties} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, [], CharQuestionMark, RelationshipTypes, RangeLiteral, []} = ST) ->
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
    RT = {"[" ++ CharQuestionMark ++ RelationshipTypesNew ++ "*" ++ RangeLiteralNew ++ "]", NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, [], CharQuestionMark, RelationshipTypes, RangeLiteral, Properties} = ST) ->
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
    RT = {"[" ++ CharQuestionMark ++ RelationshipTypesNew ++ "*" ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, [], [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, [], [], Properties} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, [], RangeLiteral, []} = ST) ->
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
    RT = {"[" ++ VariableNew ++ CharQuestionMark ++ "*" ++ RangeLiteralNew ++ "]", NewCtx4},
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
    RT = {"[" ++ VariableNew ++ CharQuestionMark ++ "*" ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx6},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, RelationshipTypes, [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, RelationshipTypes, [], Properties} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, CharQuestionMark, RelationshipTypes, RangeLiteral, []} = ST) ->
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
    RT = {"[" ++ VariableNew ++ CharQuestionMark ++ RelationshipTypesNew ++ "*" ++ RangeLiteralNew ++ "]", NewCtx6},
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
    RT = {"[" ++ VariableNew ++ CharQuestionMark ++ RelationshipTypesNew ++ "*" ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx8},
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

fold(FType, Fun, Ctx, Lvl, {relationshipPattern, Left, RelationshipDetail, Right} = ST) ->
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
    RT = {Left ++ RelationshipDetailNew ++ Right, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipPatternSyntax
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

fold(FType, Fun, Ctx, Lvl, {relationshipTypes, RelType, RelTypeVerticalbarlist} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelTypeNew, NewCtx1} = fold(FType, Fun, NewCtx, Lvl + 1, RelType),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {RelTypeVerticalbarlistNew, NewCtx3} = fold(FType, Fun, NewCtx2, Lvl + 1, {relTypeVerticalbarlist, RelTypeVerticalbarlist}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {":" ++ RelTypeNew ++ RelTypeVerticalbarlistNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relTypeVerticalbarlist
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {relTypeVerticalbarlist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {{relTypeName, _} = RTN, Colon} ->
                ?debugFmt("wwe debugging fold/5 ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = fold(FType, Fun, CtxAcc, Lvl + 1, RTN),
                {Acc ++ "|" ++ Colon ++ SubAcc, CtxAcc1}
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
    RT = {Value_1New ++ Value_2New, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    RT = {"return " ++ case Distinct of
                           [] -> [];
                           _ -> "distinct "
                       end ++ ReturnBodyNew, NewCtx2},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% returnBody
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], [], Limit} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], Skip, []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], Skip, Limit} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, [], []} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, [], Limit} = ST) ->
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
fold(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, Skip, []} = ST) ->
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

fold(FType, Fun, Ctx, Lvl, {returnItem, Expression, []} = ST) ->
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
% returnItems
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {returnItems, Asterik, Comma, []} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Asterik ++ Comma, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;
fold(FType, Fun, Ctx, Lvl, {returnItems, Asterik, Comma, ReturnItemCommalist} = ST) ->
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
    RT = {Asterik ++ Comma ++ ReturnItemCommalistNew, NewCtx2},
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
    RT = {Value_1New ++ case Operation of
                            [] -> [];
                            _ -> " " ++ Operation ++ " "
                        end ++ Value_2New, NewCtx4},
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
% start
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {start, StartPointCommalist, []} = ST) ->
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
    RT = {VariableNew ++ "=" ++ LookupNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% union
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
    RT = {"union " ++ case length(All) of
                          0 -> [];
                          _ -> All ++ " "
                      end ++ SingleQueryNew, NewCtx2},
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
% versionNumber
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, _Lvl, {versionNumber, Value_1} = ST) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Value_1, NewCtx1},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% with
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(FType, Fun, Ctx, Lvl, {with, Distinct, ReturnBody, []} = ST) ->
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
    RT = {"with " ++ case Distinct of
                         [] -> [];
                         _ -> "distinct "
                     end ++ ReturnBodyNew, NewCtx2},
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
    RT = {"with " ++ case Distinct of
                         [] -> [];
                         _ -> "distinct "
                     end ++ ReturnBodyNew ++ " " ++ WhereNew, NewCtx4},
    ?debugFmt("wwe debugging fold/5 ===> ~n RT: ~p~n", [RT]),
    RT;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Not yet supported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

fold(_FType, Fun, Ctx, _Lvl, PTree) ->
    ?debugFmt("wwe debugging fold/5 ===> Start ~p~n PTree: ~p~n", [_Lvl, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).
