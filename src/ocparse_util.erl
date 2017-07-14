%% -----------------------------------------------------------------------------
%%
%% ocparse_util.erl: opencypher - parser utilities.
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

-module(ocparse_util).

-export([pt_to_source/5]).

% -define(NODEBUG, true).
-include_lib("eunit/include/eunit.hrl").

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% and, contains, ends with, in, or, starts with, xor, +, -, *, /, %, ^, [, ..
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {"[", [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"[..]", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {"[" = Type, {expression, _} = Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([Type, ValueNew, "]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {"[", [], {expression, _} = Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["[..", ValueNew, "]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {"[", {expression, _} = Value, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["[", ValueNew, "..]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {"[", {expression, _} = Value1, {expression, _} = Value2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value1),
    {Value2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", Value1New, "..", Value2New, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {Type, {Expression, _, _} = Value} = ST)
    when Expression == andExpression andalso Type == "xor" orelse
    Expression == multiplyDivideModuloExpression andalso (
        Type == "+" orelse
            Type == "-") orelse
    Expression == notExpression andalso Type == "and" orelse
    Expression == powerOfExpression andalso (
        Type == "*" orelse
            Type == "/" orelse
            Type == "%") orelse
    Expression == propertyOrLabelsExpression andalso (
        Type == "=~" orelse
            Type == "in" orelse
            Type == "starts with" orelse
            Type == "ends with" orelse
            Type == "contains" orelse
            Type == "ends with") orelse
    Expression == unaryAddOrSubtractExpression andalso Type == "^" orelse
    Expression == xorExpression andalso Type == "or" ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([Type, " ", ValueNew]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addOrSubtractExpressionAddonList / andExpressionAddonList /
% caseAlternativesList / comparisonExpressionAddonList / expressionCommalist /
% mergeActionList / multiplyDivideModuloExpressionAddonList / nodeLabels /
% notExpressionAddonList / orExpressionAddonList / patternPartCommalist /
% patternElementChainList / powerOfExpressionAddonList /
% propertyKeyNameExpressionCommalist / propertyLookupList / readingClauseList /
% readPartUpdatingPartWithList / relTypeVerticalbarlist / removeItemCommalist /
% returnItemCommalist / setItemCommalist / sortItemCommalist /
% stringListNullOperatorExpressionAddonList /
% unaryAddOrSubtractExpressionAddonList / unionList / updatingClauseList /
% variableCommalist / xorExpressionAddonList / yieldItemCommalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {Type, []} = ST)
    when (Type == addOrSubtractExpressionAddonList orelse
    Type == andExpressionAddonList orelse
    Type == caseAlternativesList orelse
    Type == comparisonExpressionAddonList orelse
    Type == expressionCommalist orelse
    Type == mergeActionList orelse
    Type == multiplyDivideModuloExpressionAddonList orelse
    Type == nodeLabels orelse
    Type == notExpressionAddonList orelse
    Type == orExpressionAddonList orelse
    Type == patternElementChainList orelse
    Type == patternPartCommalist orelse
    Type == powerOfExpressionAddonList orelse
    Type == propertyKeyNameExpressionCommalist orelse
    Type == propertyLookupList orelse
    Type == readingClauseList orelse
    Type == readPartUpdatingPartWithList orelse
    Type == relTypeVerticalbarlist orelse
    Type == removeItemCommalist orelse
    Type == returnItemCommalist orelse
    Type == setItemCommalist orelse
    Type == sortItemCommalist orelse
    Type == stringListNullOperatorExpressionAddonList orelse
    Type == unaryAddOrSubtractExpressionAddonList orelse
    Type == unionList orelse
    Type == updatingClauseList orelse
    Type == variableCommalist orelse
    Type == xorExpressionAddonList orelse
    Type == yieldItemCommalist) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {[], NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when (Type == addOrSubtractExpressionAddonList orelse
    Type == andExpressionAddonList orelse
    Type == caseAlternativesList orelse
    Type == comparisonExpressionAddonList orelse
    Type == expressionCommalist orelse
    Type == mergeActionList orelse
    Type == multiplyDivideModuloExpressionAddonList orelse
    Type == nodeLabels orelse
    Type == notExpressionAddonList orelse
    Type == orExpressionAddonList orelse
    Type == patternElementChainList orelse
    Type == patternPartCommalist orelse
    Type == powerOfExpressionAddonList orelse
    Type == propertyKeyNameExpressionCommalist orelse
    Type == propertyLookupList orelse
    Type == readingClauseList orelse
    Type == readPartUpdatingPartWithList orelse
    Type == relTypeVerticalbarlist orelse
    Type == removeItemCommalist orelse
    Type == returnItemCommalist orelse
    Type == setItemCommalist orelse
    Type == sortItemCommalist orelse
    Type == stringListNullOperatorExpressionAddonList orelse
    Type == unaryAddOrSubtractExpressionAddonList orelse
    Type == unionList orelse
    Type == updatingClauseList orelse
    Type == variableCommalist orelse
    Type == xorExpressionAddonList orelse
    Type == yieldItemCommalist)
    andalso is_list(Values) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValuesNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {Operator}
                when Type == notExpressionAddonList andalso Operator == "not" orelse
                Type == stringListNullOperatorExpressionAddonList andalso (
                    Operator == "is null" orelse Operator == "is not null") ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end,
                        Operator
                    ]), CtxAcc};
            {Operator}
                when Type == unaryAddOrSubtractExpressionAddonList andalso (
                Operator == "+" orelse Operator == "-") ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {lists:append([Acc, Operator, " "]), CtxAcc};
            {ListType, _}
                when Type == expressionCommalist, ListType == expression;
                Type == patternPartCommalist, ListType == patternPart;
                Type == removeItemCommalist, ListType == removeItem;
                Type == returnItemCommalist, ListType == returnItem;
                Type == sortItemCommalist, ListType == sortItem;
                Type == variableCommalist, ListType == variable;
                Type == yieldItemCommalist, ListType == yieldItem ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end,
                        SubAcc
                    ]), CtxAcc1};
            {ListType, _}
                when Type == nodeLabels, ListType == nodeLabel;
                Type == readingClauseList, ListType == readingClause;
                Type == updatingClauseList, ListType == updatingClause;
                Type == updatingClauseList, ListType == updatingStartClause ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end,
                        SubAcc
                    ]), CtxAcc1};
            {Operator, _}
                when Type == addOrSubtractExpressionAddonList andalso (
                Operator == "+" orelse Operator == "-") orelse
                Type == andExpressionAddonList andalso Operator == "and" orelse
                Type == multiplyDivideModuloExpressionAddonList andalso (
                    Operator == "*" orelse Operator == "/" orelse Operator == "%") orelse
                Type == orExpressionAddonList andalso Operator == "or" orelse
                Type == powerOfExpressionAddonList andalso Operator == "^" orelse
                Type == stringListNullOperatorExpressionAddonList andalso (
                    Operator == "[" orelse Operator == "=~" orelse Operator == "in" orelse
                        Operator == "starts with" orelse Operator == "ends with" orelse
                        Operator == "contains") orelse
                Type == xorExpressionAddonList andalso Operator == "xor" ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end,
                        SubAcc
                    ]), CtxAcc1};
            {{propertyKeyName, _}, {expression, _}}
                when Type == propertyKeyNameExpressionCommalist ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end,
                        SubAcc
                    ]), CtxAcc1};
            {propertyLookup, _}
                when Type == propertyLookupList ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ SubAcc, CtxAcc1};
            {{relTypeName, _} = RTN, Colon} ->
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, RTN),
                {lists:append([Acc, "|", Colon, SubAcc]), CtxAcc1};
            {ListType, _, _}
                when Type == caseAlternativesList, ListType == caseAlternatives;
                Type == mergeActionList, ListType == mergeAction;
                Type == unionList, ListType == union ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end,
                        SubAcc
                    ]), CtxAcc1};
            {ListType, _, _}
                when Type == patternPartCommalist, ListType == patternPart;
                Type == removeItemCommalist, ListType == removeItem;
                Type == returnItemCommalist, ListType == returnItem;
                Type == setItemCommalist, ListType == setItem;
                Type == sortItemCommalist, ListType == sortItem;
                Type == yieldItemCommalist, ListType == yieldItem ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end,
                        SubAcc
                    ]), CtxAcc1};
            {Operator, _, _}
                when Type == comparisonExpressionAddonList andalso Operator == partialComparisonExpression orelse
                Type == stringListNullOperatorExpressionAddonList andalso Operator == "[" ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end,
                        SubAcc
                    ]), CtxAcc1};
            {patternElementChain, _, _}
                when Type == patternElementChainList ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ SubAcc, CtxAcc1};
            {{readPart, _}, {updatingPart, _}, {with, _, _, _}} ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end,
                        SubAcc
                    ]), CtxAcc1};
            {ListType, _, _, _}
                when Type == setItemCommalist, ListType == setItem ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {lists:append(
                    [
                        Acc,
                        case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end,
                        SubAcc
                    ]), CtxAcc1}
        end
                                       end,
        {[], NewCtx},
        Values),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValuesNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addOrSubtractExpression / andExpression / anonymousPatternPart /
% comparisonExpression / expression / functionName /
% implicitProcedureInvocation / labelName / multiplyDivideModuloExpression /
% notExpression / orExpression / patternPart / powerOfExpression /
% procedureName / propertyKeyName / query / readingClause / relTypeName /
% removeItem / schemaName / singlePartQuery / singleQuery / sortItem /
% statement / stringListNullOperatorExpression / unaryAddOrSubtractExpression /
% updatingClause / updatingStartClause / variable / xorExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {functionName, "exists" = Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Value, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == addOrSubtractExpression;
    Type == andExpression;
    Type == anonymousPatternPart;
    Type == comparisonExpression;
    Type == expression;
    Type == functionName;
    Type == implicitProcedureInvocation;
    Type == labelName;
    Type == multiplyDivideModuloExpression;
    Type == notExpression;
    Type == orExpression;
    Type == patternPart;
    Type == powerOfExpression;
    Type == procedureName;
    Type == procedureResultField;
    Type == propertyKeyName;
    Type == query;
    Type == readingClause;
    Type == relTypeName;
    Type == removeItem;
    Type == schemaName;
    Type == singlePartQuery;
    Type == singleQuery;
    Type == sortItem;
    Type == statement;
    Type == stringListNullOperatorExpression;
    Type == unaryAddOrSubtractExpression;
    Type == updatingClause;
    Type == updatingStartClause;
    Type == variable;
    Type == xorExpression ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addOrSubtractExpression / andExpression / comparisonExpression /
% multiplyDivideModuloExpression / orExpression / powerOfExpression /
% stringListNullOperatorExpression / xorExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Expression, []} = ST)
    when Type == addOrSubtractExpression;
    Type == andExpression;
    Type == comparisonExpression;
    Type == multiplyDivideModuloExpression;
    Type == orExpression;
    Type == powerOfExpression;
    Type == stringListNullOperatorExpression;
    Type == xorExpression ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ExpressionNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {Type, Expression, ExpressionAddonList} = ST)
    when Type == addOrSubtractExpression;
    Type == andExpression;
    Type == comparisonExpression;
    Type == multiplyDivideModuloExpression;
    Type == orExpression;
    Type == powerOfExpression;
    Type == stringListNullOperatorExpression;
    Type == xorExpression ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Expression),
    {ExpressionAddonListNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {list_to_atom(atom_to_list(Type) ++ "AddonList"), ExpressionAddonList}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([
        ExpressionNew,
        case lists:suffix(" ", ExpressionNew) of
            true ->
                [];
            false ->
                " "
        end,
        ExpressionAddonListNew
    ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% atom
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {atom, {Type, _} = Value} = ST)
    when Type == functionInvocation;
    Type == listComprehension;
    Type == literal;
    Type == parameter;
    Type == parenthesizedExpression;
    Type == patternComprehension;
    Type == terminal;
    Type == variable ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, _Lvl, {atom, {count, []}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    RT = {"count(*)", NewCtx},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {atom, {Type, Value}} = ST)
    when Type == all;
    Type == any;
    Type == filter;
    Type == none;
    Type == single ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([atom_to_list(Type), "(", ValueNew, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {atom, {extract = Type, FilterExpression, []}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FilterExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, FilterExpression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([atom_to_list(Type), "(", FilterExpressionNew, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {atom, {extract = Type, FilterExpression, Expression}} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FilterExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, FilterExpression),
    {ExpressionNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Expression),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([atom_to_list(Type), "(", FilterExpressionNew, "|", ExpressionNew, ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {atom, {Type, _, _} = Value} = ST)
    when Type == functionInvocation;
    Type == listComprehension;
    Type == relationshipsPattern ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {atom, {Type, _, _, _} = Value} = ST)
    when Type == caseExpression;
    Type == functionInvocation ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {atom, {Type, _, _, _, _} = Value} = ST)
    when Type == patternComprehension ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% booleanLiteral
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {booleanLiteral, {terminal, _} = Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caseAlternatives
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {caseAlternatives, Value1, Value2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value1),
    {Value2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["when ", Value1New, " then ", Value2New]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% caseExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {caseExpression, [], Value2, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value2New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {caseAlternativesList, Value2}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["case ", Value2New, " end"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {caseExpression, [], Value2, Value3} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value2New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {caseAlternativesList, Value2}),
    {Value3New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value3),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["case ", Value2New, " else ", Value3New, " end"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {caseExpression, Value1, Value2, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value1),
    {Value2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {caseAlternativesList, Value2}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["case ", Value1New, " ", Value2New, " end"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {caseExpression, Value1, Value2, Value3} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value1),
    {Value2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {caseAlternativesList, Value2}),
    {Value3New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value3),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["case ", Value1New, " ", Value2New, " else ", Value3New, " end"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {create, Pattern} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"create " ++ PatternNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% cypher
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {cypher, Value, Semicolon} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew ++ Semicolon, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% delete
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {delete, Detach, ExpressionCommalist} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionCommalistNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(
        [
            case Detach of
                [] -> [];
                _ -> "detach "
            end,
            "delete ",
            ExpressionCommalistNew
        ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% doubleLiteral / integerLiteral / mapLiteral / relationshipPattern / stringLiteral /
% symbolicName / terminal / % * (range)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {Type, Value} = ST)
    when Type == doubleLiteral orelse
    Type == integerLiteral orelse
    Type == mapLiteral andalso Value == "[]" orelse
    Type == relationshipPattern andalso (
        Value == "<-->" orelse
            Value == "<--" orelse
            Value == "-->" orelse
            Value == "--") orelse
    Type == reservedWord orelse
    Type == stringLiteral orelse
    Type == symbolicName orelse
    Type == terminal orelse
    Type == "*" andalso Value == "*" ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Value, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% explicitProcedureInvocation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {explicitProcedureInvocation, ProcedureName, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ProcedureNameNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ProcedureName),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ProcedureNameNew ++ "()", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {explicitProcedureInvocation, ProcedureName, ExpressionCommalist} = ST)
    when is_list(ExpressionCommalist) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ProcedureNameNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ProcedureName),
    {ExpressionCommalistNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([ProcedureNameNew, "(", ExpressionCommalistNew, ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% filterExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {filterExpression, IdInColl, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IdInCollNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, IdInColl),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {IdInCollNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {filterExpression, IdInColl, Where} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IdInCollNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, IdInColl),
    {WhereNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Where),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([IdInCollNew, " ", WhereNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionInvocation
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, Distinct, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FunctionNameNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, FunctionName),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([FunctionNameNew, "(", Distinct, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {functionInvocation, FunctionName, Distinct, ExpressionCommalist} = ST)
    when is_list(ExpressionCommalist) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FunctionNameNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, FunctionName),
    {ExpressionCommalistNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(
        [
            FunctionNameNew,
            "(",
            case length(Distinct) of
                0 -> [];
                _ -> Distinct ++ " "
            end,
            ExpressionCommalistNew,
            ")"
        ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% functionName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {functionName, "exists" = Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    RT = {Value, NewCtx},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% idInColl
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {idInColl, Variable, Expression} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {ExpressionNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Expression),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([VariableNew, " in ", ExpressionNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% inQueryCall
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {inQueryCall, Value1, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"Call " ++ Value1New, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {inQueryCall, Value1, Value2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value1),
    {Value2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["Call ", Value1New, " Yield ", Value2New]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% limit / skip
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == limit;
    Type == skip ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([atom_to_list(Type), " ", ValueNew]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% listComprehension
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {listComprehension, FilterExpression, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FilterExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, FilterExpression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["[", FilterExpressionNew, "]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {listComprehension, FilterExpression, Expression} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {FilterExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, FilterExpression),
    {ExpressionNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Expression),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", FilterExpressionNew, "|", ExpressionNew, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% listLiteral
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {listLiteral, ExpressionCommalist} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionCommalistNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["[", ExpressionCommalistNew, "]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% literal
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {literal, {Type, _} = Value} = ST)
    when Type == booleanLiteral;
    Type == listLiteral;
    Type == mapLiteral;
    Type == numberLiteral;
    Type == stringLiteral;
    Type == terminal ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mapLiteral
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {mapLiteral, Value} = ST)
    when is_list(Value) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {propertyKeyNameExpressionCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["{", ValueNew, "}"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% match
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {match, Optional, Pattern, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Pattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(
        [
            case Optional of
                [] -> [];
                _ -> "optional "
            end,
            "match ",
            PatternNew
        ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {match, Optional, Pattern, Where} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Pattern),
    {WhereNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Where),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(
        [
            case Optional of
                [] -> [];
                _ -> "optional "
            end,
            "match ",
            PatternNew,
            " ",
            WhereNew
        ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% merge
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {merge, PatternPart, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternPartNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, PatternPart),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"merge " ++ PatternPartNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {merge, PatternPart, MergeActionList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternPartNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, PatternPart),
    {MergeActionListNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {mergeActionList, MergeActionList}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["merge ", PatternPartNew, " ", MergeActionListNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% mergeAction
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {mergeAction, Type, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["on ", Type, " ", ValueNew]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% multiPartQuery
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {multiPartQuery, {readPart, ReadPartList} = ReadPart, [], With, [], {singlePartQuery, _} = SinglePartQuery} = ST)
    when is_list(ReadPartList) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReadPartNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReadPart),
    {WithNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, With),
    {SinglePartQueryNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, SinglePartQuery),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([
        ReadPartNew,
        " ",
        WithNew,
        " ",
        SinglePartQueryNew
    ]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {multiPartQuery, {readPart, ReadPartList} = ReadPart, [], With, ReadPartUpdatingPartWithList,
    {singlePartQuery, _} = SinglePartQuery} = ST)
    when is_list(ReadPartList), is_list(ReadPartUpdatingPartWithList) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReadPartNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReadPart),
    {WithNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, With),
    {ReadPartUpdatingPartWithListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {readPartUpdatingPartWithList, ReadPartUpdatingPartWithList}),
    {SinglePartQueryNew, NewCtx4} = pt_to_source(FType, Fun, NewCtx3, Lvl + 1, SinglePartQuery),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append([
        ReadPartNew,
        " ",
        WithNew,
        " ",
        ReadPartUpdatingPartWithListNew,
        " ",
        SinglePartQueryNew
    ]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {multiPartQuery, {updatingStartClause, _} = UpdatingStartClause,
    {updatingPart, []}, With, [], {singlePartQuery, _} = SinglePartQuery} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingStartClauseNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, UpdatingStartClause),
    {WithNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, With),
    {SinglePartQueryNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, SinglePartQuery),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([
        UpdatingStartClauseNew,
        " ",
        WithNew,
        " ",
        SinglePartQueryNew
    ]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {multiPartQuery, {updatingStartClause, _} = UpdatingStartClause,
    {updatingPart, UpdatingPartList} = UpdatingPart, With, [], {singlePartQuery, _} = SinglePartQuery} = ST)
    when is_list(UpdatingPartList) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingStartClauseNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, UpdatingStartClause),
    {UpdatingPartNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, UpdatingPart),
    {WithNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, With),
    {SinglePartQueryNew, NewCtx4} = pt_to_source(FType, Fun, NewCtx3, Lvl + 1, SinglePartQuery),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append([
        UpdatingStartClauseNew,
        " ",
        UpdatingPartNew,
        " ",
        WithNew,
        " ",
        SinglePartQueryNew
    ]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {multiPartQuery, {updatingStartClause, _} = UpdatingStartClause,
    {updatingPart, []}, With, ReadPartUpdatingPartWithList, {singlePartQuery, _} = SinglePartQuery} = ST)
    when is_list(ReadPartUpdatingPartWithList) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingStartClauseNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, UpdatingStartClause),
    {WithNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, With),
    {ReadPartUpdatingPartWithListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {readPartUpdatingPartWithList, ReadPartUpdatingPartWithList}),
    {SinglePartQueryNew, NewCtx4} = pt_to_source(FType, Fun, NewCtx3, Lvl + 1, SinglePartQuery),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append([
        UpdatingStartClauseNew,
        " ",
        WithNew,
        " ",
        ReadPartUpdatingPartWithListNew,
        " ",
        SinglePartQueryNew
    ]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {multiPartQuery, {updatingStartClause, _} = UpdatingStartClause,
    {updatingPart, UpdatingPartList} = UpdatingPart, With, ReadPartUpdatingPartWithList,
    {singlePartQuery, _} = SinglePartQuery} = ST)
    when is_list(UpdatingPartList), is_list(ReadPartUpdatingPartWithList) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingStartClauseNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, UpdatingStartClause),
    {UpdatingPartNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, UpdatingPart),
    {WithNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, With),
    {ReadPartUpdatingPartWithListNew, NewCtx4} = pt_to_source(FType, Fun, NewCtx3, Lvl + 1, {readPartUpdatingPartWithList, ReadPartUpdatingPartWithList}),
    {SinglePartQueryNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, SinglePartQuery),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {lists:append([
        UpdatingStartClauseNew,
        " ",
        UpdatingPartNew,
        " ",
        WithNew,
        " ",
        ReadPartUpdatingPartWithListNew,
        " ",
        SinglePartQueryNew
    ]), NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% namespace
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {namespace, {symbolicName, _} = Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew ++ ".", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nodeLabel, properties, relType, where
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == nodeLabel;
    Type == properties;
    Type == relType;
    Type == where ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {case Type of
              nodeLabel -> ":";
              where -> "where ";
              _ -> []
          end ++ ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% nodePattern
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {nodePattern, [], [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"()", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, [], [], Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PropertiesNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Properties),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["(", PropertiesNew, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, [], NodeLabels, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NodeLabelsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, NodeLabels),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["(", NodeLabelsNew, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, [], NodeLabels, Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NodeLabelsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, NodeLabels),
    {PropertiesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Properties),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["(", NodeLabelsNew, PropertiesNew, ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, Variable, [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["(", VariableNew, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, Variable, [], Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {PropertiesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Properties),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["(", VariableNew, " ", PropertiesNew, ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, Variable, NodeLabels, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {NodeLabelsNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, NodeLabels),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["(", VariableNew, " ", NodeLabelsNew, ")"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, Variable, NodeLabels, Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {NodeLabelsNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, NodeLabels),
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["(", VariableNew, " ", NodeLabelsNew, PropertiesNew, ")"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% notExpression / unaryAddOrSubtractExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Expression, []} = ST)
    when Type == notExpression;
    Type == unaryAddOrSubtractExpression ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ExpressionNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {Type, Expression, ExpressionAddonList} = ST)
    when Type == notExpression;
    Type == unaryAddOrSubtractExpression ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Expression),
    {ExpressionAddonListNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {list_to_atom(atom_to_list(Type) ++ "AddonList"), ExpressionAddonList}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(
        [
            ExpressionAddonListNew,
            case lists:suffix(" ", ExpressionAddonListNew) of
                true ->
                    [];
                false ->
                    " "
            end,
            ExpressionNew
        ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% numberLiteral
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {numberLiteral, {SubType, _} = Value} = ST)
    when SubType == doubleLiteral;
    SubType == integerLiteral ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% order
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {order, SortItemCommalist} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SortItemCommalistNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {sortItemCommalist, SortItemCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"order by " ++ SortItemCommalistNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parameter
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {parameter, {symbolicName, _} = Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"$" ++ ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, _Lvl, {parameter, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"$" ++ Value, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% parenthesizedExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {parenthesizedExpression, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["(", ValueNew, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% partialComparisonExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {partialComparisonExpression, Value, Terminal} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([Terminal, " ", ValueNew]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% pattern
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {pattern, Value} = ST)
    when is_list(Value) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {patternPartCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternComprehension
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {patternComprehension, [], RelationshipsPattern, [], Expression} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipsPatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RelationshipsPattern),
    {ExpressionNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Expression),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", RelationshipsPatternNew, "|", ExpressionNew, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternComprehension, [], RelationshipsPattern, WhereExpression, Expression} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipsPatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RelationshipsPattern),
    {WhereExpressionNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, WhereExpression),
    {ExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Expression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["[", RelationshipsPatternNew, " where ", WhereExpressionNew, "|", ExpressionNew, "]"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternComprehension, Variable, RelationshipsPattern, [], Expression} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {RelationshipsPatternNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RelationshipsPattern),
    {ExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Expression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["[", VariableNew, "=", RelationshipsPatternNew, "|", ExpressionNew, "]"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternComprehension, Variable, RelationshipsPattern, WhereExpression, Expression} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {RelationshipsPatternNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RelationshipsPattern),
    {WhereExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, WhereExpression),
    {ExpressionNew, NewCtx4} = pt_to_source(FType, Fun, NewCtx3, Lvl + 1, Expression),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append(["[", VariableNew, "=", RelationshipsPatternNew, " where ", WhereExpressionNew, "|", ExpressionNew, "]"]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternElement
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {patternElement, PatternElement, "("} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PatternElementNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, PatternElement),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["(", PatternElementNew, ")"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternElement, NodePattern, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NodePatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, NodePattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {NodePatternNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternElement, NodePattern, PatternElementChainList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NodePatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, NodePattern),
    {PatternElementChainListNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {patternElementChainList, PatternElementChainList}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {NodePatternNew ++ PatternElementChainListNew, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternElementChain
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {patternElementChain, Value_1, Value_2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value_1),
    {Value_2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value_2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {Value_1New ++ Value_2New, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternPart
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {patternPart, [], Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternPart, Value_1, Value_2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value_1),
    {Value_2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value_2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([Value_1New, "=", Value_2New]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% procedureName
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {procedureName, Namespace, {symbolicName, _} = SymbolicName} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NamespaceNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Namespace),
    {SymbolicNameNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, SymbolicName),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {NamespaceNew ++ SymbolicNameNew, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {propertyExpression, Atom, PropertyLookUpList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AtomNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Atom),
    {PropertyLookUpListNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {propertyLookupList, PropertyLookUpList}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([AtomNew, " ", PropertyLookUpListNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyKeyNameExpression (Helper definition)
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {{propertyKeyName, _} = PropertyKeyName, {expression, _} = Expression} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PropertyKeyNameNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, PropertyKeyName),
    {ExpressionNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Expression),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([PropertyKeyNameNew, ":", ExpressionNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyLookup
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {propertyLookup, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"." ++ ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% propertyOrLabelsExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {propertyOrLabelsExpression, Atom, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AtomNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Atom),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {AtomNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {propertyOrLabelsExpression, Atom, Addons} = ST)
    when is_list(Addons) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {AtomNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Atom),
    {AddonsNew, NewCtx2} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {nodeLabels, _} ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ SubAcc, CtxAcc1};
            {propertyLookup, _} ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ SubAcc, CtxAcc1}
        end
                                       end,
        {[], NewCtx1},
        Addons),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([AtomNew, " ", AddonsNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% rangeLiteral
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {rangeLiteral, [], [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"*", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, _Lvl, {rangeLiteral, [], "..", []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"*..", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {rangeLiteral, [], ".." = Op, IntegerLiteral} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntegerLiteralNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, IntegerLiteral),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["*", IntegerLiteralNew, Op]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {rangeLiteral, IntegerLiteral, [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntegerLiteralNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, IntegerLiteral),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"*" ++ IntegerLiteralNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {rangeLiteral, IntegerLiteral, ".." = Op, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntegerLiteralNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, IntegerLiteral),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["*", IntegerLiteralNew, Op]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {rangeLiteral, IntegerLiteral1, ".." = Op, IntegerLiteral2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {IntegerLiteral1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, IntegerLiteral1),
    {IntegerLiteral2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, IntegerLiteral2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["*", IntegerLiteral1New, Op, IntegerLiteral2New]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% readOnlyEnd
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {readOnlyEnd, {readPart, []}, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Return),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([ReturnNew]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {readOnlyEnd, ReadPart, Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReadPartNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReadPart),
    {ReturnNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Return),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([ReadPartNew, " ", ReturnNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% readPart
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {readPart, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {[], NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {readPart, ReadingClauseList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReadingClauseListNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {readingClauseList, ReadingClauseList}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ReadingClauseListNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% readPart updatingPart with
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {{readPart, _} = ReadPart, {updatingPart, []}, {with, _, _, _} = With} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReadPartNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReadPart),
    {WithNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, With),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([ReadPartNew, " ", WithNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {{readPart, _} = ReadPart, {updatingPart, _} = UpdatingPart, {with, _, _, _} = With} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReadPartNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReadPart),
    {UpdatingPartNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, UpdatingPart),
    {WithNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, With),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([ReadPartNew, " ", UpdatingPartNew, " ", WithNew]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% readUpdateEnd
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {readUpdateEnd, ReadingClauseCommalist, UpdatingClauseCommalist, []} = ST)
    when is_list(ReadingClauseCommalist), is_list(UpdatingClauseCommalist) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReadingClauseCommalistNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {readingClauseList, ReadingClauseCommalist}),
    {UpdatingClauseCommalistNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {updatingClauseList, UpdatingClauseCommalist}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([ReadingClauseCommalistNew, " ", UpdatingClauseCommalistNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {readUpdateEnd, ReadingClauseCommalist, UpdatingClauseCommalist, Return} = ST)
    when is_list(ReadingClauseCommalist), is_list(UpdatingClauseCommalist) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReadingClauseCommalistNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {readingClauseList, ReadingClauseCommalist}),
    {UpdatingClauseCommalistNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {updatingClauseList, UpdatingClauseCommalist}),
    {ReturnNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Return),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([ReadingClauseCommalistNew, " ", UpdatingClauseCommalistNew, " ", ReturnNew]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% regularQuery
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {regularQuery, SingularQuery, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SingularQueryNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, SingularQuery),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {SingularQueryNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {regularQuery, SingularQuery, UnionList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SingularQueryNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, SingularQuery),
    {UnionListNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {unionList, UnionList}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([SingularQueryNew, " ", UnionListNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipDetail
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {relationshipDetail, [], [], [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {"[]", NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], [], [], Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {PropertiesNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Properties),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["[", PropertiesNew, "]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], [], RangeLiteral, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RangeLiteralNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RangeLiteral),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["[", RangeLiteralNew, "]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], [], RangeLiteral, Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RangeLiteralNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RangeLiteral),
    {PropertiesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Properties),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", RangeLiteralNew, PropertiesNew, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], RelationshipTypes, [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipTypesNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RelationshipTypes),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["[", RelationshipTypesNew, "]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], RelationshipTypes, [], Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipTypesNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RelationshipTypes),
    {PropertiesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Properties),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", RelationshipTypesNew, PropertiesNew, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], RelationshipTypes, RangeLiteral, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipTypesNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RelationshipTypes),
    {RangeLiteralNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RangeLiteral),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", RelationshipTypesNew, RangeLiteralNew, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], RelationshipTypes, RangeLiteral, Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipTypesNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RelationshipTypes),
    {RangeLiteralNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RangeLiteral),
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["[", RelationshipTypesNew, RangeLiteralNew, PropertiesNew, "]"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, [], [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(["[", VariableNew, "]"]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, [], [], Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {PropertiesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Properties),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", VariableNew, PropertiesNew, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, [], RangeLiteral, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {RangeLiteralNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RangeLiteral),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", VariableNew, RangeLiteralNew, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, [], RangeLiteral, Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {RangeLiteralNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RangeLiteral),
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["[", VariableNew, RangeLiteralNew, PropertiesNew, "]"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, RelationshipTypes, [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {RelationshipTypesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RelationshipTypes),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["[", VariableNew, RelationshipTypesNew, "]"]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, RelationshipTypes, [], Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {RelationshipTypesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RelationshipTypes),
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["[", VariableNew, RelationshipTypesNew, PropertiesNew, "]"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, RelationshipTypes, RangeLiteral, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {RelationshipTypesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RelationshipTypes),
    {RangeLiteralNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append(["[", VariableNew, RelationshipTypesNew, RangeLiteralNew, "]"]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, RelationshipTypes, RangeLiteral, Properties} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {VariableNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Variable),
    {RelationshipTypesNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, RelationshipTypes),
    {RangeLiteralNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
    {PropertiesNew, NewCtx4} = pt_to_source(FType, Fun, NewCtx3, Lvl + 1, Properties),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append(["[", VariableNew, RelationshipTypesNew, RangeLiteralNew, PropertiesNew, "]"]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipPattern
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {relationshipPattern, Left, [], Right} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Left ++ Right, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipPattern, Left, Value, Right} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([Left, ValueNew, Right]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipsPattern
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {relationshipsPattern, NodePattern, PatternElementChainList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {NodePatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, NodePattern),
    {PatternElementChainListNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {patternElementChainList, PatternElementChainList}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {NodePatternNew ++ PatternElementChainListNew, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relationshipTypes
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {relationshipTypes, RelType, RelTypeVerticalbarlist} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelTypeNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RelType),
    {RelTypeVerticalbarlistNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {relTypeVerticalbarlist, RelTypeVerticalbarlist}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([":", RelTypeNew, RelTypeVerticalbarlistNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% remove
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {remove, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {removeItemCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"remove " ++ ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% removeItem
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {removeItem, Value_1, Value_2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value_1),
    {Value_2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value_2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {Value_1New ++ Value_2New, NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% return
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {return, Distinct, ReturnBody} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(
        [
            "return ", case Distinct of
                           [] -> [];
                           _ -> "distinct "
                       end,
            ReturnBodyNew
        ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% returnBody
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ReturnItemsNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], [], Limit} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    {LimitNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Limit),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([ReturnItemsNew, " ", LimitNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], Skip, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    {SkipNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Skip),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([ReturnItemsNew, " ", SkipNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], Skip, Limit} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    {SkipNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Skip),
    {LimitNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Limit),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([ReturnItemsNew, " ", SkipNew, " ", LimitNew]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    {OrderNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Order),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([ReturnItemsNew, " ", OrderNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, [], Limit} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    {OrderNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Order),
    {LimitNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Limit),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([ReturnItemsNew, " ", OrderNew, " ", LimitNew]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, Skip, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    {OrderNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Order),
    {SkipNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Skip),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([ReturnItemsNew, " ", OrderNew, " ", SkipNew]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, Skip, Limit} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemsNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnItems),
    {OrderNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Order),
    {SkipNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Skip),
    {LimitNew, NewCtx4} = pt_to_source(FType, Fun, NewCtx3, Lvl + 1, Limit),
    NewCtx5 = case FType of
                  top_down -> NewCtx4;
                  bottom_up -> Fun(ST, NewCtx4)
              end,
    RT = {lists:append([ReturnItemsNew, " ", OrderNew, " ", SkipNew, " ", LimitNew]), NewCtx5},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% returnItem
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {returnItem, Expression, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Expression),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ExpressionNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnItem, Expression, Variable} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Expression),
    {VariableNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Variable),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([ExpressionNew, " as ", VariableNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% returnItems
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {returnItems, Asterik, Comma, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {Asterik ++ Comma, NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnItems, Asterik, Comma, ReturnItemCommalist} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnItemCommalistNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {returnItemCommalist, ReturnItemCommalist}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append([Asterik, Comma, ReturnItemCommalistNew]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% set
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {set, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {setItemCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"set " ++ ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% setItem
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {setItem, Value_1, Operation, Value_2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value_1),
    {Value_2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value_2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(
        [
            Value_1New,
            case Operation of
                [] -> [];
                _ -> lists:append([" ", Operation, " "])
            end,
            Value_2New
        ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% sortItem
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {sortItem, Value, Type} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(
        [
            ValueNew,
            case Type of
                [] -> [];
                _ -> " "
            end,
            Type
        ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% standaloneCall
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {standaloneCall, ExplicitProcedureInvocation, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExplicitProcedureInvocationNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ExplicitProcedureInvocation),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {"Call " ++ ExplicitProcedureInvocationNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {standaloneCall, ExplicitProcedureInvocation, {yieldItems, YieldItemsList} = YieldItems} = ST)
    when is_list(YieldItemsList) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExplicitProcedureInvocationNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ExplicitProcedureInvocation),
    {YieldItemsNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, YieldItems),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["Call ", ExplicitProcedureInvocationNew, " Yield ", YieldItemsNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% union
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {union, All, SingleQuery} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {SingleQueryNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, SingleQuery),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(
        [
            "union ",
            case length(All) of
                0 -> [];
                _ -> All ++ " "
            end,
            SingleQueryNew
        ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% unwind
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {unwind, Expression, Variable} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ExpressionNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Expression),
    {VariableNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Variable),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(["unwind ", ExpressionNew, " as ", VariableNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% updatingEnd
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {updatingEnd, UpdatingStartClause, [], []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingStartClauseNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, UpdatingStartClause),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {UpdatingStartClauseNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {updatingEnd, UpdatingStartClause, [], Return} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingStartClauseNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, UpdatingStartClause),
    {ReturnNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Return),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([UpdatingStartClauseNew, " ", ReturnNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {updatingEnd, UpdatingStartClause, UpdatingClauseCommalist, []} = ST)
    when is_list(UpdatingClauseCommalist) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingStartClauseNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, UpdatingStartClause),
    {UpdatingClauseCommalistNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {updatingClauseList, UpdatingClauseCommalist}),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([UpdatingStartClauseNew, " ", UpdatingClauseCommalistNew]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {updatingEnd, UpdatingStartClause, UpdatingClauseCommalist, Return} = ST)
    when is_list(UpdatingClauseCommalist) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingStartClauseNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, UpdatingStartClause),
    {UpdatingClauseCommalistNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, {updatingClauseList, UpdatingClauseCommalist}),
    {ReturnNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Return),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {lists:append([UpdatingStartClauseNew, " ", UpdatingClauseCommalistNew, " ", ReturnNew]), NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% updatingPart
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {updatingPart, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    NewCtx1 = case FType of
                  top_down -> NewCtx;
                  bottom_up -> Fun(ST, NewCtx)
              end,
    RT = {[], NewCtx1},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {updatingPart, UpdatingClauseList} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {UpdatingClauseListNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {updatingClauseList, UpdatingClauseList}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {UpdatingClauseListNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% with
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {with, Distinct, ReturnBody, []} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {lists:append(
        [
            "with ",
            case Distinct of
                [] -> [];
                _ -> "distinct "
            end,
            ReturnBodyNew
        ]), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {with, Distinct, ReturnBody, Where} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ReturnBodyNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, ReturnBody),
    {WhereNew, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Where),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append(
        [
            "with ",
            case Distinct of
                [] -> [];
                _ -> "distinct "
            end,
            ReturnBodyNew,
            " ",
            WhereNew
        ]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% yieldItem
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {yieldItem, [], Value2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value2New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value2),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {Value2New, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {yieldItem, Value1, Value2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value1),
    {Value2New, NewCtx2} = pt_to_source(FType, Fun, NewCtx1, Lvl + 1, Value2),
    NewCtx3 = case FType of
                  top_down -> NewCtx2;
                  bottom_up -> Fun(ST, NewCtx2)
              end,
    RT = {lists:append([Value1New, " As ", Value2New]), NewCtx3},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% yieldItems
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, _Lvl, {yieldItems, "-" = Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [_Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    RT = {Value, NewCtx},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {yieldItems, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {yieldItemCommalist, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Not yet supported
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(_FType, Fun, Ctx, _Lvl, PTree) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n PTree: ~p~n", [_Lvl, PTree]),
    Fun(PTree, Ctx),
    throw({"Parse tree not supported", PTree}).
