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

-define(NODEBUG, true).
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
    RT = {Type ++ ValueNew ++ "]", NewCtx2},
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
    RT = {"[.." ++ ValueNew ++ "]", NewCtx2},
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
    RT = {"[" ++ ValueNew ++ "..]", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {"[", {expression, _} = Value1, {expression, _} = Value2} = ST) ->
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
    {Value2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ Value1New ++ ".." ++ Value2New ++ "]", NewCtx4},
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
    RT = {Type ++ " " ++ ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addOrSubtractExpression .. xorExpression
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Value, []} = ST)
    when Type == addOrSubtractExpression;
    Type == andExpression;
    Type == comparisonExpression;
    Type == multiplyDivideModuloExpression;
    Type == notExpression;
    Type == orExpression;
    Type == powerOfExpression;
    Type == stringListNullOperatorExpression;
    Type == unaryAddOrSubtractExpression;
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
pt_to_source(FType, Fun, Ctx, Lvl, {Type, Expression, ExpressionAddonList} = ST)
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
    {ExpressionAddonListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {list_to_atom(atom_to_list(Type) ++ "AddonList"), ExpressionAddonList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ExpressionAddonListNew ++ case lists:suffix(" ", ExpressionAddonListNew) of
                                        true ->
                                            [];
                                        false ->
                                            " "
                                    end ++ ExpressionNew, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {ExpressionAddonListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {list_to_atom(atom_to_list(Type) ++ "AddonList"), ExpressionAddonList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ExpressionNew ++ " " ++ ExpressionAddonListNew, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% addOrSubtractExpressionAddonList .. xorExpressionAddonList
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when (Type == addOrSubtractExpressionAddonList orelse
    Type == andExpressionAddonList orelse
    Type == comparisonExpressionAddonList orelse
    Type == multiplyDivideModuloExpressionAddonList orelse
    Type == notExpressionAddonList orelse
    Type == orExpressionAddonList orelse
    Type == powerOfExpressionAddonList orelse
    Type == stringListNullOperatorExpressionAddonList orelse
    Type == unaryAddOrSubtractExpressionAddonList orelse
    Type == xorExpressionAddonList)
    andalso is_list(Values) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValuesNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {Operator, _, _}
                when Type == comparisonExpressionAddonList andalso Operator == partialComparisonExpression orelse
                Type == stringListNullOperatorExpressionAddonList andalso Operator == "[" ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc, CtxAcc1};
            {Operator, _}
                when Type == addOrSubtractExpressionAddonList andalso (
                Operator == "+" orelse
                    Operator == "-") orelse
                Type == andExpressionAddonList andalso Operator == "and" orelse
                Type == multiplyDivideModuloExpressionAddonList andalso (
                    Operator == "*" orelse
                        Operator == "/" orelse
                        Operator == "%") orelse
                Type == orExpressionAddonList andalso Operator == "or" orelse
                Type == powerOfExpressionAddonList andalso Operator == "^" orelse
                Type == stringListNullOperatorExpressionAddonList andalso (
                    Operator == "[" orelse
                        Operator == "=~" orelse
                        Operator == "in" orelse
                        Operator == "starts with" orelse
                        Operator == "ends with" orelse
                        Operator == "contains") orelse
                Type == xorExpressionAddonList andalso Operator == "xor" ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc, CtxAcc1};
            {Operator}
                when Type == notExpressionAddonList andalso Operator == "not" orelse
                Type == stringListNullOperatorExpressionAddonList andalso Operator == "is null" orelse
                Type == stringListNullOperatorExpressionAddonList andalso Operator == "is not null" ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ Operator, CtxAcc};
            {Operator}
                when Type == unaryAddOrSubtractExpressionAddonList andalso Operator == "+" orelse
                Type == unaryAddOrSubtractExpressionAddonList andalso Operator == "-" ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {Acc ++ Operator ++ " ", CtxAcc}
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
% anonymousPatternPart / expression / functionName / labelName / patternPart /
% propertyKeyName / query / relTypeName / removeItem / schemaName / sortItem /
% statement / variable
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
pt_to_source(FType, Fun, Ctx, Lvl, {Type, Value} = ST)
    when Type == anonymousPatternPart;
    Type == expression;
    Type == functionName;
    Type == labelName;
    Type == patternPart;
    Type == propertyKeyName;
    Type == query;
    Type == relTypeName;
    Type == removeItem;
    Type == schemaName;
    Type == sortItem;
    Type == statement;
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
    RT = {atom_to_list(Type) ++ "(" ++ ValueNew ++ ")", NewCtx2},
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
    RT = {atom_to_list(Type) ++ "(" ++ FilterExpressionNew ++ ")", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {atom, {extract = Type, FilterExpression, Expression}} = ST) ->
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
    {ExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Expression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {atom_to_list(Type) ++ "(" ++ FilterExpressionNew ++ "|" ++ ExpressionNew ++ ")", NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"when " ++ Value1New ++ " then " ++ Value2New, NewCtx4},
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
    RT = {"case " ++ Value2New ++ " end", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {caseExpression, [], Value2, Value3} = ST) ->
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
    {Value3New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value3),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"case " ++ Value2New ++ " else " ++ Value3New ++ " end", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {caseExpression, Value1, Value2, []} = ST) ->
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
    {Value2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {caseAlternativesList, Value2}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"case " ++ Value1New ++ " " ++ Value2New ++ " end", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {caseExpression, Value1, Value2, Value3} = ST) ->
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
    {Value2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {caseAlternativesList, Value2}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {Value3New, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Value3),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"case " ++ Value1New ++ " " ++ Value2New ++ " else " ++ Value3New ++ " end", NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% clause / numberLiteral
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, {SubType, _} = Value} = ST)
    when Type == clause andalso (
    SubType == remove orelse
        SubType == set) orelse
    Type == numberLiteral andalso (
        SubType == doubleLiteral orelse
            SubType == integerLiteral) ->
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
pt_to_source(FType, Fun, Ctx, Lvl, {clause, {create, _} = Value} = ST) ->
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
pt_to_source(FType, Fun, Ctx, Lvl, {clause, {Type, _, _} = Value} = ST)
    when Type == delete;
    Type == inQueryCall;
    Type == merge;
    Type == return;
    Type == unwind ->
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
pt_to_source(FType, Fun, Ctx, Lvl, {clause, {with, _, _, _} = Value} = ST) ->
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
pt_to_source(FType, Fun, Ctx, Lvl, {clause, {match, _, _, _} = Value} = ST) ->
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
% caseAlternativesList / clauseList / mergeActionList / nodeLabels / unionList
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when Type == caseAlternativesList;
    Type == clauseList;
    Type == mergeActionList;
    Type == nodeLabels;
    Type == unionList,
    is_list(Values) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {ListType, _}
                when Type == clauseList, ListType == clause;
                Type == nodeLabels, ListType == nodeLabel ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> " "
                        end ++ SubAcc, CtxAcc1};
            {ListType, _, _}
                when Type == caseAlternativesList, ListType == caseAlternatives;
                Type == mergeActionList, ListType == mergeAction;
                Type == unionList, ListType == union ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
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
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% create / limit / skip
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
    RT = {atom_to_list(Type) ++ " " ++ ValueNew, NewCtx2},
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
    RT = {case Detach of
              [] -> [];
              _ -> "detach "
          end ++ "delete " ++ ExpressionCommalistNew, NewCtx2},
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

pt_to_source(FType, Fun, Ctx, Lvl, {explicitProcedureInvocation, Value1, []} = ST) ->
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
    RT = {Value1New(), NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {explicitProcedureInvocation, Value1, Value2} = ST) ->
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
    {Value2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value1New ++ "(" ++ Value2New ++ ")", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% expressionCommalist / patternPartCommalist / propertyKeyNameExpressionCommalist /
% removeItemCommalist / returnItemCommalist / setItemCommalist / sortItemCommalist /
% variableCommalist / yieldItemCommalist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when Type == expressionCommalist;
    Type == patternPartCommalist;
    Type == propertyKeyNameExpressionCommalist;
    Type == removeItemCommalist;
    Type == returnItemCommalist;
    Type == setItemCommalist;
    Type == sortItemCommalist;
    Type == variableCommalist;
    Type == yieldItemCommalist,
    is_list(Values) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
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
                Type == variableCommalist, ListType == variable;
                Type == yieldItemCommalist, ListType == yieldItem ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
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
                Type == yieldItemCommalist, ListType == yieldItem ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ SubAcc, CtxAcc1};
            {ListType, _, _, _}
                when Type == setItemCommalist, ListType == setItem ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ case length(Acc) of
                            0 -> [];
                            _ -> ","
                        end ++ SubAcc, CtxAcc1};
            {{propertyKeyName, _}, {expression, _}}
                when Type == propertyKeyNameExpressionCommalist ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {WhereNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Where),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {IdInCollNew ++ " " ++ WhereNew, NewCtx4},
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
    RT = {FunctionNameNew ++ "(" ++ Distinct ++ ")", NewCtx2},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {ExpressionCommalistNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {expressionCommalist, ExpressionCommalist}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {FunctionNameNew ++ "(" ++ case length(Distinct) of
                                        0 -> [];
                                        _ -> Distinct ++ " "
                                    end ++ ExpressionCommalistNew ++ ")", NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {ExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Expression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {VariableNew ++ " in " ++ ExpressionNew, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"Call " ++ Value1New ++ " Yield " ++ Value2New, NewCtx4},
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
    RT = {"[" ++ FilterExpressionNew ++ "]", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {listComprehension, FilterExpression, Expression} = ST) ->
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
    {ExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Expression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ FilterExpressionNew ++ "|" ++ ExpressionNew ++ "]", NewCtx4},
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
    RT = {"[" ++ ExpressionCommalistNew ++ "]", NewCtx2},
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
    RT = {"{" ++ ValueNew ++ "}", NewCtx2},
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
    RT = {case Optional of
              [] -> [];
              _ -> "optional "
          end ++ "match " ++ PatternNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {match, Optional, Pattern, Where} = ST) ->
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
    {WhereNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Where),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {case Optional of
              [] -> [];
              _ -> "optional "
          end ++ "match " ++ PatternNew ++ " " ++ WhereNew, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {MergeActionListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {mergeActionList, MergeActionList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"merge " ++ PatternPartNew ++ " " ++ MergeActionListNew, NewCtx4},
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
    RT = {"on " ++ Type ++ " " ++ ValueNew, NewCtx2},
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
    RT = {"(" ++ PropertiesNew ++ ")", NewCtx2},
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
    RT = {"(" ++ NodeLabelsNew ++ ")", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, [], NodeLabels, Properties} = ST) ->
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
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"(" ++ NodeLabelsNew ++ PropertiesNew ++ ")", NewCtx4},
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
    RT = {"(" ++ VariableNew ++ ")", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, Variable, [], Properties} = ST) ->
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
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"(" ++ VariableNew ++ " " ++ PropertiesNew ++ ")", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, Variable, NodeLabels, []} = ST) ->
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
    {NodeLabelsNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, NodeLabels),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"(" ++ VariableNew ++ " " ++ NodeLabelsNew ++ ")", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {nodePattern, Variable, NodeLabels, Properties} = ST) ->
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
    {NodeLabelsNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, NodeLabels),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {PropertiesNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Properties),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"(" ++ VariableNew ++ " " ++ NodeLabelsNew ++ PropertiesNew ++ ")", NewCtx6},
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
    RT = {"(" ++ ValueNew ++ ")", NewCtx2},
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
    RT = {Terminal ++ " " ++ ValueNew, NewCtx2},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {ExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Expression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ RelationshipsPatternNew ++ "|" ++ ExpressionNew ++ "]", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternComprehension, [], RelationshipsPattern, WhereExpression, Expression} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {RelationshipsPatternNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, RelationshipsPattern),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {WhereExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, WhereExpression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {ExpressionNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Expression),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"[" ++ RelationshipsPatternNew ++ " where " ++ WhereExpressionNew ++ "|" ++ ExpressionNew ++ "]", NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternComprehension, Variable, RelationshipsPattern, [], Expression} = ST) ->
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
    {RelationshipsPatternNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RelationshipsPattern),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {ExpressionNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Expression),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"[" ++ VariableNew ++ "=" ++ RelationshipsPatternNew ++ "|" ++ ExpressionNew ++ "]", NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {patternComprehension, Variable, RelationshipsPattern, WhereExpression, Expression} = ST) ->
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
    {RelationshipsPatternNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RelationshipsPattern),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {WhereExpressionNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, WhereExpression),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    {ExpressionNew, NewCtx7} = pt_to_source(FType, Fun, NewCtx6, Lvl + 1, Expression),
    NewCtx8 = case FType of
                  top_down -> NewCtx7;
                  bottom_up -> Fun(ST, NewCtx7)
              end,
    RT = {"[" ++ VariableNew ++ "=" ++ RelationshipsPatternNew ++ " where " ++ WhereExpressionNew ++ "|" ++ ExpressionNew ++ "]", NewCtx8},
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
    RT = {"(" ++ PatternElementNew ++ ")", NewCtx2},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {PatternElementChainListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {patternElementChainList, PatternElementChainList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {NodePatternNew ++ PatternElementChainListNew, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value_2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value_1New ++ Value_2New, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% patternElementChainList / propertyLookupList
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {Type, Values} = ST)
    when Type == patternElementChainList;
    Type == propertyLookupList, is_list(Values) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {patternElementChain, _, _}
                when Type == patternElementChainList ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
                {Acc ++ SubAcc, CtxAcc1};
            {propertyLookup, _}
                when Type == propertyLookupList ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, F),
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value_2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value_1New ++ "=" ++ Value_2New, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {PropertyLookUpListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {propertyLookupList, PropertyLookUpList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {AtomNew ++ " " ++ PropertyLookUpListNew, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {ExpressionNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Expression),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {PropertyKeyNameNew ++ ":" ++ ExpressionNew, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {AddonsNew, NewCtx3} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
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
        {[], NewCtx2},
        Addons),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {AtomNew ++ " " ++ AddonsNew, NewCtx4},
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
pt_to_source(FType, Fun, Ctx, Lvl, {rangeLiteral, Value, [], []} = ST) ->
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
    RT = {"*" ++ ValueNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {rangeLiteral, Value, ".." = Op, []} = ST) ->
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
    RT = {"*" ++ ValueNew ++ Op, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {rangeLiteral, Value_1, ".." = Op, Value_2} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {Value_1New, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, Value_1),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value_2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"*" ++ Value_1New ++ Op ++ Value_2New, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {UnionListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {unionList, UnionList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {SingularQueryNew ++ " " ++ UnionListNew, NewCtx4},
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
    RT = {"[" ++ PropertiesNew ++ "]", NewCtx2},
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
    RT = {"[" ++ RangeLiteralNew ++ "]", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], [], RangeLiteral, Properties} = ST) ->
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
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx4},
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
    RT = {"[" ++ RelationshipTypesNew ++ "]", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], RelationshipTypes, [], Properties} = ST) ->
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
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ RelationshipTypesNew ++ PropertiesNew ++ "]", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], RelationshipTypes, RangeLiteral, []} = ST) ->
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
    {RangeLiteralNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ RelationshipTypesNew ++ RangeLiteralNew ++ "]", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, [], RelationshipTypes, RangeLiteral, Properties} = ST) ->
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
    {RangeLiteralNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {PropertiesNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Properties),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"[" ++ RelationshipTypesNew ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx6},
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
    RT = {"[" ++ VariableNew ++ "]", NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, [], [], Properties} = ST) ->
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
    {PropertiesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Properties),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ VariableNew ++ PropertiesNew ++ "]", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, [], RangeLiteral, []} = ST) ->
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
    {RangeLiteralNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ VariableNew ++ RangeLiteralNew ++ "]", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, [], RangeLiteral, Properties} = ST) ->
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
    {RangeLiteralNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RangeLiteral),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {PropertiesNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Properties),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"[" ++ VariableNew ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, RelationshipTypes, [], []} = ST) ->
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
    {RelationshipTypesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RelationshipTypes),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"[" ++ VariableNew ++ RelationshipTypesNew ++ "]", NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, RelationshipTypes, [], Properties} = ST) ->
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
    {RelationshipTypesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RelationshipTypes),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {PropertiesNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Properties),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"[" ++ VariableNew ++ RelationshipTypesNew ++ PropertiesNew ++ "]", NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, RelationshipTypes, RangeLiteral, []} = ST) ->
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
    {RelationshipTypesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RelationshipTypes),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {RangeLiteralNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, RangeLiteral),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {"[" ++ VariableNew ++ RelationshipTypesNew ++ RangeLiteralNew ++ "]", NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {relationshipDetail, Variable, RelationshipTypes, RangeLiteral, Properties} = ST) ->
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
    {RelationshipTypesNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, RelationshipTypes),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {RangeLiteralNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, RangeLiteral),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    {PropertiesNew, NewCtx7} = pt_to_source(FType, Fun, NewCtx6, Lvl + 1, Properties),
    NewCtx8 = case FType of
                  top_down -> NewCtx7;
                  bottom_up -> Fun(ST, NewCtx7)
              end,
    RT = {"[" ++ VariableNew ++ RelationshipTypesNew ++ RangeLiteralNew ++ PropertiesNew ++ "]", NewCtx8},
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
    RT = {Left ++ ValueNew ++ Right, NewCtx2},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {PatternElementChainListNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {patternElementChainList, PatternElementChainList}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {NodePatternNew ++ PatternElementChainListNew, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {RelTypeVerticalbarlistNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, {relTypeVerticalbarlist, RelTypeVerticalbarlist}),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {":" ++ RelTypeNew ++ RelTypeVerticalbarlistNew, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% relTypeVerticalbarlist
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {relTypeVerticalbarlist, Values} = ST)
    when is_list(Values) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = lists:foldl(fun(F, {Acc, CtxAcc}) ->
        case F of
            {{relTypeName, _} = RTN, Colon} ->
                ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n F: ~p~n", [F]),
                {SubAcc, CtxAcc1} = pt_to_source(FType, Fun, CtxAcc, Lvl + 1, RTN),
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value_2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value_1New ++ Value_2New, NewCtx4},
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
    RT = {"return " ++ case Distinct of
                           [] -> [];
                           _ -> "distinct "
                       end ++ ReturnBodyNew, NewCtx2},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {LimitNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Limit),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ReturnItemsNew ++ " " ++ LimitNew, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], Skip, []} = ST) ->
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
    {SkipNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Skip),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ReturnItemsNew ++ " " ++ SkipNew, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, [], Skip, Limit} = ST) ->
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
    {SkipNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Skip),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {LimitNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Limit),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {ReturnItemsNew ++ " " ++ SkipNew ++ " " ++ LimitNew, NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, [], []} = ST) ->
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
    {OrderNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Order),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ReturnItemsNew ++ " " ++ OrderNew, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, [], Limit} = ST) ->
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
    {OrderNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Order),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {LimitNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Limit),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {ReturnItemsNew ++ " " ++ OrderNew ++ " " ++ LimitNew, NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, Skip, []} = ST) ->
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
    {OrderNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Order),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {SkipNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Skip),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    RT = {ReturnItemsNew ++ " " ++ OrderNew ++ " " ++ SkipNew, NewCtx6},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {returnBody, ReturnItems, Order, Skip, Limit} = ST) ->
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
    {OrderNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Order),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    {SkipNew, NewCtx5} = pt_to_source(FType, Fun, NewCtx4, Lvl + 1, Skip),
    NewCtx6 = case FType of
                  top_down -> NewCtx5;
                  bottom_up -> Fun(ST, NewCtx5)
              end,
    {LimitNew, NewCtx7} = pt_to_source(FType, Fun, NewCtx6, Lvl + 1, Limit),
    NewCtx8 = case FType of
                  top_down -> NewCtx7;
                  bottom_up -> Fun(ST, NewCtx7)
              end,
    RT = {ReturnItemsNew ++ " " ++ OrderNew ++ " " ++ SkipNew ++ " " ++ LimitNew, NewCtx8},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {VariableNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Variable),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {ExpressionNew ++ " as " ++ VariableNew, NewCtx4},
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
    RT = {Asterik ++ Comma ++ ReturnItemCommalistNew, NewCtx2},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value_2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value_2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value_1New ++ case Operation of
                            [] -> [];
                            _ -> " " ++ Operation ++ " "
                        end ++ Value_2New, NewCtx4},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% singleQuery
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {singleQuery, Value} = ST) ->
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> Start ~p~n ST: ~p~n", [Lvl, ST]),
    NewCtx = case FType of
                 top_down -> Fun(ST, Ctx);
                 bottom_up -> Ctx
             end,
    {ValueNew, NewCtx1} = pt_to_source(FType, Fun, NewCtx, Lvl + 1, {clauseList, Value}),
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    RT = {ValueNew, NewCtx2},
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
    RT = {ValueNew ++ case Type of
                          [] -> [];
                          _ -> " "
                      end ++ Type, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;

% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% standAloneCall
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pt_to_source(FType, Fun, Ctx, Lvl, {standAloneCall, Value1, []} = ST) ->
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
pt_to_source(FType, Fun, Ctx, Lvl, {standAloneCall, Value1, Value2} = ST) ->
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
    {Value2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"Call " ++ Value1New ++ " Yield " ++ Value2New, NewCtx4},
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
    RT = {"union " ++ case length(All) of
                          0 -> [];
                          _ -> All ++ " "
                      end ++ SingleQueryNew, NewCtx2},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {VariableNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Variable),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"unwind " ++ ExpressionNew ++ " as " ++ VariableNew, NewCtx4},
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
    RT = {"with " ++ case Distinct of
                         [] -> [];
                         _ -> "distinct "
                     end ++ ReturnBodyNew, NewCtx2},
    ?debugFmt(?MODULE_STRING ++ ":pt_to_source ===> ~n RT: ~p~n", [RT]),
    RT;
pt_to_source(FType, Fun, Ctx, Lvl, {with, Distinct, ReturnBody, Where} = ST) ->
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
    {WhereNew, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Where),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {"with " ++ case Distinct of
                         [] -> [];
                         _ -> "distinct "
                     end ++ ReturnBodyNew ++ " " ++ WhereNew, NewCtx4},
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
    NewCtx2 = case FType of
                  top_down -> NewCtx1;
                  bottom_up -> Fun(ST, NewCtx1)
              end,
    {Value2New, NewCtx3} = pt_to_source(FType, Fun, NewCtx2, Lvl + 1, Value2),
    NewCtx4 = case FType of
                  top_down -> NewCtx3;
                  bottom_up -> Fun(ST, NewCtx3)
              end,
    RT = {Value1New ++ " As " ++ Value2New, NewCtx4},
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
