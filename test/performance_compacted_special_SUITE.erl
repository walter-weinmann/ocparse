%%%-------------------------------------------------------------------
%%% File        : performance_compacted_special_SUITE.erl
%%% Description : Test Suite for rule: special.
%%%
%%% Created     : 16.11.2017
%%%-------------------------------------------------------------------
-module(performance_compacted_special_SUITE).

-export([
    all/0,
    end_per_suite/1,
    init_per_suite/1,
    suite/0,
    test_compacted/1
]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - SUITE
%%--------------------------------------------------------------------

suite() ->
    [
        {timetrap, {minutes, 30}}
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS - ALL
%%--------------------------------------------------------------------

all() ->
    [
        test_compacted
    ].

%%--------------------------------------------------------------------
%% TEST CASES
%%--------------------------------------------------------------------

test_compacted(_Config) ->
    {ok, _} = ocparse:source_to_pt("Match ()<-[*]->() Return a"),
    {ok, _} = ocparse:source_to_pt("Match ()<-[:rtn *]->() Return a"),
    {ok, _} = ocparse:source_to_pt("Match ()<-[]->() Return a"),
    {ok, _} = ocparse:source_to_pt("Match ()<-[vn $1]->() Return a"),
    {ok, _} = ocparse:source_to_pt("Match ()<-[vn * $1]->() Return a"),
    {ok, _} = ocparse:source_to_pt("Match ()<-[vn :rtn  $1]->() Return a"),
    {ok, _} = ocparse:source_to_pt("Match ()<-[vn :rtn  *]->() Return a"),
    {ok, _} = ocparse:source_to_pt("Match ()<-[vn]->() Return a"),
    {ok, _} = ocparse:source_to_pt("Match (a) With * Order by 5 Return a"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = Case            When variable_5 Then variable_6                 End"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = Case            When variable_5 Then variable_6 Else variable_7 End"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = Case variable_4 When variable_5 Then variable_6                 End"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = Case variable_4 When variable_5 Then variable_6 Else variable_7 End"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = Exists (Distinct variable_4)"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [              (variable_5) - - (variable_6)            | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [              (variable_5) - - (variable_6) Where True | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [                                               ] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [                                   $parameter_1] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [                             * ..5             ] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [                             * ..5 $parameter_1] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [            :rel_type_name_1                   ] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [            :rel_type_name_1       $parameter_1] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [            :rel_type_name_1 * ..5             ] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [            :rel_type_name_1 * ..5 $parameter_1] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ *     ] - (variable_6) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ * ..  ] - (variable_6) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ * ..5 ] - (variable_6) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ *1    ] - (variable_6) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ *1..  ] - (variable_6) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ *1..5 ] - (variable_6) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ variable_6                                    ] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ variable_6                        $parameter_1] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ variable_6                  * ..5             ] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ variable_6                  * ..5 $parameter_1] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ variable_6 :rel_type_name_1                   ] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ variable_6 :rel_type_name_1       $parameter_1] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ variable_6 :rel_type_name_1 * ..5             ] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ (variable_5) - [ variable_6 :rel_type_name_1 * ..5 $parameter_1] - (variable_7) | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ variable_4 = (variable_5) - - (variable_6)            | False]"),
    {ok, _} = ocparse:source_to_pt("Merge variable_1 = (variable_2) On Create Set variable_3 = [ variable_4 = (variable_5) - - (variable_6) Where True | False]"),
    ok.
