%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 协同目标
%%% @end
%%% Created : 05. 五月 2018 09:59
%%%-------------------------------------------------------------------
-module(data_goal_reward).
-author("zhengjia").

%% API
-export([goal_list/0,
    goal_condition/1,
    goal_reward/1]).

goal_list() ->
    [1, 2, 3, 4, 5].

goal_condition(1) ->
    [{total_score, 99999}, {score, 100}];
goal_condition(2) ->
    [{total_score, 999999}, {title, 5}];
goal_condition(3) ->
    [{total_score, 9999999}, {gold, 100}];
goal_condition(4) ->
    [{total_score, 99999999}, {pi_fu, 4}];
goal_condition(5) ->
    [{total_score, 999999999}, {level, 12}].

goal_reward(_) ->
    [].
