%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2018 11:39
%%%-------------------------------------------------------------------
-module(data_battle).
-author("zhengjia").

%% API
-export([init_hp/0,
    battle_skill/1,
    match_score/0,
    miss_hp/0,
    big_skill_hp/0,
    star/1,
    status/1,
    fight_time_add_cd/0,
    reward/1]).

%%初始血量
init_hp()->
    100.

%%技能消耗{积分, CD}
battle_skill(1)->
    {20, 10};
battle_skill(2)->
    {30, 10};
battle_skill(3)->
    {40, 10};
battle_skill(_)->
    {100, 10}.

%%消除获得的积分
match_score()->
    20.

%%漏掉扣除的血量
miss_hp()->
    10.

%%大招扣除的血量
big_skill_hp()->
    10.

%%段位有多少颗星星
star(1)->
    1;
star(2)->
    2;
star(3)->
    3;
star(4)->
    5;
star(5)->
    5;
star(6) ->
    5;
star(7) ->
    6;
star(8) ->
    6;
star(9) ->
    6;
star(10) ->
    8;
star(11) ->
    -1.

%%多人模式阶段结束时间
%%{周，星期几，时间}
status(0) ->
    {2, 7, {24, 0, 0}};
status(1) ->
    {1, 1, {5, 0, 0}};
status(2) ->
    {1, 1, {6, 0, 0}}.

%%匹配次数恢复cd
fight_time_add_cd() ->
    7200.

%%赛季奖励
reward(1) ->
    [{gold, 1}];
reward(2) ->
    [{gold, 1}];
reward(3) ->
    [{gold, 1}];
reward(4) ->
    [{gold, 1}];
reward(5) ->
    [{gold, 1}];
reward(6) ->
    [{gold, 1}];
reward(7) ->
    [{gold, 1}];
reward(8) ->
    [{gold, 1}];
reward(9) ->
    [{gold, 1}];
reward(10) ->
    [{gold, 1}];
reward(11) ->
    [{gold, 1}].