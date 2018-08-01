%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 四月 2018 10:20
%%%-------------------------------------------------------------------
-module(data_skill).
-author("zhengjia").

%% API
-export([skill_list/0,
    skill_active_level/1]).

skill_list()->
    [0,1,2,3,4].

skill_active_level(0)->
    1;
skill_active_level(1)->
    2;
skill_active_level(4)->
    3;
skill_active_level(2)->
    5;
skill_active_level(3)->
    12.