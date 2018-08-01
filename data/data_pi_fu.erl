%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 五月 2018 23:11
%%%-------------------------------------------------------------------
-module(data_pi_fu).
-author("zhengjia").

%% API
-export([default_pi_fu/0,
    buy_cost/1]).

%%默认激活的皮肤
default_pi_fu()->
    [0].

%%购买皮肤需要的金币
buy_cost(_)->
    [{gold,1000}].
