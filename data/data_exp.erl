%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 四月 2018 10:14
%%%-------------------------------------------------------------------
-module(data_exp).
-author("zhengjia").

%% API
-export([level2exp/1]).

level2exp(1)->
    0;
level2exp(2)->
    100;
level2exp(3)->
    500;
level2exp(4)->
    1200;
level2exp(5)->
    2100;
level2exp(6)->
    3200;
level2exp(7)->
    5000;
level2exp(8)->
    8000;
level2exp(9)->
    12000;
level2exp(10)->
    18000;
level2exp(11)->
    25000;
level2exp(12)->
    33000;
level2exp(13)->
    42000;
level2exp(14)->
    52000;
level2exp(15)->
    65000;
level2exp(16)->
    80000;
level2exp(17)->
    100000;
level2exp(18)->
    130000;
level2exp(19)->
    170000;
level2exp(20)->
    220000;
level2exp(21)->
    280000;
level2exp(22)->
    350000;
level2exp(23)->
    420000;
level2exp(24)->
    500000;
level2exp(25)->
    600000;
level2exp(26)->
    720000;
level2exp(27)->
    850000;
level2exp(28)->
    1000000;
level2exp(29)->
    1200000;
level2exp(30)->
    1500000;
level2exp(_)->
    2000000.
