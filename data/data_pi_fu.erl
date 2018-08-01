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
    buy_cost/1,
    buy_condition/1]).

%%默认激活的皮肤
default_pi_fu()->
    [0].

%%购买皮肤需要的金币
%%骑士
buy_cost(0)->
	[];
%%弓箭女
buy_cost(1)->
	[{gold,1000}];
%%宅女
buy_cost(2)->
	[{gold,2000}];
%%时尚男
buy_cost(3)->
	[{gold,2000}];
%%大力士
buy_cost(4)->
	[{gold,3000}];
%%护士女
buy_cost(5)->
	[{gold,3000}];
%%扫把头
buy_cost(6)->
	[{gold,5000}];
%%足球运动员
buy_cost(7)->
	[{gold,10000}];
%%性感女
buy_cost(8)->
	[{gold,20000}];
%%乖乖女
buy_cost(9)->
	[{gold,30000}];
%%悟空男
buy_cost(10)->
	[{gold,50000}];
%%刀客
buy_cost(11)->
	[{gold,100000}].

%%购买皮肤需要的条件
%%{score, X}:个人解救人数到达X人
%%{title, X}:获得称号X
%%{level, X}:等级达到X
%%骑士
buy_condition(0)->
	[];
%%弓箭女
buy_condition(1)->
	[];
%%宅女
buy_condition(2)->
	[];
%%时尚男
buy_condition(3)->
	[];
%%大力士
buy_condition(4)->
	[{title, 4}];
%%护士女
buy_condition(5)->
	[{title, 5}];
%%扫把头
buy_condition(6)->
	[{score, 1000}];
%%足球运动员
buy_condition(7)->
	[{title, 8}];
%%性感女
buy_condition(8)->
	[{title, 9}];
%%乖乖女
buy_condition(9)->
	[{level, 15}];
%%悟空男
buy_condition(10)->
	[{score, 100000}];
%%刀客
buy_condition(11)->
	[{level, 20}, {title, 10}].
