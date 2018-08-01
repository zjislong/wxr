%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. 五月 2018 10:06
%%%-------------------------------------------------------------------
-module(reward_lib).
-author("zhengjia").
-include("type.hrl").
-include("def_player.hrl").

%% API
-export([reward/4]).

%%发放奖励
reward(#player{} = Player, Reward, Type, Data) ->
    case player_money_lib:check_add(Player, Reward) of
        ok ->
            Player1 = player_money_lib:add(Player, Reward, Type, Data),
            Msg = player_lib:s_player_info(Player1),
            {ok, [Msg], Player1};
        error ->
            {error, "wrong reward"}
    end;
reward(ID, Reward, Type, Data) ->
    global:send({player, ID}, {apply_append_player, ?MODULE, reward, [Reward, Type, Data]}).
