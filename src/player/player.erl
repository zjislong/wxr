%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. 五月 2018 01:07
%%%-------------------------------------------------------------------
-module(player).
-author("zhengjia").
-include("type.hrl").
-include("proto.hrl").
-include("def_player.hrl").
-include("option.hrl").

%% API
-export([c_player_info/2,
    c_buy_pi_fu/2,
    c_change_pi_fu/2,
    c_goal_reward/2,
    c_get_goal_reward/2]).

c_player_info(#c_player_info{}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    Msg = player_lib:s_player_info(Player),
    {ok, [Msg]}.

c_buy_pi_fu(#c_buy_pi_fu{pi_fu = PiFu}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    case lists:member(PiFu, Player#player.pi_fu) of
        true ->
            {ok, [#s_buy_pi_fu{pi_fu = PiFu, result = 0}]};
        false ->
            NeedGold = data_pi_fu:buy_cost(PiFu),
            case player_money_lib:check_deduct(Player, NeedGold) of
                ok ->
                    Player2 = player_money_lib:deduct(Player, NeedGold, ?OPTION_BUY_PIFU, PiFu),
                    Player3 = Player2#player{pi_fu = [PiFu | Player#player.pi_fu]},
                    Msg = player_lib:s_player_info(Player3),
                    {ok, [Msg, #s_buy_pi_fu{pi_fu = PiFu, result = 1}], Player3};
                error ->
                    {ok, [#s_buy_pi_fu{pi_fu = PiFu, result = 0}]}
            end
    end.

c_change_pi_fu(#c_change_pi_fu{pi_fu = PiFu}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    case lists:member(PiFu, Player#player.pi_fu) of
        true ->
            Player2 = Player#player{cur_pi_fu = PiFu},
            Msg = player_lib:s_player_info(Player2),
            {ok, [Msg, #s_change_pi_fu{pi_fu = PiFu, result = 1}], Player2};
        false ->
            {ok, [#s_change_pi_fu{pi_fu = PiFu, result = 0}]}
    end.

c_goal_reward(#c_goal_reward{}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    CanGoalReward = goal_reward_lib:can_goal_reward(Player),
    Msg = #s_goal_reward{can_reward = CanGoalReward, geted_reward = Player#player.geted_goal_reward},
    {ok, [Msg]}.

c_get_goal_reward(#c_get_goal_reward{reward_id = RewardID}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    case goal_reward_lib:get_goal_reward(Player, RewardID) of
        {ok, Player1} ->
            {ok, [#s_get_goal_reward{reward_id = RewardID, result = 1}], Player1};
        error ->
            {ok, [#s_get_goal_reward{reward_id = RewardID, result = 0}]}
    end.
