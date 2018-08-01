%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. 五月 2018 09:51
%%%-------------------------------------------------------------------
-module(goal_reward_lib).
-author("zhengjia").
-include("type.hrl").
-include("def_public_data.hrl").
-include("def_player.hrl").

%% API
-export([can_goal_reward/1,
    get_goal_reward/2]).

%%获取可以领取的福利
-spec can_goal_reward(Player :: #player{}) -> [non_neg_integer()].
can_goal_reward(Player) ->
    [RewardID || RewardID <- data_goal_reward:goal_list(), can_get(Player, RewardID)].

%%领取福利
-spec get_goal_reward(Player :: #player{}, RewardID :: non_neg_integer()) -> {ok, #player{}}|error.
get_goal_reward(Player, RewardID) ->
    CanGoalReward = can_goal_reward(Player),
    case lists:member(RewardID, CanGoalReward) andalso not lists:member(RewardID, Player#player.geted_goal_reward) of
        true ->
            %%todo do_rewrad
            {ok, Player#player{geted_goal_reward = [RewardID | Player#player.geted_goal_reward]}};
        false ->
            error
    end.

%%检查福利领取条件是否打成
-spec can_get(Player :: #player{}, RewardID :: non_neg_integer()) -> boolean().
can_get(Player, RewardID) ->
    Condition = data_goal_reward:goal_condition(RewardID),
    can_get1(Player, Condition).

can_get1(_, []) ->
    true;
can_get1(Player, [{total_score, Score} | R]) ->
    case public_data:get(?single_game_goal_scroe, 0) >= Score of
        true ->
            can_get1(Player, R);
        false ->
            false
    end;
can_get1(Player, [{score, Score} | R]) ->
    case Player#player.score >= Score of
        true ->
            can_get1(Player, R);
        false ->
            false
    end;
can_get1(Player, [{gold, Gold} | R]) ->
    case Player#player.gold >= Gold of
        true ->
            can_get1(Player, R);
        false ->
            false
    end;
can_get1(Player, [{pi_fu, Num} | R]) ->
    case length(Player#player.pi_fu) >= Num of
        true ->
            can_get1(Player, R);
        false ->
            false
    end;
can_get1(Player, [{level, Level} | R]) ->
    case player_lib:exp2level(Player#player.exp) >= Level of
        true ->
            can_get1(Player, R);
        false ->
            false
    end;
can_get1(Player, [{title, TitleID} | R]) ->
    case Player#player.titile >= TitleID of
        true ->
            can_get1(Player, R);
        false ->
            false
    end;
can_get1(Player, [_ | R]) ->
    can_get1(Player, R).