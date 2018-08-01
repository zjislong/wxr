%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. 四月 2018 02:59
%%%-------------------------------------------------------------------
-module(single_game_lib).
-author("zhengjia").
-include("type.hrl").
-include("def_public_data.hrl").

%% API
-export([add_score/1]).

%%更新大目标信息
-spec add_score(Score :: non_neg_integer()) -> non_neg_integer().
add_score(Score) ->
    case cache:get(?game_stage, 1) of
        1 ->
            Score1 = cache:get(?single_game_goal_scroe, 0),
            Score2 = Score1 + Score,
            NeedScore = data_single_game:goal_need_score(),
            case Score2 >= NeedScore of
                true ->
                    cache:put(?single_game_goal_scroe, 0),
                    cache:put(?game_stage, 2),
                    0;
                false ->
                    cache:put(?single_game_goal_scroe, Score2),
                    Score2
            end
    end.
