-module(player_pi_fu_lib).
-include("type.hrl").
-include("def_player.hrl").
-include("def_public_data.hrl").

-export([check_buy/2]).

%%检查是否可以购买皮肤
-spec check_buy(Player :: #player{}, PiFu :: non_neg_integer()) -> {ok, NeedGold :: [term()]}|{error, non_neg_integer()}.
check_buy(Player, PiFu)->
    case lists:member(PiFu, Player#player.pi_fu) of
        true ->
            {error, 1};
        false ->
            case can_buy(Player, PiFu) of
                true ->
                    NeedGold = data_pi_fu:buy_cost(PiFu),
                    case player_money_lib:check_deduct(Player, NeedGold) of
                        ok ->
                            {ok, NeedGold};
                        error ->
                            {error, 3}
                    end;
                false->
                    {error, 2}
            end
    end.

%%检查福利领取条件是否打成
-spec can_buy(Player :: #player{}, PiFu :: non_neg_integer()) -> boolean().
can_buy(Player, PiFu) ->
    Condition = data_pi_fu:buy_condition(PiFu),
    can_buy1(Player, Condition).

can_buy1(_, []) ->
    true;
can_buy1(Player, [{total_score, Score} | R]) ->
    case public_data:get(?single_game_goal_scroe, 0) >= Score of
        true ->
            can_buy1(Player, R);
        false ->
            false
    end;
can_buy1(Player, [{score, Score} | R]) ->
    case Player#player.score >= Score of
        true ->
            can_buy1(Player, R);
        false ->
            false
    end;
can_buy1(Player, [{gold, Gold} | R]) ->
    case Player#player.gold >= Gold of
        true ->
            can_buy1(Player, R);
        false ->
            false
    end;
can_buy1(Player, [{pi_fu, Num} | R]) ->
    case length(Player#player.pi_fu) >= Num of
        true ->
            can_buy1(Player, R);
        false ->
            false
    end;
can_buy1(Player, [{level, Level} | R]) ->
    case player_lib:exp2level(Player#player.exp) >= Level of
        true ->
            can_buy1(Player, R);
        false ->
            false
    end;
can_buy1(Player, [{title, TitleID} | R]) ->
    case Player#player.titile >= TitleID of
        true ->
            can_buy1(Player, R);
        false ->
            false
    end;
can_buy1(Player, [_ | R]) ->
    can_buy1(Player, R).