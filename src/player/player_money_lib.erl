%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 02. 五月 2018 23:12
%%%-------------------------------------------------------------------
-module(player_money_lib).
-author("zhengjia").
-include("type.hrl").
-include("def_player.hrl").

%% API
-export([add/4,
    deduct/4,
    check_add/2,
    check_deduct/2]).

%%加
-spec add(Player :: #player{}, Reward :: [term()], Type :: non_neg_integer(), Data :: non_neg_integer()) -> NewPlayer :: #player{}.
add(Player, [], _, _) ->
    Player;
add(Player, [{gold, Value} | R], Type, Data) when Value > 0 ->
    Player2 = Player#player{gold = Player#player.gold + Value},
    add(Player2, R, Type, Data);
add(Player, [{exp, Value} | R], Type, Data) when Value > 0 ->
    Player2 = Player#player{exp = Player#player.exp + Value},
    add(Player2, R, Type, Data);
add(Player, [{score, Value} | R], Type, Data) when Value > 0 ->
    Player2 = Player#player{score = Player#player.score + Value},
    add(Player2, R, Type, Data);
add(Player, [_ | R], Type, Data) ->
    add(Player, R, Type, Data).

%%减
-spec deduct(Player :: #player{}, Reward :: [term()], Type :: non_neg_integer(), Data :: non_neg_integer()) -> NewPlayer :: #player{}.
deduct(Player, [], _, _) ->
    Player;
deduct(Player, [{gold, Value} | R], Type, Data) when Value > 0 ->
    Player2 = Player#player{gold = Player#player.gold - Value},
    deduct(Player2, R, Type, Data);
deduct(Player, [{exp, Value} | R], Type, Data) when Value > 0 ->
    Player2 = Player#player{exp = Player#player.exp - Value},
    deduct(Player2, R, Type, Data);
deduct(Player, [{score, Value} | R], Type, Data) when Value > 0 ->
    Player2 = Player#player{score = Player#player.score - Value},
    deduct(Player2, R, Type, Data);
deduct(Player, [_ | R], Type, Data) ->
    deduct(Player, R, Type, Data).

%%检查加
-spec check_add(Player :: #player{}, Reward :: [term()]) -> ok|error.
check_add(_Player, []) ->
    ok;
check_add(Player, [{_, Value} | R]) ->
    case Value > 0 of
        true ->
            check_add(Player, R);
        false ->
            error
    end;
check_add(Player, [_ | R]) ->
    check_add(Player, R).

%%检查减
-spec check_deduct(Player :: #player{}, Reward :: [term()]) -> ok|error.
check_deduct(_Player, []) ->
    ok;
check_deduct(Player, [{gold, Value} | R]) ->
    case Value > 0 andalso Player#player.gold > Value of
        true ->
            check_deduct(Player, R);
        false ->
            error
    end;
check_deduct(Player, [{exp, Value} | R]) ->
    case Value > 0 andalso Player#player.exp > Value of
        true ->
            check_deduct(Player, R);
        false ->
            error
    end;
check_deduct(Player, [{score, Value} | R]) ->
    case Value > 0 andalso Player#player.score > Value of
        true ->
            check_deduct(Player, R);
        false ->
            error
    end;
check_deduct(Player, [_ | R]) ->
    check_deduct(Player, R).
