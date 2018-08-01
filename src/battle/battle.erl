%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2018 10:27
%%%-------------------------------------------------------------------
-module(battle).
-author("zhengjia").
-include("type.hrl").
-include("def_ets.hrl").
-include("def_player.hrl").
-include("def_battle.hrl").
-include("proto.hrl").

%% API
-export([c_friend_battle_invite/2,
    c_match_battle/2,
    c_catch_mark/2,
    c_miss_mark/2,
    c_use_skill/2,
    c_battle_info/2,
    c_cancel_match/2]).

c_friend_battle_invite(#c_friend_battle_invite{}, PlayerID) ->
    gen_server:cast({global, battle_master_srv}, {join_match, PlayerID, PlayerID}),
    {ok, [#s_friend_battle_invite{result = 1, invite_code = PlayerID}]}.
c_match_battle(#c_match_battle{}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    {StarLv, _} = battle_lib:star(Player#player.star),
    gen_server:cast({global, battle_master_srv}, {join_match, Player#player.player_id, StarLv}),
    Max = data_battle:max_fight_count(),
    case Player#player.fight_count of
        Max ->
            {ok, [], Player#player{fight_count = Max - 1, fight_count_time = misc:unixtime() + data_battle:fight_time_add_cd()}};
        Count ->
            {ok, [], Player#player{fight_count = Count - 1}}
    end.
c_catch_mark(#c_catch_mark{combo = Combo, big_skill = BigSkill}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    case is_pid(Player#player.battle_srv) of
        true ->
            gen_server:cast(Player#player.battle_srv, {catch_mark, PlayerID, Combo, BigSkill});
        false ->
            skip
    end,
    {ok, []}.
c_miss_mark(#c_miss_mark{}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    case is_pid(Player#player.battle_srv) of
        true ->
            gen_server:cast(Player#player.battle_srv, {miss_mark, PlayerID});
        false ->
            skip
    end,
    {ok, []}.
c_use_skill(#c_use_skill{skill = Skill}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    case is_pid(Player#player.battle_srv) of
        true ->
            gen_server:cast(Player#player.battle_srv, {use_skill, PlayerID, Skill});
        false ->
            skip
    end,
    {ok, []}.
c_battle_info(#c_battle_info{}, PlayerID) ->
    gen_server:cast({global, battle_master_srv}, {send_battle_info, PlayerID}),
    {ok, []}.
c_cancel_match(_, PlayerID) ->
    gen_server:cast({global, battle_master_srv}, {cancel_match, PlayerID}),
    Player = player_lib:get_player(PlayerID),
    case is_pid(Player#player.battle_srv) of
        true ->
            gen_server:cast(Player#player.battle_srv, {give_up, PlayerID});
        false ->
            skip
    end,
    {ok, []}.