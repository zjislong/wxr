%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 四月 2018 11:18
%%%-------------------------------------------------------------------
-module(login).
-author("zhengjia").
-include("type.hrl").
-include("proto.hrl").
-include("def_player.hrl").
-include("def_public_data.hrl").

%% API
-export([c_login/2,
    c_heart/2,
    c_finish_cg/2,
    c_finish_tip/2]).

c_login(Clogin, _) ->
    Player = player_lib:get_player(Clogin#c_login.player_id),
    Msg1 = #s_login{player_id = Clogin#c_login.player_id,
        is_load_cg = Player#player.is_load_cg,
        is_load_tip = Player#player.is_load_tip},
    Msg3 = #s_globle_goal{save_total = cache:get(?single_game_goal_scroe, 0),
        need_save = data_single_game:goal_need_score()},
    Msg2 = player_lib:s_player_info(Player),
    Player1 = Player#player{
        name = Clogin#c_login.name,
        head = Clogin#c_login.head,
        gender = Clogin#c_login.gender,
        city = Clogin#c_login.city,
        province = Clogin#c_login.province,
        country = Clogin#c_login.country},
    {ok, [Msg1, Msg2, Msg3], Player1}.

c_heart(_, _) ->
    {ok, [#s_heart{}]}.

c_finish_cg(_, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    {ok, [], Player#player{is_load_cg = 1}}.

c_finish_tip(_, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    {ok, [], Player#player{is_load_tip = 1}}.
