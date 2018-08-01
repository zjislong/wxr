%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 四月 2018 11:24
%%%-------------------------------------------------------------------
-module(single_game).
-author("zhengjia").
-include("type.hrl").
-include("proto.hrl").
-include("def_player.hrl").
-include("def_public_data.hrl").
-include("option.hrl").

%% API
-export([c_single_game/2,
    c_single_game_end/2,
    c_single_game_tip/2]).

c_single_game(#c_single_game{}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    ActiveSkills = player_lib:get_skills(Player),
    {[RandomSkill], _} = misc:random_list(ActiveSkills, 1),
    Stage = public_data:get(?game_stage, 1),
    Msg = #s_single_game{stage = Stage,
        skill = RandomSkill,
        is_load_tip = Player#player.is_load_game_tip,
        cur_pi_fu = Player#player.cur_pi_fu},
    {ok, [Msg]}.

c_single_game_end(#c_single_game_end{score = S, exp = Exp, gold = Gold}, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    IsFirstGame1 = max(1, Player#player.is_first_game),
    Player1 = Player#player{is_first_game = IsFirstGame1},
    Player2 = player_money_lib:add(Player1, [{exp, Exp}, {gold, Gold}, {score, S}], ?OPTION_SINGLE_GAME_REWARD, 0),
    Player3 = player_lib:update_title(Player2, S),
    case S > 0 of
        true ->
            rank_lib:update(Player#player.country,Player#player.province,{add,S}),
            rank_lib:update(Player#player.province,Player#player.city,{add,S}),
            rank_lib:update("personal",Player#player.player_id,{replace,S});
        false ->
            skip
    end,
    Msg = #s_single_game_end{
        result = 1,
        is_first = IsFirstGame1,
        total_score = Player2#player.score,
        total_exp = Player2#player.exp},
    NewS = single_game_lib:add_score(S),
    Msg3 = #s_globle_goal{save_total = NewS, need_save = data_single_game:goal_need_score()},
    Msg2 = player_lib:s_player_info(Player3),
    {ok, [Msg, Msg2, Msg3], Player3}.

c_single_game_tip(_, PlayerID) ->
    Player = player_lib:get_player(PlayerID),
    {ok, [], Player#player{is_load_game_tip = 1}}.
