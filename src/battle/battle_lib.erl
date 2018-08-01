%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2018 10:36
%%%-------------------------------------------------------------------
-module(battle_lib).
-author("zhengjia").
-include("type.hrl").
-include("def_battle.hrl").
-include("def_public_data.hrl").
-include("proto.hrl").
-include("def_player.hrl").
-include("def_ets.hrl").

%% API
-export([get_battle_player/1,
    set_battle_player/1,
    new_battle_id/0,
    battle_start/2,
    catch_mark/4,
    miss_mark/2,
    use_skill/3,
    give_up/2,
    deduct_hp/2,
    judge/2,
    star/1,
    recover_fight_count/2,
    update_star_callback/2,
    start_battle_srv/1,
    start_battle_srv_callback/1]).

%%获取战斗玩家
-spec get_battle_player(ID :: string()) -> #battle_player{}.
get_battle_player(ID) ->
    case get(?BATTLE_PLAYER(ID)) of
        #battle_player{} = Player ->
            Player;
        _ ->
            #battle_player{
                id = ID,
                hp = data_battle:init_hp()
            }
    end.

%%更新战斗玩家
-spec set_battle_player(Player :: #battle_player{}) -> #battle_player{}|undefined.
set_battle_player(Player) ->
    put(?BATTLE_PLAYER(Player#battle_player.id), Player).

%%生成battle_id
-spec new_battle_id() -> non_neg_integer().
new_battle_id() ->
    public_data:add(?battle_id).

%%战斗开始
-spec battle_start(ID1::string(), ID2::string()) -> ok|error.
battle_start(ID1, [$a,$i|_]=ID2) ->
    #player{star = Star2, name = Name2, head = Head2, gender = Gender2} = player_lib:get_player(ID2),
    send_msg_to_client(ID1, [#s_battle_start{target_id = ID2,
     hp = data_battle:init_hp(),
      target_star = Star2,
      target_name = Name2,
      target_head = Head2,
      target_gender = Gender2}]);
battle_start(ID1, ID2) ->
    #player{star = Star1, name = Name1, head = Head1, gender = Gender1} = player_lib:get_player(ID1),
    #player{star = Star2, name = Name2, head = Head2, gender = Gender2} = player_lib:get_player(ID2),
    send_msg_to_client(ID1, [#s_battle_start{target_id = ID2,
     hp = data_battle:init_hp(),
      target_star = Star2,
      target_name = Name2,
      target_head = Head2,
      target_gender = Gender2}]),
    send_msg_to_client(ID2, [#s_battle_start{target_id = ID1,
     hp = data_battle:init_hp(),
      target_star = Star1,
      target_name = Name1,
      target_head = Head1,
      target_gender = Gender1}]).

%%消除ok|error
-spec catch_mark(ID :: string(), TarID :: string(), Combo :: non_neg_integer(), BigSkill :: 0|1) -> ok|error.
catch_mark(ID, TarID, Combo, BigSkill) ->
    AddScore = Combo * data_battle:match_score(),
    BigSkillHp = BigSkill * data_battle:big_skill_hp(),
    {Player, Msg1} = add_score(ID, AddScore),
    {TarPlayer, Msg2} = deduct_hp(TarID, BigSkillHp),
    set_battle_player(Player),
    set_battle_player(TarPlayer),
    {Msg3, Msg4} = judge(ID, TarID),
    send_msg_to_client(ID, [Msg1, Msg2 | Msg3]),
    send_msg_to_client(TarID, [Msg2 | Msg4]).

%%消除失败
-spec miss_mark(ID :: string(), TarID :: string()) -> ok|error.
miss_mark(ID, TarID) ->
    MissHp = data_battle:miss_hp(),
    {Player, Msg1} = deduct_hp(ID, MissHp),
    set_battle_player(Player),
    {Msg2, Msg3} = judge(ID, TarID),
    send_msg_to_client(ID, [Msg1 | Msg2]),
    send_msg_to_client(TarID, [Msg1 | Msg3]).

%%使用技能
-spec use_skill(ID :: string(), TarID :: string(), Skill :: non_neg_integer()) -> ok|error.
use_skill(ID, TarID, Skill) ->
    Player = get_battle_player(ID),
    SkillCD = case Skill of
                  1 ->
                      Player#battle_player.skill_cd1;
                  2 ->
                      Player#battle_player.skill_cd2;
                  3 ->
                      Player#battle_player.skill_cd3
              end,
    Now = misc:unixtime(),
    case Now > SkillCD of
        true ->
            {DeductScore, CD} = data_battle:battle_skill(Skill),
            case add_score(Player, -DeductScore) of
                {Player1, Msg1} ->
                    Player2 = case Skill of
                                  1 ->
                                      Player1#battle_player{skill_cd1 = Now + CD};
                                  2 ->
                                      Player1#battle_player{skill_cd2 = Now + CD};
                                  3 ->
                                      Player1#battle_player{skill_cd3 = Now + CD}
                              end,
                    Msg2 = #s_use_skill{id = TarID, skill = Skill},
                    set_battle_player(Player2),
                    send_msg_to_client(ID, [Msg1, Msg2]),
                    send_msg_to_client(TarID, [Msg2]);
                error ->
                    lager:info("skill score error~n", []),
                    error
            end;
        false ->
            lager:info("skill cd error~n", []),
            error
    end.

%%断线认输
-spec give_up(ID :: string(), TarID :: string()) -> ok|error.
give_up(ID, TarID) ->
    Player = get_battle_player(ID),
    set_battle_player(Player#battle_player{hp = 0}),
    {Msg1, Msg2} = judge(ID, TarID),
    send_msg_to_client(ID, Msg1),
    send_msg_to_client(TarID, Msg2).

%%加积分
-spec add_score(Player :: string()|#battle_player{}, AddScore :: integer()) -> {#battle_player{}, #s_score_change{}}|error.
add_score(#battle_player{} = Player, AddScore) ->
    case Player#battle_player.score + AddScore >= 0 of
        true ->
            Player1 = Player#battle_player{score = Player#battle_player.score + AddScore},
            {Player1, #s_score_change{score = Player1#battle_player.score}};
        false ->
            error
    end;
add_score(ID, AddScore) ->
    Player = get_battle_player(ID),
    add_score(Player, AddScore).

%%减血量
-spec deduct_hp(Player :: string()|#battle_player{}, Hp :: non_neg_integer()) -> {#battle_player{}, #s_hp_change{}}.
deduct_hp(#battle_player{} = Player, Hp) ->
    Player1 = Player#battle_player{hp = max(0, Player#battle_player.hp - Hp)},
    {Player1, #s_hp_change{id = Player1#battle_player.id, hp = Player1#battle_player.hp}};
deduct_hp(ID, Hp) ->
    Player = get_battle_player(ID),
    deduct_hp(Player, Hp).

%%判输赢
-spec judge(ID1 :: string(), ID2 :: string()) -> {[#s_battle_end{}], [#s_battle_end{}]}.
judge(ID1, ID2) ->
    Player1 = get_battle_player(ID1),
    Player2 = get_battle_player(ID2),
    case Player1#battle_player.hp =< 0 of
        true ->
            gen_server:cast(self(), stop),
            Star1 = update_star(ID1, 0),
            Star2 = update_star(ID2, 1),
            {[#s_battle_end{battle_result = 0, star = Star1}], [#s_battle_end{battle_result = 1, star = Star2}]};
        false ->
            case Player2#battle_player.hp =< 0 of
                true ->
                    gen_server:cast(self(), stop),
                    Star1 = update_star(ID1, 1),
                    Star2 = update_star(ID2, 0),
                    {[#s_battle_end{battle_result = 1, star = Star1}], [#s_battle_end{battle_result = 0, star = Star2}]};
                false ->
                    {[], []}
            end
    end.

%%更新段位
-spec update_star(ID :: string(), Win :: 0|1) -> non_neg_integer().
update_star([$a,$i|_], _) ->
    0;
update_star(ID, 0) ->
    Player = player_lib:get_player(ID),
    {_, Star} = star(Player#player.star),
    case Star of
        0 ->
            Player#player.star;
        _ ->
            rank_lib:update("pvp", ID, {add, -1}),
            global:send({player, ID}, {apply_append_player, ?MODULE, update_star_callback, [-1]}),
            Player#player.star - 1
    end;
update_star(ID, 1) ->
    Player = player_lib:get_player(ID),
    rank_lib:update("pvp", ID, {add, 1}),
    global:send({player, ID}, {apply_append_player, ?MODULE, update_star_callback, [1]}),
    Player#player.star + 1.

update_star_callback(Player, Change)->
    {ok, [], Player#player{star = Player#player.star + Change}}.

%%星星数转换为{段位，剩余星星}形式
-spec star(Star :: non_neg_integer()) -> {non_neg_integer(), non_neg_integer()}.
star(Star) ->
    star1(Star, 1).

star1(Star, Lv) ->
    case data_battle:star(Lv) of
        -1 ->
            {Lv, Star};
        NeedStar when NeedStar > Star ->
            {Lv, Star};
        NeedStar ->
            star1(Star - NeedStar, Lv + 1)
    end.

%%恢复匹配次数
-spec recover_fight_count(Player :: string(), LoopTime :: non_neg_integer()) -> ok|skip.
recover_fight_count(PlayerID, LoopTime) ->
    Player = player_lib:get_player(PlayerID),
    case Player#player.fight_count_time =/= 0 andalso Player#player.fight_count_time =< LoopTime of
        true ->
            Cd = data_battle:fight_time_add_cd(),
            Max = data_battle:max_fight_count(),
            FightCount = max(Max, Player#player.fight_count + 1 + trunc((LoopTime - Player#player.fight_count_time) / Cd)),
            FightCountTime = case FightCount >= Max of
                                 true ->
                                     0;
                                 false ->
                                     LoopTime + data_battle:fight_time_add_cd()
                             end,
            Player1 = Player#player{fight_count_time = FightCountTime, fight_count = FightCount},
            gen_server:cast({global, battle_master_srv}, {send_battle_info, Player1}),
            player_lib:put_player(Player1),
            ok;
        false ->
            skip
    end.

send_msg_to_client([$a,$i|_], _)->
    ok;
send_msg_to_client(ID, Msgs)->
    player_lib:send_msg_to_client(ID, Msgs).

%%在第一个玩家所在节点的battle_sup下启动battle_srv
-spec start_battle_srv(Args :: [string(),...]) -> no_return().
start_battle_srv([ID|_]=Args) ->
    global:send({player, ID}, {apply, ?MODULE, start_battle_srv_callback, [Args]}).

-spec start_battle_srv_callback(Args :: [string(),...]) -> ok.
start_battle_srv_callback(Args) ->
    supervisor:start_child(battle_sup, Args),
    ok.