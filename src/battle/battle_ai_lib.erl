%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2018 10:36
%%%-------------------------------------------------------------------
-module(battle_ai_lib).
-author("zhengjia").
-include("type.hrl").
-include("proto.hrl").

%% API
-export([create_ai_queue/4,
    loop/2,
    big_skill/2,
    miss_mark/2,
    use_skill/2]).

%%创建ai行为队列
create_ai_queue(Now, StarLv, ID, TarID) ->
    AiAction = data_battle_ai:ai_action(StarLv, ID, TarID),
    create_ai_queue1(Now, AiAction, []).

create_ai_queue1(_, [], AiQueue) ->
    lists:sort(AiQueue);
create_ai_queue1(Now, [{InterVal, F, Args}|AiAction], AiQueue) ->
    create_ai_queue1(Now, AiAction, [{Now + InterVal * C, F, Args}|| C <- lists:seq(1, 180 div InterVal)] ++ AiQueue).

%%loop
-spec loop(Now :: non_neg_integer(), AiQueue :: [term()]) -> NewAiQueue :: [term()].
loop(Now, [{Time, F, Args}|Rest] = AiQueue)->
    case Time =< Now of
        true->
            erlang:apply(?MODULE, F, Args),
            Rest;
        false->
            AiQueue
    end.

%%消除
-spec big_skill(ID :: string(), TarID :: string()) -> no_return().
big_skill(ID, TarID) ->
    BigSkillHp = data_battle:big_skill_hp(),
    {TarPlayer, Msg2} = battle_lib:deduct_hp(TarID, BigSkillHp),
    battle_lib:set_battle_player(TarPlayer),
    {_, Msg4} = battle_lib:judge(ID, TarID),
    player_lib:send_msg_to_client(TarID, [Msg2 | Msg4]).

%%消除失败
-spec miss_mark(ID :: string(), TarID :: string()) -> no_return().
miss_mark(ID, TarID) ->
    MissHp = data_battle:miss_hp(),
    {Player, Msg1} = battle_lib:deduct_hp(ID, MissHp),
    battle_lib:set_battle_player(Player),
    {_, Msg3} = battle_lib:judge(ID, TarID),
    player_lib:send_msg_to_client(TarID, [Msg1 | Msg3]).

%%使用技能
-spec use_skill(TarID :: string(), Skill :: non_neg_integer()) -> no_return().
use_skill(TarID, Skill) ->
    Msg2 = #s_use_skill{id = TarID, skill = Skill},
    player_lib:send_msg_to_client(TarID, [Msg2]).
