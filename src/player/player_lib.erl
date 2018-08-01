%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 四月 2018 09:52
%%%-------------------------------------------------------------------
-module(player_lib).
-author("zhengjia").
-include("type.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("def_player.hrl").
-include("def_ets.hrl").
-include("proto.hrl").

%% API
-export([get_player/1,
    exp2level/1,
    get_skills/1,
    save_player/1,
    s_player_info/1,
    update_title/2,
    send_msg_to_client/2,
    loop/2]).

-spec get_player(PlayerID :: string()) -> #player{}.
get_player(PlayerID) ->
    case lbm_kv:get(player, PlayerID) of
        {ok, [{_, Player}]} ->
            Player;
        _ ->
            load_player(PlayerID)
    end.

-spec exp2level(Exp :: non_neg_integer()) -> non_neg_integer().
exp2level(Exp) ->
    exp2level_1(Exp, 1).

exp2level_1(Exp, Level) ->
    case data_exp:level2exp(Level) of
        NeedExp when Exp >= NeedExp ->
            exp2level_1(Exp - NeedExp, Level + 1);
        _ ->
            Level
    end.

-spec get_skills(Player :: #player{}) -> [non_neg_integer()].
get_skills(Player) ->
    Level = exp2level(Player#player.exp),
    SkillList = data_skill:skill_list(),
    [Skill || Skill <- SkillList, Level >= data_skill:skill_active_level(Skill)].

%%加载玩家信息
-spec load_player(PlayerID :: string()) -> #player{}.
load_player(PlayerID) ->
    Player = case db:query("select * from GPlayer where PlayerID = ?", [PlayerID]) of
                 [[_,
                    IsLoadCg,
                    IsLoadTip,
                    IsFirstGame,
                    IsLoadGameTip,
                    Title,
                    Name,
                    Head,
                    Gender,
                    City,
                    Province,
                    Country,
                    Exp,
                    Gold,
                    Score,
                    CurPiFu,
                    PiFu,
                    GetedGoalReward,
                    Star,
                    FightCount,
                    FightCountTime,
                    LoopTime]] ->
                     #player{player_id = PlayerID,
                         is_load_cg = IsLoadCg,
                         is_load_tip = IsLoadTip,
                         is_first_game = IsFirstGame,
                         is_load_game_tip = IsLoadGameTip,
                         titile = Title,
                         name = misc:bitstring_to_term(Name, ""),
                         head = misc:bitstring_to_term(Head, ""),
                         gender = Gender,
                         city = misc:bitstring_to_term(City, ""),
                         province = misc:bitstring_to_term(Province, ""),
                         country = misc:bitstring_to_term(Country, ""),
                         exp = Exp,
                         gold = Gold,
                         score = Score,
                         cur_pi_fu = CurPiFu,
                         pi_fu = misc:bitstring_to_term(PiFu, data_pi_fu:default_pi_fu()),
                         geted_goal_reward = misc:bitstring_to_term(GetedGoalReward, []),
                         star = Star,
                         fight_count = FightCount,
                         fight_count_time = FightCountTime,
                         last_loop_time = LoopTime};
                 _ ->
                     #player{player_id = PlayerID, pi_fu = data_pi_fu:default_pi_fu()}
             end,
    Player.

-spec save_player(Player :: #player{}) -> term().
save_player(Player) ->
    #player{
        player_id = PlayerID,
        is_load_cg = IsLoadCg,
        is_load_tip = IsLoadTip,
        is_first_game = IsFirstGame,
        is_load_game_tip = IsLoadGameTip,
        titile = Title,
        name = Name,
        head = Head,
        gender = Gender,
        city = City,
        province = Province,
        country = Country,
        exp = Exp,
        gold = Gold,
        score = Score,
        cur_pi_fu = CurPiFu,
        pi_fu = PiFu,
        geted_goal_reward = GetedGoalReward,
        star = Star,
        fight_count = FightCount,
        fight_count_time = FightCountTime,
        last_loop_time = LoopTime} = Player,
    db:query("replace into GPlayer values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        [PlayerID,
            IsLoadCg,
            IsLoadTip,
            IsFirstGame,
            IsLoadGameTip,
            Title,
            Name,
            Head,
            Gender,
            City,
            Province,
            Country,
            Exp,
            Gold,
            Score,
            CurPiFu,
            PiFu,
            GetedGoalReward,
            Star,
            FightCount,
            FightCountTime,
            LoopTime]).

%%玩家信息s_player_info
-spec s_player_info(Player :: #player{}) -> #s_player_info{}.
s_player_info(Player) ->
    ActiveSkills = player_lib:get_skills(Player),
    Msg = #s_player_info{
        total_exp = Player#player.exp,
        cur_pi_fu = Player#player.cur_pi_fu,
        have_pi_fu = Player#player.pi_fu,
        skills = ActiveSkills,
        gold = Player#player.gold},
    Msg.

%%更新称号
-spec update_title(Player :: #player{}, Score :: non_neg_integer()) -> #player{}.
update_title(Player, Score) ->
    Title = score2title(Score),
    Player#player{titile = max(Title, Player#player.titile)}.

score2title(Score) ->
    TitleList = data_title:title_list(),
    score2title1(Score, TitleList, 0).

score2title1(_, [], Title) ->
    Title;
score2title1(Score, [Title | R], CurTitle) ->
    case data_title:title(Title) =< Score of
        true ->
            score2title1(Score, R, Title);
        false ->
            CurTitle
    end.

%%发送协议数据
-spec send_msg_to_client(PlayerID::string(), Msgs::[term()])->no_return().
send_msg_to_client(PlayerID, Msgs) ->
    global:send({player, PlayerID}, {msg, Msgs}).

%%循环
loop(PlayerID, LoopTime) ->
    battle_lib:recover_fight_count(PlayerID, LoopTime).