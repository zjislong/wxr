%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 四月 2018 11:48
%%%-------------------------------------------------------------------
-author("zhengjia").

%%玩家信息
-record(player,
{
    player_id = 0 :: string()|matchspec_atom(),      %%玩家id
    battle_srv = 0 :: 0|pid()|matchspec_atom(),      %%战斗pid
    is_load_cg = 0 :: 0|1|matchspec_atom(),     %%是否播放开场动画
    is_load_tip = 0 :: 0|1|matchspec_atom(),    %%是否播放新手引导
    is_first_game = 0 :: 0|1|matchspec_atom(),  %%是否第一次单机
    is_load_game_tip = 0 :: 0|1|matchspec_atom(),%%是否播发单人游戏新手引导
    name = "" :: string()|matchspec_atom(),  %%昵称
    head = "" :: string()|matchspec_atom(),  %%头像
    gender = "0" :: string()|matchspec_atom(),    %%性别
    city = "" :: string()|matchspec_atom(),      %%市
    province = "" :: string()|matchspec_atom(),  %%省
    country = "" :: string()|matchspec_atom(),   %%国家
    titile = 0 :: non_neg_integer()|matchspec_atom(),         %%称号
    exp = 0 :: non_neg_integer()|matchspec_atom(),            %%经验
    gold = 0 :: non_neg_integer()|matchspec_atom(),           %%金币
    score = 0 :: non_neg_integer()|matchspec_atom(),          %%积分
    cur_pi_fu = 0 :: non_neg_integer()|matchspec_atom(),      %%激活皮肤
    pi_fu = [] :: [non_neg_integer()]|matchspec_atom(),         %%拥有皮肤
    geted_goal_reward = [] :: [non_neg_integer()]|matchspec_atom(),   %%已经领取过的福利
    star = 0 :: non_neg_integer()|matchspec_atom(),     %%多人模式段位
    fight_count = 10 :: non_neg_integer()|matchspec_atom(), %%多人模式匹配次数
    fight_count_time = 0 :: non_neg_integer()|matchspec_atom(), %%多人模式匹配次数恢复时间
    last_loop_time = 0 :: non_neg_integer()|matchspec_atom(),   %%上一次循环操作时间
    session_key = "" :: string()|matchspec_atom()    %%登录会话的key
}).
