%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 五月 2018 05:30
%%%-------------------------------------------------------------------
-module(rank).
-author("zhengjia").
-include("type.hrl").
-include("proto.hrl").
-include("def_player.hrl").

%% API
-export([c_rank_info/2]).

%%请求排行榜
c_rank_info(#c_rank_info{tag = Tag, rank = Rank}, PlayerID) ->
    SelfKey = case Tag of
                "China" ->
                    #player{province = Province} = player_lib:get_player(PlayerID),
                    Province;
                "personal" ->
                    PlayerID;
                _->
                    #player{city = City} = player_lib:get_player(PlayerID),
                    City
            end,
    {PRankSelf, PRankInfo} = gen_server:call({global, rank_manager_srv}, {apply, rank_lib, get_p_rank_info, [Tag, Rank, SelfKey]}),
    Msg = #s_rank_info{tag = Tag, rank_self = PRankSelf, rank_info = PRankInfo},
    {ok, [Msg]}.