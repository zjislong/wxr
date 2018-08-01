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
c_rank_info(#c_rank_info{tag = Tag, rank = Rank}, _) ->
    PRankInfo = gen_server:call({global, rank_manager_srv}, {mfa, rank_lib, get_p_rank_info, [Tag, Rank]}),
    Msg = #s_rank_info{tag = Tag, rank_info = PRankInfo},
    {ok, [Msg]}.