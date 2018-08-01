%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 五月 2018 06:19
%%%-------------------------------------------------------------------
-module(proto_route).
-author("zhengjia").
-include("proto.hrl").

%% API
-export([route/1]).

route(MsgID) ->
    msgID2mod(MsgID div 100).

msgID2mod(10) ->
    login;
msgID2mod(11) ->
    player;
msgID2mod(12) ->
    single_game;
msgID2mod(13) ->
    rank;
msgID2mod(14) ->
    battle;
msgID2mod(_) ->
    null.