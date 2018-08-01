-module(player_handler).
-behavior(cowboy_websocket).
-include("proto.hrl").
-export([init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

init(Req, _State) ->
    {cowboy_websocket, Req, #{}, #{idle_timeout => 20000}}.

websocket_init(_State) ->
    {ok, #{pid => self()}}.

%%获取player_id直接处理
websocket_handle({binary, <<1001:16, Length:32, Binary:Length/binary>>}, State) ->
    #c_get_player_id{code = Code} = proto:decode_msg(Binary, c_get_player_id),
    case login_lib:get_player_id(Code) of
        {PlyaerID, SessionKey} ->
            Msg = #s_get_player_id{player_id = PlyaerID},
            {reply, [{binary, proto_lib:encode_msg(Msg)}], State#{session_key => SessionKey}};
        error ->
            {stop, State}
    end;
%%登录直接处理
websocket_handle({binary, <<1003:16, Length:32, Binary:Length/binary>>}, State) ->
    Node = node_pool:get_node(player),
    case supervisor:start_child({player_sup, Node}, [self(), Binary]) of
        {ok, PlayerPid} ->
            {ok, State#{player_pid => PlayerPid}};
        _->
            {stop, State}
    end;
%%心跳直接处理
websocket_handle({binary, <<1006:16, Length:32>>}, State) ->
    {reply, [{binary, <<1007:16, Length:32>>}], State};
%%其它协议仍给player节点处理
websocket_handle({binary, Data}, State) ->
    PlayerPid = maps:get(player_pid, State),
    PlayerPid ! {binary, Data},
    {ok, State};
websocket_handle(Req, State) ->
    lager:info("websocket_handle unknow req: ~p", [Req]),
    {reply, {text, "this is wxr server!"}, State}.

websocket_info({msg, Msgs}, State) ->
    {reply, Msgs, State};
websocket_info(_Info, State) ->
    {ok, State}.

terminate(Reason, _Req, State) ->
    lager:info("gate:~p stop, reson:~p~n", [State, Reason]),
    case maps:get(player_pid, State, undefined) of
        undefined ->
            ok;
        PlayerPid ->
            PlayerPid ! stop,
            ok
    end.