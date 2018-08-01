%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. 四月 2018 11:43
%%%-------------------------------------------------------------------
-module(wxr_srv).
-author("zhengjia").
-include("type.hrl").
-include("def_player.hrl").
-include("def_ets.hrl").
-include("def_public_data.hrl").
-include("def_rank.hrl").
-include("def_battle.hrl").

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    server_type=player::main|player|game,
    connected_main_server=0::0|1
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(ServerType::atom(), ServerVar::[term()]) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ServerType, ServerVar) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [ServerType, ServerVar], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
-spec(init(Args :: term()) ->
    {ok, State :: #state{}} | {ok, State :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).

init([main, ServerVar])->
    process_flag(trap_exit, true),
    start_web(player_handler, ServerVar),
    node_pool:new_pool(player),
    node_pool:new_pool(game),
    public_data:init(),
    lbm_kv:create(player),
    erlang:send_after(30000,self(),loop),
    {ok, #state{server_type=main, connected_main_server = 1}};
init([player, _])->
    process_flag(trap_exit, true),
    connect_main_server(infinity),
    join_pool(player),
    erlang:send_after(30000,self(),loop),
    {ok, #state{server_type = player, connected_main_server = 1}};
init([game, _]) ->
    process_flag(trap_exit, true),
    ets:new(?ETS_RANK_SRV, [{keypos, #rank_srv.tag}, public, set, named_table]),
    connect_main_server(infinity),
    join_pool(game),
    erlang:send_after(30000,self(),loop),
    {ok, #state{server_type = game, connected_main_server = 1}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #state{}) ->
    {reply, Reply :: term(), NewState :: #state{}} |
    {reply, Reply :: term(), NewState :: #state{}, timeout() | hibernate} |
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #state{}} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
-spec(handle_info(Info :: timeout() | term(), State :: #state{}) ->
    {noreply, NewState :: #state{}} |
    {noreply, NewState :: #state{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #state{}}).
handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        C:R:Stacktrace ->
          lager:info("wxr_srv handle_info:~p fail:{~p,~p,~p}", [Info,C,R,Stacktrace]),
          {noreply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
-spec(terminate(Reason :: (normal | shutdown | {shutdown, term()} | term()),
    State :: #state{}) -> term()).
terminate(_Reason, State) ->
    lager:info("~p terminate", [?MODULE]),
    case State#state.server_type of
        main ->
            public_data:save(),
            ok;
        _->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #state{},
    Extra :: term()) ->
    {ok, NewState :: #state{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%循环
do_handle_info(loop, State) ->
    erlang:send_after(30000,self(),loop),
    spawn(misc,gc,[8000000000]),
    case State#state.server_type =/= main andalso State#state.connected_main_server == 0 of
        true->
            case connect_main_server() of
                true->
                    join_pool(State#state.server_type),
                    {noreply, State#state{connected_main_server = 1}};
                false->
                    {noreply, State}
                end;
        false->
            {noreply, State}
    end;
%%与main_server断开了
do_handle_info({nodedown, _}, State) ->
    {noreply, State#state{connected_main_server = 0}};
do_handle_info(Info, State) ->
    lager:info("~p recv unknown info ~p~n", [?MODULE, Info]),
    {noreply, State}.

%%启动cowboy
start_web(Handler, Var) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {'_', Handler, []}
        ]}
    ]),
    Port = proplists:get_value(port, Var),
    {ok, _} = cowboy:start_tls(wxr_https, [
        {port, Port},
        {cacertfile, "priv/ssl/1_root_bundle.crt"},
        {certfile, "priv/ssl/2_vxwxrs1.yeahyou.cn.crt"},
        {keyfile, "priv/ssl/3_vxwxrs1.yeahyou.cn.key"}
    ], #{env => #{dispatch => Dispatch}}).

%%连接main_server
connect_main_server()->
    {_,MainServer} = application:get_env(wxr, main_server),
    connect_main_server(MainServer, 5).

connect_main_server(Count)->
    {_,MainServer} = application:get_env(wxr, main_server),
    connect_main_server(MainServer, Count).

connect_main_server(_MainServer, 0) -> false;
connect_main_server(MainServer, infinity) ->
    case net_adm:ping(MainServer) of
        pong ->
            erlang:monitor_node(MainServer, true),
            true;
        pang ->
            connect_main_server(MainServer, infinity)
    end;
connect_main_server(MainServer, Count) ->
    case net_adm:ping(MainServer) of
        pong ->
            true;
        pang ->
            misc:sleep(300),
            connect_main_server(MainServer, Count-1)
    end.

join_pool(ServerType)->
    case global:whereis_name(ServerType) of
	    undefined ->
            misc:sleep(300),
            join_pool(ServerType);
        _->
            node_pool:attach(ServerType, node())
    end.