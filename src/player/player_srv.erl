%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 五月 2018 16:06
%%%-------------------------------------------------------------------
-module(player_srv).
-author("zhengjia").
-include("type.hrl").
-include("def_player.hrl").
-include("def_ets.hrl").
-include("proto.hrl").
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
    player_id = 0 :: string()|matchspec_atom(),      %%玩家id
    pid = 0 :: 0|pid()|matchspec_atom(),             %%player_srv的pid
    gate_pid = 0 :: 0|pid()|matchspec_atom(),         %%player_handler的pid
    last_loop_time = 0 :: non_neg_integer()|matchspec_atom()   %%上一次循环操作时间
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
-spec(start_link(From::pid(), Binary::binary()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(From, Binary) ->
    gen_server:start_link(?MODULE, [From, Binary], []).

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
init([From, Binary]) ->
    process_flag(trap_exit, true),
    Clogin = proto:decode_msg(Binary, c_login),
    {ok, Msgs, Player} = login:c_login(Clogin, 0),
    Pid = self(),
    case global:whereis_name({player, Clogin#c_login.player_id}) of
        undefined ->
            skip;
        OldHandler ->
            erlang:send(OldHandler, stop)
    end,
    global:register_name({player, Clogin#c_login.player_id}, Pid),
    [lager:info("pid ~p player ~p send msg: ~p", [Pid, Player#player.player_id, M]) || M <- Msgs],
    player_lib:put_player(Player),
    Msgs2 = [{binary, proto_lib:encode_msg(M)} || M <- Msgs],
    From ! {msg, Msgs2},
    {ok, #state{player_id = Player#player.player_id, pid = Pid, gate_pid = From}, 3000}.

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
          lager:info("player_srv handle_info:~p fail:{~p,~p,~p}", [Info,C,R,Stacktrace]),
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
terminate(Reason, State) ->
    lager:info("pid ~p player ~p logout,reson: ~p", [State#state.pid, State#state.player_id, Reason]),
    gen_server:cast({global, battle_master_srv}, {cancel_match, State#state.player_id}),
    Player = player_lib:get_player(State#state.player_id),
    case is_pid(Player#player.battle_srv) of
        true ->
            gen_server:cast(Player#player.battle_srv, {give_up, State#state.player_id});
        false ->
            skip
    end,
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process player when code is changed
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
do_handle_info({binary, Data}, State) ->
    case proto_lib:decode_msg(Data) of
        {Mod, Fun, Msg} ->
            lager:info("pid ~p player ~p recv msg: ~p", [State#state.pid, State#state.player_id, Msg]),
            do_handle_result(Mod:Fun(Msg, State#state.player_id), State);
        {error, Reason} ->
            lager:error("player error: ~p", [Reason]),
            {noreply, State}
    end;
do_handle_info(timeout, State) ->
    {stop, normal, State};
do_handle_info({loop, Now}, State) ->
    player_lib:loop(State#state.player_id, Now),
    {noreply, State#state{last_loop_time = Now}};
do_handle_info({msg, Msgs}, State) ->
    [lager:info("pid ~p player ~p send msg: ~p", [State#state.pid, State#state.player_id, M]) || M <- Msgs],
    Msgs2 = [{binary, proto_lib:encode_msg(M)} || M <- Msgs],
    State#state.gate_pid ! {msg, Msgs2},
    {noreply, State};
do_handle_info({apply_append_player, Mod, Fun, Args}, State) ->
    Player = player_lib:get_player(State#state.player_id),
    do_handle_result(erlang:apply(Mod, Fun, [Player|Args]), State);
do_handle_info({apply, Mod, Fun, Args}, State) ->
    do_handle_result(erlang:apply(Mod, Fun, Args), State);
do_handle_info(stop, State) ->
    {stop, normal, State};
do_handle_info(Info, State) ->
    lager:info("~p recv unknown info ~p~n", [?MODULE, Info]),
    {noreply, State}.

do_handle_result({error, _}, State) ->
    {noreply, State};
do_handle_result({_, Msgs}, State) ->
    case Msgs of
        [_|_] ->
            [lager:info("pid ~p player ~p send msg: ~p", [State#state.pid, State#state.player_id, M]) || M <- Msgs],
            Msgs2 = [{binary, proto_lib:encode_msg(M)} || M <- Msgs],
            State#state.gate_pid ! {msg, Msgs2};
        _->
            skip
    end,
    {noreply, State};
do_handle_result({_, Msgs, Player}, State) ->
    case Msgs of
        [_|_] ->
            [lager:info("pid ~p player ~p send msg: ~p", [State#state.pid, State#state.player_id, M]) || M <- Msgs],
            Msgs2 = [{binary, proto_lib:encode_msg(M)} || M <- Msgs],
            State#state.gate_pid ! {msg, Msgs2};
        _->
            skip
    end,
    player_lib:put_player(Player),
    player_lib:save_player(Player#player{last_loop_time = State#state.last_loop_time}),
    {noreply, State};
do_handle_result(_, State)->
    {noreply, State}.
