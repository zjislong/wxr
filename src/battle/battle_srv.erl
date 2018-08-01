%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 08. 五月 2018 10:37
%%%-------------------------------------------------------------------
-module(battle_srv).
-author("zhengjia").
-include("type.hrl").
-include("def_battle.hrl").
-include("def_ets.hrl").
-include("def_player.hrl").
-behaviour(gen_server).

%% API
-export([start_link/1, start_link/2]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    battle_id = 0 :: non_neg_integer(),
    id1 = 0 :: string(),
    id2 = 0 :: string(),
    ai_queue = [] :: [term()]
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
-spec(start_link(ID1 :: string()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ID1) ->
    gen_server:start_link(?MODULE, [ID1], []).

-spec(start_link(ID1 :: string(), ID2 :: string()) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(ID1, ID2) ->
    gen_server:start_link(?MODULE, [ID1, ID2], []).

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
init([ID1]) ->
    process_flag(trap_exit, true),
    ID2 = "ai"++ID1,
    lager:info("~p vs ~p start battle~n", [ID1, ID2]),
    BattleID = battle_lib:new_battle_id(),
    battle_lib:set_battle_player(battle_lib:get_battle_player(ID1)),
    battle_lib:set_battle_player(battle_lib:get_battle_player(ID2)),
    Pid = self(),
    erlang:send(Pid, battle_start),
    Player = player_lib:update_player(ID1, fun(_, {value, Player}) -> {value, Player#player{battle_srv = Pid}} end),
    {StarLv,_} = battle_lib:star(Player#player.star),
    {[AiName], _} = misc:random_list(data_battle_ai:ai_name(), 1),
    {[Head], _} = misc:random_list(data_battle_ai:ai_name(), 1),
    {[Province], _} = misc:random_list(data_battle_ai:ai_province(), 1),
    {[City], _} = misc:random_list(data_battle_ai:ai_city(Province), 1),
    player_lib:put_player(#player{player_id = ID2, name = AiName, head = Head, province = Province, city = City, star = Player#player.star}),
    {ok, #state{battle_id = BattleID, id1 = ID1, id2 = ID2, ai_queue = battle_ai_lib:create_ai_queue(misc:unixtime(), StarLv, ID2, ID1)}};
init([ID1, ID2]) ->
    process_flag(trap_exit, true),
    lager:info("~p vs ~p start battle~n", [ID1, ID2]),
    BattleID = battle_lib:new_battle_id(),
    battle_lib:set_battle_player(battle_lib:get_battle_player(ID1)),
    battle_lib:set_battle_player(battle_lib:get_battle_player(ID2)),
    Pid = self(),
    erlang:send(Pid, battle_start),
    player_lib:update_player(ID1, fun(_, {value, Player}) -> {value, Player#player{battle_srv = Pid}} end),
    player_lib:update_player(ID2, fun(_, {value, Player}) -> {value, Player#player{battle_srv = Pid}} end),
    {ok, #state{battle_id = BattleID, id1 = ID1, id2 = ID2}}.

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
handle_cast(Req, State) ->
    try
        do_handle_cast(Req, State)
    catch
        C:R:Stacktrace ->
          lager:info("battle_srv handle_cast:~p fail:{~p,~p,~p}", [Req,C,R,Stacktrace]),
          {noreply, State}
    end.

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
%%对战开始
handle_info(battle_start, State) ->
    battle_lib:battle_start(State#state.id1, State#state.id2),
    case State#state.id2 of
        [$a,$i|_] ->
            erlang:send_after(1000, self(), ai_loop);
        _->
            skip
    end,
    {noreply, State};
%%ai_loop
handle_info(ai_loop, State) ->
    NewAiQueue = battle_ai_lib:loop(misc:unixtime(), State#state.ai_queue),
    erlang:send_after(1000, self(), ai_loop),
    {noreply, State#state{ai_queue = NewAiQueue}};
handle_info(Info, State) ->
    lager:info("~p recv unknown info ~p~n", [?MODULE, Info]),
    {noreply, State}.

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
    player_lib:update_player(State#state.id1, fun(_, {value, Player}) -> {value, Player#player{battle_srv = 0}} end),
    case State#state.id2 of
        [$a,$i|_] ->
            player_lib:del_player(State#state.id2);
        _->
            player_lib:update_player(State#state.id2, fun(_, {value, Player}) -> {value, Player#player{battle_srv = 0}} end)
    end,
    ok.

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
%%消除
do_handle_cast({catch_mark, ID, Combo, BigSkill}, State) ->
    TarID = case State#state.id1 of
                ID ->
                    State#state.id2;
                _ ->
                    State#state.id1
            end,
    battle_lib:catch_mark(ID, TarID, Combo, BigSkill),
    {noreply, State};
%%消除失败
do_handle_cast({miss_mark, ID}, State) ->
    TarID = case State#state.id1 of
                ID ->
                    State#state.id2;
                _ ->
                    State#state.id1
            end,
    battle_lib:miss_mark(ID, TarID),
    {noreply, State};
%%使用技能
do_handle_cast({use_skill, ID, Skill}, State) ->
    TarID = case State#state.id1 of
                ID ->
                    State#state.id2;
                _ ->
                    State#state.id1
            end,
    battle_lib:use_skill(ID, TarID, Skill),
    {noreply, State};
%%断线认输
do_handle_cast({give_up, ID}, State) ->
    TarID = case State#state.id1 of
                ID ->
                    State#state.id2;
                _ ->
                    State#state.id1
            end,
    battle_lib:give_up(ID, TarID),
    {noreply, State};
%%战斗结束
do_handle_cast(stop, State) ->
    {stop, normal, State};
do_handle_cast(_Request, State) ->
    {noreply, State}.