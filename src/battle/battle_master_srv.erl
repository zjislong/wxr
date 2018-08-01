%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 09. 五月 2018 04:56
%%%-------------------------------------------------------------------
-module(battle_master_srv).
-author("zhengjia").
-include("type.hrl").
-include_lib("stdlib/include/ms_transform.hrl").
-include("def_player.hrl").
-include("def_battle.hrl").
-include("proto.hrl").
-include("def_rank.hrl").
-include("def_ets.hrl").
-include("option.hrl").
-include("def_public_data.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    status = 0 :: 0|1|2, %%阶段0匹配阶段1等待结算阶段2结算阶段
    time = 0 :: non_neg_integer() %%阶段结束时间
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
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    process_flag(trap_exit, true),
    global:register_name(?SERVER, self()),
    erlang:send_after(1000, self(), loop),
    case public_data:get(?battle_manager, {0, status_time(0)}) of
        {Status, Time} ->
            {ok, #state{status = Status, time = Time}};
        _ ->
            {ok, #state{status = 0, time = status_time(0)}}
    end.

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
          lager:info("battle_master_srv handle_cast:~p fail:{~p,~p,~p}", [Req,C,R,Stacktrace]),
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
handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        C:R:Stacktrace ->
          lager:info("battle_master_srv handle_info:~p fail:{~p,~p,~p}", [Info,C,R,Stacktrace]),
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
    public_data:put(?battle_manager, {State#state.status, State#state.time}),
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
%%进入匹配队列
do_handle_cast({join_match, ID, Key}, State) ->
    join_match(ID, Key),
    send_battle_info([ID], State),
    {noreply, State};
%%离开匹配队列
do_handle_cast({cancel_match, ID}, State) ->
    cancel_match(ID),
    send_battle_info([ID], State),
    {noreply, State};
%%发送对人模式信息
do_handle_cast({send_battle_info, PlayerID}, State) ->
    send_battle_info([PlayerID], State),
    {noreply, State};
do_handle_cast(_Request, State) ->
    {noreply, State}.

%%匹配操作
do_handle_info(loop, State) ->
    erlang:send_after(1000, self(), loop),
    Now = misc:unixtime(),
    case State#state.time > Now of
        true ->
            case State#state.status of
                0 ->
                    match(Now);
                _ ->
                    skip
            end,
            {noreply, State};
        false ->
            Status = status_change(State#state.status),
            State1 = State#state{status = Status, time = status_time(Status)},
            send_battle_info(State1),
            {noreply, State1}
    end;
do_handle_info(_Info, State) ->
    {noreply, State}.

%%阶段切换
status_change(0) ->
    1;
status_change(1) ->
    reward(),
    2;
status_change(2) ->
    gen_server:call(rank_manager_srv, {apply, rank_lib,reset,["pvp"]}),
    0.

%%加入匹配
join_match(ID, Key) ->
    {OldKey, _} = dictionary:key_get(?BATTLE_MATCH_PLAYER(ID), {Key, 0}),
    cancel_match(ID, OldKey),
    dictionary:key_put([?BATTLE_MATCH_LIST, ?BATTLE_MATCH_LIST(Key), ?BATTLE_MATCH_PLAYER(ID)], {Key, misc:unixtime()}).

%%退出匹配
cancel_match(ID) ->
    {OldKey, _} = dictionary:key_get(?BATTLE_MATCH_PLAYER(ID), {1, 0}),
    cancel_match(ID, OldKey).

cancel_match(ID, Key) ->
    dictionary:key_delete(?BATTLE_MATCH_LIST(Key), ?BATTLE_MATCH_PLAYER(ID)).

%%匹配
match(Now) ->
    KeyList = dictionary:key_get(?BATTLE_MATCH_LIST, sets),
    match(Now, lists:keysort(2, sets:to_list(KeyList)), {-1, []}).

match(Now, [], {LastKey, Rest}) ->
    dictionary:key_erase(?BATTLE_MATCH_LIST(LastKey)),
    case Rest of
        [{Time, ?BATTLE_MATCH_PLAYER(ID) = E}] ->
            case Now - Time > 25 of
                true ->
                    battle_lib:start_battle_srv([ID]);
                false ->
                    dictionary:key_put([?BATTLE_MATCH_LIST(LastKey), E], {LastKey, Time})
            end;
        _ ->
            skip
    end;
match(Now, [?BATTLE_MATCH_LIST(Key) | Keys], {LastKey, Rest}) ->
    IDList = dictionary:key_get(?BATTLE_MATCH_LIST(Key), sets),
    case sets:size(IDList) of
        0 ->
            match(Now, Keys, {LastKey, Rest});
        _ ->
            dictionary:key_erase(?BATTLE_MATCH_LIST(LastKey)),
            List = sets:fold(fun(E, Acc) ->
                                {_, Time} = dictionary:key_get(E, {1, 0}),
                                [{Time, E} | Acc]
                             end, [], IDList),
            List1 = lists:sort(List),
            Rest1 = match1(Rest ++ lists:reverse(List1)),
            match(Now, Keys, {Key, Rest1})
    end.

match1([]) ->
    [];
match1([R]) ->
    [R];
match1([{_, ?BATTLE_MATCH_PLAYER(ID)} | R]) ->
    {[{_, ?BATTLE_MATCH_PLAYER(TarID)}], Rest} = misc:random_list(R, 1),
    battle_lib:start_battle_srv([ID, TarID]),
    match1(Rest).

%%阶段结束时间
status_time(Status) ->
    {Week, WeekNum1, Time} = data_battle:status(Status),
    Date = date(),
    WeekNum2 = calendar:day_of_the_week(Date),
    misc:datetime_to_seconds({Date, Time}) + (WeekNum1 - WeekNum2) * 86400 + Week * 7 * 86400.

%%赛季结算
reward() ->
    case ets:lookup(?ETS_RANK_SRV, "pvp") of
        [#rank_srv{key_ets_name = KeyEtsName}] ->
            Ms = ets:fun2ms(fun(#rank{key = Key, value = Value}) -> {Key, Value} end),
            RankInfo = ets:select(KeyEtsName, Ms),
            reward(RankInfo);
        _ ->
            skip
    end,
    ok.

reward([]) ->
    ok;
reward([{ID, Star} | RankInfo]) ->
    {StarLv, _} = battle_lib:star(Star),
    Reward = data_battle:reward(StarLv),
    reward_lib:reward(ID, Reward, ?OPTION_BATTLE_STAR_REWARD, Star),
    reward(RankInfo).

%%发送多人模式信息
-spec send_battle_info(State::#state{}) -> no_return().
send_battle_info(State) ->
    #rank_srv{detail = Detail} = case ets:lookup(?ETS_RANK_SRV, "pvp") of
                                     [] ->
                                         supervisor:start_child(rank_sup, ["pvp", rank_detail(1, 0, [])]),
                                         [RankSrv] = ets:lookup(?ETS_RANK_SRV, "pvp"),
                                         RankSrv;
                                     [RankSrv] ->
                                         RankSrv
                                 end,
    StarInfo = [#p_star_info{star_lv = element(1, battle_lib:star(S)), count = Count} || #rank_detail{value_range = {S, _}, count = Count} <- Detail],
    send_battle_info_1(State, StarInfo, misc:unixtime()).

-spec send_battle_info(IDs::[string()], State::#state{}) -> [ok|error].
send_battle_info(IDs, State) ->
    #rank_srv{detail = Detail} = case ets:lookup(?ETS_RANK_SRV, "pvp") of
         [] ->
             supervisor:start_child(rank_sup, ["pvp", rank_detail(1, 0, [])]),
             [RankSrv] = ets:lookup(?ETS_RANK_SRV, "pvp"),
             RankSrv;
         [RankSrv] ->
             RankSrv
    end,
    StarInfo = [#p_star_info{star_lv = element(1, battle_lib:star(S)), count = Count} || #rank_detail{value_range = {S, _}, count = Count} <- Detail],
    Now = misc:unixtime(),
    [send_battle_info_1(ID, State, StarInfo, Now)||ID<-IDs].

-spec send_battle_info_1(State::#state{}, StarInfo::[#p_star_info{}], Now::pos_integer())-> [ok|error].
send_battle_info_1(State, StarInfo, Now)->
    [send_battle_info_1(Player, State, StarInfo, Now)||{_, Player}<-player_lib:get_all_players()].

-spec send_battle_info_1(#player{}|string(), State::#state{}, StarInfo::[#p_star_info{}], Now::pos_integer())-> ok|error.
send_battle_info_1(#player{} = Player, State, StarInfo, Now) ->
    {_, Time} = dictionary:key_get(?BATTLE_MATCH_PLAYER(Player#player.player_id), {1, 0}),
    AddSecond = case Player#player.fight_count_time of
                    0 ->
                        0;
                    _->
                        Player#player.fight_count_time - Now
                end,
    MatchSecond = case Time of
                    0 ->
                        0;
                    _->
                        Now - Time + 1
                end,
    player_lib:send_msg_to_client(Player#player.player_id, [#s_battle_info{status = State#state.status,
        time = State#state.time,
        fight_count = Player#player.fight_count,
        add_second = AddSecond,
        info = StarInfo,
        match_second = MatchSecond,
        star = Player#player.star}]);
send_battle_info_1(ID, State, StarInfo, Now) ->
    Player = player_lib:get_player(ID),
    send_battle_info_1(Player, State, StarInfo, Now).

rank_detail(StarLv, Star, Detail) ->
    case data_battle:star(StarLv) of
        -1 ->
            lists:reverse([#rank_detail{value_range = {Star, Star + 9999}} | Detail]);
        NeedStar ->
            rank_detail(StarLv + 1, Star + NeedStar, [#rank_detail{value_range = {Star, Star + NeedStar - 1}} | Detail])
    end.