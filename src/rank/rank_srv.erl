%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 五月 2018 05:40
%%%-------------------------------------------------------------------
-module(rank_srv).
-author("zhengjia").
-include("type.hrl").
-include("def_rank.hrl").
-include("def_ets.hrl").

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

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link(Tag :: string(), Detail :: [#rank_detail{}]) ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link(Tag, Detail) ->
    gen_server:start_link(?MODULE, [Tag, Detail], []).

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
    {ok, State :: #rank_srv{}} | {ok, State :: #rank_srv{}, timeout() | hibernate} |
    {stop, Reason :: term()} | ignore).
init([Tag, Detail]) ->
    process_flag(trap_exit, true),
    {DBTableName, KeyEtsName, RankEtsName, Detail1} = rank_lib:init(Tag, Detail),
    State = #rank_srv{tag = Tag, pid = self(), db_name = DBTableName, key_ets_name = KeyEtsName, rank_ets_name = RankEtsName, detail = Detail1},
    ets:insert(?ETS_RANK_SRV, State),
    erlang:send_after(1000, self(), loop),
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_call(Request :: term(), From :: {pid(), Tag :: term()},
    State :: #rank_srv{}) ->
    {reply, Reply :: term(), NewState :: #rank_srv{}} |
    {reply, Reply :: term(), NewState :: #rank_srv{}, timeout() | hibernate} |
    {noreply, NewState :: #rank_srv{}} |
    {noreply, NewState :: #rank_srv{}, timeout() | hibernate} |
    {stop, Reason :: term(), Reply :: term(), NewState :: #rank_srv{}} |
    {stop, Reason :: term(), NewState :: #rank_srv{}}).
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @end
%%--------------------------------------------------------------------
-spec(handle_cast(Request :: term(), State :: #rank_srv{}) ->
    {noreply, NewState :: #rank_srv{}} |
    {noreply, NewState :: #rank_srv{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #rank_srv{}}).
handle_cast(Req, State) ->
    try
        do_handle_cast(Req, State)
    catch
        C:R:Stacktrace ->
          lager:info("rank_srv handle_cast:~p fail:{~p,~p,~p}", [Req,C,R,Stacktrace]),
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
-spec(handle_info(Info :: timeout() | term(), State :: #rank_srv{}) ->
    {noreply, NewState :: #rank_srv{}} |
    {noreply, NewState :: #rank_srv{}, timeout() | hibernate} |
    {stop, Reason :: term(), NewState :: #rank_srv{}}).
handle_info(Info, State) ->
    try
        do_handle_info(Info, State)
    catch
        C:R:Stacktrace ->
          lager:info("rank_srv handle_info:~p fail:{~p,~p,~p}", [Info,C,R,Stacktrace]),
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
    State :: #rank_srv{}) -> term()).
terminate(_Reason, State) ->
    lager:info("rank_srv ~p terminate", [State#rank_srv.tag]),
    rank_lib:save(State#rank_srv.db_name, State#rank_srv.key_ets_name).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process rank_srv when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
-spec(code_change(OldVsn :: term() | {down, term()}, State :: #rank_srv{},
    Extra :: term()) ->
    {ok, NewState :: #rank_srv{}} | {error, Reason :: term()}).
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%更新排行榜
do_handle_cast({update, Key, Add}, #rank_srv{key_ets_name = KeyEtsName, rank_ets_name = RankEtsName, detail = Detail} = State) ->
    #rank{value = OldValue} = rank_lib:get_rank_info(KeyEtsName, Key, #rank{key = Key}),
    rank_lib:update(KeyEtsName, RankEtsName, Key, Add),
    #rank{value = NewValue} = rank_lib:get_rank_info(KeyEtsName, Key, #rank{key = Key}),
    Detail1 = rank_lib:update_detail(Detail, OldValue, -1, []),
    Detail2 = rank_lib:update_detail(Detail1, NewValue, 1, []),
    State1 = State#rank_srv{detail = Detail2},
    ets:insert(?ETS_RANK_SRV, State1),
    {noreply, State1};
%%重置
do_handle_cast(reset, #rank_srv{db_name = DBName, key_ets_name = KeyEtsName, rank_ets_name = RankEtsName, detail = Detail} = State) ->
    db:query("truncate "++DBName, []),
    ets:delete_all_objects(KeyEtsName),
    ets:delete_all_objects(RankEtsName),
    Detail1 = [D#rank_detail{count = 0} || D <- Detail],
    State1 = State#rank_srv{detail = Detail1},
    ets:insert(?ETS_RANK_SRV, State1),
    {noreply, State};
do_handle_cast(_Request, State)->
    {noreply, State}.

do_handle_info(loop, State) ->
    erlang:send_after(1000, self(), loop),
    case time() of
        {_, 0, 0} ->
            rank_lib:save(State#rank_srv.db_name, State#rank_srv.key_ets_name);
        _ ->
            skip
    end,
    {noreply, State};
do_handle_info(Info, State) ->
    lager:info("~p recv unknown info ~p~n", [?MODULE, Info]),
    {noreply, State}.