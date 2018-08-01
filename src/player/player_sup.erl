%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. 五月 2018 16:06
%%%-------------------------------------------------------------------
-module(player_sup).
-author("zhengjia").

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
    {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%% Child :: #{id := term(),
%%      start := {M :: module(), F :: atom(), A :: [term()] | undefined},
%%      restart => permanent | transient | temporary,
%%      shutdown => brutal_kill | timeout(),
%%      type => worker | supervisor,
%%      modules => [module()] | dynamic}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{simple_one_for_one, 1000, 3600}, [
        #{id => player_srv, start => {player_srv, start_link, []}, restart => temporary}
    ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
