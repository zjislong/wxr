%%%-------------------------------------------------------------------
%% @doc wxr top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(wxr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id := term(),
%%      start := {M :: module(), F :: atom(), A :: [term()] | undefined},
%%      restart => permanent | transient | temporary,
%%      shutdown => brutal_kill | timeout(),
%%      type => worker | supervisor,
%%      modules => [module()] | dynamic}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {_, ServerType} = application:get_env(wxr, server_type),
    {_, ServerVar} = application:get_env(wxr, server_var),
    Child = case ServerType of
        main ->
            [
                %%主进程
                #{id => wxr, start => {wxr_srv, start_link, [main, ServerVar]}}
            ];
        player ->
            [
                %%主进程
                #{id => wxr, start => {wxr_srv, start_link, [player, ServerVar]}},
                %%定时器
                #{id => timer_srv, start => {timer_srv, start_link, []}},
                %%玩家
                #{id => player_sup, start => {player_sup, start_link, []}, type => supervisor},
                %%战斗
                #{id => battle_sup, start => {battle_sup, start_link, []}, type => supervisor}
            ];
        game ->
            [
                %%主进程
                #{id => wxr, start => {wxr_srv, start_link, [game, ServerVar]}},
                %%排行榜
                #{id => rank_manager_srv, start => {rank_manager_srv, start_link, []}},
                %%排行榜监控树
                #{id => rank_sup, start => {rank_sup, start_link, []}, type => supervisor},
                %%战斗匹配
                #{id => battle_master_srv, start => {battle_master_srv, start_link, []}}
            ]
    end,
    {ok, {{one_for_one, 1000, 3600}, Child}}.

%%====================================================================
%% Internal functions
%%====================================================================
