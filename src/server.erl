%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. 五月 2018 09:06
%%%-------------------------------------------------------------------
-module(server).
-author("zhengjia").

%% API
-export([start/0, stop/0]).

start() ->
    application:ensure_all_started(wxr),
    info().

stop() ->
    application:stop(wxr),
    init:stop().

info() ->
    SchedId = erlang:system_info(scheduler_id),
    SchedNum = erlang:system_info(schedulers),
    ProcCount = erlang:system_info(process_count),
    ProcLimit = erlang:system_info(process_limit),
    ProcMemUsed = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTot = erlang:memory(total),
    io:format("system info display as follow:
                       ~n   Scheduler id:                         ~p
                       ~n   Num scheduler:                        ~p
                       ~n   Process count:                        ~p
                       ~n   Process limit:                        ~p
                       ~n   Memory used by erlang processes:      ~p
                       ~n   Memory allocated by erlang processes: ~p
                       ~n   The total amount of memory allocated: ~p
                       ~n",
        [SchedId, SchedNum, ProcCount, ProcLimit,
            ProcMemUsed, ProcMemAlloc, MemTot]),
    ok.