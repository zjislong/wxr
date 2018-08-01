%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 24. 四月 2018 10:26
%%%-------------------------------------------------------------------
-module(misc).
-author("zhengjia").

%% API
-export([unixtime/0,
    longunixtime/0,
    random_list/2,
    term_to_bitstring/1,
    bitstring_to_term/2,
    seconds_to_datetime/1,
    datetime_to_seconds/1,
    gc/0,
    gc/1,
    sleep/1,
    ipv4/0,
    ipv6/0]).

%% 取得当前的unix时间戳
-spec unixtime() -> non_neg_integer().
unixtime() ->
    erlang:system_time(second).

-spec longunixtime() -> non_neg_integer().
longunixtime() ->
    erlang:system_time(millisecond).

%% 从列表List中随机选取SelectNum个元素，组成新的列表，新列表的元素排列顺序与其在List中顺序相同
-spec random_list(List :: [term()], SelectNum :: non_neg_integer()) -> {[term()], [term()]}.
random_list(List, SelectNum) ->
    Len = length(List),
    case Len =< SelectNum of
        true ->
            {List, []};
        false ->
            random_list(List, SelectNum, Len, [], [])
    end.

random_list(Rest, 0, _, Result, NotSelect) ->
    {lists:reverse(Result), lists:reverse(NotSelect)++Rest};
random_list([Head| Rest], SelectNum, Len, Result, NotSelect) ->
    case rand:uniform() =< SelectNum / Len of
        true ->
            random_list(Rest, SelectNum-1, Len-1, [Head|Result], NotSelect);
        false ->
            random_list(Rest, SelectNum, Len-1, Result, [Head|NotSelect])
    end.

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
-spec term_to_bitstring(Term :: term()) -> bitstring().
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
-spec bitstring_to_term(BitString :: bitstring()|undefined, DefaultTerm :: term()) -> term().
bitstring_to_term(undefined, DefaultTerm) ->
    DefaultTerm;
bitstring_to_term(BitString, DefaultTerm) ->
    string_to_term(binary_to_list(BitString), DefaultTerm).

string_to_term(String, DefaultTerm) ->
    case erl_scan:string(String ++ ".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err -> DefaultTerm
            end;
        _Error ->
            DefaultTerm
    end.

datetime_to_seconds({_Date, _Time} = Datetime) ->
    calendar:datetime_to_gregorian_seconds(Datetime) - calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {8, 0, 0}}).

seconds_to_datetime(MTime) ->
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds({{1970, 1, 1}, {8, 0, 0}}) + MTime).

gc()->
    gc(0).

gc(MemoryLimit)->
    OldMemory = erlang:memory(processes),
    case OldMemory > MemoryLimit of
        true->
            [gc1(P)||P<-processes()],
            Memory = erlang:memory(processes),
            OldMemory-Memory;
        false->
            skip
    end.

gc1(P)->
    case process_info(P,[status]) of
        [{_,waiting}] ->
            erlang:garbage_collect(P);
        _->
            skip
    end.

sleep(I) -> receive after I -> ok end.

ipv4()->
  {_, Addrs}= inet:getifaddrs(),
  case [IP||{_,Addr}<-Addrs,{addr,IP}<-Addr, IP =/= {127,0,0,1},IP=/={0,0,0,0},size(IP)==4] of
    [IP|_]->
      inet:ntoa(IP);
    _->
      "localhost"
  end.

ipv6()->
  {_, Addrs}= inet:getifaddrs(),
  case [IP||{_,Addr}<-Addrs,{addr,IP}<-Addr, IP =/= {127,0,0,1},IP=/={0,0,0,0},size(IP)==8] of
    [IP|_]->
      inet:ntoa(IP);
    _->
      "localhost"
  end.