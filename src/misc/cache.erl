-module(cache).
-include("def_public_data.hrl").
-export([init/0,save/0,get/2,put/2,add/1]).

init()->
    lbm_kv:create(cache),
    Rows = db:query("select * from GPublicData", []),
    lbm_kv:put(cache,[{misc:bitstring_to_term(Key, 0), misc:bitstring_to_term(Value, 0)} || [Key, Value] <- Rows]),
    default().

save()->
    {ok, KeyValues}= lbm_kv:match(cache, '_', '_'),
    [db:query("replace into GPublicData values (?,?)", [misc:term_to_bitstring(Key), misc:term_to_bitstring(Value)]) || {Key,  Value} <- KeyValues].

get(Key, Default)->
    case lbm_kv:get(cache, Key) of
        {ok, [{_, Value}]} ->
            Value;
        _ ->
            Default
    end.

put(Key, Value)->
    lbm_kv:put(cache,[{Key, Value}]).

add(Key)->
    case lbm_kv:update(cache, Key, fun(_, {value, OldValue}) -> {value, OldValue+1} end) of
        {ok,_,[{_, Value}]}->
            Value + 1;
        _->
            1
    end.

default() ->
    [default(Key) || Key <- [?battle_id]].

default(Key) ->
    case lbm_kv:get(cache, Key) of
        {ok, [{_, _}]} ->
            skip;
        _ ->
            lbm_kv:put(cache,[{Key, default_value(Key)}])
    end.

default_value(?battle_id) ->
    case application:get_env(wxr, server_number) of
        {ok, ServerNumber} ->
            ServerNumber * 100000000;
        _ ->
            1 * 100000000
    end.
    