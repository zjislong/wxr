%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 多重key索引的进程字典，索引用sets格式保存，put操作的value以term格式保存，append/delete操作的value以sets格式保存
%%% @end
%%% Created : 12. 五月 2018 04:53
%%%-------------------------------------------------------------------
-module(dictionary).
-author("zhengjia").

%% API
-export([key_get/2, key_put/2, key_append/2, key_delete/2, key_erase/1]).

-spec key_get(Key :: term()|[term()], DefaultValue :: term()) -> term().
key_get(Key, DefaultValue) ->
    case get(Key) of
        undefined ->
            default(DefaultValue);
        Value ->
            Value
    end.

-spec key_put(Key :: term()|[term()], Value :: term()) -> term().
key_put([Key], Value) ->
    put(Key, Value);
key_put([Key1, Key2 | Keys], Value) ->
    key_append(Key1, Key2),
    key_put([Key2 | Keys], Value);
key_put(Key, Value) ->
    put(Key, Value).

-spec key_append(Key :: term()|[term()], Value :: term()) -> term().
key_append([Key], Value) ->
    do_key_append(Key, Value);
key_append([Key1, Key2 | Keys], Value) ->
    key_append(Key1, Key2),
    key_append([Key2 | Keys], Value);
key_append(Key, Value) ->
    do_key_append(Key, Value).

do_key_append(Key, Value) ->
    Old = key_get(Key, sets),
    case sets:is_set(Old) of
        true ->
            key_put(Key, sets:add_element(Value, Old));
        false ->
            error
    end.

-spec key_delete(Key :: term(), Value :: term()) -> term().
key_delete(Key, Value) ->
    Old = key_get(Key, sets),
    case sets:is_set(Old) of
        true ->
            key_erase(Value),
            key_put(Key, sets:del_element(Value, Old));
        false ->
            error
    end.

key_erase([]) ->
    ok;
key_erase([Key | Keys]) ->
    do_key_erase(Key),
    key_erase(Keys);
key_erase(Key) ->
    do_key_erase(Key).

do_key_erase(Key) ->
    Old = key_get(Key, 0),
    erase(Key),
    case sets:is_set(Old) of
        true ->
            List = sets:to_list(Old),
            key_erase(List);
        false ->
            ok
    end.

default(sets) ->
    sets:new();
default(Value) ->
    Value.