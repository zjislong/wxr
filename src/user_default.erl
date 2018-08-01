-module(user_default).

-include_lib("stdlib/include/ms_transform.hrl").

-compile(export_all).

ob() ->
    observer:start().

p(A) when is_atom(A) ->
    whereis(A);
p(A) when is_list(A) ->
    list_to_pid(A);
p(A) when is_pid(A) ->
    A.

p(A, Flag) ->
    element(2, process_info(p(A), Flag)).

%% 查看一个进程的进程字典
d(Name) ->
    p(Name, dictionary).

d(Name, Key) ->
    Dicts = p(Name, dictionary),
    case lists:keyfind(Key, 1, Dicts) of
        {_, _} = Dict -> Dict;
        _ -> undefined
    end.

%% 杀死一个进程
kill(Process) ->
    exit(p(Process), kill).

%%erlang中文字符串格式化输出
u8(String) ->
    io:format("~ts~n", [unicode:characters_to_list(list_to_binary(String))]).

q() ->
    io:format("can not stop sever~n").

beam2erl(File) ->
    case beam_lib:chunks(File, [abstract_code]) of
        {ok, {Module, [{_, {_, Source}}]}} ->
            {ok, IO} = file:open(atom_to_list(Module) ++ ".erl", [write]),
            io:format(IO, "~s~n", [erl_prettypr:format(erl_syntax:form_list(Source))]),
            file:close(IO);
        _ ->
            error
    end.