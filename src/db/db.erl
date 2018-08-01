-module(db).
-export([query/2]).

query(Query, ParamsOrTimeout)->
    case mysql_poolboy:query(game, Query, ParamsOrTimeout) of
        {ok, _, Result} ->
            Result;
        {error, {Code, SQLState, Message}} ->
            lager:error("query: ~p, ~p has mysql error:~p(~ts) ~ts~n",[Query, ParamsOrTimeout, Code, SQLState, Message]),
            [];
        _->
            []
    end.
