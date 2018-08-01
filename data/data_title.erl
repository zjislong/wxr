%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% 称号
%%% @end
%%% Created : 05. 五月 2018 11:43
%%%-------------------------------------------------------------------
-module(data_title).
-author("zhengjia").

%% API
-export([title_list/0, title/1]).

title_list() ->
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10].

title(0) ->
    0;
title(1) ->
    50;
title(2) ->
    81;
title(3) ->
    121;
title(4) ->
    181;
title(5) ->
    251;
title(6) ->
    321;
title(7) ->
    401;
title(8) ->
    501;
title(9) ->
    701;
title(10) ->
    1001.