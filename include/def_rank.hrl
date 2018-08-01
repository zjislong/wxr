%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 五月 2018 06:09
%%%-------------------------------------------------------------------
-author("zhengjia").

-record(rank_detail, {
    value_range = {0, 100} :: {non_neg_integer(), non_neg_integer()}|matchspec_atom(),   %%值范围
    count = 0 :: non_neg_integer()|matchspec_atom() %%key计数
}).

-record(rank_srv, {
    tag = "default" :: string()|matchspec_atom(),   %%rank_srv标识
    pid = 0 :: 0|pid()|matchspec_atom(),     %%rank_srv的pid
    db_name = "GRank_default" :: string()|matchspec_atom(),  %%数据库表名
    key_ets_name = default_key_ets :: atom()|matchspec_atom(),   %%key_ets名字
    rank_ets_name = default_rank_ets :: atom()|matchspec_atom(),  %%rank_ets名字
    detail = [] :: [#rank_detail{}]|matchspec_atom()     %%排行榜分段统计信息
}).

-record(rank, {
    key = "" :: string()|non_neg_integer()|matchspec_atom(),   %%排行榜key
    value = 0 :: non_neg_integer()|matchspec_atom(),  %%值
    rank = 0 :: non_neg_integer()|matchspec_atom(),   %%排名
    save = 0 :: 0|1|matchspec_atom()    %%需要保存0不需要1需要
}).
