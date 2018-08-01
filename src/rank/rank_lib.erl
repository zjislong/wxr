%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 五月 2018 05:30
%%%-------------------------------------------------------------------
-module(rank_lib).
-author("zhengjia").
-include("type.hrl").
-include("def_rank.hrl").
-include("proto.hrl").
-include("def_public_data.hrl").
-include("def_ets.hrl").
-include("def_player.hrl").

%% API
-export([init/2,
    save/2,
    update/3,
    update/4,
    update_callback/3,
    get_rank_info/3,
    get_p_rank_info/3,
    update_detail/4,
    reset/1]).

%%初始化
-spec init(Tag :: string(), Detail :: [#rank_detail{}]) -> {DBTableName :: string(), KeyEtsName :: atom(), RankEtsName :: atom(), [#rank_detail{}]}.
init(Tag, Detail) ->
    KeyEtsName = key_ets_name(Tag),
    RankEtsName = rank_ets_name(Tag),
    DBTableName = db_name(Tag),
    create_db_table(DBTableName),
    ets:new(KeyEtsName, [{keypos, #rank.key}, named_table, set, public]),
    ets:new(RankEtsName, [{keypos, #rank.rank}, named_table, set, public]),
    Rows = db:query("select * from " ++ DBTableName, []),
    RankInfo = [#rank{key = misc:bitstring_to_term(Key, ""), value = Value, rank = Rank} || [Key, Value, Rank] <- Rows],
    set_rank_info(KeyEtsName, RankInfo),
    set_rank_info(RankEtsName, RankInfo),
    Detail1 = init_detail(RankInfo, Detail),
    {DBTableName, KeyEtsName, RankEtsName, Detail1}.

%%持久化
-spec save(DBTableName :: string(), KeyEtsName :: atom()) -> ok.
save(DBTableName, KeyEtsName) ->
    case ets:match_object(KeyEtsName, #rank{save = 1, _ = '_'}) of
        [] ->
            ok;
        RankInfos ->
            [db:query("replace into " ++ DBTableName ++ " values (?, ?, ?)", [misc:term_to_bitstring(Key), Value, Rank]) || #rank{key = Key, value = Value, rank = Rank} <- RankInfos],
            set_rank_info(KeyEtsName, [R#rank{save = 0} || R <- RankInfos]),
            ok
    end.

%%db表名
-spec db_name(Tag :: string()) -> string().
db_name(Tag) ->
    "GRank_" ++ Tag.

%%排行榜ets名字
-spec rank_ets_name(Tag :: string()) -> atom().
rank_ets_name(Tag) ->
    list_to_atom("ets_rank_" ++ Tag).

%%排行榜ets名字
-spec key_ets_name(Tag :: string()) -> atom().
key_ets_name(Tag) ->
    list_to_atom("ets_rank_key_" ++ Tag).

%%更新排行榜
-spec update(Tag :: string(), Key :: string(), Change :: {add, integer()}|{replace, integer()}) -> term().
update(Tag, Key, Change) ->
    gen_server:cast({global, rank_manager_srv}, {apply, rank_lib, update_callback, [Tag, Key, Change]}).

update_callback(Tag, Key, Change) ->
    case ets:lookup(?ETS_RANK_SRV, Tag) of
        [#rank_srv{pid = Srv}] ->
            ok;
        _ ->
            {ok, Srv} = supervisor:start_child(rank_sup, [Tag, []])
    end,
    gen_server:cast(Srv, {update, Key, Change}).

%%更新排行榜
-spec update(KeyEtsName :: atom(), RankEtsName :: atom(), Key :: string(), Change :: {add, integer()}|{replace, integer()}) -> ok.
update(KeyEtsName, RankEtsName, Key, {add, Add}) ->
    #rank{value = Value} = RankInfo = get_rank_info(KeyEtsName, Key, #rank{key = Key}),
    case Add > 0 of
        true ->
            update1(KeyEtsName, RankEtsName, RankInfo#rank{value = Value + Add, save = 1}, 1);
        false ->
            update1(KeyEtsName, RankEtsName, RankInfo#rank{value = Value + Add, save = 1}, -1)
    end;
update(KeyEtsName, RankEtsName, Key, {replace, Replace}) ->
    #rank{value = Value} = RankInfo = get_rank_info(KeyEtsName, Key, #rank{key = Key}),
    case Replace > Value of
        true ->
            update1(KeyEtsName, RankEtsName, RankInfo#rank{value = Replace, save = 1}, 1);
        false ->
            ok
    end.

update1(KeyEtsName, RankEtsName, #rank{rank = 0} = RankInfo, Incr) ->
    case Incr of
        1 ->
            Rank = ets:info(KeyEtsName, size) + 1,
            update1(KeyEtsName, RankEtsName, RankInfo#rank{rank = Rank}, 1);
        -1 ->
            ok
    end;
update1(KeyEtsName, RankEtsName, #rank{rank = 1} = RankInfo, 1) ->
    set_rank_info(KeyEtsName, RankInfo),
    set_rank_info(RankEtsName, RankInfo),
    ok;
update1(KeyEtsName, RankEtsName, #rank{value = Value, rank = Rank} = RankInfo, Incr) ->
    TarRank = Rank - Incr,
    case get_rank_info(RankEtsName, TarRank, error) of
        #rank{value = TarValue} = TarRankInfo when TarValue * Incr < Value * Incr->
            TarRankInfo1 = TarRankInfo#rank{rank = Rank, save = 1},
            set_rank_info(KeyEtsName, TarRankInfo1),
            set_rank_info(RankEtsName, TarRankInfo1),
            update1(KeyEtsName, RankEtsName, RankInfo#rank{rank = TarRank}, Incr);
        _ ->
            set_rank_info(KeyEtsName, RankInfo),
            set_rank_info(RankEtsName, RankInfo)
    end.

%%获取排行数据
-spec get_rank_info(EtsName :: atom(), Key :: string()|non_neg_integer(), DefaultInfo :: term()) -> RankInfo :: #rank{}|term().
get_rank_info(EtsName, Key, DefaultInfo) ->
    case ets:lookup(EtsName, Key) of
        [#rank{} = RankInfo] ->
            RankInfo;
        _ ->
            DefaultInfo
    end.

%%更新排行数据
-spec set_rank_info(EtsName :: atom(), RankInfo :: #rank{}|[#rank{}]) -> true.
set_rank_info(EtsName, RankInfo) ->
    lager:info("~p~n", [RankInfo]),
    ets:insert(EtsName, RankInfo).

%%创建db表
-spec create_db_table(DBTableName :: string()) -> term().
create_db_table(DBTableName) ->
    db:query("call PCreateRankTable('" ++ DBTableName ++ "')", []).

%%获取排名数据
-spec get_p_rank_info(Tag :: string(), Rank :: non_neg_integer(), SelfKey :: string()) -> {#p_rank{}, [#p_rank{}]}.
get_p_rank_info(Tag, Rank, SelfKey) ->
    case ets:lookup(?ETS_RANK_SRV, Tag) of
        [#rank_srv{key_ets_name = KeyEtsName, rank_ets_name = RankEtsName}] ->
            ok;
        _ ->
            supervisor:start_child(rank_sup, [Tag, []]),
            [#rank_srv{key_ets_name = KeyEtsName, rank_ets_name = RankEtsName}] = ets:lookup(?ETS_RANK_SRV, Tag)
    end,
    {
        rank2p_rank(get_rank_info(KeyEtsName, SelfKey, #rank{key = SelfKey})),
        [rank2p_rank(RankInfo) || R <- lists:seq(Rank, Rank + 9), RankInfo <- [get_rank_info(RankEtsName, R, error)], RankInfo =/= error]
    }.

%%排名数据结构转化
-spec rank2p_rank(RankInfo :: #rank{}) -> #p_rank{}.
rank2p_rank(RankInfo) ->
    #player{titile = Title, name = Name, head = Head, gender = Gender} = player_lib:get_player(RankInfo#rank.key),
    #p_rank{key = RankInfo#rank.key,
        value = RankInfo#rank.value,
        rank = RankInfo#rank.rank,
        title = Title,
        name = Name,
        head = Head,
        gender = Gender}.

%%生成初始rank_detail
-spec init_detail(RankInfo :: [#rank{}], Detail :: [#rank_detail{}]) -> [#rank_detail{}].
init_detail([], Detail) ->
    Detail;
init_detail([#rank{value = Value} | RankInfo], Detail) ->
    Detail1 = update_detail(Detail, Value, 1, []),
    init_detail(RankInfo, Detail1).

%%更新rank_detail
-spec update_detail(Detail :: [#rank_detail{}], Value :: non_neg_integer(), Add :: 1|-1, Res :: [#rank_detail{}]) -> [#rank_detail{}].
update_detail([], _, _, Res) ->
    Res;
update_detail([#rank_detail{value_range = {S, E}} = D | Detail], Value, Add, Res) when S =< Value andalso Value =< E ->
    update_detail(Detail, Value, Add, [D#rank_detail{count = max(0, D#rank_detail.count + Add)} | Res]);
update_detail([D | Detail], Value, Add, Res) ->
    update_detail(Detail, Value, Add, [D | Res]).

%%重置排行榜
-spec reset(Tag :: string()) -> term().
reset(Tag) ->
    case ets:lookup(?ETS_RANK_SRV, Tag) of
        [#rank_srv{pid = Srv}] ->
            ok;
        _ ->
            {ok, Srv} = supervisor:start_child(rank_sup, [Tag, []])
    end,
    gen_server:cast(Srv, reset).