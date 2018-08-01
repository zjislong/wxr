-module(data_battle_ai).

-export([ai_action/3,
    ai_name/0,
    ai_head/0,
    ai_province/0,
    ai_city/1]).

%%ai行为 ai_action(StarLv::段位, ID::不动, TarID:不动)
ai_action(_StarLv, ID, TarID)->
    [
        %%掉血{间隔,...}
        {5, miss_mark, [ID, TarID]},
        %%乌云{间隔,...}
        {10, use_skill, [TarID, 1]},
        %%加速{间隔,...}
        {10, use_skill, [TarID, 2]},
        %%抵挡{间隔,...}
        {15, use_skill, [TarID, 3]},
        %%大招{间隔,...}
        {50, big_skill, [ID, TarID]}
    ].

%%ai昵称
ai_name()->
    [
        <<"慕容狗蛋"/utf8>>,
        <<"西门铁柱"/utf8>>,
        <<"皇甫翠花"/utf8>>,
        <<"爱新觉罗痛经"/utf8>>
    ].

%%ai头像
ai_head()->
    [
        ""
    ].

%%ai省
ai_province()->
    [
        "SiChuan",
        "ChongQi"
    ].

%%ai市
ai_city("SiChuan")->
    [
        "ChengDu",
        "MianYang"
    ];
ai_city("ChongQi")->
    [
        "ChongQi"
    ].