%%%-------------------------------------------------------------------
%%% @author zhengjia
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. 五月 2018 06:05
%%%-------------------------------------------------------------------
-module(proto_lib).
-author("zhengjia").
-include("proto.hrl").

%% API
-export([decode_msg/1,
    encode_msg/1]).

decode_msg(Data) ->
    case Data of
        <<MsgID:16, Length:32, Binary:Length/binary>> ->
            case proto_route:route(MsgID) of
                null ->
                    {error, "wrong msgID"};
                Mod ->
                    Fun = msgID2proto(MsgID),
                    Msg = proto:decode_msg(Binary, Fun),
                    {Mod, Fun, Msg}
            end;
        _ ->
            {error, "wrong data"}
    end.

encode_msg(Msg) ->
    MsgID = proto2msgID(Msg),
    Binary = proto:encode_msg(Msg),
    Length = byte_size(Binary),
    <<MsgID:16, Length:32, Binary/binary>>.

msgID2proto(MsgID) ->
    proto:enum_symbol_by_value(msgID, MsgID).

proto2msgID(Term) ->
    ProtoName = element(1, Term),
    proto:enum_value_by_symbol(msgID, ProtoName).