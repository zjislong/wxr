-module(login_lib).
-export([get_player_id/1]).

-define(URL, "https://api.weixin.qq.com/sns/jscode2session").
-define(APPID, "wxdaca5e2dc316e29e").
-define(SECRET, "8fbfb96c19e0517f0a4ecfcb5c2cc5b3").

%%从微信取玩家openid
-spec get_player_id(Code::string()) -> {PlayerID::string(), SessionKey::string()}|error.
get_player_id(Code)->
    Url = lists:concat([?URL,
    "?appid=",?APPID,
    "&secret=",?SECRET,
    "&js_code=",Code,
    "&grant_type=authorization_code"]),
    case httpc:request(get, {Url, []}, [], []) of
        {ok, {_, _, Result}}->
            {ok, JsonData, _} = rfc4627:decode(Result),
            case rfc4627:get_field(JsonData, "openid") of
                {ok, PlayerID} ->
                    {ok, SessionKey} = rfc4627:get_field(JsonData, "session_key"),
                    {PlayerID, SessionKey};
                _->
                   {ok, ErrCode} = rfc4627:get_field(JsonData, "errcode"),
                   {ok, ErrMsg} = rfc4627:get_field(JsonData, "errmsg"),
                   lager:error("code:~p get_player_id errcode:~p,errmsg:~ts~n",[Code, ErrCode, ErrMsg]),
                   error
            end;
        {error, Reason}->
            lager:error("code:~p get_player_id error:~p~n",[Code, Reason]),
            error
    end.