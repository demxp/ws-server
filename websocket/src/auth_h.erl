%% Feel free to use, reuse and abuse the code in this file.

%% @doc POST echo handler.
-module(auth_h).

-export([init/2]).

init(Req0, Opts) ->
  Method = cowboy_req:method(Req0),
  HasBody = cowboy_req:has_body(Req0),
  Mode = cowboy_req:binding(mode, Req0),
  Req = filterReq(Method, HasBody, Mode, Opts, Req0),
  {ok, Req, Opts}.


filterReq(<<"GET">>, false, <<"refresh">>, _Opts, Req0) ->
  try
    #{refreshToken := Refresh} = cowboy_req:match_cookies([{refreshToken, [], <<"test">>}], Req0),
    case Refresh of
      <<"test">> -> throw(incorrect);
      _ when byte_size(Refresh) == 36 -> ok;
      _ -> throw(incorrect)
    end,
    {ok, AccessToken, RefreshToken} = procAuth(refresh, Refresh),
    Req1 = setRfrCookie(RefreshToken, Req0),    
    Body = jsone:encode(#{status => ok, access => AccessToken, refresh => RefreshToken}),
    cowboy_req:reply(200, #{
        <<"content-type">>   => <<"application/json">>,
        <<"content-length">> => integer_to_list(byte_size(Body))
    }, Body, Req1)
  catch
    _:Err -> 
    u:trace("ERROR", Err),
    cowboy_req:reply(403, Req0)
  end;
filterReq(<<"POST">>, true, <<"login">>, Opts, Req0) ->
  try
    {ok, Body, Req} = cowboy_req:read_urlencoded_body(Req0),
    {_, Login} = lists:keyfind(<<"login">>, 1, Body),
    {_, Password} = lists:keyfind(<<"password">>, 1, Body),    
    {ok, _AccessToken, RefreshToken} = procAuth(login, {Login, Password}),
    Req1 = setRfrCookie(RefreshToken, Req),
    cowboy_req:reply(301, #{
        <<"location">> => maps:get(afterlogin, Opts)
    }, Req1)
  catch
    _:Err -> 
    u:trace("ERROR", Err),
    cowboy_req:reply(403, Req0)
  end;
filterReq(<<"GET">>, false, <<"login">>, Opts, Req0) ->
  try
    File = code:priv_dir(websocket) ++ "/" ++ maps:get(loginpage, Opts),
    case filelib:is_regular(File) of
      false -> throw(notfound);
      true -> ok
    end,
    {ok, Body} = file:read_file(File),
    cowboy_req:reply(200, #{
        <<"content-type">>   => <<"text/html">>,
        <<"content-length">> => integer_to_list(byte_size(Body))
    }, Body, Req0)
  catch
    _:Err -> 
    u:trace("ERROR", Err),
    cowboy_req:reply(404, Req0)
  end;
filterReq(_, _, _, _, Req) ->
  %% Method not allowed.
  cowboy_req:reply(405, Req).  

procAuth(refresh, Refresh) ->
  user_token:refresh(self(), {Refresh}),
  waitAuth();
procAuth(login, {Login, Password}) ->
  user_token:login(self(), {Login, Password}),
  waitAuth().

waitAuth() ->
  receive
      {cast_reply, {Mode, Data}} when Mode == login; Mode == refresh -> Data;
      _ -> throw(replyerror)
  after
      5000 ->
        throw(timeout)
  end.

setRfrCookie(Refresh, Req0) ->
  #{token := RefreshToken} = Refresh,
  {_, ExpireRefresh} = websocket_app:get_env(<<"admin.tokens.expire-refresh">>, 5184000),
  cowboy_req:set_resp_cookie(<<"refreshToken">>, RefreshToken, Req0, 
    #{http_only => true, path => <<"/auth">>, max_age => ExpireRefresh}).