-module(access_log).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req0, Env) ->
  {IpAddr, _} = cowboy_req:peer(Req0),
	Url = cowboy_req:path(Req0),
  IpAddrBin = u:ip2binary(IpAddr),
  {ok, Timeout} = websocket_app:get_env(<<"server.request.min-timeout">>, 500),
	case access_test:test(IpAddrBin, Url, Timeout) of
		{ok, _} ->
			{ok, Req0, Env};			
		{error, _} ->
      Body = <<"403 Too many requests.">>,
      Headers = maps:put(<<"content-length">>, integer_to_list(byte_size(Body)), cowboy_req:headers(Req0)),
      Req = cowboy_req:reply(403, Headers, Body, Req0),
			{stop, Req}
	end.