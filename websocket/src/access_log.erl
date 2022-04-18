-module(access_log).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
    {PeerAddr, _} = cowboy_req:peer(Req),
	Url = cowboy_req:url(Req),	
	case access_test:test(PeerAddr, Url) of
		{ok, _} ->
			{ok, Req, Env};			
		{error, _} ->
			{error, 403, Req}
	end.