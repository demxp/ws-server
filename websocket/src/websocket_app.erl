%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).
-export([custom_404_hook/4]).

%% API.
start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/static/css/[...]", cowboy_static, {priv_dir, websocket, <<"static/css">>, [
				{mimetypes, cow_mimetypes, all}
			]}},
			{"/static/js/[...]", cowboy_static, {priv_dir, websocket, <<"static/js">>, [
				{mimetypes, cow_mimetypes, all}
			]}},
			{"/static/img/[...]", cowboy_static, {priv_dir, websocket, <<"static/img">>, [
				{mimetypes, cow_mimetypes, all}
			]}},			
			{"/", cowboy_static, {priv_file, websocket, "hi.html"}},
			{"/admininterface", cowboy_static, {priv_file, websocket, "index.html"}},
			{"/websocket", ws_handler, []}
		]}
	]),
	
	{ok, _} = cowboy:start_clear(http, [{port, 1881}], #{
    env => #{dispatch => Dispatch}
  }),

	%PrivDir = code:priv_dir(websocket),
	%{ok, _} = cowboy:start_https(https, 100, [
	%	{port, 443},
	%	{cacertfile, PrivDir ++ "/ssl/cowboy-ca.crt"},
	%	{certfile, PrivDir ++ "/ssl/server.crt"},
	%	{keyfile, PrivDir ++ "/ssl/server.key"}
	%], Env),
	websocket_sup:start_link().

custom_404_hook(404, Headers, <<>>, Req) ->
    Body = <<"404 Not Found.">>,
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(byte_size(Body))}),
	acc_log(404, Req),
    cowboy_req:reply(404, Headers2, Body, Req);
custom_404_hook(403, Headers, <<>>, Req) ->
    Body = <<"403 Forbidden.">>,
    Headers2 = lists:keyreplace(<<"content-length">>, 1, Headers,
        {<<"content-length">>, integer_to_list(byte_size(Body))}),
	acc_log(403, Req),		
    cowboy_req:reply(403, Headers2, Body, Req);	
custom_404_hook(Status, _, _, Req) ->
	acc_log(Status, Req),
    Req.
	
acc_log(Status, Req) ->
    {PeerAddr, _} = cowboy_req:peer(Req),
	Url = cowboy_req:url(Req),	
	Method = cowboy_req:method(Req),
	PeerStr = inet_parse:ntoa(PeerAddr),		
	histwriter ! {access, [{peer, PeerStr}, {method, Method}, {url, Url}, {status, Status}]}.

stop(_State) ->
  ok = cowboy:stop_listener(http).
