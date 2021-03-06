%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_app).
-behaviour(application).

%% API.
-export([
  start/2,
  stop/1,
  custom_404_hook/4,
  get_env/1,
  get_env/2
]).


%% API.
start(_Type, _Args) ->
  load_config(code:priv_dir(websocket) ++ "/sett.dat"),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/static/[...]", cowboy_static, {priv_dir, websocket, <<"www/static">>, [
				{mimetypes, cow_mimetypes, all}
			]}},			
			{"/", cowboy_static, {priv_file, websocket, "www/hi.html"}},
			{"/admin", cowboy_static, {priv_file, websocket, "www/index.html"}},
      {"/auth/:mode", auth_h, #{loginpage => "www/login.html", afterlogin => "/admin"}},
			{"/websocket", ws_handler, #{}},
      {'_', cowboy_static, {priv_file, websocket, "www/errors/404.html"}}
		]}
	]),
	{ok, Port} = websocket_app:get_env(<<"server.port">>),
	{ok, _} = cowboy:start_clear(http, [{port, Port}], #{
    env => #{dispatch => Dispatch},
    middlewares => [cowboy_router, access_log, cowboy_handler]
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

load_config(File) ->
  {ok, Data} = file:read_file(File),
  D = jsone:decode(Data, [{object_format, proplist}]),
  ConfigList = u:parse_config(D),
  ets:new(settings_tab, [set, {keypos, 1}, named_table]),
  SaveParam = fun({Key, Val}) ->
    ets:insert(settings_tab, {{env, Key}, Val})
  end,
  lists:map(SaveParam, ConfigList),
  {ok, loaded}.

get_env(Key, Default) ->
    case ets:lookup(settings_tab, {env, Key}) of
  [{_, Val}] -> {ok, Val};
  _ -> {ok, Default}
    end.
get_env(Key) ->
    case ets:lookup(settings_tab, {env, Key}) of
  [{_, Val}] -> {ok, Val};
  _ -> undefined
    end.


stop(_State) ->
  ok = cowboy:stop_listener(http).
