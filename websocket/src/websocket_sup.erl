%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
	Procs = [
    {access_test_db,{access_test, start_link, []}, permanent, 2000, worker, []},
		{message_router,{router, start_link, []}, permanent, 2000, worker, []}
%%	{history_writer,{histwriter, start_link, []}, permanent, 2000, worker, []},
  ],
	{ok, {{one_for_one, 10, 10}, Procs}}.
