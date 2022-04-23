%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(websocket_rooms_sup).
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

init(_Args) ->
  {ok, {{simple_one_for_one,10,60}, [{room_worker, {room_router, start_link, []}, transient, 2000, worker, [room_router]}]}}.
