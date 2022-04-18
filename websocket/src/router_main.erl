-module(router_main).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([get_room/1]).

-define(SERVER, ?MODULE).
-record(room,{id, name, options, created}).
-record(state,{db=ets:new(?MODULE, [set]), name2pid=ets:new(?MODULE, [bag, {keypos, 1}]), id2pid=ets:new(?MODULE, [bag, {keypos, 1}])}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_room(RoomId) ->
  gen_server:call(?SERVER, {getroom, RoomId}).

init([]) ->
  process_flag(trap_exit, true),
  self() ! {initall},
  {ok, #state{}}.

handle_call({getroom, Id}, _From, #state{} = State) ->
  RoomId = ets:lookup(State#state.name2pid, Id),
  try
    {reply, {ok, element(2, lists:last(RoomId))}, State}
  catch
    _:_ -> {reply, {error, notfound}, State}
  end.

handle_cast(_Msg, #state{} = State) ->
  {noreply, State}.

handle_info({initall}, #state{} = State) ->
  openDb("./roomsDb.dets"),
  initAll(State),
  {noreply, State}; 
handle_info(Info, #state{} = State) ->
  case Info of
    {'EXIT', Pid, _Why} ->
      % force logout:
      handle_call({logout, Pid}, blah, State);
    Wtf ->
      io:format("Caught unhandled message: ~w\n", [Wtf])
  end,
  {noreply, State}.

openDb(File) ->
    io:format("RoomDB opened:~p~n" , [File]),
    NewId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    IfNew = #room{id = NewId, name = <<"All">>, options = [], created = u:utime()},
    Bool = filelib:is_file(File),
    case dets:open_file(?MODULE, [{file, File},{auto_save, 60000}]) of
        {ok, ?MODULE} ->
            case Bool of
                true -> void;
                false -> ok = dets:insert(?MODULE, IfNew)
            end,
            true;
        {error,_Reason} ->
            io:format("cannot open dets table~n" ),
            exit(eDetsOpen)
    end.

initAll(State) ->
  dets:traverse(?MODULE, fun(Room) -> runRoom(Room, State), continue end),
  State.

runRoom(Room, #state{} = State) ->
  {ok, RoomPid} = supervisor:start_child(websocket_rooms_sup, {
    Room#room.id,
    {room_router, start_link, [Room]},
    permanent, % and this should be restarted to kill whole supervisor
    2000,
    worker,
    []
  }),
  ets:insert(State#state.db, Room),
  ets:insert(State#state.name2pid, {Room#room.name, RoomPid}),
  ets:insert(State#state.id2pid, {Room#room.id, RoomPid}),
  RoomPid.

stopRoom(Room) ->
  supervisor:terminate_child(websocket_rooms_sup, Room#room.id),
  supervisor:delete_child(websocket_rooms_sup, Room#room.id).

terminate(_Reason, _State) ->
  dets:traverse(?MODULE, fun(Room) -> stopRoom(Room), continue end),
  dets:close(?MODULE),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.