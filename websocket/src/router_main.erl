-module(router_main).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([
  get_room/1,
  list_rooms/1,
  create_room/1,
  remove_room/1
]).

-define(SERVER, ?MODULE).
-record(room,{name, options, created}).
-record(state,{db=ets:new(?MODULE, [set, {keypos, 1}]), name2pid=ets:new(?MODULE, [bag, {keypos, 1}])}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_room(RoomId) ->
  gen_server:call(?SERVER, {getroom, RoomId}).

create_room(Name) ->
  gen_server:call(?SERVER, {create_room, Name}).

remove_room(Id) ->
  gen_server:call(?SERVER, {remove_room, Id}).

list_rooms(Pid) ->
  gen_server:cast(?SERVER, {list_rooms, Pid}).

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
  end;
handle_call({create_room, Name}, _From, #state{} = State) ->
  RoomId = ets:match(State#state.db, {'$1', #room{name=Name, _='_'}}),
  case RoomId of
    [] ->
      NewId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
      Room = {NewId, #room{name = Name, options = [], created = u:utime()}},
      dets:insert(?MODULE, Room),
      Pid = runRoom(Room, State),
      {reply, {ok, {created, Pid}}, State};
    [_Room] -> {reply, {error, exist}, State};
    _ -> {reply, {error, dberror}, State}
  end;
handle_call({remove_room, Id}, _From, #state{} = State) ->
  RoomId = ets:lookup(State#state.db, Id),
  case RoomId of
    [{IdRoom, Params}] ->
      dets:delete(?MODULE, Id),
      stopRoom({IdRoom, Params}, room_removing, State),
      {reply, {ok, {removed}}, State};
    [] -> {reply, {error, notfound}, State};
    _ -> {reply, {error, dberror}, State}
  end.  

handle_cast({list_rooms, Pid}, #state{} = State) ->
  RoomCompact = fun(R, Acc) ->
    {Id, Room} = R,
    M = #{id => Id, name => Room#room.name, created => Room#room.created},
    [M | Acc]
  end,
  Lst = ets:foldl(RoomCompact, [], State#state.db),
  Pid ! {cast_reply, {list_rooms, Lst}},
  {noreply, State};
handle_cast(_Msg, #state{} = State) ->
  {noreply, State}.

handle_info({initall}, #state{} = State) ->
  openDb(code:priv_dir(websocket) ++ "/roomsDb.dets"),
  initAll(State),
  {noreply, State}; 
handle_info(Info, #state{} = State) ->
  RemoveRoomData = fun(Room) ->
    {Id, RoomData} = Room,
    ets:delete(State#state.db, Id),
    ets:delete(State#state.name2pid, RoomData#room.name)
  end,
  case Info of
    {'EXIT', _Pid, {shutdown,{room_removing, IdRoom}}} ->
      u:ets_result(ets:lookup(State#state.db, IdRoom), RemoveRoomData);
    _ ->
      u:trace("BAD MESSAGE ROUTER_MAIN", Info)
  end,
  {noreply, State}.

openDb(File) ->
    io:format("RoomDB opened:~p~n" , [File]),
    NewId = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
    IfNew = {NewId, #room{name = <<"All">>, options = [], created = u:utime()}},
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
  {Id, RoomParams} = Room,
  {ok, RoomPid} = supervisor:start_child(websocket_rooms_sup, [{Id, RoomParams#room.name}]),
  link(RoomPid),
  ets:insert(State#state.db, Room),
  ets:insert(State#state.name2pid, {RoomParams#room.name, RoomPid}),
  RoomPid.

stopRoom(RoomData, Reason, State) ->
  {Id, Room} = RoomData,
  RoomPid = ets:lookup(State#state.name2pid, Room#room.name),
  case RoomPid of
    [{_RoomName, Pid}] ->
      room_router:stop_room(Pid, {shutdown, {Reason, Id}}),
      ok;
    _ -> 
      error
  end.  

terminate(_Reason, State) ->
  ets:foldl(fun(RoomData, _Acc) -> stopRoom(RoomData, stop_all_rooms, State) end, [], State#state.db),
  dets:close(?MODULE),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.