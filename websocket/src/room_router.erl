-module(room_router).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([send/2, login/2, logout/2, setadm/2]).

-define(SERVER, ?MODULE).

-record(room,{id, name, options, created}).
-record(settings,{logging=1, histsend=0, userstat=1, broadcast_lists=0}).
-record(state,{room=#room{}, increment=0, banincr=0, db=ets:new(?MODULE, [set]), pid2id=ets:new(?MODULE, [bag]), banned=ets:new(?MODULE, [bag]), settings=#settings{}}).
-record(user,{pid, ip, status, lact}).

start_link(Params) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) ->
	process_flag(trap_exit, true),
	RoomData = #room{id = Params#room.id, name = Params#room.name},
	State = #state{increment = 1, banincr = 1, room = RoomData},
	{ok, State}.

% sends Msg to anyone logged in as Id
send(ServerPid, {Msg, Name, Num}) ->
	gen_server:cast(ServerPid, {send, Msg, Name, Num}).

login(ServerPid, {Pid, Ip}) when is_pid(Pid) ->
	gen_server:call(ServerPid, {register, Pid, Ip}).

logout(ServerPid, Pid) when is_pid(Pid) ->
	gen_server:call(ServerPid, {logout, Pid}).

setadm(ServerPid, Usnum) ->
	gen_server:call(ServerPid, {setadminstatus, Usnum}).

handle_call({register, Pid, Ip}, _From, #state{} = State) when is_pid(Pid) ->
	case test_ban(Ip, State) of
		{ok, nf, _} ->
			ets:insert(State#state.db, {State#state.increment, #user{pid=Pid, ip=Ip, status='ordinar', lact=u:utime()}}),
			ets:insert(State#state.pid2id, {Pid, State#state.increment}),
			link(Pid),
			% if
			% 	State#state.settings#settings.broadcast_lists == 1 ->
			% 		lst();
			% 	State#state.settings#settings.broadcast_lists == 0 ->
			% 		ok;
			% 	true ->
			% 		ok
			% end,
			Oldincr = State#state.increment,
			Newincr = State#state.increment + 1,
			% if
			% 	State#state.settings#settings.histsend == 1 ->
			% 		histwriter:gethist(Room, Pid);
			% 	State#state.settings#settings.histsend == 0 ->
			% 		ok;
			% 	true -> 
			% 		ok
			% end,
			{reply, {normal, Oldincr}, State#state{increment=Newincr}};
		{ok, banned, _} -> 
			{reply, {banned, 0}, State}
	end;
handle_call({logout, Pid}, _From, #state{} = State) when is_pid(Pid) ->
	unlink(Pid),
	PidRows = ets:lookup(State#state.pid2id, Pid),
	case PidRows of
		[] -> ok;
		_ ->
			[{_Pid,ServNum}|_] = PidRows,
			ets:delete(State#state.pid2id, Pid),
			ets:delete(State#state.db, ServNum)
			% SendList = State#state.settings#settings.broadcast_lists,
			% if
			% 	SendList == 1 -> lst();
			% 	true -> ok
			% end;
	end,
	{reply, ok, State};
handle_call({setadminstatus, Usernum}, _From, #state{} = State) ->
	User = ets:lookup(State#state.db, Usernum),
	case User of
		[] ->
			ok;
		_ ->
			[{_Num, Curruserdata}] = User,
			ets:update_element(State#state.db, Usernum, {2, Curruserdata#user{status='admin', lact=u:utime()}})
	end,
	{reply, ok, State}.	
%handle_call({send, Id, Msg}, _From, State) ->
% get pids who are logged in as this Id
%Pids = [ P || { _Id, P } <- ets:lookup(State#state.id2pid, Id) ],
% send Msg to them all
%M = {router_msg, Msg},
%[ Pid ! M || Pid <- Pids ],
%{reply, ok, State}.

% handle death and cleanup of logged in processes
handle_info(Info, #state{} = State) ->
	case Info of
		{'EXIT', Pid, _Why} ->
			% force logout:
			handle_call({logout, Pid}, blah, State);
		Wtf ->
			io:format("Caught unhandled message: ~w\n", [Wtf])
	end,
	{noreply, State}.

handle_cast({send, Message, _Name, Servnum}, #state{} = State) ->
	Wrapper = fun({_Userid, User}, Acc) ->
		User#user.pid ! Message,
		Acc
	end,
	ets:foldl(Wrapper, [], State#state.db),
	updateActivity(Servnum, State),
	{noreply, State};
% handle_cast({getlst}, #state{} = State) ->
% 	Lst = ets:match_object(State#state.db,{'_',#user{room=Room, _='_'}}),
% 	if
% 		Settings#settings.userstat == 1 ->
% 			Nlst = [[{id, Id},{name, Name},{status, Status}] || {Id, #user{name=Name, status=Status}} <- Lst],
% 			Dtsend = {sreply, jsonx:encode([{action, list},{user, system},{message, Nlst}])},
% 			[ Pid ! Dtsend || {_Num, #user{pid=Pid}} <- Lst ];
% 		Settings#settings.userstat == 0 ->
% 			Nlst = [[{id, Id},{name, Name}] || {Id, #user{name=Name}} <- Lst],
% 			Dtsend = {sreply, jsonx:encode([{action, list},{user, system},{message, Nlst}])},
% 			[ Pid ! Dtsend || {_Num, #user{pid=Pid}} <- Lst ];
% 		true -> 
% 			ok
% 	end,		
% 	{noreply, State};
handle_cast(_Msg, #state{} = State) ->
	{noreply, State}.

% setsettings(State, Param) ->
% 	{Name, Stat} = Param,
% 	State1 = case Name of
% 		logging ->
% 			State#state.settings#settings{logging=Stat};
% 		histsend ->
% 			State#state.settings#settings{histsend=Stat};
% 		userstat ->
% 			State#state.settings#settings{userstat=Stat};
% 		broadcast_lists ->
% 			State#state.settings#settings{broadcast_lists=Stat}
% 	end,
% 	State1.

updateActivity(Servnum, State) ->
	User = ets:lookup(State#state.db, Servnum),
	case User of
		[] ->
			ok;
		_ ->
			[{Usernum, Curruserdata}] = User,
			ets:update_element(State#state.db, Usernum, {2, Curruserdata#user{lact=u:utime()}})
	end.	
	
test_ban(Ip, #state{} = State) ->
	Lst = ets:match(State#state.banned,{'$1',Ip}),
	case Lst of
		[] ->
			{ok, nf, State};
		_ ->
			{ok, banned, State}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.