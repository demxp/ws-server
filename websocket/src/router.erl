-module(router).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([send/4, login/5, logout/1, lst/1, admlst/1, kick/1, userban/1, unban/1, banlst/1, setadm/1, ss/2, gs/0]).

-define(SERVER, ?MODULE).

-record(settings,{logging=1, histsend=0, userstat=1, broadcast_lists=0}).
-record(state,{increment=0, banincr=0, db=ets:new(?MODULE, [set]), pid2id=ets:new(?MODULE, [bag]), banned=ets:new(?MODULE, [bag]), settings=ets:new(?MODULE, [set])}).
-record(user,{pid, id, name, room, ip, status, lact}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% sends Msg to anyone logged in as Id
send(Msg, Room, Name, Num) ->
	gen_server:cast(?SERVER, {send, Msg, Room, Name, Num}).

login(Id, Pid, Name, Room, Ip) when is_pid(Pid) ->
	gen_server:call(?SERVER, {register, Id, Pid, Name, Room, Ip}).

logout(Pid) when is_pid(Pid) ->
	gen_server:call(?SERVER, {logout, Pid}).

kick(Usernum) ->
	gen_server:call(?SERVER, {userkick, Usernum}).	
	
userban(Usernum) ->
	gen_server:call(?SERVER, {userbanip, Usernum}).		
	
unban(Usernum) ->
	gen_server:call(?SERVER, {unbanip, Usernum}).	
	
setadm(Usnum) ->
	gen_server:call(?SERVER, {setadminstatus, Usnum}).
	
banlst(Pid) ->	
	gen_server:cast(?SERVER, {getbanned, Pid}).

lst(Room) ->
	gen_server:cast(?SERVER, {getlst, Room}).
	
admlst(Pid) ->
	gen_server:cast(?SERVER, {getadmlst, Pid}).	
	
ss(Room,{Param, Value}) ->
	gen_server:call(?SERVER, {setsettings, {Room, Param, Value}}).
	
gs() ->
	gen_server:call(?SERVER, {getsettings}).

init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{increment=1}}.

handle_call({register, Id, Pid, Name, Room, Ip}, _From, #state{} = State) when is_pid(Pid) ->
	Settings = getsettings(Room, State),
	case test_ban(Ip, State) of
		{ok, nf, _} ->
			ets:insert(State#state.db, {State#state.increment, #user{pid=Pid, id=Id, name=Name, room=Room, ip=Ip, status='ordinar', lact=u:utime()}}),
			ets:insert(State#state.pid2id, {Pid, State#state.increment}),
			link(Pid),
			if
				Settings#settings.broadcast_lists == 1 ->
					lst(Room);
				Settings#settings.broadcast_lists == 0 ->
					ok;
				true ->
					ok
			end,
			Oldincr = State#state.increment,
			Newincr = State#state.increment + 1,
			if
				Settings#settings.histsend == 1 ->
					histwriter:gethist(Room, Pid);
				Settings#settings.histsend == 0 ->
					ok;
				true -> 
					ok
			end,
			{reply, {normal, Oldincr}, State#state{increment=Newincr}};
		{ok, banned, _} -> 
			{reply, {banned, 0}, State}
	end;
handle_call({logout, Pid}, _From, #state{} = State) when is_pid(Pid) ->
	unlink(Pid),
	PidRows = ets:lookup(State#state.pid2id, Pid),
	case PidRows of
		[] ->
			ok;
		_ ->
			[{_Pid,ServNum}|_] = PidRows,
			ets:delete(State#state.pid2id, Pid),
			User = ets:lookup(State#state.db, ServNum),
			ets:delete(State#state.db, ServNum),
			case User of
				[{_Usernum, Curruserdata}] ->
					Room = Curruserdata#user.room,
					Settings = getsettings(Room, State),
					if
						Settings#settings.broadcast_lists == 1, is_binary(Room) ->
							lst(Room);
						Settings#settings.broadcast_lists == 0 ->
							ok;
						true ->
							ok
					end;
				_ ->
					ok
			end
	end,
	{reply, ok, State};
handle_call({unbanip, Num}, _From, #state{} = State) ->
	Lst = ets:lookup(State#state.banned, binary_to_integer(Num)),
	case Lst of
		[] ->
			ok;
		_ ->
			ets:delete(State#state.banned, binary_to_integer(Num))
	end,	
	{reply, ok, State};	
handle_call({userkick, Usernum}, _From, #state{} = State) ->
	User = ets:lookup(State#state.db, binary_to_integer(Usernum)),
	case User of
		[] ->
			ok;
		_ ->
			[{_Num, #user{pid=Pid}}] = User,		
			Dtsend = {sreply, jsonx:encode([{action, systemsay},{user, system},{message, youkicked}])},
			Pid ! Dtsend,
			Pid ! {disconn, ""}
	end,
	{reply, ok, State};
handle_call({userbanip, Usernum}, _From, #state{banincr = Numip} = State) ->
	User = ets:lookup(State#state.db, binary_to_integer(Usernum)),
	case User of
		[] ->
			ok;
		_ ->
			[{_Num, #user{pid=Pid, ip=Ip}}] = User,			
			ets:insert(State#state.banned, {Numip,Ip}),
			Dtsend = {sreply, jsonx:encode([{action, systemsay},{user, system},{message, youbanned}])},
			Pid ! Dtsend,
			Pid ! {disconn, ""}
	end,
	{reply, ok, State#state{banincr=Numip+1}};
handle_call({setadminstatus, Usernum}, _From, #state{} = State) ->
	User = ets:lookup(State#state.db, Usernum),
	case User of
		[] ->
			ok;
		_ ->
			[{_Num, Curruserdata}] = User,
			ets:update_element(State#state.db, Usernum, {2, Curruserdata#user{status='admin', lact=u:utime()}})
	end,
	{reply, ok, State};
handle_call({setsettings, {Room, Param, e}}, _From, #state{} = State) ->
	case Param of
		log -> 
			u:trace("Logging - enabled"),
			setsettings(Room, State, {logging, 1}),
			{reply, ok, State};
		hsend ->
			u:trace("Send history - enabled"),		
			setsettings(Room, State, {histsend, 1}),
			{reply, ok, State};
		ustat ->
			u:trace("Send userstatus - enabled"),				
			setsettings(Room, State, {userstat, 1}),
			{reply, ok, State};
		lssend ->
			u:trace("Send lists - enabled"),				
			setsettings(Room, State, {broadcast_lists, 1}),
			{reply, ok, State};
		_ ->
			u:trace("Undefined parameter"),				
			{reply, ok, State}
	end;
handle_call({setsettings, {Room, Param, d}}, _From, #state{} = State) ->
	case Param of
		log -> 
			u:trace("Logging - disabled"),
			setsettings(Room, State, {logging, 0}),
			{reply, ok, State};
		hsend ->
			u:trace("Send history - disabled"),		
			setsettings(Room, State, {histsend, 0}),
			{reply, ok, State};
		ustat ->
			u:trace("Send userstatus - disabled"),				
			setsettings(Room, State, {userstat, 0}),
			{reply, ok, State};
		lssend ->
			u:trace("Send lists - disabled"),				
			setsettings(Room, State, {broadcast_lists, 0}),
			{reply, ok, State};
		_ ->
			u:trace("Undefined parameter"),				
			{reply, ok, State}
	end;
handle_call({setsettings, {_Room, _Param, _}}, _From, #state{} = State) ->
	u:trace("Error param"),				
	{reply, ok, State};
handle_call({getsettings}, _From, #state{} = State) ->
	Lst = ets:tab2list(State#state.settings),
	{reply, Lst, State}.
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

handle_cast({send, Message, Room, _Name, Servnum}, #state{} = State) ->
	Settings = getsettings(Room, State),
	Lst = ets:match_object(State#state.db,{'_',#user{room=Room, _='_'}}),
	[ Pid ! Message || {_Num, #user{pid=Pid}} <- Lst ],
	if
		Settings#settings.logging == 1 ->
			User = ets:lookup(State#state.db, Servnum),
			case User of
				[] ->
					ok;
				_ ->
					[{Usernum, Curruserdata}] = User,
					histwriter ! {savemessage,[Message, Room, Curruserdata#user.name, Servnum, list_to_binary(inet:ntoa(Curruserdata#user.ip))]},								
					ets:update_element(State#state.db, Usernum, {2, Curruserdata#user{lact=u:utime()}})
			end;
		Settings#settings.logging == 0 ->
			ok;
		true -> 
			ok
	end,	
	{noreply, State};
handle_cast({getlst, Room}, #state{} = State) ->
	Settings = getsettings(Room, State),
	Lst = ets:match_object(State#state.db,{'_',#user{room=Room, _='_'}}),
	if
		Settings#settings.userstat == 1 ->
			Nlst = [[{id, Id},{name, Name},{status, Status}] || {Id, #user{name=Name, status=Status}} <- Lst],
			Dtsend = {sreply, jsonx:encode([{action, list},{user, system},{message, Nlst}])},
			[ Pid ! Dtsend || {_Num, #user{pid=Pid}} <- Lst ];
		Settings#settings.userstat == 0 ->
			Nlst = [[{id, Id},{name, Name}] || {Id, #user{name=Name}} <- Lst],
			Dtsend = {sreply, jsonx:encode([{action, list},{user, system},{message, Nlst}])},
			[ Pid ! Dtsend || {_Num, #user{pid=Pid}} <- Lst ];
		true -> 
			ok
	end,		
	{noreply, State};
handle_cast({getadmlst, Pid}, #state{} = State) ->
	Lst = ets:match(State#state.db,{'$1','$2'}),
	Nlst = [[{snum, Num},{id, Id},{name, Name},{room, Room},{userip, list_to_binary(inet:ntoa(Ip))},{status,Status},{lact,Lact}] || [Num, #user{id=Id,name=Name,room=Room,ip=Ip,status=Status,lact=Lact}] <- Lst],
	Dtsend = {sreply, jsonx:encode([{action, admlist},{user, system},{message, Nlst}])},
	Pid ! Dtsend,	
	{noreply, State};	
handle_cast({getbanned, Pid}, #state{} = State) ->
	Lst = ets:match(State#state.banned,{'$1','$2'}),
	Nlst = [[{snum, Num},{userip, list_to_binary(inet:ntoa(Ip))}] || [Num,Ip] <- Lst],
	Dtsend = {sreply, jsonx:encode([{action, admbaniplst},{user, system},{message, Nlst}])},
	Pid ! Dtsend,	
	{noreply, State};	
handle_cast(_Msg, #state{} = State) ->
	{noreply, State}.

getsettings(Room, State) when is_binary(Room) ->
	Data = ets:lookup(State#state.settings, Room),
	case Data of
		[{Room,Current}] ->
			Current#settings{};
		[] ->
			ets:insert(State#state.settings, {Room, #settings{logging=1, histsend=0, userstat=1, broadcast_lists=0}}),
			#settings{logging=1, histsend=0, userstat=1, broadcast_lists=0}
	end.
	
setsettings(Room, State, Param) ->
	Data = ets:lookup(State#state.settings, Room),
	{Name, Stat} = Param,
	case Data of
		[{Room,Current}] ->
			Nstat = case Name of
				logging ->
					Current#settings{logging=Stat};
				histsend ->
					Current#settings{histsend=Stat};
				userstat ->
					Current#settings{userstat=Stat};
				broadcast_lists ->
					Current#settings{broadcast_lists=Stat}
			end,			
			ets:update_element(State#state.settings, Room, {2, Nstat});
		[] ->
			Nstat = case Name of
				logging ->
					#settings{logging=Stat, histsend=0, userstat=1, broadcast_lists=0};
				histsend ->
					#settings{logging=1, histsend=Stat, userstat=1, broadcast_lists=0};
				userstat ->
					#settings{logging=1, histsend=0, userstat=Stat, broadcast_lists=0};
				broadcast_lists ->
					#settings{logging=1, histsend=0, userstat=1, broadcast_lists=Stat}
			end,
			ets:insert(State#state.settings, {Room, Nstat})
	end.
	
test_ban(Ip, #state{} = State) ->
	Lst = ets:match(State#state.banned,{'$1',Ip}),
	case Lst of
		[] ->
			{ok, nf, State};
		[[_Num]] ->
			{ok, banned, State};
		[[_Num]|_] ->
			{ok, banned, State}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.