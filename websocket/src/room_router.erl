-module(room_router).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([
	send/2,
	login/2,
	logout/2,
	set_user_vars/3,
	roomlist/2,
	kickuser/2,
	banuser/2,
	unbanuser/2,
	banlist/2
]).

-define(SERVER, ?MODULE).

-record(room,{id, name, options, created}).
-record(settings,{logging=0, histsend=0, userstat=0, broadcast_lists=0}).
-record(state,{room=#room{}, increment=0, db=ets:new(?MODULE, [set, {keypos, 1}]), pid2id=ets:new(?MODULE, [bag]), settings=#settings{}}).
-record(user,{params=#{}, pid, lact}).

start_link(Params) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Params, []).

init(Params) ->
	process_flag(trap_exit, true),
	RoomData = #room{id = Params#room.id, name = Params#room.name},
	State = #state{increment = 1, room = RoomData},
	{ok, State}.

% sends Msg to anyone logged in as Id
send(ServerPid, {Msg, Name, Num}) ->
	gen_server:cast(ServerPid, {send, Msg, Name, Num}).

login(ServerPid, {Pid, Ip}) when is_pid(Pid) ->
	gen_server:call(ServerPid, {register, Pid, Ip}).

logout(ServerPid, Pid) when is_pid(Pid) ->
	gen_server:call(ServerPid, {logout, Pid}).

set_user_vars(ServerPid, Usnum, Vars) ->
	gen_server:call(ServerPid, {set_user_vars, Usnum, Vars}).

roomlist(ServerPid, Pid) when is_pid(Pid) ->
	gen_server:cast(ServerPid, {roomlist, Pid}).

kickuser(ServerPid, Usernum) ->
	gen_server:call(ServerPid, {kickuser, Usernum}).		

banuser(ServerPid, Usernum) ->
	gen_server:call(ServerPid, {banuser, Usernum}).		
	
unbanuser(ServerPid, Ipaddr) ->
	gen_server:call(ServerPid, {unbanip, Ipaddr}).	

banlist(ServerPid, Pid) ->	
	gen_server:cast(ServerPid, {banlist, Pid}).

handle_call({register, Pid, Ip}, _From, #state{} = State) when is_pid(Pid) ->
	case test_ban(Ip, State) of
		{ok, nf, _} ->
			UserKey = <<<<"user-">>/binary, (integer_to_binary(State#state.increment))/binary>>,
			UserIpKey = <<<<"ip-">>/binary,UserKey/binary>>,
			ets:insert(State#state.db, {UserKey, #user{pid=Pid, lact=u:utime()}}),
			ets:insert(State#state.db, {UserIpKey, u:ip2binary(Ip)}),
			ets:insert(State#state.pid2id, {Pid, UserKey}),
			link(Pid),
			% if
			% 	State#state.settings#settings.broadcast_lists == 1 ->
			% 		lst();
			% 	State#state.settings#settings.broadcast_lists == 0 ->
			% 		ok;
			% 	true ->
			% 		ok
			% end,
			Newincr = State#state.increment + 1,
			% if
			% 	State#state.settings#settings.histsend == 1 ->
			% 		histwriter:gethist(Room, Pid);
			% 	State#state.settings#settings.histsend == 0 ->
			% 		ok;
			% 	true -> 
			% 		ok
			% end,
			{reply, {normal, UserKey}, State#state{increment=Newincr}};
		{ok, banned, _} -> 
			{reply, {banned, 0}, State}
	end;
handle_call({logout, Pid}, _From, #state{} = State) when is_pid(Pid) ->
	unlink(Pid),
	ClearUser = fun({UserPid, UserKey}) ->
		ets:delete(State#state.pid2id, UserPid),
		ets:delete(State#state.db, UserKey)
		% SendList = State#state.settings#settings.broadcast_lists,
		% if
		% 	SendList == 1 -> lst();
		% 	true -> ok
		% end;
	end,
	u:ets_result(ets:lookup(State#state.pid2id, Pid),ClearUser),
	{reply, ok, State};
handle_call({kickuser, Usernum}, _From, #state{} = State) ->
	SendMessage = fun({_Key, User}) ->
		Pid = User#user.pid,
		Pid ! {sreply, jsone:encode([{action, kickuser},{user, system},{message, <<>>},{error, youkicked}])},
		Pid ! {disconn, ""}
	end,
	u:ets_result(ets:lookup(State#state.db, Usernum), SendMessage),
	{reply, ok, State};
handle_call({banuser, Usernum}, _From, #state{} = State) ->
	SendMessage = fun({_Key, User}) ->
		Pid = User#user.pid,
		Pid ! {sreply, jsone:encode([{action, banuser},{user, system},{message, <<>>},{error, youbanned}])},
		Pid ! {disconn, ""}
	end,
	AddBannedIp = fun({_Key, Ip}) ->
		IpKey = <<<<"ip-banned-">>/binary,Ip/binary>>,
		ets:insert(State#state.db, {IpKey, u:utime()})
	end,
	UserIpKey = <<<<"ip-">>/binary,Usernum/binary>>,
	u:ets_result(ets:lookup(State#state.db, UserIpKey),AddBannedIp),
	u:ets_result(ets:lookup(State#state.db, Usernum), SendMessage),
	{reply, ok, State};
handle_call({unbanip, Ipaddr}, _From, #state{} = State) ->
	IpKey = <<<<"ip-banned-">>/binary,Ipaddr/binary>>,
	ets:delete(State#state.db, IpKey),
	{reply, ok, State};		
handle_call({set_user_vars, Usernum, Vars}, _From, #state{} = State) ->
	UpdateUser = fun({Id, User}) ->
		NUser = maps:merge(User#user.params, Vars),
		User1 = User#user{params = NUser},
		ets:insert(State#state.db, {Id, User1})
	end,
	u:ets_result(ets:lookup(State#state.db, Usernum), UpdateUser),
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
	Wrapper = fun({<<KI:5/binary, _H/binary>>, User}, Acc) ->
		case KI of
			<<"user-">> -> User#user.pid ! Message;
			_ -> ok
		end,
		Acc
	end,
	ets:foldl(Wrapper, [], State#state.db),
	updateActivity(Servnum, State),
	{noreply, State};
handle_cast({roomlist, Pid}, #state{} = State) ->
	Wrapper = fun({<<KI:5/binary, H/binary>>, Item}, Acc) ->
		case KI of
			<<"user-">> ->
				Ulist = maps:to_list(Item#user.params),
				Ulist1 = [{lact, Item#user.lact} | Ulist],
				[{<<KI/binary,H/binary>>, Ulist1} | Acc];
			_ -> ok,
				Acc
		end
	end,
	Lst = ets:foldl(Wrapper, [], State#state.db),
	Pid ! {sreply, jsone:encode([{action, roomlist},{user, system},{message, Lst}])},
	{noreply, State};
handle_cast({banlist, Pid}, #state{} = State) ->
	Wrapper = fun({<<KI:6/binary, H/binary>>, Time}, Acc) ->
		case KI of
			<<"ip-ban">> ->
				Str = <<KI/binary,H/binary>>,
				Ipaddr = binary:part(Str, {10, byte_size(Str)-10}),
				[[{ipaddr, Ipaddr},{time, Time}] | Acc];
			_ -> ok,
				Acc
		end
	end,
	Lst = ets:foldl(Wrapper, [], State#state.db),
	Pid ! {sreply, jsone:encode([{action, banlist},{user, system},{message, Lst}])},
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

updateActivity(Usernum, State) ->
	UpdateUser = fun({Id, User}) ->
		NUser = User#user{lact=u:utime()},
		ets:insert(State#state.db, {Id, NUser})
	end,
	u:ets_result(ets:lookup(State#state.db, Usernum), UpdateUser).

test_ban(Ip, State) ->
	BinIp = u:ip2binary(Ip),
	IpKey = <<<<"ip-banned-">>/binary,BinIp/binary>>,
	Res = ets:member(State#state.db, IpKey),
	case Res of
		false -> {ok, nf, State};
		true -> {ok, banned, State}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.