-module(ws_handler).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).

-record(user,{servid=0, id="0", name = <<"Anonymous">>, ip, role = <<"ordinar">>, registered="none", token="none", room, room_pid, ping_timer}).

init(Req, _Opts) ->
	{IpAddress, _} = cowboy_req:peer(Req),
	Url = cowboy_req:path(Req),	
	State = #user{ip=IpAddress},
	Ret = case access_test:test(IpAddress, Url) of
		{ok, _} ->
			State1 = schedule_ping(State, 30000),
			{cowboy_websocket, Req, State1, #{idle_timeout => 30000*2}};
		{error, _} ->
			Req1 = cowboy_req:reply(403, #{}, <<"Too many connections.">>, Req),
			{ok, Req1, undefined}
	end,
	Ret.


websocket_handle({text, Msg}, State) ->
	MssDec = jsone:try_decode(Msg, [{object_format, tuple},reject_invalid_utf8,{keys, atom}]),
	%u:trace('JSON DATA',MssDec),
	NewState = case MssDec of
		{error,_} -> 
			u:trace('ERROR IN JSON 1',Msg),
			State;
		{ok, Data, _} -> analize(Data,State)
	end,
	{[], NewState};
websocket_handle({pong, _}, State) ->
  {[], schedule_ping(State, 30000)};	
websocket_handle(_Data, State) ->
	{[], State}.

%websocket_info({timeout, _Ref, Msg}, State) ->
%	{reply, {text, Msg}, Req, State};
websocket_info({sreply, Msg}, State) ->
	{[{text, Msg}], State};
websocket_info({disconn, Msg}, State) ->
  {[{close, 1000, Msg}], State};
websocket_info(do_ping, State) ->
  {[{ping, <<>>}], State};	
websocket_info(_Info, State) ->
	{[], State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
	
analize({Data}, State1) ->
	Action = proplists:get_value(action, Data, <<"ERROR">>),
	AccessToken = proplists:get_value(token, Data, <<"ERROR">>),
	State = case user_tokens:userCheckAccess(AccessToken) of
		true -> 
			TokenParams = user_tokens:userParseAccess(AccessToken),
			State1#user{name = maps:get(<<"name">>, TokenParams, <<"Anonymous">>), role = maps:get(<<"role">>, TokenParams, <<"ordinar">>)};
		false -> State1
	end,
	Reply = defaultReply([{action, Action},{role, State#user.role},{name, State#user.name}]),
	RPid = State#user.room_pid,
	try
		case binary_to_list(Action) of
			"register" ->
				Usname = proplists:get_value(name, Data, <<"Anonymous">>),
				Usid = proplists:get_value(userid, Data, 0),
				Usroom = proplists:get_value(room, Data, <<"All">>),
				Userip = State#user.ip,
				{Stat, RoomPid} = router_main:get_room(Usroom),
				case Stat of
					error ->
						self() ! Reply([{error,roomnotfound}]),
						State;
					ok -> 
						{Regstat, Usernum} = room_router:login(RoomPid, {self(), Userip}),
						case Regstat of
							'normal' ->
								room_router:set_user_vars(RoomPid, Usernum, #{name => Usname, id => Usid, ipaddr => u:ip2binary(Userip), role => <<"ordinar">>}),
								self() ! Reply([{status,ok},{num, Usernum},{name, Usname}]),
								State#user{servid=Usernum, id=Usid, name=Usname, registered="yes", room=Usroom, room_pid=RoomPid};
							'banned' ->
								self() ! Reply({disconn, [{error,youbanned}]}),
								State
						end
				end;
			"say" ->
				checkRoomRegistration(State),
				Dtsend = Reply([{num, State#user.servid},{id, State#user.id},{message, proplists:get_value(message, Data)}]),
				room_router:send(RPid, {Dtsend, State#user.name, State#user.servid}),
				State;
			"ping" ->
				self() ! {sreply, jsone:encode([{action,ping}, {message, pong}])},
				State;
			"login" ->
				Login = proplists:get_value(login, Data, <<"Anonymous">>),
				Password = proplists:get_value(password, Data, <<"password">>),
				case user_tokens:userLogin(Login, self(), Password) of
					{ok, {Access, Refresh}} -> 
						TPar = user_tokens:userParseAccess(maps:get(token,Access)),
						Nm = maps:get(<<"name">>, TPar, <<"Anonymous">>),
						Rl = maps:get(<<"role">>, TPar, <<"ordinar">>),
						ifRegistered(fun() -> room_router:set_user_vars(RPid, State#user.servid, #{name => Nm, role => Rl}) end, State),
						self() ! Reply([{status,ok},{role, Rl},{name, Nm},{token, Access},{refresh, Refresh}]);
					{error, _} -> 
						self() ! Reply([{error,errorlogindata}])
				end,
				State;
			"refresh" ->
				RefreshToken = proplists:get_value(refresh, Data, <<"badtoken">>),
				case user_tokens:userRefreshTokens(RefreshToken) of
					{ok, {Access, Refresh}} -> 
						TPar = user_tokens:userParseAccess(maps:get(token,Access)),
						Nm = maps:get(<<"name">>, TPar, <<"Anonymous">>),
						Rl = maps:get(<<"role">>, TPar, <<"ordinar">>),
						ifRegistered(fun() -> room_router:set_user_vars(RPid, State#user.servid, #{name => Nm, role => Rl}) end, State),						
						self() ! Reply([{status,ok},{role, Rl},{name, Nm},{token, Access},{refresh, Refresh}]);
					{error, _} -> 
						self() ! Reply([{error,errorrefresh}])
				end,
				State;
			"operation" ->
				checkRoomRegistration(State),
				Fun = fun() ->
					Dtsend = Reply([{num, State#user.servid},{operation, proplists:get_value(operation, Data, <<"empty">>)}]),
					room_router:send(RPid, {Dtsend, State#user.name, State#user.servid})
				end,
				execOnly([<<"admin">>,<<"moderator">>], Fun, State, Reply),
				State;
			"roomlist" ->
				checkRoomRegistration(State),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:roomlist(RPid, self()) end, State, Reply),
				State;
			"banuser" ->
				checkRoomRegistration(State),
				UserId = proplists:get_value(user, Data, <<"user-no">>),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:banuser(RPid, UserId) end, State, Reply),
				State;
			"unbanuser" ->
				checkRoomRegistration(State),
				IpAddr = proplists:get_value(ipaddr, Data, <<"10">>),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:unbanuser(RPid, IpAddr) end, State, Reply),
				State;
			"kickuser" ->
				checkRoomRegistration(State),
				UserId = proplists:get_value(user, Data, <<"user-no">>),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:kickuser(RPid, UserId) end, State, Reply),
				State;
			"banlist" ->
				checkRoomRegistration(State),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:banlist(RPid, self()) end, State, Reply),
				State;
			% "getroomsett" ->
			% 	Token = proplists:get_value(<<"token">>, Data),
			% 	case admaccess(Token, State) of
			% 		{ok, _} ->
			% 			Curr = room_router:gs(),
			% 			Formatted = [[{room, Room},{params,[Par1,Par2,Par3,Par4]}]||{Room,{_,Par1,Par2,Par3,Par4}} <- Curr],
			% 			Dtsend = {sreply, jsone:encode([{action, settings},{user, system},{message, Formatted}])},
			% 			self() ! Dtsend,
			% 			State;
			% 		{error, _} ->
			% 			State
			% 	end;
			% "setroomsett" ->
			% 	Token = proplists:get_value(<<"token">>, Data),
			% 	case admaccess(Token, State) of
			% 		{ok, _} ->
			% 			Room = proplists:get_value(<<"room">>, Data),
			% 			Param = proplists:get_value(<<"param">>, Data),
			% 			Command = proplists:get_value(<<"comm">>, Data),
			% 			case Param of
			% 				<<"hsend">> ->
			% 					room_router:ss(Room,{binary_to_atom(Param, latin1), binary_to_atom(Command, latin1)});
			% 				<<"log">> ->
			% 					room_router:ss(Room,{binary_to_atom(Param, latin1), binary_to_atom(Command, latin1)});						
			% 				<<"ustat">> ->
			% 					room_router:ss(Room,{binary_to_atom(Param, latin1), binary_to_atom(Command, latin1)});						
			% 				<<"lssend">> ->
			% 					room_router:ss(Room,{binary_to_atom(Param, latin1), binary_to_atom(Command, latin1)});						
			% 				_ ->
			% 					ok
			% 			end,
			% 			Curr = room_router:gs(),
			% 			Formatted = [[{room, Croom},{params,[Par1,Par2,Par3,Par4]}]||{Croom,{_,Par1,Par2,Par3,Par4}} <- Curr],
			% 			Dtsend = {sreply, jsone:encode([{action, settings},{user, system},{message, Formatted}])},
			% 			self() ! Dtsend,
			% 			State;
			% 		{error, _} ->
			% 			State
			% 	end;			
			{'EXIT',_} -> u:trace('ERROR IN JSON 2'),
				State;
			_ 				 -> u:trace('Incorrect command', Data),
				State
		end
	catch
		throw:not_registered -> 
			self() ! Reply([{error,notregistered}]),
			State;
		throw:not_allowed ->
			self() ! Reply([{error,not_allowed_by_you_role}]),
			State
	end.

schedule_ping(State, Interval) ->
  Ref = erlang:send_after(Interval, self(), do_ping),
  State#user{ping_timer=Ref}.
	
defaultReply(DefaultFields) ->
	DefaultData = #{action => systemsay, user => system, message => <<>>},
	Updated = lists:foldl(fun(Elem, Acc) -> maps:put(element(1, Elem), element(2, Elem), Acc) end, DefaultData, DefaultFields),	
	fun(Data) -> 
		if
			is_list(Data) -> sendReply(sreply, Data, Updated);
			is_tuple(Data) -> 
				{Mode, Data1} = Data,
				sendReply(Mode, Data1, Updated);
			true -> ok
		end
	end.

sendReply(Mode, Data, DefaultData) ->
	Updated = lists:foldl(fun(Elem, Acc) -> maps:put(element(1, Elem), element(2, Elem), Acc) end, DefaultData, Data),
	{Mode, jsone:encode(Updated)}.

execOnly(AllowedRoles, Fun, State, Reply) ->
	UserRole = State#user.role,
	case lists:search(fun(Role) -> Role == UserRole end, AllowedRoles) of
		{value, _Value} -> 
			Fun(),
			self() ! Reply([{status, ok}]);
		false -> 
			throw(not_allowed)
	end.

checkRoomRegistration(State) ->
	case State#user.registered of
		"none" ->
			throw(not_registered);
		"yes" -> ok
	end.
ifRegistered(Fun, State) ->
	case State#user.registered of
		"none" -> false;
		"yes" -> Fun()
	end.	
% admaccess(Token, State) when State#user.status =:= "admin" ->
% 	SavedToken = list_to_binary(State#user.token),
% 	case SavedToken of
% 		Token ->
% 			{ok, State};
% 		_ ->
% 			Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, erroradminregister}])},
% 			self() ! Dtsend,				
% 			{error, State}
% 	end;
% admaccess(_Token, State) ->
% 	Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, erroradminregister}])},
% 	self() ! Dtsend,				
% 	{error, State}.
			
