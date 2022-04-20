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
	IpAddress = State#user.ip,
	%u:trace('JSON DATA',MssDec),
	NewState = case MssDec of
		{error,_} -> 
			u:trace('ERROR IN JSON 1',Msg),
			State;
		{ok, Data, _} -> analize(Data,State,IpAddress)
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
	
analize({Data}, State1, Userip) ->
	Action = proplists:get_value(action, Data, <<"ERROR">>),
	AccessToken = proplists:get_value(token, Data, <<"ERROR">>),
	State = case user_tokens:userCheckAccess(AccessToken) of
		true -> 
			TokenParams = user_tokens:userParseAccess(AccessToken),
			State1#user{name = maps:get(<<"name">>, TokenParams, <<"Anonymous">>), role = maps:get(<<"role">>, TokenParams, <<"ordinar">>)};
		false -> State1
	end,
	Reply = defaultReply([{action, Action}]),
	case binary_to_list(Action) of
		"register" ->
			Usname = proplists:get_value(name, Data, <<"Anonymous">>),
			Usid = proplists:get_value(userid, Data, 0),
			Usroom = proplists:get_value(room, Data, <<"All">>),
			{Stat, RoomPid} = router_main:get_room(Usroom),
			case Stat of
				error ->
					self() ! Reply([{error,roomnotfound}]),
					exit(self(), normal);
				ok -> 
					{Regstat, Usernum} = room_router:login(RoomPid, {self(), Userip}),
					case Regstat of
						'normal' ->
							room_router:set_user_vars(RoomPid, Usernum, #{name => Usname, id => Usid, ipaddr => u:ip2binary(Userip), role => <<"ordinar">>}),
							self() ! Reply([{message, Usname}]),
							#user{servid=Usernum, id=Usid, name=Usname, ip=Userip, registered="yes", room=Usroom, room_pid=RoomPid};
						'banned' ->
							self() ! Reply({disconn, [{error,youbanned}]})
					end
			end;
		"say" ->
			case State#user.registered of
				"none" ->
					self() ! Reply([{error,notregistered}]);
				"yes" ->
					Dtsend = Reply([{num, State#user.servid},{role, State#user.role},{id, State#user.id},{name, State#user.name},{message, proplists:get_value(message, Data)}]),
					room_router:send(State#user.room_pid, {Dtsend, State#user.name, State#user.servid})
			end,
			State;
		"ping" ->
			self() ! Reply([{message,pong}]),
			State;			
		"login" ->
			Login = proplists:get_value(login, Data, <<"Anonymous">>),
			Password = proplists:get_value(password, Data, <<"password">>),
			case user_tokens:userLogin(Login, self(), Password) of
				{ok, {Access, Refresh}} -> 
					TPar = user_tokens:userParseAccess(maps:get(token,Access)),
					Nm = maps:get(<<"name">>, TPar, <<"Anonymous">>),
					Rl = maps:get(<<"role">>, TPar, <<"ordinar">>),
					room_router:set_user_vars(State#user.room_pid, State#user.servid, #{name => Nm, role => Rl}),
					self() ! Reply([{status,ok},{token, Access},{refresh, Refresh}]);
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
					room_router:set_user_vars(State#user.room_pid, State#user.servid, #{name => Nm, role => Rl}),
					self() ! Reply([{status,ok},{token, Access},{refresh, Refresh}]);
				{error, _} -> 
					self() ! Reply([{error,errorrefresh}])
			end,
			State;
		"operation" ->
			Rl = State#user.role,
			case Rl of
				_ when Rl == <<"ordinar">> -> ok;
				_ when Rl == <<"admin">>;
							 Rl == <<"moderator">> ->
					Dtsend = Reply([{num, State#user.servid},{user, system},{role, State#user.role},{name, State#user.name},{message, proplists:get_value(message, Data, <<"empty">>)}]),
					room_router:send(Dtsend, State#user.room, State#user.name, State#user.servid)
			end,
			State;
		"roomlist" ->
			Rl = State#user.role,		
			case Rl of
				_ when Rl == <<"ordinar">> -> ok;
				_ when Rl == <<"admin">>;
							 Rl == <<"moderator">> -> room_router:roomlist(State#user.room_pid, self())
			end,
			State;
		"banuser" ->
			Rl = State#user.role,		
			case Rl of
				_ when Rl == <<"ordinar">> -> ok;
				_ when Rl == <<"admin">>;
							 Rl == <<"moderator">> -> room_router:banuser(State#user.room_pid, proplists:get_value(user, Data, <<"user-no">>))
			end,
			State;
		"unbanuser" ->
			Rl = State#user.role,		
			case Rl of
				_ when Rl == <<"ordinar">> -> ok;
				_ when Rl == <<"admin">>;
							 Rl == <<"moderator">> -> room_router:unbanuser(State#user.room_pid, proplists:get_value(ipaddr, Data, <<"10">>))
			end,
			State;
		"banlist" ->
			Rl = State#user.role,		
			case Rl of
				_ when Rl == <<"ordinar">> -> ok;
				_ when Rl == <<"admin">>;
							 Rl == <<"moderator">> -> room_router:banlist(State#user.room_pid, self())
			end,
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
		% "admkick" ->
		% 	Token = proplists:get_value(<<"token">>, Data),
		% 	case admaccess(Token, State) of
		% 		{ok, _} ->
		% 			Usernum = proplists:get_value(<<"user">>, Data),
		% 			room_router:kick(Usernum),
		% 			State;
		% 		{error, _} ->
		% 			State
		% 	end;	
		{'EXIT',_} -> 
			u:trace('ERROR IN JSON 2'),
			State;
		_ ->
			u:trace('Incorrect command', Data),
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
			
