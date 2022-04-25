-module(ws_handler).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).

-record(user,{servid=0, registered="none", token="none", room, room_pid, ping_timer, fields=#{}}).

init(Req, _Opts) ->
	{IpAddress, _} = cowboy_req:peer(Req),
	Url = cowboy_req:path(Req),	
	State = #user{fields=setUserFields(#{}, [{ipaddr, u:ip2binary(IpAddress)},{role, <<"ordinar">>},{name, <<"Anonymous">>}])},
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
websocket_info({room_closed}, State) ->
	Text = jsone:encode([{action,room_close}]),
	NewState = State#user{servid=0, registered="none", room=undefined, room_pid=undefined},
	{[{text, Text}], NewState};
websocket_info({cast_reply, Msg}, State) ->
	{Text, State1} = case Msg of
		{login, _Data} -> processTokenResponce(Msg, State);
		{refresh, _Data} -> processTokenResponce(Msg, State);
		{create_user, Data} -> 
			{Status, Resp} = Data,
			{jsone:encode(#{action => create_admin, status => Status, data => Resp}), State};
		{remove_user, Data} -> 
			{Status, Resp} = Data,
			{jsone:encode(#{action => remove_admin, status => Status, data => Resp}), State};
		{list_users, Data} -> 
			{Status, Resp} = Data,
			{jsone:encode(#{action => list_admins, status => Status, data => Resp}), State};			
		_ -> 
			{Action, Data, Status} = Msg,
			{jsone:encode(#{action => Action, status => Status, data => Data}), State}
	end,
	{[{text, Text}], State1};
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

processTokenResponce(Data, State) ->
	case Data of
		{Mode, {ok, Access, Refresh}} -> 
			UpdatedUserFields = renewMapFromToken(maps:get(token,Access), State#user.fields),
			ifRegistered(fun() -> room_router:set_user_vars(State#user.room_pid, State#user.servid, UpdatedUserFields) end, State),
			{jsone:encode(#{action => Mode, status => ok, token => Access, refresh => Refresh}), State#user{fields = UpdatedUserFields}};
		{Mode, {error, Err}} -> 
			{jsone:encode(#{action => Mode, error => Err}), State}
	end.


analize({Data}, State1) ->
	ExcludeUserFields = [ipaddr],
	Action = proplists:get_value(action, Data, <<"ERROR">>),
	AccessToken = proplists:get_value(token, Data, <<"ERROR">>),
	State = case user_token:check(AccessToken) of
		true -> 
			State1#user{fields = renewMapFromToken(AccessToken, State1#user.fields)};
		false -> 
			State1
	end,
	Reply = defaultReply([{action, Action},{user, getUserFields(State#user.fields, {exclude, ExcludeUserFields})}]),
	ReplyNoUser = defaultReply([{action, Action}]),
	CURole = getUserFields(State#user.fields, role),
	RPid = State#user.room_pid,
	try
		case binary_to_list(Action) of
			"enterinroom" ->
				Usroom = proplists:get_value(room, Data, <<"All">>),
				{Stat, RoomPid} = router_main:get_room(Usroom),
				case Stat of
					error ->
						self() ! Reply([{error,roomnotfound},{room, Usroom},{user, <<>>}]),
						State;
					ok -> 
						{Regstat, Usernum} = room_router:login(RoomPid, {self(), getUserFields(State#user.fields, ipaddr)}),
						case Regstat of
							'normal' ->
								self() ! Reply([{status,ok},{num, Usernum},{room, Usroom}]),
								room_router:set_user_vars(RoomPid, State#user.servid, State#user.fields),
								State#user{servid=Usernum, registered="yes", room=Usroom, room_pid=RoomPid};
							'banned' ->
								self() ! Reply({disconn, [{error,youbanned},{user, <<>>}]}),
								State
						end
				end;			
			"setdata" ->
				Fields = proplists:get_value(fields, Data, []),
				UpdatedUserFields = setUserFields(State#user.fields, Fields),
				ifRegistered(fun() -> room_router:set_user_vars(RPid, State#user.servid, UpdatedUserFields) end, State),
				self() ! Reply([{status,ok},{user, getUserFields(UpdatedUserFields, {exclude, ExcludeUserFields})}]),
				State#user{fields = UpdatedUserFields};
			"say" ->
				checkRoomRegistration(State),
				Dtsend = Reply([{num, State#user.servid},{message, proplists:get_value(message, Data)}]),
				room_router:send(RPid, {Dtsend, State#user.servid}),
				State;
			"whisper" ->
				checkRoomRegistration(State),
				Recipient = proplists:get_value(to, Data, <<"empty">>),
				Dtsend = Reply([{num, State#user.servid},{message, proplists:get_value(message, Data)}]),
				room_router:whisper(RPid, {Dtsend, State#user.servid, Recipient}),
				State;
			"ping" ->
				self() ! {sreply, jsone:encode([{action,ping}, {message, pong}])},
				State;
			"login" ->
				Login = proplists:get_value(login, Data, <<"Anonymous">>),
				Password = proplists:get_value(password, Data, <<"password">>),
				user_token:login(self(), {Login, Password}),
				State;
			"refresh" ->
				RefreshToken = proplists:get_value(refresh, Data, <<"badtoken">>),
				user_token:refresh(self(), {RefreshToken}),
				State;
			"operation" ->
				checkRoomRegistration(State),
				Fun = fun() ->
					Dtsend = Reply([{num, State#user.servid},{operation, proplists:get_value(operation, Data, <<"empty">>)}]),
					room_router:send(RPid, {Dtsend, State#user.servid})
				end,
				execOnly([<<"admin">>,<<"moderator">>], Fun, CURole),
				State;
			"roomlist" ->
				checkRoomRegistration(State),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:roomlist(RPid, self()) end, CURole),
				State;
			"banuser" ->
				checkRoomRegistration(State),
				UserId = proplists:get_value(user, Data, <<"user-no">>),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:banuser(RPid, UserId) end, CURole, ReplyNoUser),
				State;
			"unbanuser" ->
				checkRoomRegistration(State),
				IpAddr = proplists:get_value(ipaddr, Data, <<"10">>),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:unbanuser(RPid, IpAddr) end, CURole, ReplyNoUser),
				State;
			"kickuser" ->
				checkRoomRegistration(State),
				UserId = proplists:get_value(user, Data, <<"user-no">>),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:kickuser(RPid, UserId) end, CURole, ReplyNoUser),
				State;
			"banlist" ->
				checkRoomRegistration(State),
				execOnly([<<"admin">>,<<"moderator">>], fun() -> room_router:banlist(RPid, self()) end, CURole),
				State;
			"list_rooms" ->
				execOnly([<<"admin">>], fun() -> router_main:list_rooms(self()) end, CURole),
				State;
			"create_room" ->
				RoomToCreate = proplists:get_value(room, Data, <<"All">>),
				execOnly([<<"admin">>], fun() -> router_main:create_room(RoomToCreate) end, CURole, ReplyNoUser),
				State;
			"remove_room" ->
				RoomToCreate = proplists:get_value(room, Data, <<"000">>),
				execOnly([<<"admin">>], fun() -> router_main:remove_room(RoomToCreate) end, CURole, ReplyNoUser),
				State;
			"list_admins" ->
				execOnly([<<"admin">>], fun() -> user_token:list_users(self()) end, CURole),
				State;
			"create_admin" ->
				Login = proplists:get_value(login, Data, <<"login">>),
				Name = proplists:get_value(name, Data, <<"Anonymous">>),
				Password = proplists:get_value(password, Data, <<"simple-password">>),
				Role = proplists:get_value(role, Data, <<"ordinar">>),
				execOnly([<<"admin">>], fun() -> user_token:create_user(self(), {Login, Name, Password, Role}) end, CURole),
				State;
			"remove_admin" ->
				UserId = proplists:get_value(id, Data, <<"000">>),
				execOnly([<<"admin">>], fun() -> user_token:remove_user(self(), UserId) end, CURole),
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
	DefaultData = #{action => systemsay},
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
	Update = fun(Elem, Acc) -> 
		case element(2, Elem) of
			<<>> -> maps:remove(element(1, Elem), Acc);
			_ -> maps:put(element(1, Elem), element(2, Elem), Acc)
		end
  end,
	{Mode, jsone:encode(lists:foldl(Update, DefaultData, Data))}.

execOnly(AllowedRoles, Fun, UserRole) ->
	execOnly(AllowedRoles, Fun, UserRole, false).
execOnly(AllowedRoles, Fun, UserRole, Reply) ->
	case lists:search(fun(Role) -> Role == UserRole end, AllowedRoles) of
		{value, _Value} -> 
			Stat = Fun(),
			ToReply = case Stat of
				ok -> {status, ok};
				{ok, _T} -> {status, ok};
				error -> {error, notnamed};
				{error, Message} -> {error, Message}
			end,
			if 
				is_function(Reply) -> self() ! Reply([ToReply]);
				true -> ok
			end;
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

renewMapFromToken(AccessToken, Map) ->
	TokenParams = user_token:parse(AccessToken),
	NameUserFromToken = maps:get(<<"name">>, TokenParams, <<"Anonymous">>),
	RoleUserFromToken = maps:get(<<"role">>, TokenParams, <<"ordinar">>),
	setUserFields(Map, [{name, NameUserFromToken}, {role, RoleUserFromToken}]).


getUserFields(Map, {exclude, Fields}) when is_list(Fields) ->
	lists:foldl(fun(Elem, Acc) -> maps:remove(Elem, Acc) end, Map, Fields);
getUserFields(Map, Key) when is_atom(Key) ->
	getUserFields(Map, Key, <<"DEFAULTVALUE">>).
getUserFields(Map, Key, Default) ->
	maps:get(Key, Map, Default).
	

setUserFields(Map, {Data}) when is_list(Data) ->
	setUserFields(Map, Data);
setUserFields(Map, Data) when is_tuple(Data) ->
	{Param, Value} = Data,
	maps:put(Param, Value, Map);
setUserFields(Map, Data) when is_list(Data) ->
	lists:foldl(fun(Elem, Acc) -> setUserFields(Acc, Elem) end, Map, Data).
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
			
