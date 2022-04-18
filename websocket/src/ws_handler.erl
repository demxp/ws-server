-module(ws_handler).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).

-record(user,{servid=0, id="0", name="user", ip, status="ordinar", registered="none", token="none", room, room_pid, ping_timer}).

init(Req, _Opts) ->
	{IpAddress, _} = cowboy_req:peer(Req),
	Url = cowboy_req:path(Req),	
	State = #user{ip=IpAddress},
	Ret = case access_test:test(IpAddress, Url) of
		{ok, _} ->
			State1 = schedule_ping(State, 30000),
			{cowboy_websocket, Req, State1, #{idle_timeout => 30000*2}};
		{error, _} ->
			Req1 = cowboy_req:reply(403, [], <<"Too many connections.">>, Req),
			{ok, Req1, undefined}
	end,
	Ret.


websocket_handle({text, Msg}, State) ->
	MssDec = jsone:try_decode(Msg, [{object_format, tuple},reject_invalid_utf8,{keys, atom}]),
	IpAddress = State#user.ip,
	u:trace('JSON DATA',MssDec),
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
	exit(self(), normal),
	{[{text, Msg}], State};
websocket_info(do_ping, State) ->
  {[{ping, <<>>}], State};	
websocket_info(_Info, State) ->
	{[], State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
	
schedule_ping(State, Interval) ->
  Ref = erlang:send_after(Interval, self(), do_ping),
  State#user{ping_timer=Ref}.
	
	
	

analize({Data}, State, Userip) ->
	case catch binary_to_list(proplists:get_value(action, Data, <<"ERROR">>)) of
		"register" ->
			Usname = proplists:get_value(name, Data, <<"Anonymous">>),
			Usid = proplists:get_value(userid, Data, 0),
			Usroom = proplists:get_value(room, Data, <<"All">>),
			{Stat, RoomPid} = router_main:get_room(Usroom),
			case Stat of
				error ->
					Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, roomnotfound}])},
					self() ! Dtsend,
					exit(self(), normal);
				ok -> 
					{Regstat, Usernum} = room_router:login(RoomPid, {self(), Userip}),
					case Regstat of
						'normal' ->
							self() ! {sreply, jsone:encode([{action, register},{user, system},{message, Usname}])},
							#user{servid=Usernum, id=Usid, name=Usname, ip=Userip, registered="yes", room=Usroom, room_pid=RoomPid};
						'banned' ->
							self() ! {disconn, jsone:encode([{action, systemsay},{user, system},{message, youbanned}])}
					end
			end;
		"say" ->
			case State#user.registered of
				"none" ->
					self() ! {sreply, jsone:encode([{action, systemsay},{user, system},{message, errornotregistered}])};
				"yes" ->
					Dtsend = {sreply, jsone:encode([{action, say},{num, State#user.servid},{id, State#user.id},{name, State#user.name},{message, proplists:get_value(message, Data)}])},
					room_router:send(State#user.room_pid, {Dtsend, State#user.name, State#user.servid})
			end,
			State;
		"ping" ->
			Dtsend = {sreply, jsone:encode([{action, ping},{user, system},{message, pong}])},
			self() ! Dtsend,		
			State;			
		"admregister" ->
			Login = proplists:get_value(login, Data),
			Password = proplists:get_value(password, Data),
			case user_tokens:userLogin(Login, self(), Password) of
				{ok, {Access, Refresh}} -> 
					room_router:setadm(State#user.room_pid, State#user.servid),
					Dtsend = {sreply, jsone:encode([{action, admregister},{user, system},{message, completeadminregister},{token, Access},{refresh, Refresh}])},
					self() ! Dtsend,
					State#user{status="admin", token=Access};
				{error, _} -> 
					Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, erroradminregister}])},
					self() ! Dtsend,				
					State
			end;
		"admrefresh" ->
			RefreshToken = proplists:get_value(refresh, Data),
			case user_tokens:userRefreshTokens(RefreshToken) of
				{ok, {Access, Refresh}} -> 
					Dtsend = {sreply, jsone:encode([{action, admrefresh},{user, system},{message, completerefreshtokens},{token, Access},{refresh, Refresh}])},
					self() ! Dtsend;
				{error, _} -> 
					Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, errorrefreshtokens}])},
					self() ! Dtsend,				
					State
			end;
		% "admsay" ->
		% 	Token = proplists:get_value(<<"token">>, Data),
		% 	case admaccess(Token, State) of
		% 		{ok, _} ->
		% 			Dtsend = {sreply, jsone:encode([{action, admsay},{num, State#user.servid},{user, system},{message, proplists:get_value(<<"message">>, Data)}])},
		% 			room_router:send(Dtsend, State#user.room, State#user.name, State#user.servid),
		% 			State;
		% 		{error, _} ->
		% 			State
		% 	end;
		% "admoperations" ->
		% 	Token = proplists:get_value(<<"token">>, Data),
		% 	case admaccess(Token, State) of
		% 		{ok, _} ->
		% 			Dtsend = {sreply, jsone:encode([{action, operation},{num, State#user.servid},{user, system},{message, proplists:get_value(<<"message">>, Data)}])},
		% 			room_router:send(Dtsend, State#user.room, State#user.name, State#user.servid),
		% 			State;
		% 		{error, _} ->
		% 			State
		% 	end;
		% "getadmlist" ->
		% 	Token = proplists:get_value(<<"token">>, Data),
		% 	case admaccess(Token, State) of
		% 		{ok, _} ->
		% 			room_router:admlst(self()),
		% 			State;
		% 		{error, _} ->
		% 			State
		% 	end;
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
		% "getbanlist" ->
		% 	Token = proplists:get_value(<<"token">>, Data),
		% 	case admaccess(Token, State) of
		% 		{ok, _} ->
		% 			room_router:banlst(self()),
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
		% "admuserban" ->
		% 	Token = proplists:get_value(<<"token">>, Data),
		% 	case admaccess(Token, State) of
		% 		{ok, _} ->
		% 			Usernum = proplists:get_value(<<"user">>, Data),
		% 			room_router:userban(Usernum),
		% 			State;
		% 		{error, _} ->
		% 			State
		% 	end;			
		% "admunban" ->
		% 	Token = proplists:get_value(<<"token">>, Data),
		% 	case admaccess(Token, State) of
		% 		{ok, _} ->
		% 			Usernum = proplists:get_value(<<"ipnumber">>, Data),
		% 			room_router:unban(Usernum),
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
			
