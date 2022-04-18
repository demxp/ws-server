-module(ws_handler).

-export([init/2]).
-export([websocket_handle/2]).
-export([websocket_info/2]).
-export([websocket_terminate/3]).

-record(user,{servid=0, id="0", name="user", ip, status="ordinar", registered="none", token="none", room="all", ping_timer}).

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
			Usname = proplists:get_value(name, Data),
			Usid = proplists:get_value(userid, Data),
			Usroom = proplists:get_value(room, Data),
			case Usroom of
				<<>> ->
					Usroom1 = <<"all">>,
					{Regstat, Usernum} = router:login(Usid, self(), Usname, Usroom1, Userip),
					case Regstat of
						'normal' ->
							Dtsend = {sreply, jsone:encode([{action, newname},{user, system},{message, Usname}])},
							self() ! Dtsend,
							#user{servid=Usernum, id=Usid, name=Usname, ip=Userip, registered="yes", room=Usroom1};
						'banned' ->
							Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, youbanned}])},
							self() ! Dtsend,
							self() ! {disconn, ""}
					end;
				undefined ->
					Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, errorenterroom}])},
					self() ! Dtsend,
					State;
				_ ->
					{Regstat, Usernum} = router:login(Usid, self(), Usname, Usroom, Userip),
					case Regstat of
						'normal' ->
							Dtsend = {sreply, jsone:encode([{action, newname},{user, system},{message, Usname}])},
							self() ! Dtsend,
							#user{servid=Usernum, id=Usid, name=Usname, ip=Userip, registered="yes", room=Usroom};
						'banned' ->
							Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, youbanned}])},
							self() ! Dtsend,
							self() ! {disconn, ""}
					end
			end;
		"say" ->
			case State#user.registered of
				"none" ->
					Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, errornotregistered}])},
					self() ! Dtsend;
				"yes" ->
					Dtsend = {sreply, jsone:encode([{action, say},{num, State#user.servid},{id, State#user.id},{name, State#user.name},{message, proplists:get_value(<<"message">>, Data)}])},
					router:send(Dtsend, State#user.room, State#user.name, State#user.servid)
			end,
			State;
		"ping" ->
			Dtsend = {sreply, jsone:encode([{action, check},{user, system},{message, pong}])},
			self() ! Dtsend,		
			State;			
		"admregister" ->
			Login = proplists:get_value(login, Data),
			Password = proplists:get_value(password, Data),
			Tokens = user_tokens:userLogin(Login, self(), Password),

			case Tokens of
				{ok, {Access, _Refresh}} -> 
					%router:setadm(State#user.servid),
					Dtsend = {sreply, jsone:encode([{action, compladmin},{user, system},{message, completeadminregister},{token, list_to_binary(Access)}])},
					self() ! Dtsend,
					State#user{status="admin", token=Access};
				{error, _} -> 
					Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, erroradminregister}])},
					self() ! Dtsend,				
					State
			end;

		"admsay" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					Dtsend = {sreply, jsone:encode([{action, admsay},{num, State#user.servid},{user, system},{message, proplists:get_value(<<"message">>, Data)}])},
					router:send(Dtsend, State#user.room, State#user.name, State#user.servid),
					State;
				{error, _} ->
					State
			end;
		"admoperations" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					Dtsend = {sreply, jsone:encode([{action, operation},{num, State#user.servid},{user, system},{message, proplists:get_value(<<"message">>, Data)}])},
					router:send(Dtsend, State#user.room, State#user.name, State#user.servid),
					State;
				{error, _} ->
					State
			end;
		"getadmlist" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					router:admlst(self()),
					State;
				{error, _} ->
					State
			end;
		"getroomsett" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					Curr = router:gs(),
					Formatted = [[{room, Room},{params,[Par1,Par2,Par3,Par4]}]||{Room,{_,Par1,Par2,Par3,Par4}} <- Curr],
					Dtsend = {sreply, jsone:encode([{action, settings},{user, system},{message, Formatted}])},
					self() ! Dtsend,
					State;
				{error, _} ->
					State
			end;
		"setroomsett" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					Room = proplists:get_value(<<"room">>, Data),
					Param = proplists:get_value(<<"param">>, Data),
					Command = proplists:get_value(<<"comm">>, Data),
					case Param of
						<<"hsend">> ->
							router:ss(Room,{binary_to_atom(Param, latin1), binary_to_atom(Command, latin1)});
						<<"log">> ->
							router:ss(Room,{binary_to_atom(Param, latin1), binary_to_atom(Command, latin1)});						
						<<"ustat">> ->
							router:ss(Room,{binary_to_atom(Param, latin1), binary_to_atom(Command, latin1)});						
						<<"lssend">> ->
							router:ss(Room,{binary_to_atom(Param, latin1), binary_to_atom(Command, latin1)});						
						_ ->
							ok
					end,
					Curr = router:gs(),
					Formatted = [[{room, Croom},{params,[Par1,Par2,Par3,Par4]}]||{Croom,{_,Par1,Par2,Par3,Par4}} <- Curr],
					Dtsend = {sreply, jsone:encode([{action, settings},{user, system},{message, Formatted}])},
					self() ! Dtsend,
					State;
				{error, _} ->
					State
			end;			
		"getbanlist" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					router:banlst(self()),
					State;
				{error, _} ->
					State
			end;	
		"admkick" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					Usernum = proplists:get_value(<<"user">>, Data),
					router:kick(Usernum),
					State;
				{error, _} ->
					State
			end;	
		"admuserban" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					Usernum = proplists:get_value(<<"user">>, Data),
					router:userban(Usernum),
					State;
				{error, _} ->
					State
			end;			
		"admunban" ->
			Token = proplists:get_value(<<"token">>, Data),
			case admaccess(Token, State) of
				{ok, _} ->
					Usernum = proplists:get_value(<<"ipnumber">>, Data),
					router:unban(Usernum),
					State;
				{error, _} ->
					State
			end;			
		{'EXIT',_} -> 
			u:trace('ERROR IN JSON 2'),
			State;
		_ ->
			u:trace('Incorrect command', Data),
			State
	end.
	
admaccess(Token, State) when State#user.status =:= "admin" ->
	SavedToken = list_to_binary(State#user.token),
	case SavedToken of
		Token ->
			{ok, State};
		_ ->
			Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, erroradminregister}])},
			self() ! Dtsend,				
			{error, State}
	end;
admaccess(_Token, State) ->
	Dtsend = {sreply, jsone:encode([{action, systemsay},{user, system},{message, erroradminregister}])},
	self() ! Dtsend,				
	{error, State}.
			
