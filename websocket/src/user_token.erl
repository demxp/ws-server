-module(user_token).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([
  login/2,
  refresh/2,
  update/0
]).

-define(SERVER, ?MODULE).
-record(state,{db=ets:new(?MODULE, [bag, {keypos, 1}]), save_timer, iswork = 0}).
-record(user,{login, name, password, role}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  self() ! initall,
  State = schedule_save(#state{}, websocket_app:get_env(<<"server.login.save-interval">>)),
  {ok, State}.

login(Pid, {Login, Password}) ->
  gen_server:cast(?SERVER, {login, Login, Password, Pid}).

refresh(Pid, {Token}) ->
  gen_server:cast(?SERVER, {refresh, Token, Pid}).

update() ->
  gen_server:call(?SERVER, update).

handle_call(update, _From, State) ->
  updateFile(State),
  {reply, {ok}, State};
handle_call(stop, _From, State) ->
  {stop, normal, shutdown_ok, State};
handle_call(_Msg, _From, State) ->
  {reply, {ok, {removed}}, State}.

handle_cast({login, Login, Password, Pid}, #state{} = State) ->
  % PasswordHash = list_to_binary(u:md5_hex(Password)),
  PasswordHash = Password,
  Resl = ets:match(State#state.db, {'$1', #user{login=Login, password=PasswordHash, _='_'}}),
  try
    [User] = lists:last(Resl),
    {ok, Access, Refresh} = genTokens(userid(User), State),
    {ok, complete} = saveSession(userid(User), Refresh, State),
    Pid ! {cast_reply, {login, {ok, Access, Refresh}}},
    {noreply, State}
  catch
    _:_ -> 
    Pid ! {cast_reply, {login, {error, notfound}}},
    {noreply, State}
  end;
handle_cast({refresh, Token, Pid}, #state{} = State) ->
  Resl = ets:match_object(State#state.db, {'_', {Token, '_'}}),
  try
    {SessionKey, {Token, Expire}}  = lists:last(Resl),
    CTime = u:utime(asinteger),
    if 
      Expire < CTime -> throw(expired);
      true -> ok
    end,
    {ok, Access, Refresh} = genTokens(userid(SessionKey), State),
    {ok, complete} = saveSession(userid(SessionKey), Refresh, State),
    Pid ! {cast_reply, {refresh, {ok, Access, Refresh}}},
    {noreply, State}
  catch
    _:_ -> 
    Pid ! {cast_reply, {refresh, {error, notfound}}},
    {noreply, State}
  end;
handle_cast(_Msg, #state{} = State) ->
  {noreply, State}.

handle_info(do_save, #state{} = State) ->
  State1 = schedule_save(State, websocket_app:get_env(<<"server.login.save-interval">>)),  
  updateFile(State1),
  {noreply, State1}; 
handle_info(initall, #state{} = State) ->
  initAll(State),
  {noreply, State}; 
handle_info(Info, #state{} = State) ->
  u:trace("BAD MESSAGE ROUTER_MAIN", Info),
  {noreply, State}.

initAll(State) ->
  {ok, IoDev} = authFileOpen(),
  {ok, complete} = parseString(IoDev, State).

parseString(IoDev, State) ->
  Line = file:read_line(IoDev),
  case Line of
    {ok, Record} ->
      UserStruct = jsone:decode(string:trim(Record)),
      UserId = maps:get(<<"userid">>, UserStruct),
      UserData = maps:get(<<"userdata">>, UserStruct),
      Sessions = maps:get(<<"sessions">>, UserStruct),

      UserRecord = maps:fold(fun(Key, Val, Acc) -> 
        Key1 = binary_to_atom(Key),
        Index = u:index_of(Key1, record_info(fields, user)),
        setelement(Index+1, Acc, Val)
      end, #user{}, UserData),
      ets:insert(State#state.db, {userkey(UserId), UserRecord}),
      lists:map(fun(S) -> ets:insert(State#state.db, {sesskey(UserId), {maps:get(<<"token">>, S),maps:get(<<"expire">>, S)}}) end, Sessions),
      parseString(IoDev, State);
    eof ->
      {ok, complete}
  end.

genTokens(UserId, State) ->
  {ok, Salt} = websocket_app:get_env(<<"server.salt">>),
  {ok, ExpireAccess} = websocket_app:get_env(<<"admin.tokens.expire-access">>, 1800), %30 min default
  {ok, ExpireRefresh} = websocket_app:get_env(<<"admin.tokens.expire-refresh">>, 5184000),  %60 days default
  [{Id, User}] = ets:lookup(State#state.db, userkey(UserId)),
  RefreshToken = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
  Refresh = #{expire => u:utime(asinteger) + ExpireRefresh, token => RefreshToken},
  Header = [{alg, <<"MD5">>}, {typ, <<"JWT">>}],
  Payload = [{exp, u:utime(asinteger) + ExpireAccess}, {id, Id}, {login, User#user.login}, {name, User#user.name}, {role, User#user.role}],
  Data = base64:encode_to_string(jsone:encode(Header)) ++ "." ++ base64:encode_to_string(jsone:encode(Payload)),
  Hash = list_to_binary(u:md5_hex(Data ++ Salt)),
  BinData = list_to_binary(Data),
  Access = #{expire => u:utime(asinteger) + ExpireAccess, token => <<BinData/binary, <<".">>/binary, Hash/binary>>},  
  {ok, Access, Refresh}.

saveSession(UserId, RefreshToken, State) ->
  SessionKey = sesskey(userid(UserId)),
  Resl = ets:lookup(State#state.db, SessionKey),
  case length(Resl) >= 5 of
    true -> ets:delete(State#state.db, SessionKey);
    false -> ok
  end,
  ets:insert(State#state.db, {SessionKey, {maps:get(token, RefreshToken),maps:get(expire, RefreshToken)}}),
  {ok, complete}.

updateFile(State) ->
  Users = ets:match_object(State#state.db, {'_', #user{_='_'}}),
  AddSessions = fun(Elem, Acc) ->
    {Key, UserData} = Elem,
    Resl = ets:lookup(State#state.db, sesskey(userid(Key))),
    Sessions = lists:foldl(fun(Sess, Acc1) -> 
      {_SKey, {SToken, SExp}} = Sess,
      [#{expire => SExp, token => SToken} | Acc1]
    end, [], Resl),
    UserRecInfo = record_info(fields, user),
    UserRecord = lists:foldl(fun(Param, Acc2) ->
      Index = u:index_of(Param, UserRecInfo),
      maps:put(Param, element(Index+1, UserData), Acc2)
    end, #{}, UserRecInfo),
    UserRecordFull = jsone:encode(#{userid => userid(Key), userdata => UserRecord, sessions => Sessions}),
    <<Acc/binary, UserRecordFull/binary, <<"\n">>/binary>>
  end,
  Data = lists:foldl(AddSessions, <<>>, Users),
  file:write_file("./auth.dat", Data).

userid(<<"user-record-", UserId/binary>>) ->
  UserId;
userid(<<"session-", UserId/binary>>) ->
  UserId;
userid(<<UserId:36/binary>>) ->
  UserId;
userid(_K) ->
  {error, incorrect}.

userkey(<<UserId:36/binary>>) ->
  <<<<"user-record-">>/binary,UserId/binary>>;
userkey(_K) ->
  {error, incorrect}.

sesskey(<<UserId:36/binary>>) ->
  <<<<"session-">>/binary,UserId/binary>>;
sesskey(_K) ->
  {error, incorrect}.

authFileOpen() ->
  D = file:open("./auth.dat", [read, write, binary]),
  case D of
    {ok, IoDev} ->
      {ok, IoDev};
    {error, Reason} ->
      u:trace("AUTH_FILE_OPEN_ERROR", Reason),
      {error, readfileerror}
  end.

schedule_save(State, Interval) ->
  {ok, IntervalSec} = Interval,
  Ref = erlang:send_after(IntervalSec*1000, self(), do_save),
  State#state{save_timer=Ref}.

terminate(_Reason, State) ->
  updateFile(State),
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.