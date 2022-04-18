-module(user_tokens).
-compile(nowarn_unused_function).

-export([userLogin/3]).

userLogin(Login,_Pid,Password) ->
  D = authDbOpen(),
  case D of
    {ok, IoDev} ->
      case findUserOnDb(Login, Password, IoDev) of
        {ok, UserStruct} -> 
          file:close(IoDev),
          {_, AccessToken, RefreshToken} = genTokens(UserStruct),
          NewSession = saveSession(UserStruct, RefreshToken),
          updateDb(NewSession),
          {ok, {AccessToken, RefreshToken}};
        {error, Reason} ->
          file:close(IoDev),
          u:trace("AUTH_FILE_NOT_FOUND", Reason),
          {error, Reason}
      end;
    {error, Reason} ->
      {error, Reason}
  end.

genTokens(UserStruct) ->
  genTokens(UserStruct, 1800).

genTokens(UserStruct, ExpireSeconds) ->
  {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
  Utime = MegaSecs * 1000000 + Secs,
  Salt = u:getSetting(salt),
  Refresh = uuid:uuid_to_string(uuid:get_v4(), binary_standard),
  Header = [{alg, <<"MD5">>}, {typ, <<"JWT">>}],
  UserName = maps:get(<<"name">>, maps:get(<<"userdata">>, UserStruct)),
  UserLogin = maps:get(<<"login">>, maps:get(<<"userdata">>, UserStruct)),
  Payload = [{exp, Utime + ExpireSeconds}, {login, UserLogin}, {name, UserName}],
  Data = base64:encode_to_string(jsone:encode(Header)) ++ "." ++ base64:encode_to_string(jsone:encode(Payload)),
  Hash = u:md5_hex(lists:concat([Data, Salt])),
  {ok, Data ++ "." ++ Hash, Refresh}.
  

saveSession(UserStruct, RefreshToken) ->
  saveSession(UserStruct, RefreshToken, 5184000). %60 days default

saveSession(UserStruct, RefreshToken, ExpireSeconds) ->
  {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
  Expire = MegaSecs * 1000000 + Secs + ExpireSeconds,
  Session = #{expire => Expire, token => RefreshToken},
  Sessions = maps:get(<<"sessions">>, UserStruct),
  AddSession = case lists:foldl(fun(_X, Sum) -> 1 + Sum end, 0, Sessions) < 5 of
    true ->
      Sessions ++ [Session];
    _ -> 
      [] ++ [Session]
  end,
  UserStruct1 = maps:update(<<"sessions">>, AddSession, UserStruct),
  UserStruct1.

updateDb(UserStruct) ->
  UserId = maps:get(<<"userid">>, UserStruct),
  D = authDbOpen(),
  case D of
    {ok, IoDev} ->
      UpdatedDb = updateDb(UserStruct, IoDev, UserId, <<>>),
      file:close(IoDev),
      file:write_file("./auth.dat", UpdatedDb);
    {error, Reason} ->
      {error, Reason}
  end.  
updateDb(UserStruct, IoDev, UserId, Acc) ->
  Line = file:read_line(IoDev),
  case Line of
    {ok, Record} ->
      Id = maps:get(<<"userid">>, jsone:decode(Record)),
      Item = case Id of
        UserId -> 
          jsone:encode(UserStruct);
        _ -> 
          string:trim(Record)
      end,
      Acc1 = <<Acc/binary, Item/binary, <<"\n">>/binary>>,
      updateDb(UserStruct, IoDev, UserId, Acc1);
    eof ->
      Acc
  end.

findUserOnDb(Login, Password, IoDev) ->
  Line = file:read_line(IoDev),
  case Line of
    {ok, Record} ->
      UserStruct = jsone:decode(string:trim(Record)),
      DbLogin = maps:get(<<"login">>, maps:get(<<"userdata">>, UserStruct)),
      DbPassword = maps:get(<<"password">>, maps:get(<<"userdata">>, UserStruct)),
      case {DbLogin, DbPassword} of
        {Login, Password} -> 
          {ok, UserStruct};
        _ -> 
          findUserOnDb(Login, Password, IoDev)
      end;
    eof ->
      {error, notfoundindb}
  end.

authDbOpen() ->
  D = file:open("./auth.dat", [read, write, binary]),
  case D of
    {ok, IoDev} ->
      {ok, IoDev};
    {error, Reason} ->
      u:trace("AUTH_FILE_OPEN_ERROR", Reason),
      {error, readfileerror}
  end.