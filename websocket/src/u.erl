-module(u).
-compile(nowarn_unused_function).

-export([trace/1, trace/2, traceB/1, traceB/2, testadmin/3, md5_hex/1, getSetting/1, utime/0]).

trace(X) -> spawn(fun() -> io:format("~p~n",[X]) end).
traceB(X) -> spawn(fun() -> io:format("~s~n",[X]) end).
trace(X,Y) -> spawn(fun() -> io:format("~s: ~p~n",[X,Y]) end).
traceB(X,Y) -> spawn(fun() -> io:format("~s: ~s~n",[X,Y]) end).

parseTuple(String) ->
  {ok, Ts, _} = erl_scan:string(String),
  {ok, Tup} = erl_parse:parse_term(Ts),
  {ok, Tup}.

utime()->
  {MegaSecs, MilliSecs, _Microsecs} = erlang:timestamp(),
  integer_to_binary(MegaSecs * 1000000 + MilliSecs).

getSetting(Key) ->
  D = file:consult("./sett.dat"),
  case D of
    {ok, DataL} -> 
      {_, Val} = lists:keyfind(Key,1,DataL),
      Val;
    {error,_} -> 
      u:trace(D),
      {error}
  end.

testadmin(generate,Key,Pid) -> 
	D = file:consult("./sett.dat"),
	case D of
		{ok, DataL} -> 
			Test = lists:member({admpass,Key},DataL),
	    case Test of
				true -> 
					{_,Salt} = lists:keyfind(salt,1,DataL),
					AdmEncripted = md5_hex(lists:concat([pid_to_list(Pid), Salt])),
					{ok, AdmEncripted};
				false -> {error} end;
		{error,_} -> 
			u:trace(D),
			{error}
	end;
testadmin(check,Key,Pid) ->
	D = file:consult("./sett.dat"),
	case D of
		{ok, DataL} -> 
			{_,Salt} = lists:keyfind(salt,1,DataL),
			AdmEncripted = md5_hex(lists:concat([pid_to_list(Pid), Salt])),
			case Key =:= AdmEncripted of
					true ->	{ok, AdmEncripted};
					false -> {error} end;
		{error,_} -> 
			u:trace(D),
			{error}
	end.

md5_hex(S) ->
    Md5_bin =  erlang:md5(S),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).
 
list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).
 
int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].
 
hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
        $a + (N-10).