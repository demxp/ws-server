-module(u).
-compile(nowarn_unused_function).
%-compile(nowarn_unused_vars).

-export([
  trace/1,
  trace/2,
  traceB/1,
  traceB/2,
  md5_hex/1,
  utime/0,
  utime/1,
  utime/2,
  apply_foo/2,
  ip2binary/1,
  ets_result/2,
  parse_config/1,
  index_of/2,
  sha256_hex/1
]).

trace(X) -> spawn(fun() -> io:format("~p~n",[X]) end).
traceB(X) -> spawn(fun() -> io:format("~s~n",[X]) end).
trace(X,Y) -> spawn(fun() -> io:format("~s: ~p~n",[X,Y]) end).
traceB(X,Y) -> spawn(fun() -> io:format("~s: ~s~n",[X,Y]) end).

apply_foo(Foo, Thingy) -> Foo(Thingy).

ets_result(Result, Fun) ->
  case Result of
    [] -> {error, empty};
    _ when is_list(Result) -> 
      case length(Result) of
        1 -> Fun(lists:last(Result));
        _ -> lists:map(Fun, Result)
      end;
    _ -> {error, incorrectinput}
  end.

parseTuple(String) ->
  {ok, Ts, _} = erl_scan:string(String),
  {ok, Tup} = erl_parse:parse_term(Ts),
  {ok, Tup}.

index_of(Item, List) -> index_of(Item, List, 1).

index_of(_, [], _)  -> not_found;
index_of(Item, [Item|_], Index) -> Index;
index_of(Item, [_|Tl], Index) -> index_of(Item, Tl, Index+1).

utime() -> utime(asbinary, sc).
utime(Bin) -> utime(Bin, sc).
utime(Bin, sc) -> utime(Bin, 1000000);
utime(Bin, mlsc) -> utime(Bin, 1000);
utime(Bin, mcsc)-> utime(Bin, 1);
utime(Bin, Div) when is_integer(Div) ->
  {MegaSecs, Secs, Microsecs} = erlang:timestamp(),
  Fun = case Bin of
    asinteger -> fun(I) -> I end;
    asbinary -> fun(I) -> integer_to_binary(I) end
  end,
  Fun(((MegaSecs * 1000000 + Secs) * 1000000 + Microsecs) div Div).

  % code:add_pathz("/app/websocket/deps/jsone/ebin").
  % {ok, Data} = file:read_file("./sett.dat").
  % D = jsone:decode(Data, [{object_format, proplist}]).

parse_config(JsonConfig) -> parse_config(JsonConfig, <<>>, [], []).
parse_config(Inp, StrAcc, Acc, _Base) when is_list(Inp), length(Inp) > 0 -> 
  [First|Tail] = Inp,
  Valid = fun(Term) ->
    if 
      is_list(Term) -> 
        case lists:search(fun(I) -> not is_tuple(I) end, Term) of
          false -> Term;
          _ -> list_to_binary(Term)
        end;
      true -> Term
    end
  end,
  Val = Valid(element(2, First)),
  Nm = element(1, First),
  Acc2 = case is_list(Val) of
    true -> 
      Acc3 = parse_config(Val, <<StrAcc/binary,Nm/binary,<<".">>/binary>>, [], []),
      Acc3 ++ Acc;
    false ->
      [ {<<StrAcc/binary,Nm/binary>>, Val} | Acc]
  end,
  parse_config(Tail, StrAcc, Acc2, Tail);
parse_config([], StrAcc, Acc, Base) when length(Base) > 0 -> 
  parse_config(Base, StrAcc, Acc, Base);
parse_config([], _StrAcc, Acc, []) -> 
  Acc.

sha256_hex(S) ->
  Lst = binary_to_list(crypto:hash(sha256, S)),
  lists:flatten(list_to_hex(Lst)).

md5_hex(S) when is_list(S) ->
  S1 = list_to_binary(S),
  md5_hex(S1);
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

ip2binary(Tuple) -> ip2binary(Tuple, 1, tuple_size(Tuple), <<>>).
ip2binary(Tuple, Pos, Size, Acc) when Pos < Size -> 
    Bin = integer_to_binary(element(Pos,Tuple)),
    NewAcc = <<Acc/binary,Bin/binary,<<".">>/binary>>,
    ip2binary(Tuple, Pos+1, Size, NewAcc);
ip2binary(Tuple, Pos, Size, Acc) when Pos == Size ->  
    Bin = integer_to_binary(element(Pos,Tuple)),
    NewAcc = <<Acc/binary,Bin/binary>>,
    ip2binary(Tuple, Pos+1, Size, NewAcc);
ip2binary(_Tuple,_Pos,_Size, Acc) -> Acc.