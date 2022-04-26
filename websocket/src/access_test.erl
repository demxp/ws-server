-module(access_test).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([test/2, test/3]).

-define(SERVER, ?MODULE).

-record(state,{db=ets:new(?MODULE, [set, {keypos, 1}])}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) -> {ok, #state{}}.

test(Ip, Url) ->
	gen_server:call(?SERVER, {testip, Ip, Url, 3000}). %one request per Timeout millisecs default

test(Ip, Url, Timeout) ->
	gen_server:call(?SERVER, {testip, Ip, Url, Timeout}).
	
handle_call({testip, Ip, Url, Timeout}, _From, State) ->
	Utime = u:utime(asinteger, mlsc),
	RequestKey = integer_to_binary(erlang:crc32(<<Ip/binary,Url/binary>>)),
	Lst = ets:lookup(State#state.db, RequestKey),
	ets:insert(State#state.db, {RequestKey, Utime}),
	case Lst of
		[] -> {reply, {ok, new}, State};
		[{_, Time}] ->
			case Time + Timeout =< Utime of
				true -> {reply, {ok, timeout}, State};
				false -> {reply, {error, fast}, State}
			end;
		_ ->
			{reply, {error, fast}, State}
	end.

handle_info(_Info, State) ->
	{noreply, State}.

handle_cast(_Msg, State) ->	
	{noreply, State}.


	
terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
