-module(access_test).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,terminate/2, code_change/3]).
-export([test/2]).

-define(SERVER, ?MODULE).

-record(state,{incr=0, db=ets:new(?MODULE, [ordered_set])}).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% sends Msg to anyone logged in as Id
test(Ip, Url) ->
	gen_server:call(?SERVER, {testip, Ip, Url}).
	
init([]) ->
	% set this so we can catch death of logged in pids:
	process_flag(trap_exit, true),
	% use ets for routing tables
	{ok, #state{}}.

handle_call({testip, Ip, Url}, _From, State) ->
    {MegaSecs, Secs, _MicroSecs} = erlang:timestamp(),
    Utime = MegaSecs * 1000000 + Secs,
	Oldnum = if
		State#state.incr >= 5000 ->
			0;
		true ->
			State#state.incr
	end,
	Lst = ets:match(State#state.db,{'$1',Ip, Url,'$2'}),
	case Lst of
		[] ->
			ets:insert(State#state.db, {Oldnum, Ip, Url, Utime}),
			{reply, {ok, new}, State#state{incr=Oldnum+1}};
		[[Num, Time]] ->
			case Time + 2 < Utime of
				true ->
					ets:insert(State#state.db, {Num, Ip, Url, Utime}),
					{reply, {ok, timeout}, State};
				false ->
					ets:insert(State#state.db, {Num, Ip, Url, Utime}),
					{reply, {error, fast}, State}
			end;
		[[_Num, _Time]|_] ->
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
