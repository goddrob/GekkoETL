-module(news_gen).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("../include/defs.hrl").

-define(AMOUNT_OF_C_PROC, 100).
-record(state, {tickers, con_count}).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start(Sleeptime) ->
	Return = gen_server:start_link({local, ?MODULE}, ?MODULE, [], []),
	repeat(),
	timer:apply_interval(Sleeptime, ?MODULE, repeat, []),
	Return.

%% Calls for the server to start parsing information.
%% If the response is already_started, it will sleep before calling
%% again.
repeat() ->
	Var = gen_server:call(?MODULE, start),
	case Var of
		already_started ->
			timer:sleep(60000),
			repeat();
		_ ->
			ok
	end.

%% init/1
%% ====================================================================

init([]) ->
	{ok, #state{}}.

%% handle_call/3
%% ====================================================================
handle_call(start, _From, State) when State#state.tickers == undefined ->
	Tickers = start_up(),
	Reply = ok,
	{reply, Reply, #state{tickers = Tickers, con_count = ?AMOUNT_OF_C_PROC - 1}};
handle_call(start, _From, State) ->
	Reply = already_started,
	{reply, Reply, State}.


%% handle_cast/2
%% ====================================================================
handle_cast(_Msg, State) ->
	{noreply, State}.
	
%% handle_info/2
%% ====================================================================
handle_info({'EXIT', _Pid, {badmatch, Ticker, Restarts}}, State) ->
	restart_worker(Ticker,  Restarts),
	{noreply, State};

handle_info({'EXIT', _Pid, {catch_all, Msg}}, State) ->
	error_logging:cast_error(Msg),
	{noreply, State};

handle_info({'EXIT', _Pid, {normal, Ticker}}, State)
  when State#state.con_count == 0 ->
	NewState = create_newstate(State, Ticker, segment_done),
	split_and_spawn(NewState#state.tickers),
	common_methods:print(?MODULE, "Processing segment of: ", ?AMOUNT_OF_C_PROC),
	{noreply, NewState};

handle_info({'EXIT', _Pid, {normal, Ticker}}, State) ->	
	NewState = create_newstate(State, Ticker, normal),
	parse_or_pause(NewState).

create_newstate(State, Ticker_To_Remove, segment_done) ->
	#state{con_count = ?AMOUNT_OF_C_PROC - 1, 
		tickers = State#state.tickers -- [Ticker_To_Remove]};

create_newstate(State, Ticker_To_Remove, normal) ->
	#state{con_count = State#state.con_count - 1, 
		tickers = State#state.tickers -- [Ticker_To_Remove]}.

parse_or_pause(State) ->
	case State#state.tickers == [] of
		false ->
			{noreply, State};
		true ->
			common_methods:print(?MODULE, "Finished"),
			{noreply, #state{}}
	end.

%% terminate/2
%% ====================================================================
terminate(_Reason, _State) ->
	ok.


%% code_change/3
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

start_up() ->
	inets:start(),
	odbc:start(),
	process_flag(trap_exit, true),
	common_methods:print(?MODULE, "Starting..."),
	Tickers = common_methods:read_existing_file(),
	split_and_spawn(Tickers),
	Tickers.

split_and_spawn(Tickers) ->
	try
		{Segment, _Rest} = lists:split(?AMOUNT_OF_C_PROC, Tickers),
		iterate_and_spawn(Segment)
	catch
		error:_Could_Not_Split ->
			iterate_and_spawn(Tickers)
	end.

iterate_and_spawn([One_Ticker|Rest]) ->
	spawn_link(news_worker, process_ticker, [One_Ticker, 0]),
	iterate_and_spawn(Rest);
iterate_and_spawn([]) ->
	ok.

restart_worker(Ticker, N) ->
	case N > ?Amount_Of_Restarts of
		true ->
			common_methods:print(?MODULE, "This ticker reached max amount of restarts: ", Ticker),
			?MODULE ! {'EXIT', restart_pid, {normal, Ticker}};
		false ->
			common_methods:print(?MODULE, "This ticker is being restarted: ", Ticker),
			spawn_link(news_worker, process_ticker, [Ticker, N + 1])
	end.
%% restart_inets() ->
%% 	inets:stop(httpc, news),
%% 	inets:start(httpc, [{profile, news}]).


