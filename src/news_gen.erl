%% @Author: Dani Hodovic
%% ====================================================================
%% Description: 
%% Genserver for parsing historical data.
%% Spawns child processes that parse and upload and keeps track of these
%% ====================================================================

-module(news_gen).
-behaviour(gen_server).

%% API exports
-export([start/1, pre_process/1]).

%% OTP exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Includes for various records/constants we use
-include("../include/defs.hrl").

%% Amount of concurrent processes
-define(AMOUNT_OF_C_PROC, 100).

%% State for the server
-record(state, {tickers, con_count}).

%% ====================================================================
%% API functions
%% ====================================================================

start(Sleeptime) ->
	spawn_link(fun() -> pre_process(Sleeptime) end),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% Functions that have to do with initiating the system
%% ====================================================================
%% Waits for the server to start before initiating
pre_process(SleepTime) ->
	timer:sleep(timer:seconds(5)),
	start_logging(),
	call_parsing(),
	schedule_repetitions(SleepTime).

%% Schedules the repetitions of when to parse. Time in miliseconds is passed
%% as an argument
schedule_repetitions(SleepTime) ->
	timer:apply_interval(SleepTime, ?MODULE, call_parsing, []).

%% Calls for the server to start parsing information.
%% If the response is already_started, it will sleep 1 minute
%% before calling again.
call_parsing() ->
	Var = gen_server:call(?MODULE, start),
	case Var of
		already_started ->
			timer:sleep(60000),
			call_parsing();
		_ ->
			ok
	end.

%% ====================================================================
%% OTP functions 
%% ====================================================================


init([]) ->
	{ok, #state{}}.

%% ====================================================================
%% Handles calls to start parsing the source
%% If it is already processing with an active state,
%% it returns that it has already_started
handle_call(start, _From, State) when State#state.tickers == undefined ->
	Tickers = read_tickers_and_init_segment(),
	Reply = ok,
	{reply, Reply, #state{tickers = Tickers, con_count = ?AMOUNT_OF_C_PROC - 1}};
handle_call(start, _From, State) ->
	Reply = already_started,
	{reply, Reply, State}.

%% ====================================================================
%% Unused
handle_cast(_Msg, State) ->
	{noreply, State}.
	
%% ====================================================================
%% This handles if a child process crashes with an error we expect
%% and calls to restart the worker
handle_info({'EXIT', _Pid, {badmatch, Ticker, Restarts}}, State) ->
	restart_worker(Ticker,  Restarts),
	{noreply, State};

%% This checks for unexpected errors and reports that error
handle_info({'EXIT', _Pid, {catch_all, {Msg, Ticker}}}, State) ->
	report_error(unexpected, Msg),
	NewState = create_newstate(State, Ticker, normal),
	{noreply, NewState};

%% This handle info handles when all the spawned
%% children are finished, i.e the segment is done. 
%% It then proceeds to spawn another segment of processes
handle_info({'EXIT', _Pid, {normal, Ticker}}, State)
  when State#state.con_count == 0 ->
	NewState = create_newstate(State, Ticker, segment_done),
	split_and_spawn(NewState#state.tickers),
	common_methods:print(?MODULE, "Processing segment of: ", ?AMOUNT_OF_C_PROC),
	{noreply, NewState};

%% This handles when there processes are still terminating
%% but the required count to start the new segment isnt enough,
%% i.e it is counting down the processes left from the segment
handle_info({'EXIT', _Pid, {normal, Ticker}}, State) ->	
	NewState = create_newstate(State, Ticker, normal),
	parse_or_pause(NewState).

%% ====================================================================
%% Creates a new state for the server, when the segment is done
%% which meants that the count for active processes is reset
create_newstate(State, Ticker_To_Remove, segment_done) ->
	#state{con_count = ?AMOUNT_OF_C_PROC - 1, 
		tickers = State#state.tickers -- [Ticker_To_Remove]};

%% Creates a new state for the server when there is one process less,
%% i.e decrementing the amount of active processes by one
create_newstate(State, Ticker_To_Remove, normal) ->
	#state{con_count = State#state.con_count - 1, 
		tickers = State#state.tickers -- [Ticker_To_Remove]}.

%% ====================================================================
%% Checks wether the amount of tickets left are 0, i.e an empty tickers list
%% Tt then changes the state to empty and pauses the process spawning (parsing)
%% til it receives the next call
%% If it is not empty it keeps the same state given to it
parse_or_pause(State) ->
	case State#state.tickers == [] of
		false ->
			{noreply, State};
		true ->
			common_methods:print(?MODULE, "Finished"),
			{noreply, #state{}}
	end.

%% ====================================================================
%% Unused
terminate(_Reason, _State) ->
	ok.


%% ====================================================================
%% Unused
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================

%% Reads the tickers, and calls to spawn the first tickers
read_tickers_and_init_segment() ->
	inets:start(),
	odbc:start(),
	process_flag(trap_exit, true),
	common_methods:print(?MODULE, "Starting..."),
	Tickers = common_methods:read_existing_file(),
	split_and_spawn(Tickers),
	Tickers.

%% ====================================================================

%% Spawns processes according to the set amount of
%% concurrent processes. If the list of tickers cant be split
%% i.e we are at the last segment, it just catches the split error
%% and spawns the processes for the last segment
split_and_spawn(Tickers) ->
	try
		{Segment, _Rest} = lists:split(?AMOUNT_OF_C_PROC, Tickers),
		iterate_and_spawn(Segment)
	catch
		error:_Could_Not_Split ->
			iterate_and_spawn(Tickers)
	end.

%% ====================================================================

%% Iterates over a list and spawns processes for 
%% the tickers in that list
iterate_and_spawn([One_Ticker|Rest]) ->
	spawn_link(news_worker, process_ticker, [One_Ticker, 0]),
	iterate_and_spawn(Rest);
iterate_and_spawn([]) ->
	ok.

%% ====================================================================

%% Restarts the crashing worker. 
%% Increments the amount of restarts for the process by 1
%% If the max restart potential is reached, it ignores restarting 
%% the process and moves on with it's life
restart_worker(Ticker, N) ->
	case N > ?Amount_Of_Restarts of
		true ->
			common_methods:print(?MODULE, "This ticker reached max amount of restarts: ", Ticker),
			report_error(max_restarts, Ticker),
			?MODULE ! {'EXIT', restart_pid, {normal, Ticker}};
		false ->
			common_methods:print(?MODULE, "This ticker is being restarted: ", Ticker),
			spawn_link(news_worker, process_ticker, [Ticker, N + 1])
	end.

%% ====================================================================

%% Starts the logger for this server
start_logging() ->
	error_logger:logfile({open, ?Logfile_News}).

%% ====================================================================

%% Reports the unexpected error handled in the second handle_info clause
%% or
%% Reports the ticker that has reached max restarts
report_error(max_restarts, Ticker) ->
	error_logger:error_report("Error reached max restarts for ticker: " + Ticker);
report_error(unexpected, Msg) ->
	error_logger:error_report({"Unexpected error, call dani... ", Msg}).
