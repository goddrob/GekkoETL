%% @Author: Dani Hodovic
%% ====================================================================
%% Description: 
%% Genserver for parsing historical data.
%% Spawns child processes that parse and upload and keeps track of these
%% ====================================================================

-module(hist_gen).
-behaviour(gen_server).

%% API exports
-export([start/2, test/0, pre_process/2]).

%% OTP exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% Includes for various records/constants we use
-include("../include/defs.hrl").

%% Amount of concurrent processes
-define(AMOUNT_OF_C_PROC, 100).

%% State for the server
-record(state, {tickers, con_count, dates}).

%% ====================================================================
%% API functions
%% ====================================================================

test() ->
	spawn_link(fun() -> pre_process(timer:minutes(10), {"20000202", "20050303"}) end),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(SleepTime, Dates) ->
	spawn_link(fun() -> pre_process(SleepTime, Dates) end),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% ====================================================================
%% Functions that have to do with initiating the system
%% ====================================================================

%% Waits for server to start, or schedules a 
%% sleep in order to let the news and nasdaq to finish first
%% Starts the logging
%% Starts the parsing
%% Schedules repetitions
pre_process(SleepTime, Dates) ->
	timer:sleep(timer:minutes(1)),
	start_logging(),
	call_parsing(Dates),
	schedule_repetitions(SleepTime).
	
%% Schedules the repetitions of when to parse. Time in miliseconds is passed
%% as an argument. It sends the date parsing to be default, which
%% results in data from the last 3 days being fetched
schedule_repetitions(SleepTime) ->
	timer:apply_interval(SleepTime, ?MODULE, call_parsing, [default]).

%% Calls for the server to start parsing information.
%% If the response is already_started, it will sleep 1 minute
%% before calling again.
call_parsing(Dates) ->
	Var = gen_server:call(?MODULE, Dates),
	case Var of
		already_started ->
			timer:sleep(60000),
			call_parsing(Dates);
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
handle_call(Dates, _From, State) when State#state.tickers == undefined ->
	{Tickers, NewDates} = start_up(Dates),
	Reply = ok,
	{reply, Reply, #state{tickers = Tickers, con_count = ?AMOUNT_OF_C_PROC - 1, dates = NewDates}};
handle_call(_Dates, _From, State) ->
	Reply = already_started,
	{reply, Reply, State}.


%% ====================================================================
%% Unused
handle_cast(_Msg, State)	->
	{noreply, State}.

%% ====================================================================

%% This handles if a child process crashes with an error we expect
%% and calls to restart the worker
handle_info({'EXIT', _Pid, {badmatch, Ticker, Restarts}}, State) ->
	restart_worker(Ticker, State#state.dates, Restarts),
	{noreply, State};

%% This checks for unexpected errors and reports that error
handle_info({catch_all, {Msg, Ticker}}, State) ->
	report_error(unexpected, Msg),
	NewState = create_newstate(State, Ticker, normal),
	{noreply, NewState};

%% This handle info handles when all the spawned
%% children are finished, i.e the segment is done. 
%% It then proceeds to spawn another segment of processes
handle_info({'EXIT', _Pid, {normal, Ticker}}, State)
  when State#state.con_count == 0 ->
	NewState = create_newstate(State, Ticker, segment_done),
	common_methods:print(?MODULE, "Processing segment of: ", ?AMOUNT_OF_C_PROC),
	split_and_spawn(NewState#state.tickers, State#state.dates),
	{noreply, NewState};

%% This handles when there processes are still terminating
%% but the required count to start the new segment isnt enough,
%% i.e it is counting down the processes left from the segment
handle_info({'EXIT', _Pid, {normal, Ticker}}, State) ->	
	NewState = create_newstate(State, Ticker, normal),
	parse_or_pause(NewState).


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

%% Creates a new state for the server, when the segment is done
%% which meants that the count for active processes is reset
create_newstate(State, Ticker_To_Remove, segment_done) ->
	#state{con_count = ?AMOUNT_OF_C_PROC - 1, 
		dates = State#state.dates,  
		tickers = State#state.tickers -- [Ticker_To_Remove]};

%% Creates a new state for the server when there is one process less,
%% i.e decrementing the amount of active processes by one
create_newstate(State, Ticker_To_Remove, normal) ->
	#state{con_count = State#state.con_count - 1, 
		dates = State#state.dates,  
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

%% Calls to evaluate the dates
%% Calls to get the tickers
%% Spawns the first segment
start_up(D) ->
	Dates = eval_dates(D),
	inets:start(),
	odbc:start(),
	process_flag(trap_exit, true),
	common_methods:print(?MODULE, "Reading tickers..."),
	Tickers = common_methods:read_existing_file(),
	split_and_spawn(Tickers, Dates),
	{Tickers, Dates}.

%% ====================================================================

%% Checks if the date is a tuple
%% If it is, that means it has to parse
%% more historical data than what is default (3 days)
eval_dates(D) ->
	case is_tuple(D) of
		true ->
			{Old, Recent} = D,
			_Dates = convert_dates(Old, Recent);
		false ->
			_Dates = convert_dates(default)
	end.

%% ====================================================================

%% Spawns processes according to the set amount of
%% concurrent processes. If the list of tickers cant be split
%% i.e we are at the last segment, it just catches the split error
%% and spawns the processes for the last segment
split_and_spawn(Tickers, Dates) ->
	try
		{Segment, _Rest} = lists:split(?AMOUNT_OF_C_PROC, Tickers),
		iterate_and_spawn(Segment, Dates)
	catch
		error:_Could_Not_Split ->
			iterate_and_spawn(Tickers, Dates)
	end.

%% ====================================================================

%% Iterates over a list and spawns processes for 
%% the tickers in that list based on the provided dates
iterate_and_spawn([One_Ticker|Rest], Dates) ->
	spawn_link(hist_worker, process_ticker, [One_Ticker, Dates, 0]),
	iterate_and_spawn(Rest, Dates);
iterate_and_spawn([], _Dates) ->
	ok.

%% ====================================================================

%% Restarts the crashing worker. 
%% Increments the amount of restarts for the process by 1
%% If the max restart potential is reached, it ignores restarting 
%% the process and moves on with it's life
restart_worker(Ticker, Dates, N) ->
	case N == ?Amount_Of_Restarts of
		true ->
			common_methods:print(?MODULE, "This ticker reached max amount of restarts: ", Ticker),
			report_error(max_restarts, Ticker),
			?MODULE ! {'EXIT', restart_pid, {normal, Ticker}};
		false ->
			common_methods:print(?MODULE, "This ticker is being restarted: ", Ticker),
			spawn_link(hist_worker, process_ticker, [Ticker, Dates, N + 1])
	end.

%% ====================================================================

%% Default date converter. Takes todays date and three days back,
%% and returns these days as the interval to be parsed.
convert_dates(default) ->
	{{Year, Month, Day}, {_,_,_}} = calendar:local_time(),
	Recent_Return = date_helper(integer_to_list(Year) 
								++ integer_to_list(Month) ++ integer_to_list(Day)),
	case Day < 3 of
		false ->
			Old_Return = date_helper(integer_to_list(Year) ++
								 integer_to_list(Month) ++ integer_to_list(Day - 3));
		true ->
			Old_Return = date_helper(integer_to_list(Year)++
								 integer_to_list(Month - 1) ++ integer_to_list(28))
	end,
	case Old_Return < Recent_Return of
		true ->
			{Old_Return, Recent_Return};
		false ->
			throw("Invalid dates, the second argument must be the recent date")
	end.

%% Takes the local time, and the date provided to return 
%% the dates intervals that need to be parsed.
%% Does some date checking to verify that proper dates are
%% given

convert_dates(Old, Recent) ->
	Old_Return = date_helper(Old),
	Recent_Return = date_helper(Recent),
	case Old_Return < Recent_Return of
		true -> 
			{Old_Return, Recent_Return};
		false ->
			throw("Invalid dates, the second argument must be the recent date")
	end.

%% ====================================================================

date_helper(Date) ->
	{Year_String, MonthDay_String} = lists:split(4, Date),
	{Month_String, Day_String} = lists:split(2, MonthDay_String),
	Year = list_to_integer(Year_String),
	Month = list_to_integer(Month_String),
	Day = list_to_integer(Day_String),
	case calendar:valid_date(Year, Month, Day) of
		false -> 
			throw("Invalid dates");
		true -> 
			Return_Year = integer_to_list(Year),
			Return_Day = integer_to_list(Day),
			if 
				Month == 1 -> Return_Month = "12";
				true -> Return_Month = integer_to_list(Month - 1)
			end,
			{Return_Year, Return_Month, Return_Day}
	end.

%% ====================================================================
%% Starts the logger for this server
start_logging() ->
	error_logger:logfile({open, ?Logfile_Historical}).

%% ====================================================================
%% Reports the unexpected error handled in the second handle_info clause
%% or
%% Reports the ticker that has reached max restarts
report_error(max_restarts, Ticker) ->
	error_logger:error_report("Error reached max restarts for ticker: " + Ticker);
report_error(unexpected, Msg) ->
	error_logger:error_report({"Unexpected error, call dani... ", Msg}).
