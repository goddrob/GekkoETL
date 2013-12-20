%% @Author: Dani Hodovic
%% ====================================================================
%% Description: 
%% The supervisor responsible for spawning and maintaining
%% all of it's children alive
%% ====================================================================

-module(etl_sup).
-behaviour(supervisor).
-export([init/1]).
-export([start/0]).

%% Includes for various records/constants we use
-include("../include/defs.hrl").

%% ====================================================================
%% API functions
%% ====================================================================

start() ->
	start_tools(),
	start_logging(),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================

%% Initiates all of the children
%% The time argument passed to some of them is the interval time

init([]) ->
	Nasdaq = {nasdaq,{nasdaq_bridge, start, [timer:hours(3)]},
	      transient,2000,worker,[nasdaqTickers]},

	Database = {database,{server_sql, start_link, []},
	      permanent,2000,worker,[server_daily]},

	Daily = {daily,{server_daily, start_link, []},
	      permanent,2000,worker,[server_sql]},
	
	News = {news,{news_gen, start, [timer:hours(1)]},
	      permanent,2000,worker,[news_worker]},
	
	Historical = {historical,
			{hist_gen, start, [{"19701201", "20131203"}, timer:hours(6)]},
	     		permanent,brutal_kill,worker,[hist_worker]},
	
    {ok,{{one_for_one,0,1}, [Nasdaq, News, Historical, Database, Daily]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

%% ====================================================================
%% Starts the applications that are required for the system to run

start_tools() ->
	inets:start(),
	odbc:start().

%% ====================================================================
start_logging() ->
	error_logger:logfile({open, ?Logfile_Sup}).
