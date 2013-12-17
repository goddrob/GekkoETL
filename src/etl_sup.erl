-module(etl_sup).
-behaviour(supervisor).
-export([init/1]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================

start() ->
	start_libraries(),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_libraries() ->
	inets:start(),
	odbc:start().



%% init/1
%% ====================================================================
init([]) ->
	Nasdaq = {nasdaq,{nasdaq_bridge, start, [60000]},
	      transient,2000,worker,[nasdaqTickers]},

	Database = {database,{server_sql, start_link, []},
	      permanent,2000,worker,[server_daily]},

	Daily = {daily,{server_daily, start_link, []},
	      permanent,2000,worker,[server_sql]},
	
	News = {news,{news_gen, start, [30000]},
	      permanent,2000,worker,[news_worker]},
	
	Historical = {historical,
				{hist_gen, start, [{"20131201", "20131203"}, 60000]},
	     		permanent,brutal_kill,worker,[hist_worker]},
	Error_Logging = {error_logging,{error_logging, start, []},
	      permanent,2000,worker,[]},
	
    {ok,{{one_for_one,0,1}, [Error_Logging, Nasdaq, News, Historical, Database, Daily]}}.

%% ====================================================================
%% Internal functions
%% ====================================================================

