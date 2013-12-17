-module(nasdaq_bridge).
-behaviour(supervisor_bridge).
-export([init/1, terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-compile(export_all).

start(Time) ->
	supervisor_bridge:start_link({local, ?MODULE}, ?MODULE, [Time]).



%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

%% init/1 
%% ====================================================================
init(Time) ->
    case nasdaqTickers:start(Time) of
	{ok, Pid} ->
	    {ok, Pid, #state{}};
	Error ->
	    Error
    end.

%% terminate/2
%% ====================================================================
terminate(Reason, State) ->
%%     nasdaqTickers:stop(),
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


