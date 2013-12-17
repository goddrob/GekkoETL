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


%% init/1 
%% ====================================================================
init(Time) ->
    case nasdaqTickers:start(Time) of
	{ok, Pid} ->
	    {ok, Pid, stateless};
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


