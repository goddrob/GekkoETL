%% @Author: Dani Hodovic
%% ====================================================================
%% Description: 
%% Since the module nasdaqTickers could not be used under OTP,
%% but was functional I decided to bridge it to OTP. All the bridge really does
%% is make sure that nasdaqTickers can be used under the OTP
%% ====================================================================
-module(nasdaq_bridge).
-behaviour(supervisor_bridge).

%% API exports
-export([start/1]).

%% OTP exports
-export([init/1, terminate/2]).

%% ====================================================================
%% API functions
%% ====================================================================

start(Time) ->
	supervisor_bridge:start_link({local, ?MODULE}, ?MODULE, [Time]).

%% ====================================================================
%% OTP functions
%% ====================================================================

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
%% Unused
terminate(_Reason, _State) ->
    ok.
