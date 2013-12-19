%% @Author: Dani Hodovic
%% ====================================================================
%% Description: 
%% The app file responsible for starting the supervisor
%% ====================================================================

-module(etl_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    etl_sup:start().

stop(_State) ->
    ok.
