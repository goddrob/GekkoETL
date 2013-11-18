
-module(hist_db).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include("../include/defs.hrl").
-compile(export_all).



start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_DB_Pid) ->
	odbc:start(),
	{ok, Pid} = odbc:connect(?ConnectStr,[{timeout, 2000000}]),
    {ok, Pid}.

call(Server, List) ->
	gen_server:call(Server, List).

handle_call(List, _From, DB_Pid) ->
	Query = gen_query(historical, List),
	_Result = odbc:sql_query(DB_Pid, Query),
    Reply = ok,
    {reply, Reply, DB_Pid}.



%% Private Helper functions 
%% Generate queries from records 
gen_query(Type,List) -> gen_query(Type,List,"").
gen_query(_,[],Query) -> Query;
gen_query(Type,[H|T],Query) -> gen_query(Type,T,Query++gen_entry(Type,H)++";").

gen_entry(historical, R) -> "EXEC s_addHistorical @Symbol='"++R#hist_stock.symbol++"',@Date='"++R#hist_stock.date++"',@Open="++R#hist_stock.open++",@Close="++R#hist_stock.close++",@MaxPrice="++R#hist_stock.high++",@MinPrice="++R#hist_stock.low++",@Volume="++R#hist_stock.volume.

handle_info(_Info, State) ->
    {noreply, State}.
	
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.