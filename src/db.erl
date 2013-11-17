-module(db).
-compile(export_all).
-include("../include/defs.hrl").
start() ->
	case whereis(db) of 
	undefined ->
		odbc:start(), 
		register(?MODULE, spawn_link(?MODULE, loop, []));
	_ -> defined
	end.

loop() ->
{ok, Pid} = odbc:connect(?ConnectStr,[{timeout, 2000000}]),
loop(Pid).

loop(Pid) ->
receive
	{From, Msg} -> 
		update(Msg, Pid),
		From ! ok,
		loop(Pid);
	Catch -> io:format("Catch all db: ~p~n", [Catch]),
		loop(Pid)
	after 50000 ->
		odbc:stop()
end.

update(List, Pid) ->
	Query = gen_query(historical, List),
	_Result = odbc:sql_query(Pid, Query).


%% Private Helper functions 
%% Generate queries from records 
gen_query(Type,List) -> gen_query(Type,List,"").
gen_query(_,[],Query) -> Query;
gen_query(Type,[H|T],Query) -> gen_query(Type,T,Query++gen_entry(Type,H)++";").

gen_entry(historical, R) -> "EXEC s_addHistorical @Symbol='"++R#hist_stock.symbol++"',@Date='"++R#hist_stock.date++"',@Open="++R#hist_stock.open++",@Close="++R#hist_stock.close++",@MaxPrice="++R#hist_stock.high++",@MinPrice="++R#hist_stock.low++",@Volume="++R#hist_stock.volume.
