%% Old DBLOAD
%% this module is not used anymore but left for testing purposes. DO NOT REMOVE or I KEEL YOU


-module(dbload).
-compile(export_all).
-include("../include/defs.hrl").
%%-define(ConnectStr, "DSN=Gekko;UID=sa;PWD=Qwerty020390").


load_daily_batch(List) -> 
	Query = gen_query(daily,List),
	odbc:start(),
	{ok,Pid} = odbc:connect(?ConnectStr,[]),
	R = odbc:sql_query(Pid,Query),
	io:format("Updated ~p rows",[R]),
	odbc:stop().
load_historical_batch(List) -> ok.

%% Type = daily | historical
gen_query(Type,List) -> gen_query(Type,List,"").
gen_query(Type,[],Query) -> Query;
gen_query(Type,[H|T],Query) -> gen_query(Type,T,Query++gen_entry(Type,H)++";").

gen_entry(daily,R) -> lists:flatten(io_lib:format("INSERT INTO TodayStock VALUES ('"++R#dailyStock.name++"','"++R#dailyStock.symbol++"','"++R#dailyStock.date++"',~p,~p,~p,~p,~p,~p,~p,~p,0,0)",[R#dailyStock.currPrice,R#dailyStock.changeValue,R#dailyStock.changePercent,R#dailyStock.prevClose,R#dailyStock.open,R#dailyStock.high,R#dailyStock.low,R#dailyStock.volume])).

hist_record() -> 
	R = #hist_stock{symbol = "AABC", date="19980223 14:23:05",open = "14",close = "23",high = "25", low="12",volume="1000"}.
test_record(a) -> 
	R = #dailyStock{name = "Test Company", symbol = "TCMP", date="19980223 14:23:05",
		currPrice = 15, changeValue = 1, changePercent = 10, prevClose = 14, open = 14, high = 17,
		low = 13, volume = 100};
test_record(b) -> 
	R = #dailyStock{name = "Test Company 2", symbol = "TCMP2", date="19980224 14:23:05",
		currPrice = 15, changeValue = 1, changePercent = 10, prevClose = 14, open = 14, high = 17,
		low = 13, volume = 100}.
		
test() -> 
	odbc:start(),
	{ok,Pid} = odbc:connect(?ConnectStr,[]),
	io:format("Started: ~p~n",[Pid]),
	Query = "SELECT * FROM TodayStock",
	{selected,Columns,Rows} = odbc:sql_query(Pid,Query),
	io:format("~p~n~p~n",[Columns,Rows]),
	odbc:stop().
	
test_read(R) -> lists:flatten(io_lib:format("INSERT INTO TodayStock VALUES ('"++R#dailyStock.name++"','"++R#dailyStock.symbol++"','"++R#dailyStock.date++"',~p,~p,~p,~p,~p,~p,~p,~p,0,0)",[R#dailyStock.currPrice,R#dailyStock.changeValue,R#dailyStock.changePercent,R#dailyStock.prevClose,R#dailyStock.open,R#dailyStock.high,R#dailyStock.low,R#dailyStock.volume])).
gen_entry2(historical,R) -> "EXEC s_addDaily @Symbol='"++R#hist_stock.symbol++"',@Date='"++R#hist_stock.date++"',@Open="++R#hist_stock.open++",@Close="++R#hist_stock.close++",@MaxPrice="++R#hist_stock.high++",@MinPrice="++R#hist_stock.low++",@Volume="++R#hist_stock.volume.