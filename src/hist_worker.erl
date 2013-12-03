-module(hist_worker).
%% Test Comment
-include("../include/defs.hrl").
-compile(export_all).
-define(AMOUNT, 100).

%% Processes each ticker by getching the CSV for 
%% the historical data
process_ticker(Ticker, Dates) ->
	{_,{Hour,Min,Sec}} = erlang:localtime(),
	{{Old_Year, Old_Month, Old_Day}, 
	 {Recent_Year, Recent_Month, Recent_Day}} = Dates,
	URL = "http://ichart.yahoo.com/table.csv?s="++Ticker
		++"&a=" ++ Old_Month ++"&b="++ Old_Day ++ "&c=" ++ Old_Year
		++"&d="++ Recent_Month ++ "&e=" ++ Recent_Day ++ "&f="++ Recent_Year
			++"&d=m&ignore=.csv",
	try
		{ok, {_,_,CSV}} = httpc:request(URL, foo),
		case (string:chr(CSV, $!) > 0) of 
			false ->	
				io:format("Pid: ~p, Processing Ticker:~p at ~p:~p:~p~n", [self(), Ticker, Hour, Min, Sec]),
				parse_csv({CSV}, Ticker);
			true -> invalid_csv
		end
	catch
		error:{badmatch, _} -> 
			exit({badmatch, {Ticker, Dates}});
		error:CatchAll -> 
			io:format("~p~n", [CatchAll])
	end.
	

%% Parses a single CSV file and calls iterate_records method to
%% iterate over it and create records
parse_csv(CSV, Ticker) ->
	[_|Relevant_Info] = re:split(tuple_to_list(CSV), "\n",
						[{return,list},{parts,infinity}]),
						
	{ok, Pid} = odbc:connect(?ConnectStr,[{timeout, 500000}]),
	iterate_records(Relevant_Info, [], Ticker, Pid),
	odbc:disconnect(Pid).

% Iterates over records and calls iterate_records function 
% to make the actualy records for each line	
iterate_records(List, Acc, Ticker, Pid) when length(Acc) == ?AMOUNT ->
	_Result = odbc:sql_query(Pid, lists:flatten(Acc)),
	iterate_records(List, [], Ticker, Pid);
iterate_records([H|T], Acc, Ticker, Pid) ->
	case H == [] of
		true -> 
			Result = odbc:sql_query(Pid, lists:flatten(Acc));
		false ->
			iterate_records(T, [make_records(H, Ticker)|Acc], Ticker, Pid)
	end.

%% Makes the records
make_records(Line, Ticker) ->
	% io:format("~p~n", Line).
	[Date, Open, High, Low, Close, Volume, _] = string:tokens(Line, ","),
	_E = "EXEC s_addHistorical " ++
	"@Symbol='" ++ Ticker ++ "'," ++
	"@Date='" ++ Date ++ "'," ++
	"@Open=" ++ Open ++ "," ++
	"@Close=" ++ Close ++ "," ++
	"@MaxPrice=" ++ High ++ "," ++
	"@MinPrice=" ++ Low ++ "," ++
	"@Volume=" ++ Volume ++ ";".

