-module(hist_worker).
%% Test Comment
-include("../include/defs.hrl").
-compile(export_all).
-define(AMOUNT, 100).



%% Processes each ticker by getching the CSV for 
%% the historical data
process_ticker(Ticker, Dates, Restarts) ->
	odbc:start(),
	inets:start(),
	URL = create_url(Ticker, Dates),
	try
		Data = download_data(URL),
		validate_csv(Data, Ticker),
		exit({normal, Ticker})
	catch
		error:{badmatch,{error,socket_closed_remotely}} -> 
			io:format("Socket closed remotely, ticker: ~p~n", [Ticker]),
			exit({badmatch, Ticker, Restarts});
		error:{badmatch, {error, {failed_connect,_}}} ->
			io:format("Failed connect, ticker: ~p~n", [Ticker]),
			exit({badmatch, Ticker, Restarts});
		error:{badmatch,{error,timeout}} ->
			io:format("Timed out, ticker:~p~n", [Ticker]),
			exit({badmatch, Ticker, Restarts})
	%% 		error:Catch_all -> 
	%% 			io:format("~nIn ex, catch_all, Msg: ~p~n", [Catch_all]),
	%% 			throw("Dani look here : " ++ Ticker),
	%% 			exit({catch_all, Catch_all})
	end.

download_data(URL) ->
	{ok, {_,_,CSV}} = httpc:request(get, {URL, []}, 
					[{connect_timeout, ?Url_Connect_Timeout}, 
					 {timeout, ?Url_Connection_Alive_Timeout}], []),
	CSV.

create_url(Ticker, Dates) ->
	{{Old_Year, Old_Month, Old_Day}, {Recent_Year, Recent_Month, Recent_Day}} = Dates,
	"http://ichart.yahoo.com/table.csv?s="++Ticker
		++"&a=" ++ Old_Month ++"&b="++ Old_Day ++ "&c=" ++ Old_Year
		++"&d="++ Recent_Month ++ "&e=" ++ Recent_Day ++ "&f="++ Recent_Year
		++"&d=m&ignore=.csv".

%% 	Checks if the csv is valid, if it contains an exclamation mark (!), 
%% 	it's a website. It's a work around, but it works.
validate_csv(CSV, Ticker) ->
	case (string:chr(CSV, $!) > 0) of 
		false ->	
			process_and_upload({CSV}, Ticker);
		true -> 
			common_methods:print(?MODULE, "Yahoo says ticker not found: ", Ticker)
	end.


process_and_upload(CSV, Ticker) ->
	[_|Relevant_Info] = re:split(tuple_to_list(CSV), "\n",
								 [{return,list},{parts,infinity}]),
	{ok, Pid} = odbc:connect(?ConnectStr,[{timeout, ?Database_Connection_Timeout}]),
	iterate_records(Relevant_Info, [], Ticker, Pid),
	odbc:disconnect(Pid).


iterate_records(List, Acc, Ticker, Pid) when length(Acc) == ?AMOUNT ->
	_Result = odbc:sql_query(Pid, lists:flatten(Acc)),
	iterate_records(List, [], Ticker, Pid);
iterate_records([H|T], Acc, Ticker, Pid) ->
	case H == [] of
		true -> 
			_Result = odbc:sql_query(Pid, lists:flatten(Acc));
		false ->
			iterate_records(T, [make_records(H, Ticker)|Acc], Ticker, Pid)
	end.

%% Makes the records
make_records(Line, Ticker) ->
	[Date, Open, High, Low, Close, Volume, _] = string:tokens(Line, ","),
	_E = "EXEC s_addHistorical " ++
			 "@Symbol='" ++ Ticker ++ "'," ++
			 "@Date='" ++ Date ++ "'," ++
			 "@Open=" ++ Open ++ "," ++
			 "@Close=" ++ Close ++ "," ++
			 "@MaxPrice=" ++ High ++ "," ++
			 "@MinPrice=" ++ Low ++ "," ++
			 "@Volume=" ++ Volume ++ ";".

