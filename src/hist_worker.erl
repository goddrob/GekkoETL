%% @Author: Dani Hodovic
%% ====================================================================
%% Description: 
%% The worker for fetching CSV files. Terminates normally or abnormally
%% which is caught by the parent process
%% Uploads to the database
%% ====================================================================

-module(hist_worker).

%% API exports
-export([process_ticker/3]).

%% Include file for various constants and connection strings
-include("../include/defs.hrl").

%% ====================================================================

%% Processes each ticker by fetching the CSV for 
%% the historical data. If it fails, it terminates
%% with various exit messages that are passed to the parent process
process_ticker(Ticker, Dates, Restarts) ->
	odbc:start(),
	inets:start(),
	URL = create_url(Ticker, Dates),
	try
		Data = download_data(URL),
		validate_csv(Data, Ticker),
		common_methods:print(?MODULE, "Processed ticker: ", Ticker),
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
			exit({badmatch, Ticker, Restarts});
		error:Catch_all -> 
			exit({catch_all, {Catch_all, Ticker}})
	end.

%% ====================================================================

%% Downloads the CSV file with certain connection constraints
download_data(URL) ->
	{ok, {_,_,CSV}} = httpc:request(get, {URL, []}, 
									[{connect_timeout, ?Url_Connect_Timeout}, 
									 {timeout, ?Url_Connection_Alive_Timeout}], []),
	CSV.

%% ====================================================================

%% Creates the url based on the tickers and dates provided
create_url(Ticker, Dates) ->
	{{Old_Year, Old_Month, Old_Day}, {Recent_Year, Recent_Month, Recent_Day}} = Dates,
	"http://ichart.yahoo.com/table.csv?s="++Ticker
		++"&a=" ++ Old_Month ++"&b="++ Old_Day ++ "&c=" ++ Old_Year
		++"&d="++ Recent_Month ++ "&e=" ++ Recent_Day ++ "&f="++ Recent_Year
		++"&d=m&ignore=.csv".

%% ====================================================================

%% 	Checks if the downloaded data is a website or a csv file
validate_csv(CSV, Ticker) ->
	{First_Sentence_of_CSV, _} = lists:split(14, CSV),
	case First_Sentence_of_CSV == "<!doctype html" of 
		false ->	
%% 			It's a csv and NOT a site
			process_and_upload(CSV, Ticker);
		true -> 
%% 			Its a website
			common_methods:print(?MODULE, "Yahoo says ticker not found: ", Ticker)
	end.

%% ====================================================================
process_and_upload(CSV, Ticker) ->
	[_|Relevant_Info] = re:split(CSV, "\n",
								 [{return,list},{parts,infinity}]),
	{ok, Pid} = odbc:connect(?ConnectStr,[{timeout, ?Database_Connection_Timeout}]),
	iterate_and_upload(Relevant_Info, [], Ticker, Pid),
	odbc:disconnect(Pid).

%% ====================================================================

%% Iterates over given list of CSV lines. If it reaches 
%% the maximum batch amount (amount of rows to update) it uploads and reiterates
%% The reason I skipped doing records is because I suspected string building 
%% and record conversion to be a bottleneck
iterate_and_upload(List, Acc, Ticker, Pid) when length(Acc) == ?Database_Upload_Batch ->
	_Result = odbc:sql_query(Pid, lists:flatten(Acc)),
	iterate_and_upload(List, [], Ticker, Pid);
iterate_and_upload([H|T], Acc, Ticker, Pid) ->
	case H == [] of
		true -> 
			_Result = odbc:sql_query(Pid, lists:flatten(Acc));
		false ->
			iterate_and_upload(T, [make_query(H, Ticker)|Acc], Ticker, Pid)
	end.

%% ====================================================================

%% Creates the query
make_query(Line, Ticker) ->
	[Date, Open, High, Low, Close, Volume, _] = string:tokens(Line, ","),
	_E = "EXEC s_addHistorical " ++
			 "@Symbol='" ++ Ticker ++ "'," ++
			 "@Date='" ++ Date ++ "'," ++
			 "@Open=" ++ Open ++ "," ++
			 "@Close=" ++ Close ++ "," ++
			 "@MaxPrice=" ++ High ++ "," ++
			 "@MinPrice=" ++ Low ++ "," ++
			 "@Volume=" ++ Volume ++ ";".