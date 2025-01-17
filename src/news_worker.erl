%% @Author: Georgi Dungarov
%% ====================================================================
%% Description: 
%% The supervisor responsible for spawning and maintaining
%% all of it's children alive
%% ====================================================================
-module(news_worker).
-compile(export_all).
-include_lib("xmerl/include/xmerl.hrl").
-include("../include/defs.hrl").

%% Author: Dani
%% Generates an URL and downloads the XML from the URL. 
%% If errors occurr the process exits in a way that is 
%% caught by the parent process
process_ticker(Ticker, Restarts) ->
	inets:start(),
	odbc:start(),
	try
		URL = "http://articlefeeds.nasdaq.com/nasdaq/symbols?symbol=" 
							++ string:to_upper(Ticker),
		Body = download_data(URL),
		{Xml, _Rest } = xmerl_scan:string(Body),
		printItems(getElements(Xml), Ticker),
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

download_data(URL) ->
	{ok, {_,_,CSV}} = httpc:request(get, {URL, []}, 
					[{connect_timeout, ?Url_Connect_Timeout}, 
					 {timeout, ?Url_Connection_Alive_Timeout}], []),
	CSV.

getElements([H|T]) when H#xmlElement.name == item ->
	[H | getElements(T)];
getElements([H|T]) when is_record(H, xmlElement) ->
	getElements(H#xmlElement.content) ++
		getElements(T);                                                                 
getElements(X) when is_record(X, xmlElement) ->
	getElements(X#xmlElement.content);
getElements([_|T]) ->
	getElements(T);
getElements([]) ->
	[].

printItems(Items, Ticker) ->
	printItems(Items, [], Ticker).

printItems([H|T], Acc, Ticker) ->
	printItems(T, [create_query(H, Ticker)|Acc], Ticker);
printItems([], Acc, _Ticker) -> 
 	{ok, Pid} = odbc:connect(?ConnectStr,[{timeout, ?Database_Connection_Timeout}]),
	_Result = odbc:sql_query(Pid, Acc),
%% 	io:format("~p~n", [Result]),
	odbc:disconnect(Pid).

first(Item, Tag) ->
	hd([X || X <- Item#xmlElement.content,
			 X#xmlElement.name == Tag]).

textOf(Item) ->
	lists:flatten([X#xmlText.value || X <- Item#xmlElement.content,
									  element(1,X) == xmlText]).

create_query(Item, Ticker)	->	 
	Record = #news{date = format_date(textOf(first(Item, pubDate))), 
				   url = textOf(first(Item, link)),
				   headline = filter_headline(textOf(first(Item, title))),
				   ticker = Ticker}, 
	gen_entry(news, Record) ++ ";".

filter_headline(Var) ->
	NewList = delete(Var, []),
	re:replace(NewList, "&#39;", "", [global, {return, list}]).

%% Deletes characters that aren't parsable /Dani
delete([H|T], Acc) when H > 127 ->
	delete(T, Acc);
delete([H|T], Acc) ->
	delete(T, Acc ++ [H]);
delete([], Acc) -> 
	Acc.

%% Uses the date library to split the string and 
%% parse it with the help of ec_date:parse/1  /Dani
format_date(Var) ->
	{A, _} = lists:split(16, Var),
	{{Y, M, D}, _} = ec_date:parse(A),
	Date = integer_to_list(Y) ++ "-"
			   ++ integer_to_list(M) ++ "-"
			   ++ integer_to_list(D),
	Date.

%% @Author: Robert
gen_entry(news,R) -> "EXEC s_addNews @Date='"++R#news.date++"',@Symbol='"++R#news.ticker++"',@Headline='"++R#news.headline++"',@Hyperlink='"++R#news.url++"'".