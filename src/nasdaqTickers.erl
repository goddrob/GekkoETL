%% @Author: Sinan
%% ====================================================================
%% Description: 
%% Reads the tickers from nasdaq and stores them in a local txt file
%% ====================================================================

-module(nasdaqTickers).
-export([start/1, read/0, getServer/1,get_tickers/0,filteredValues/3]).
-compile(export_all).

%% Includes for various records/constants we use
-include("../include/defs.hrl").


start(SleepTime) ->
	start_logging(),
	read(),
	schedule_repetitions(SleepTime),
	{ok, self()}.

schedule_repetitions(SleepTime) ->
	timer:apply_interval(SleepTime, ?MODULE, read, []).

%% @Author: Dani Hodovic
%% Gets the tickers from Sinans methods and writes them to a 
%% local .txt file
read() ->
	try
		inets:start(),
		Tickers = get_tickers(),
		io:format("[NasdaqTickers] - Read and wrote tickers~n"),
		file:write_file("tickers.txt", io_lib:fwrite("~p.\n", [Tickers]))
	catch
		_Error:Reason ->
			Reason
%% 			throw("Cannot read tickers online")
	end.

%% @Author: Sinan
%% get the body from the url
getServer(Quary) ->
	{ok, {_,_,Body}} = httpc:request(Quary),
	{Body}.

%% @Author: Sinan
%% Receiving the url's body and save it in Data
%% replace and split using the regular expressions module
get_tickers()-> 
	Data = getServer("http://www.nasdaq.com/screening/companies-by-name.aspx?&render=download"),
	
	[_|Values] = re:split(tuple_to_list(Data), ",\r\n|\n",[{return,list},{parts,2}]),
	
	CheckingForSpace = re:replace(Values,"\\\x20","",[global,{return,list}]),
	
	[H|T] = re:split(CheckingForSpace,"\\\x22,\r\n\\\x22|\\\x22,\\\x22|\\\x22,\r\n",[{return,list},trim]),
	
	F = filteredValues(T,[H],0),
	convert_to_atoms(F).

filteredValues([],Acc,_)->
	%%         Value2 = deleteSymbols(Acc),
	Acc;


filteredValues([H|T],Acc,Counter) when Counter == 8 ->
	filteredValues(T,[H]++Acc,0);
filteredValues([_|T],Acc,Counter) ->
	filteredValues(T,Acc,Counter+1).

%% deleteSymbols([]) -> [];
%% deleteSymbols([H|T]) ->
%%         case (string:chr(H, $\") == 0) and (string:chr(H, $^) == 0) and (string:chr(H, $/) == 0) of %"
%%                 false -> deleteSymbols(T);
%%                 true -> [H|deleteSymbols(T)]
%%         end.


%% Converts the list to atoms for easier future parsing/Dani
convert_to_atoms(List) -> 
	convert_to_atoms(List, []).

convert_to_atoms([H|T], Acc) ->
	convert_to_atoms(T, [list_to_atom(H)|Acc]);
convert_to_atoms([], Acc) -> 
	Acc.

start_logging() ->
	error_logger:logfile({open, ?Logfile_Nasdaq}).
