-module(parser_hist_worker).
-include("../include/defs.hrl").
-compile(export_all).

%% Processes each ticker by getching the CSV for 
%% the historical data
process_ticker(Ticker, Dates) ->
	{_,{Hour,Min,Sec}} = erlang:localtime(),
	io:format("Pid: ~p, Processing Ticker:~p at ~p:~p:~p~n", [self(), Ticker, Hour, Min, Sec]),
	{{Old_Year, Old_Month, Old_Day}, 
	 {Recent_Year, Recent_Month, Recent_Day}} = Dates,
	URL = "http://ichart.yahoo.com/table.csv?s="++Ticker
		++"&a=" ++ Old_Month ++"&b="++ Old_Day ++ "&c=" ++ Old_Year
		++"&d="++ Recent_Month ++ "&e=" ++ Recent_Day ++ "&f="++ Recent_Year
			++"&d=m&ignore=.csv",
	% try
		% {ok, Pid} = inets:start(httpc, foo),
		{ok, {_,_,CSV}} = httpc:request(URL, foo),
		% inets:stop(httpc, Pid),
		case (string:chr(CSV, $!) > 0) of 
			false ->	parse_csv({CSV}, Ticker);
			true -> 	parser_hist_main ! {self(), unparsable_CSV}
		end.
	% catch
		% error:_Error -> hist_server ! {self(), {error, Ticker, Dates}}
	% end.


%% Parses a single CSV file and calls iterate_records method to
%% iterate over it and create records
parse_csv(CSV, Ticker) ->
	[_|List] = re:split(tuple_to_list(CSV), "\n",
						[{return,list},{parts,infinity}]),
	T = lists:flatten(iterate_records(List, [], Ticker)),
	case hist_db:call(hist_db, T) of
		ok -> parser_hist_main ! {self(), done};
		_ -> io:format("Bigtime Error~n")
	end.



% Iterates over records and calls iterate_records function 
% to make the actualy records for each line	
iterate_records([H|T], Acc, Ticker) ->
	String = string:tokens(H, ","),
	case String =/= [] of
	 true -> iterate_records(T, [make_records(H, Ticker)|Acc], Ticker);
	 false -> Acc
	end;
iterate_records([], Acc, _Ticker) -> Acc.

%% Makes the records
make_records(Line, Ticker) ->
	[Date, Open, High, Low, Close, Volume, _] = string:tokens(Line, ","),
	#hist_stock{symbol = Ticker, date = Date, open = Open, high = High,
				low = Low, close = Close, volume = Volume}.
	% db:update(R).
%% 	dbload:insert(Stock).
				