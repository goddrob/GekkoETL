% Author : Dani Hodovic
% Historical parser V2. Incomplete as it needs to be compatible with the database. 
% For now it contains a test method for spawning
 % mini-processes and receiving messages from those printing it in in the shell.

-module(parser_historical).
-export([main/0]).
-include("../include/defs.hrl").

% For testing purposes
% -record(rec, {symbol = "", date = "", open = "0", high = "0", low = "0", close = "0", volume = "0"}).


main() ->
	inets:start(),
	All_Tickers = nasdaqTickers:get(),
	iterate_tickers(All_Tickers).

iterate_tickers([H|T]) ->
	spawn_link(fun() -> processTicker(H) end),
	iterate_tickers(T);
iterate_tickers([]) ->
	ok.

processTicker(Ticker) ->
	{{Year, Month, Day}, _Time} = calendar:local_time(),
	SYear = integer_to_list(Year),
	SMonth = integer_to_list(Month),
	SDay = integer_to_list(Day),
	S = "http://ichart.yahoo.com/table.csv?s="++Ticker
		++"&a=5"++"&b=15"++"&c=2013"++"&d="++ SMonth ++ "&e="++ SDay ++ "&f="++ SYear
			++"&d=m&ignore=.csv",
	CSV = httpQ:get(S),
	case (string:chr(CSV, $!) > 0) of 
		false -> parse_csv({CSV}, Ticker);
		true -> error
	end.

parse_csv(CSV, Ticker) ->
	[_|List] = re:split(tuple_to_list(CSV), "\n",[{return,list},{parts,infinity}]),
	dbload:load_historical_batch(iterate_records(List, [], Ticker)).
	
	% For testing, ignore.
	% server ! {iterate_records(List, [], Ticker)}.

% Iterates over records and calls another function 
% to make the actualy records for each line	
iterate_records([H|T], Acc, Ticker) ->
	String = string:tokens(H, ","),
	case String =/= [] of
	 true -> iterate_records(T, [make_rec(String, 0, #hist_stock{}, Ticker)|Acc], Ticker);
	 false -> Acc
	end;
iterate_records([], Acc, _Ticker) -> Acc.

make_rec([H|T], N, Rec, Ticker) -> 
	make_rec(T, N + 1, Rec#hist_stock{symbol = Ticker, date=((H -- [$-]) --[$-])}).
make_rec([H|T], N, Rec) when N == 1 -> 
	make_rec(T, N + 1, Rec#hist_stock{open=H});
make_rec([H|T], N, Rec) when N == 2 -> 
	make_rec(T, N + 1, Rec#hist_stock{high=H});
make_rec([H|T], N, Rec) when N == 3 -> 
	make_rec(T, N + 1, Rec#hist_stock{low=H});
make_rec([H|T], N, Rec) when N == 4 -> 
	make_rec(T, N + 1, Rec#hist_stock{close=H});
make_rec([H|T], N, Rec) when N == 5 -> 
	make_rec(T, N + 1, Rec#hist_stock{volume=H});
make_rec(_, _, Rec) ->
	Rec.

% Printer for testing
print(Var) ->
	io:format("*************"),
	io:format("~p", [Var]),
	io:format("*************~n").

	
% Only use for local test cases without binding to the database, printing only for the following two methods.
test() ->
	case whereis(server) of
		undefined -> register(server, self());
		_ -> registered_already
	end,
	inets:start(),
	print(inets_started),
	All_Tickers = nasdaqTickers:get(),
	{A,_} = lists:split(2, All_Tickers),
	print(process_starting),
	iterate_tickers(A),
	loop().
	
loop() ->
receive
	M -> io:format("~p~n",[M]), loop()
	after 3000 ->
		timeout
end.
