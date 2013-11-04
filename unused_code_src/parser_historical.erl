% @AUTHOR: DANI HODOVIC
-module(parser_historical).
% -export(main/0).
-compile(export_all).
-include("../include/defs.hrl").
% NOT ClEANED, NOT COMPLETED, NOT COMMENTED, INITIAL VERSION
% MISSING DEPENDENCIES, NOT CONCURRENT - V2 OUT SOON!

main() ->
	inets:start(),
	All_Tickers = nasdaqTickers:get(),
	{A,_} = lists:split(50, All_Tickers),
	print(process_started),
	lists:flatten(iterate_tickers(A, [])).

iterate_tickers([H|T], Acc) ->
	iterate_tickers(T, [processTicker(H)|Acc]);
iterate_tickers([], Acc) ->
	Acc.

processTicker(Ticker) ->
	{{Year, Month, Day}, _Time} = calendar:local_time(),
	SYear = integer_to_list(Year),
	SMonth = integer_to_list(Month),
	SDay = integer_to_list(Day),
	S = "http://ichart.yahoo.com/table.csv?s="++Ticker
		++"&a=5"++"&b=15"++"&c=2013"++"&d="++ SMonth ++ "&e="++ SDay ++ "&f="++ SYear
			++"&d=m&ignore=.csv",
	{_, {_,_,CSV}} = httpc:request(S),
	case (string:chr(CSV, $!) > 0) of 
		false -> parse_csv({CSV}, Ticker);
		true -> error
	end.

parse_csv(CSV, Ticker) ->
	[_|List] = re:split(tuple_to_list(CSV), "\n",[{return,list},{parts,infinity}]),
	iterate_records(List, [], Ticker).

% Iterates over records and calls another function 
% to make the actualy records for each line	
iterate_records([H|T], Acc, Ticker) ->
	String = string:tokens(H, ","),
	case String =/= [] of
	 true -> iterate_records(T, [make_rec(String, 0, #histStock{}, Ticker)|Acc], Ticker);
	 false -> Acc
	end;
iterate_records([], Acc, _Ticker) -> Acc.

make_rec([H|T], N, Rec, Ticker) -> 
	make_rec(T, N + 1, Rec#histStock{symbol = Ticker, date=((H -- [$-]) --[$-])}).
make_rec([H|T], N, Rec) when N == 1 -> 
	make_rec(T, N + 1, Rec#histStock{open=H});
make_rec([H|T], N, Rec) when N == 2 -> 
	make_rec(T, N + 1, Rec#histStock{high=H});
make_rec([H|T], N, Rec) when N == 3 -> 
	make_rec(T, N + 1, Rec#histStock{low=H});
make_rec([H|T], N, Rec) when N == 4 -> 
	make_rec(T, N + 1, Rec#histStock{close=H});
make_rec([H|T], N, Rec) when N == 5 -> 
	make_rec(T, N + 1, Rec#histStock{volume=H});
make_rec(_, _, Rec) ->
	Rec.

% Printer for testing
print(Var) ->
	io:format("*************"),
	io:format("~p", [Var]),
	io:format("*************~n").
	

% {531042000, 1000 list, 1 process}
%  
