-module(hist_main).
-include("../include/defs.hrl").
-compile(export_all).

-define(AMOUNT_OF_C_PROC, 500).
%Test comment GIT

test() ->
	Tickers = startup(),
	% {A, _} = lists:split(1, Tickers),
	parse_yahoo(Tickers, convert_dates(default)).

start(Old, Recent) ->
	main(Old, Recent).
	
start(default) ->
	main(default).
	
main(default) ->
	Tickers = startup(),
	Dates = convert_dates(default),
	parse_yahoo(Tickers, Dates),
	timer:apply_interval(216000000, ?MODULE, parse_yahoo, [Tickers, Dates]).

main(Old, Recent) ->
	Tickers = startup(),
	Dates = convert_dates(Old, Recent),
	parse_yahoo(Tickers, Dates),
	RecentDates = convert_dates(default),
	timer:apply_interval(216000000, ?MODULE, parse_yahoo, [Tickers, RecentDates]).
	
startup() ->
	inets:start(),
	odbc:start(),
	register(),
	parse_nasdaq().

register() ->
	case whereis(?MODULE) of
		undefined -> register(?MODULE, self());
		_ -> already_defined
	end.
	
parse_nasdaq() ->
	io:format("Parsing Nasdaq..."),
	inets:start(httpc, [{profile, foo}]),
	Tickers = lists:reverse(nasdaqTickers:get()),
	io:format("done~n~n"),
	Tickers.

parse_yahoo(Tickers, Dates) when length(Tickers) > ?AMOUNT_OF_C_PROC ->
	{A, B} = lists:split(?AMOUNT_OF_C_PROC, Tickers),
	spawn_workers(A, Dates),
	parse_yahoo(B, Dates);
parse_yahoo(Tickers, Dates) ->
	spawn_workers(Tickers, Dates),
	io:format("~n~nDone........restarting in: 6 hours~n~n").

spawn_workers(Tickers, Dates) ->
	spawn_workers(Tickers, Dates, 0).

spawn_workers([One_Ticker|Rest], Dates, Children) ->
	spawn_link(hist_worker, process_ticker, [One_Ticker, Dates]),
	spawn_workers(Rest, Dates, Children + 1);
spawn_workers([], _Dates, Children) ->
	loop_receive(Children).

loop_receive(Children) ->
	process_flag(trap_exit, true),
	loop_receive(Children, 0).
loop_receive(Children, Normal_Exits) ->
	case Children == Normal_Exits of 
		true ->
			{_, {H, Min, Sec}} = calendar:local_time(),
			io:format("**Time: ~p:~p:~p, Finished segment of size: ~p**~n", [H, Min, Sec, Children]);
		false ->
			receive
				{'EXIT', _Pid, normal} ->
					loop_receive(Children, Normal_Exits + 1);
				{'EXIT', _Pid, {badmatch, {Ticker, Date}}} ->
					Pid = spawn_link(hist_worker, process_ticker, [Ticker, Date]),
					io:format("Badmatched Ticker: ~p, restarting Pid: ~p~n", [Ticker, Pid]),
					loop_receive(Children, Normal_Exits);
				Catch_All -> 
					io:format("Catch_All: ~p~n", [Catch_All]),
					loop_receive(Children, Normal_Exits + 1)
			end
	end.
restart_inets() ->
	inets:stop(httpc, foo),
	inets:start(httpc, [{profile, foo}]).
	
convert_dates(default) ->
	{{Year, Month, Day}, {_,_,_}} = calendar:local_time(),
	Recent_Return = date_helper(integer_to_list(Year) 
		++ integer_to_list(Month) ++ integer_to_list(Day)),
	case Day < 3 of
		false ->
			Old_Return = date_helper(integer_to_list(Year) ++
				integer_to_list(Month) ++ integer_to_list(Day - 3));
		true ->
			Old_Return = date_helper(integer_to_list(Year)++
				integer_to_list(Month - 1) ++ integer_to_list(28))
	end,
	case Old_Return < Recent_Return of
		true ->
			{Old_Return, Recent_Return};
		false ->
			throw("Invalid dates, the second argument must be the recent date")
	end.

convert_dates(Old, Recent) ->
	Old_Return = date_helper(Old),
	Recent_Return = date_helper(Recent),
	case Old_Return < Recent_Return of
		true -> 
			{Old_Return, Recent_Return};
		false ->
			throw("Invalid dates, the second argument must be the recent date")
	end.
	
date_helper(Date) ->
	{Year_String, MonthDay_String} = lists:split(4, Date),
	{Month_String, Day_String} = lists:split(2, MonthDay_String),
	Year = list_to_integer(Year_String),
	Month = list_to_integer(Month_String),
	Day = list_to_integer(Day_String),
	case calendar:valid_date(Year, Month, Day) of
		false -> throw("Invalid dates");
		true -> 
			Return_Year = integer_to_list(Year),
			Return_Day = integer_to_list(Day),
			if 
				Month == 1 -> Return_Month = "12";
				true -> Return_Month = integer_to_list(Month - 1)
			end,
			{Return_Year, Return_Month, Return_Day}
	end.
