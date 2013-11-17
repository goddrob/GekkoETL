-module(parser_hist_main).
-include("../include/defs.hrl").
-compile(export_all).

start(Old, Recent) ->
	inets:start(),
	register(),
	spawn_link(db, start, []),
	Dates = convert_dates(Old, Recent),
	Tickers = parse_nasdaq(),

	parse_yahoo(Tickers, Dates).
	
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

parse_yahoo(Tickers, Dates) ->
	parse_yahoo(Tickers, Dates, 80).

parse_yahoo(Tickers, Dates, Segment_Size) when length(Tickers) > Segment_Size ->
	{A, B} = lists:split(Segment_Size, Tickers),
	spawn_workers(A, Dates),
	io:format("Segment done~n"),
	restart_inets(),
	parse_yahoo(B, Dates, Segment_Size);
parse_yahoo(Tickers, Dates, _Segment_Size) ->
	spawn_workers(Tickers, Dates),
	ok.

spawn_workers(Tickers, Dates) ->
	spawn_workers(Tickers, Dates, 0).

spawn_workers([One_Ticker|Rest], Dates, Children) ->
	spawn_link(parser_hist_worker, process_ticker, [One_Ticker, Dates]),
	spawn_workers(Rest, Dates, Children + 1);
spawn_workers([], _Dates, Children) ->
	loop_receive(Children).

loop_receive(Children) ->
	loop_receive(Children, 0).
loop_receive(Children, Normal_Exits) ->
	case Children == Normal_Exits of 
		true ->
			all_are_finished;
		false ->
			receive
				{Pid, done} ->
					io:format("Finished Pid at: ~p~n", [Pid]),
					loop_receive(Children, Normal_Exits + 1);
				{Pid, unparsable_CSV} ->
					io:format("Could not read CSV at: ~p~n", [Pid]),
					loop_receive(Children, Normal_Exits + 1);
				{Pid, {error, _Ticker, _Dates}} ->
					io:format("Error in Pid: ~p...~n", [Pid]),
					loop_receive(Children, Normal_Exits + 1);
				Catch_All -> 
					io:format("Catch_All: ~p", [Catch_All])
			end
	end.

restart_inets() ->
	inets:stop(httpc, foo),
	inets:start(httpc, [{profile, foo}]).

	
convert_dates(Old, now) ->
	{{Recent_Year, Recent_Month, Recent_Day}, _Time} = calendar:local_time(),	
	{Old_Year, MonthDay} = lists:split(2, Old),
	{Old_Month, Old_Day} = lists:split(2, MonthDay),
	io:format("Dates From: " ++ Old_Year ++ Old_Month ++ Old_Day ++ "~n"
			 ++ "Dates To: " ++ Recent_Year ++ Recent_Month ++ Recent_Day ++ "~n~n"),
	{{Old_Year, Old_Month, Old_Day}, {Recent_Year, Recent_Month, Recent_Day}};
convert_dates(Old, Recent) ->
	{Old_Year, Old_MonthDay} = lists:split(2, Old),
	{Old_Month, Old_Day} = lists:split(2, Old_MonthDay),
	{Recent_Year, Recent_MonthDay} = lists:split(2, Recent),
	{Recent_Month, Recent_Day} = lists:split(2, Recent_MonthDay),
	io:format("Dates From: " ++ Old_Year ++ Old_Month ++ Old_Day ++ "~n"
			 ++ "Dates To: " ++ Recent_Year ++ Recent_Month ++ Recent_Day ++ "~n~n"),
	{{Old_Year, Old_Month, Old_Day}, {Recent_Year, Recent_Month, Recent_Day}}.

	