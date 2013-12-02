-module(news_main).
-compile(export_all).
-define(AMOUNT_OF_C_PROC, 200).

start() ->
	Tickers = startup(),
	% {A,_} = lists:split(1000, Tickers),
	parse_news(Tickers).
%%         timer:apply_interval(216000000, ?MODULE, parse_news, [Tickers, Dates]).

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
	inets:start(httpc, [{profile, news}]),
	Tickers = lists:reverse(nasdaqTickers:get()),
	io:format("done~n~n"),
	Tickers.

parse_news(Tickers) when length(Tickers) > ?AMOUNT_OF_C_PROC ->
	{A, B} = lists:split(?AMOUNT_OF_C_PROC, Tickers),
	spawn_workers(A),
	parse_news(B);
parse_news(Tickers) ->
	spawn_workers(Tickers),
	io:format("~n~nDone........restarting in: 6 hours~n~n").

spawn_workers(Tickers) ->
	spawn_workers(Tickers, 0).

spawn_workers([One_Ticker|Rest], Children) ->
	spawn_link(news_worker, process_ticker, [One_Ticker]),
	spawn_workers(Rest, Children + 1);
spawn_workers([], Children) ->
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
				{'EXIT', _Pid, {badmatch, Ticker}} ->
					Pid = spawn_link(news_worker, process_ticker, [Ticker]),
					io:format("Badmatched Ticker: ~p, restarting Pid: ~p~n", [Ticker, Pid]),
					loop_receive(Children, Normal_Exits);
				Catch_All -> 
					io:format("Catch_All: ~p~n", [Catch_All]),
					loop_receive(Children, Normal_Exits + 1)
			end
	end.

restart_inets() ->
	inets:stop(httpc, news),
	inets:start(httpc, [{profile, news}]).