%% @Author: Dani Hodovic
%% ====================================================================
%% Description: 
%% A general purpose library for methods shared between 
%% multiple modules
%% ====================================================================

-module(common_methods).
-export([read_existing_file/0, print/2, print/3]).


%% read_existing_file/0
%% ====================================================================
%% Reads the tickers and calls parse_binary if the file exists

read_existing_file() ->
	case filelib:is_file("tickers.txt") of
		true ->
			{ok, Binary} = file:read_file("tickers.txt"),
			parse_binary(binary_to_list(Binary));
		false ->
			throw("Tickers.txt not found")
	end.

%% parse_binary/1
%% ====================================================================
%% Parses the binary file, removes duplicate tickers and returns a unique list

parse_binary(Binary) ->
	String = re:replace(Binary, "\\[|]|'|\\s|\\n| |\\.|\\\"|\\^+.|~", "", [global, {return, list}]),
	DuplicateList = string:tokens(String, ","),
	UniqueList = lists:usort(DuplicateList),
	UniqueList.

%% print/2
%% ====================================================================
%% Reads the module and the msg, and prints differently
%% depending on who the caller was. This method was made to save 
%% me time, by avoiding to write io:format/2 over and over again

print(Module, Msg) ->
	case Module of
		hist_gen ->
			io:format("[Historical] - ");
		hist_worker ->
			io:format("[Historical] - ");
		news_gen ->
			io:format("[News] - ");
		news_worker ->
			io:format("[News] - ");
		nasdaqTickers ->
			io:format("[Nasdaq] - ");
		nasdaq_bridge ->
			io:format("[Nasdaq] - ")
	end,
	io:format("~s~n", [Msg]).

%% print/3
%% ====================================================================
%% This time with an additional parameter

print(Module, Msg, Param) ->
	case Module of
		hist_gen ->
			io:format("[Historical] - ");
		hist_worker ->
			io:format("[Historical] - ");
		news_gen ->
			io:format("[News] - ");
		news_worker ->
			io:format("[News] - ");
		nasdaqTickers ->
			io:format("[Nasdaq] - ");
		nasdaq_bridge ->
			io:format("[Nasdaq] - ")
	end,
	io:format("~s ~p~n", [Msg, Param]).
