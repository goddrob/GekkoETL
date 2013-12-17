-module(error_logging).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-compile(export_all).


-define(Log_File, "errors_for_dani.txt").


%% ====================================================================
%% API functions
%% ====================================================================
-export([]).

start() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

cast_error(Msg) ->
	gen_server:cast(?MODULE, Msg).

%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(state, {}).

init([]) ->
	process_flag(trap_exit, true),
	timer:apply_interval(216000000, ?MODULE, clear_file, []),
    {ok, #state{}}.


handle_call(Request, From, State) ->
    Reply = ok,
    {reply, Reply, State}.


handle_cast(Error, State) ->
	write_to_file(Error),
    {noreply, State}.


handle_info(Info, State) ->
    {noreply, State}.


terminate(Reason, State) ->
    ok.


code_change(OldVsn, State, Extra) ->
    {ok, State}.


write_to_file(Msg) ->
	String = Msg,
	case filelib:is_file(?Log_File) of
		true ->
			file:write_file(?Log_File, io_lib:fwrite("\n~p\n", [String]), [append]);
		false ->
			file:write_file(?Log_File, io_lib:fwrite("\n~p\n", [String]))
	end.

clear_file() ->
	case filelib:is_file(?Log_File) of
		true ->
			remove_content(?Log_File);
		false ->
			ok
	end.


remove_content(File) ->
	case filelib:file_size(File) > 10000000 of
		true ->
			file:write_file(File, io_lib:fwrite("~p \n", [""]));
		false ->
			ok
	end.

			