%% 
%% @AUTHOR : Robert Petre
%% SQL Handling Gen Server
%% HOW TO USE:
%% call_daily(DailyRecordList)
%% call_historical(HistoricalRecordList)
%% call_news(NewsRecordList)
%% stop() / stops the gen_serv

-module(server_sql).
-include("../include/defs.hrl").
-behaviour(gen_server).

-export([start_link/0, call_daily/1, stop/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


start_link() ->
	gen_server:start_link({local,?SQLSERVER},?MODULE,[], []).
	
%%
%% Custom Functions
%%

%%Call for daily stock entries
call_daily(List) -> 
		call_daily(List,[],0).	
call_daily(List,NewList,20) ->  
	gen_server:cast(whereis(serverSQL),{daily,NewList}),
	call_daily(List,[],0);
call_daily([],NewList,_) -> gen_server:cast(whereis(serverSQL),{daily,NewList});
call_daily([H|T],NewList,Inc) ->
	call_daily(T,[H|NewList],Inc+1).
	
%% Stops sql gen server
stop() ->
	gen_server:call(whereis(serverSQL),stop).

%% Gen Server
init([]) -> 
	odbc:start(),
	{ok, stateless}.


handle_call(stop, _From, State) ->
	odbc:stop(),
	{stop, normal, {sql,shutdown_ok}, State}.
	
handle_cast({daily,RecList}, State) ->
	Query = gen_query(daily,RecList),
	{ok,Pid} = odbc:connect(?ConnectStr,[]),
	odbc:sql_query(Pid,Query),
	odbc:disconnect(Pid),
   {noreply, State}.
  
  
  
handle_info(_Info, State) ->
  {noreply, State}.
  
terminate(_Reason, _State) ->
  ok.
  
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Private Helper functions 
%% Generate queries from records 
gen_query(Type,List) -> gen_query(Type,List,"").
gen_query(_,[],Query) -> Query;
gen_query(Type,[H|T],Query) -> gen_query(Type,T,Query++gen_entry(Type,H)++";").

gen_entry(daily,R) -> lists:flatten(io_lib:format("EXEC s_addDaily @Name='"++R#dailyStock.name++"',@Symbol='"++R#dailyStock.symbol++"',@Datetime='"++R#dailyStock.date++"',@CurrentPrice=~p,@ChangeValue=~p,@ChangePercent=~p,@PreviousClose=~p,@Open=~p,@DayMaxPrice=~p,@DayMinPrice=~p,@Volume=~p",[R#dailyStock.currPrice,R#dailyStock.changeValue,R#dailyStock.changePercent,R#dailyStock.prevClose,R#dailyStock.open,R#dailyStock.high,R#dailyStock.low,R#dailyStock.volume])).