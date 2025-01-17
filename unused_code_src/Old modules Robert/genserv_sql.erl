%%
%% SQL Handling Gen Server
%% HOW TO USE:
%% get Pid when you start link and call one of the custom functions supplying the Pid and List of records to it.
%%
-module(genserv_sql).
-include("../include/defs.hrl").
-behaviour(gen_server).

-export([start_link/0, call_daily/2, call_hist/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
		 
-record(state,{connection}).
-define(SERVER, ?MODULE).

start_link() ->
	gen_server:start_link({local,sqlserv},?MODULE,[], []).
	
%%
%% Custom Functions
%%

%%Call for daily stock entries
call_daily(Pid,List) -> 
		call_daily(Pid,List,[],0).	
call_daily(Pid,List,NewList,20) ->  
	gen_server:cast(Pid,{daily,NewList}),
	call_daily(Pid,List,[],0);
call_daily(Pid,[],NewList,_) -> gen_server:cast(Pid,{daily,NewList});
call_daily(Pid,[H|T],NewList,Inc) ->
	call_daily(Pid,T,[H|NewList],Inc+1).
	
%% Call for historical entries	
call_hist(Pid,List) -> 
	call_hist(Pid,List,[],0).
call_hist(Pid,List,NewList,20) ->
	gen_server:cast(Pid,{historical,NewList}),
	call_hist(Pid,List,[],0);
call_hist(Pid,[],NewList,_) -> gen_server:cast(Pid,{historical,NewList});
call_hist(Pid,[H|T],NewList,Inc) -> call_hist(Pid,T,[H|NewList],Inc+1).

%% Gen Server
init([]) -> 
	odbc:start(),
	{ok,Con} = odbc:connect(?ConnectStr, []),
	{ok, #state{connection=Con}}.

handle_call({daily,RecList}, _From, State) ->
  Query = gen_query(daily,RecList),
  {ok,Pid} = odbc:connect(?ConnectStr,[]),
  Reply = odbc:sql_query(Pid,Query),
  {reply, Reply, State};
handle_call(terminate, _From, State) ->
  {stop, normal, ok, State}.
handle_cast({daily,RecList}, State) ->
	Query = gen_query(daily,RecList),
	{ok,Pid} = odbc:connect(?ConnectStr,[]),
	odbc:sql_query(Pid,Query),
	odbc:disconnect(Pid),
   {noreply, State};
handle_cast({historical,RecList}, State) ->
	Query = gen_query(historical,RecList),
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
gen_query(Type,List) -> gen_query(Type,List,"").
gen_query(_,[],Query) -> Query;
gen_query(Type,[H|T],Query) -> gen_query(Type,T,Query++gen_entry(Type,H)++";").

gen_entry(daily,R) -> lists:flatten(io_lib:format("EXEC s_addDaily @Name='"++R#dailyStock.name++"',@Symbol='"++R#dailyStock.symbol++"',@Datetime='"++R#dailyStock.date++"',@CurrentPrice=~p,@ChangeValue=~p,@ChangePercent=~p,@PreviousClose=~p,@Open=~p,@DayMaxPrice=~p,@DayMinPrice=~p,@Volume=~p",[R#dailyStock.currPrice,R#dailyStock.changeValue,R#dailyStock.changePercent,R#dailyStock.prevClose,R#dailyStock.open,R#dailyStock.high,R#dailyStock.low,R#dailyStock.volume]));
gen_entry(historical,R) -> "EXEC s_addHistorical @Symbol='"++R#hist_stock.symbol++"',@Date='"++R#hist_stock.date++"',@Open="++R#hist_stock.open++",@Close="++R#hist_stock.close++",@MaxPrice="++R#hist_stock.high++",@MinPrice="++R#hist_stock.low++",@Volume="++R#hist_stock.volume.
