%%% @author  SinanAbdulwahhab

-module(httpCALLING).
-export([geta/1,filteredValues/3]).

geta(google)-> Data = httpQ:get("http://www.nasdaq.com/screening/companies-by-name.aspx?&render=download"),


	[Attrib|Values] = re:split(tuple_to_list(Data), ",\r\n|\n",[{return,list},{parts,2}]),
	
	
	[_|FilteredAttribs] = re:split(Attrib,"\\\x22,\\\x22|\\\x22",[{return,list},trim]),
	[H|T] = re:split(Values,"\\\x22,\r\n\\\x22|\\\x22,\\\x22|\\\x22,\r\n",[{return,list},trim]),
	
	filteredValues(T,[H],0).
	
	filteredValues([],Acc,Counter)->
		Acc;
	filteredValues([H|T],Acc,Counter) when Counter == 8 ->
		filteredValues(T,[H]++Acc,0);
	filteredValues([_|T],Acc,Counter) ->
		filteredValues(T,Acc,Counter+1).