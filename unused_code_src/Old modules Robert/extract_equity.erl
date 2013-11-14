-module(extract_equity).
-export([grab_data/0]).
-include("../include/defs.hrl").


open_file(test) -> 
	case file:open("../test_files/bihourly201206221430.csv",[read]) of
	{ok, File} -> File;
	{error, Reason} -> {error, Reason}
	end;
open_file(ftp) -> ok.
grab_data() -> 
	File = open_file(test),
	io:get_line(File,""),
	parse_file(File,[]).

parse_file(File,RecList) -> 
	case io:get_line(File,"") of
		eof -> RecList;
		{error, Reason} -> {error,Reason};
		Data -> parse_file(File,[parse_line(Data)|RecList])		
	end.

parse_line(Data) -> [Name,Symbol,Date,CurrPrice,ChangeValue,ChangePercent,PrevClose,Open,Volume,Min,Max|_] = re:split(Data,"[,]",[{return,list}]),
	#dailyStock{name=Name,symbol=Symbol,date=fix_date(Date),currPrice=fix_float(CurrPrice),changeValue=fix_float(ChangeValue),changePercent=fix_float(ChangePercent),prevClose=fix_float(PrevClose),open=fix_float(Open),high=fix_float(Max),low=fix_float(Min),volume=fix_float(Volume)}.

fix_float(String) -> {N,_} = string:to_float(String),
	N.
fix_date(Datetime) -> Datetime -- [$-,$-].
	