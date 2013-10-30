%%% @author  SinanAbdulwahhab

-module(httpQ).
-export([get/1]).

get(Quary) ->
	inets:start(),
	{ok, {_,Headers,Body}} = httpc:request(Quary),
	inets:stop(),
	{Body}.