%%% @author  <Dungarov@DUNGAROV-PC>
%%% @copyright (C) 2013, 
%%% @doc
%%%
%%% @end
%%% Created : 18 Oct 2013 by  <Dungarov@DUNGAROV-PC>


-module(parser).
-compile(export_all).
-include_lib("eunit/include/eunit.hrl").

parse(FilePath,ForEachLine,Opaque)->
    case file:open(FilePath,[read]) of
        {_,S} ->
            start_parsing(S,ForEachLine,Opaque);
        Error -> Error
    end.

start_parsing(S,ForEachLine,Opaque)->
    Line = io:get_line(S,''),
    case Line of
        eof -> {ok,Opaque};
        "\n" -> start_parsing(S,ForEachLine,Opaque);
        "\r\n" -> start_parsing(S,ForEachLine,Opaque);
        _ -> 
            NewOpaque = ForEachLine(scanner(clean(clean(Line,10),13)),Opaque),
            start_parsing(S,ForEachLine,NewOpaque)
    end.

check(InitString,Char,[Head|Buffer]) when Head == Char -> 
    {lists:reverse(InitString),Buffer};
check(InitString,Char,[Head|Buffer]) when Head =/= Char ->
    check([Head|InitString],Char,Buffer);
check(X,_,Buffer) when Buffer == [] -> {done,lists:reverse(X)}.
scanner(Text)-> lists:reverse(traverse_text(Text,[])).

traverse_text(Text,Buff)->
    case check("",$,,Text) of
        {done,SomeText}-> [SomeText|Buff];
        {Value,Rem}-> traverse_text(Rem,[Value|Buff])
    end.

clean(Text,Char)-> 
    string:strip(string:strip(Text,right,Char),left,Char).
