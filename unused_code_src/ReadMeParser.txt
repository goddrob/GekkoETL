Before the parser is started you need to run these lines in the compiler

1> ForEachLine = fun(Line,Buffer)-> io:format("Line: ~p~n",[Line]),Buffer end.

2>InitialBuffer = [].

You call the parser:

3> parser:parse("C:/table.csv", ForEachLine, InitialBuffer).