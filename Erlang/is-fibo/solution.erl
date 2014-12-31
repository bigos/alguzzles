-module(solution).
-export([main/0,solution/1]).

select_input() ->
    Hst = net_adm:localhost(),
    if Hst == "jacek-desktop" -> 
            {ok, Device} = file:open("./input1.txt",[read]),
            Device;
       true -> standard_io
    end.

read_integers(Device) ->
    io:fread(Device, [], "~d").

solution(Num) ->
    io:format("going to solve ~p~n",[Num]).

repeat_testcases(Dev,Times) when Times > 0 ->   
    {ok,Num} = read_integers(Dev),
    solution:solution(hd(Num)),
    repeat_testcases(Dev,Times-1);
repeat_testcases(_,0) ->
    io:format("finished testcases~n"),
    false.

main() ->
    Dev = select_input(),
    {ok, Line} = read_integers(Dev),
    Testcases = hd(Line),
    io:format("testcases ~p~n",[Testcases]),
    repeat_testcases(Dev,Testcases).
