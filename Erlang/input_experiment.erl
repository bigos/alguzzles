-module(input_experiment).
-export([main/0,select_input/0, repeat_testcases/2, solution/1]).

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
    Divnums = lists:seq(2,Num div 2 + 1),
    io:format("~p~n",[Divnums]),
    false.

repeat_testcases(Dev,Times) when Times > 0 ->   
    {ok,Num} = io:fread(Dev,[],"~d"),
    input_experiment:solution(hd(Num)),
    input_experiment:repeat_testcases(Dev,Times-1);
repeat_testcases(_,_) ->
    false.

main() ->
    Dev = select_input(),
    {ok, Line} = read_integers(Dev),
    Testcases = hd(Line),
    repeat_testcases(Dev,Testcases).
 
 
