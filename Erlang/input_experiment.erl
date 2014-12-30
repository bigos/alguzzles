-module(input_experiment).
-export([main/0,select_input/0, repeat_testcases/2]).

select_input() ->
    Hst = net_adm:localhost(),
    if Hst == "jacek-desktop" -> 
            {ok, Device} = file:open("./input1.txt",[read]),
            Device;
       true -> standard_io
    end.

read_integers(Device) ->
   io:fread(Device, [], "~d").

repeat_testcases(Fun,Times) when Times > 0 ->
    io:format("times is: ~p~n",[Times]),
    repeat_testcases(Fun,Times-1);
repeat_testcases(_,_) ->
    false.

main() ->
    Dev = select_input(),
    {ok, Line} = read_integers(Dev),
    Testcases = hd(line),
    repeat_testcases("",Testcases),
    %% need to work out what to do with the data
    io:format("your first line is: ~p~n",[hd(Line)]),
    Line2 = io:get_line(Dev,"prompt"),
    io:format("your first line is: ~p~n",[Line2]),
    Line3 = io:get_line(Dev,"prompt"),
    io:format("your first line is: ~p~n",[Line3]).
