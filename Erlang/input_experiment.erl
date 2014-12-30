-module(input_experiment).
-export([main/0,select_input/0]).

select_input() ->
    Hst = net_adm:localhost(),
    if Hst == "jacek-desktop" -> 
            {ok, Device} = file:open("./input1.txt",[read]),
            Device;
       true -> standard_io
    end.


main() ->
    Dev = select_input(),
    Line = io:get_line(Dev,"prompt"),
    io:format("your first line is: ~p~n",[Line]),
    Line2 = io:get_line(Dev,"prompt"),
    io:format("your first line is: ~p~n",[Line2]),
    Line3 = io:get_line(Dev,"prompt"),
    io:format("your first line is: ~p~n",[Line3]).
