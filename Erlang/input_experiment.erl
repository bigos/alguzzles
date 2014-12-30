-module(input_experiment).
-export([main/0,select_input/0]).

select_input() ->
    Hst = net_adm:localhost(),
    if Hst == "jacek-desktop" -> 
            {ok, Device} = file:open("./input1.txt",[read]),
            Device;
       true -> standard_io
    end.

read_integers(Device) ->
   io:fread(Device, [], "~d").

main() ->
    Dev = select_input(),
    {ok, Line} = read_integers(Dev),
    io:format("your first line is: ~p~n",[hd(Line)]),
    Line2 = io:get_line(Dev,"prompt"),
    io:format("your first line is: ~p~n",[Line2]),
    Line3 = io:get_line(Dev,"prompt"),
    io:format("your first line is: ~p~n",[Line3]).
