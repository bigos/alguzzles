-module(solution).
-export([main/0]).


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
  .


