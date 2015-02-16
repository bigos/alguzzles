-module(solution).
-export([main/0]).


select_input() ->
    Hst = net_adm:localhost(),
    Found = string:str(Hst, "acek"),
    if Found > 0 -> 
            {ok, Device} = file:open("./input2.txt",[read]),
            Device;
       true -> standard_io
    end.

read_integers(Device) ->
    io:fread(Device, [], "~d").

main() ->
    Dev = select_input(),
    {ok, Line} = read_integers(Dev),
    io:format("read ~p~n",Line),
{ok, Line2} = read_integers(Dev),
    io:format("read ~p~n",Line2),
    io:format("finished main~n")
  .


%% it correctly respopnds to substring in hostname
%% it does not read all numbers in Line2
