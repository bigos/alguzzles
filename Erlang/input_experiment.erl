-module(input_experiment).
-export([main/0]).

select_input() ->
    if net_adm:localhost() == "jacek-desktop" -> file:open("./input1.txt",[read]);
       true -> standard_io
    end.


main() ->
    io:format("your localhost is: ~p~n",[net_adm:localhost()]).
