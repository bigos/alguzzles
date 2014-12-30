-module(input_experiment).
-export([main/0, hst/0,select_input/0]).

hst() ->
    net_adm:localhost().

select_input() ->
    Hst = hst(),
    if Hst == "jacek-desktops" -> 
            file:open("./input_experiment.erl",[read]);
       true -> 
            standard_io
    end.


main() ->
    io:format("your localhost is: ~p~n",[net_adm:localhost()]).
