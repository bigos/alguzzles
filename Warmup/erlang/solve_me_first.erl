-module(solve_me_first).
-export([main/0]).
%% compile the module, then run
%% solve_me_first:main().
%% and enter 1 and 2
solveMeFirst(A, B) ->
    A + B.

main() -> 
    {ok, [A, B]} = io:fread("", "~d~d"),
    Res = solveMeFirst(A,B),
    io:format("~p~n",[Res]).
