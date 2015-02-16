-module(solution).
-export([main/0,solution/1,fib/1]).

select_input() ->
    Hst = net_adm:localhost(),
    if Hst == "jacek-desktop" -> 
            {ok, Device} = file:open("./input1.txt",[read]),
            Device;
       true -> standard_io
    end.

read_integers(Device) ->
    io:fread(Device, [], "~d").

fib(1) -> 1;
fib(2) -> 1;
fib(N) when N > 2 -> fib1(N,1,1).

fib1(3,P1,P2) -> P1 + P2; 
fib1(N,P1,P2) ->
    fib1(N-1,P2, P1 + P2).

is_fibo(N,X) ->
    Y = fib(X),
    if N =:= Y -> true;
       N < Y -> false; 
       true ->
            is_fibo(N,X+1)
    end.
is_fibo(N) ->
    is_fibo(N,1).

solution(Num) ->    
    %%io:format("going to solve ~p~n",[Num]),
    Res = is_fibo(Num),
    if Res ->
            io:format("IsFibo~n");
       true ->
            io:format("IsNotFibo~n")
    end.

repeat_testcases(Dev,Times) when Times > 0 ->   
    {ok,Num} = read_integers(Dev),
    solution:solution(hd(Num)),
    repeat_testcases(Dev,Times-1);
repeat_testcases(_,0) ->
    %% io:format("finished testcases~n"),
    ok.

main() ->
    Dev = select_input(),
    {ok, Line} = read_integers(Dev),
    Testcases = hd(Line),
    %% io:format("testcases ~p~n",[Testcases]),
    repeat_testcases(Dev,Testcases).
