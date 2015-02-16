-module(solution).
-export([main/0,select_input/0, repeat_testcases/2, solution/4]).

select_input() ->
    Hst = net_adm:localhost(),
    if Hst == "jacek-desktop" -> 
            {ok, Device} = file:open("./input1.txt",[read]),
            Device;
       true -> standard_io
    end.

read_integers(Device) ->
    io:fread(Device, [], "~d").

solution(_,[],0,Count) -> 
    io:format("~p~n",[Count]),
    false;
solution(Num,Formattednum,Fnl,Count) when Fnl > 0 ->
    %% io:format("args are: ~p ~p ~p  ~p~n",[Num, Formattednum, Fnl, Count]),
    Asciibase = 48,
    %% io:format("hd of formatted num ~p~n",[hd(Formattednum)]),
    Numx = hd(Formattednum) - Asciibase,
    %% io:format("~p ~p ~p~n",[Num, Numx, Fnl]),
    if Numx > 0 -> Res = Num rem Numx;
       true -> Res = 1
    end,
    Newcount = if Numx =:= 0 -> Count;
                  Res =:= 0 -> Count+1;
                  true -> Count
               end,
    %% io:format("newcount ~p  ~p ~p~n",[Newcount, Res,Numx]),
    solution(Num,tl(Formattednum),Fnl-1,Newcount).

repeat_testcases(Dev,Times) when Times > 0 ->   
    {ok,Num} = io:fread(Dev,[],"~d"),
    Formattednum = io_lib:format("~p",[hd(Num)]),
    %% io:format("got formatted ~p ~p~n",[hd(Num), Formattednum]),
    Fnl= string:len(hd(Formattednum)),
    solution:solution(hd(Num),hd(Formattednum),Fnl,0),
    solution:repeat_testcases(Dev,Times-1);
repeat_testcases(_,_) ->
    false.

main() ->
    Dev = select_input(),
    {ok, Line} = read_integers(Dev),
    Testcases = hd(Line),
    repeat_testcases(Dev,Testcases).


