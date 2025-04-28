-module(line).

-export([start/1, send/2, stop/1]).

start(N) when N > 0 ->
    spawn(fun() -> init(N, 0) end).

init(1, Pos) ->
    process_loop(Pos, undefined);
init(N, Pos) ->
    Next = spawn(fun() -> init(N-1, Pos+1) end),
    process_loop(Pos, Next).

process_loop(Pos, Next) ->
    receive
        {message, Msg} ->
            io:format("~p received mesage ~p~n", [Pos, Msg]),
            case Next of
                undefined -> ok;
                _ -> Next ! {message, Msg}
            end,
            process_loop(Pos, Next);
        stop ->
            case Next of
                undefined -> ok;
                _ -> Next ! stop
            end
    end.

send(Pid, Msg) ->
    Pid ! {message, Msg},
    ok.

stop(Pid) ->
    Pid ! stop,
    ok.