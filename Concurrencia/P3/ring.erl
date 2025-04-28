-module(ring).

-export([start/1, send/3, stop/1]).

start(N) when N > 0 ->
    First_pid = spawn(fun() -> process_loop(N, 0) end),
    create_ring(First_pid, First_pid, 1, N),
    First_pid.

create_ring(First_pid, Prev_pid, Count, N) when N > 0 ->
    Pid = spawn(fun() -> process_loop(Count, undefined) end),
    Prev_pid ! {set_next, Pid},
    create_ring(First_pid, Pid, Count + 1, N);
create_ring(First_pid, Last_pid, N, N) ->
    Last_pid ! {set_next, First_pid}.

process_loop(Id, Next_pid) ->
    receive
        {set_next, Next} ->
            process_loop(Id, Next);
        {message, Remaining_count, Msg} ->
            io:format("~p receiving message ~p with ~p left~n", [Id, Msg, Remaining_count]),
            if Remaining_count > 0 ->
                   Next_pid ! {message, Remaining_count - 1, Msg};
               Remaining_count == 0 ->
                   io:format("~p received last message ~p~n", [Id, Msg]);
               true ->
                   ok
            end,
            process_loop(Id, Next_pid);
        stop ->
            case Next_pid of
                undefined ->
                    ok;
                _ ->
                    Next_pid ! stop
            end
    end.

send(Pid, N, Msg) when N >= 0 ->
    Pid ! {message, N, Msg},
    ok.

stop(Pid) ->
    Pid ! stop,
    ok.
