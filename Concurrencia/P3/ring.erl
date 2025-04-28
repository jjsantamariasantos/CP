-module(ring).
-export([start/1, send/3, stop/1, process/2]).

%% Inicia el anillo con N procesos
start(N) when N > 0 ->
    Pids = spawn_processes(N),
    link_processes(Pids),
    hd(Pids).

%% Crea N procesos y los devuelve en una lista
spawn_processes(N) ->
    [spawn(?MODULE, process, [Num, undefined]) || Num <- lists:seq(0, N - 1)].

%% Enlaza los procesos en un anillo
link_processes([First | Rest]) ->
    link_processes([First | Rest], First).

link_processes([Pid], First) -> Pid ! {set_next, First};
link_processes([Pid, Next | Rest], First) ->
    Pid ! {set_next, Next},
    link_processes([Next | Rest], First).

%% Bucle principal del proceso
loop(Number, NextPid) ->
    receive
        {msg, Count, Msg} when Count > 0 ->
            io:format("~p recibió ~p, quedan ~p~n", [Number, Msg, Count - 1]),
            NextPid ! {msg, Count - 1, Msg},
            loop(Number, NextPid);
        stop ->
            io:format("Proceso ~p deteniéndose~n", [Number]),
            NextPid ! stop
    end.

%% Proceso inicial esperando su siguiente PID
process(Number, _) ->
    receive
        {set_next, NextPid} -> loop(Number, NextPid)
    end.

%% Envía un mensaje al anillo
send(Pid, Count, Msg) when Count > 0 ->
    Pid ! {msg, Count, Msg},
    ok.

%% Detiene el anillo
stop(Pid) ->
    Pid ! stop,
    ok.
