%%% ============================================================
%%% Hero Realms - Servidor de Juego
%%% Backend en Erlang
%%% ============================================================
-module(game_server).
-export([start/0, accept_loop/1, client_loop/2]).
-define(PORT, 4000).
-define(DEFAULT_TURN, "Jugador A").

%%% ============================================================
%%% INICIO DEL SERVIDOR
%%% ============================================================
start() ->
    {ok, ListenSocket} = gen_tcp:listen(?PORT, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]),
    io:format("Servidor Hero Realms iniciado en el puerto ~p~n", [?PORT]),
    %% Proceso global de estado del juego
    register(game_state, spawn(fun() -> turn_loop(?DEFAULT_TURN) end)),
    accept_loop(ListenSocket).

%%% ============================================================
%%% ACEPTAR CLIENTES
%%% ============================================================
accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() ->
        Player = assign_player(),
        gen_tcp:send(Socket, "{\"action\":\"welcome\",\"message\":\"Bienvenido al servidor Hero Realms!\"}\n"),
        io:format("~s conectado.~n", [Player]),
		CurrentTurn = get_turn(),
		gen_tcp:send(Socket, "{\"action\":\"update\",\"turn\":\"" ++ CurrentTurn ++ "\"}\n"),
        client_loop(Socket, Player)
    end),
    accept_loop(ListenSocket).

%%% ============================================================
%%% LOOP DEL CLIENTE
%%% ============================================================
client_loop(Socket, Player) ->
    inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            handle_message(binary_to_list(Data), Player, Socket),
            client_loop(Socket, Player);
        {tcp_closed, Socket} ->
            io:format("~s se desconectó~n", [Player])
    end.

%%% ============================================================
%%% PROCESAR MENSAJES DEL CLIENTE
%%% ============================================================
handle_message("END_TURN" ++ _, Player, _Socket) ->
    CurrentTurn = get_turn(),
    if
        Player =:= CurrentTurn ->
            NewTurn = case CurrentTurn of
                "Jugador A" -> "Jugador B";
                _ -> "Jugador A"
            end,
            set_turn(NewTurn),
            io:format("➡ Turno cambiado a: ~s~n", [NewTurn]),
            send_to_both("{\"action\":\"update\",\"turn\":\"" ++ NewTurn ++ "\"}\n");
        true ->
            io:format("❌ ~s intentó pasar turno, pero no era su turno.~n", [Player])
    end;

handle_message(Data, Player, _Socket) ->
    case string:find(Data, "PLAY_CARD") of
        nomatch ->
            ok;
        _ ->
            CurrentTurn = get_turn(),
            if
                Player =:= CurrentTurn ->
                    io:format("🃏 ~s jugó una carta.~n", [Player]);
                true ->
                    io:format("❌ ~s intentó jugar fuera de turno.~n", [Player])
            end
    end.

%%% ============================================================
%%% OBTENER / CAMBIAR TURNO GLOBAL
%%% ============================================================
get_turn() ->
    case whereis(game_state) of
        undefined ->
            "Jugador A";
        Pid ->
            Pid ! {get_turn, self()},
            receive
                {turn, Turn} -> Turn
            after 1000 ->
                "Jugador A"
            end
    end.

set_turn(NewTurn) ->
    case whereis(game_state) of
        undefined -> ok;
        Pid -> Pid ! {set_turn, NewTurn}, ok
    end.

%%% ============================================================
%%% ENVÍO DE MENSAJES A CLIENTES
%%% ============================================================
send_to_both(Msg) ->
    send_to(player_a, Msg),
    send_to(player_b, Msg).

send_to(Name, Msg) ->
    case whereis(Name) of
        undefined -> ok;
        Pid -> Pid ! {send, Msg}
    end.

%%% ============================================================
%%% REGISTRO DE JUGADORES (PETICIÓN DESDE CLIENTE)
%%% ============================================================
assign_player() ->
    game_state ! {assign_request, self()},
    receive
        {assigned, Name} ->
            Name
    after 2000 ->
        io:format("⏱ No se pudo asignar jugador a tiempo.~n"),
        "Unknown"
    end.

%%% ============================================================
%%% BUCLE PRINCIPAL DEL ESTADO GLOBAL (TURNOS Y REGISTROS)
%%% ============================================================
turn_loop(CurrentTurn) ->
    receive
        {assign_request, Pid} ->
            Name = assign_player_name(Pid),
            Pid ! {assigned, Name},
            turn_loop(CurrentTurn);

        {set_turn, NewTurn} ->
            io:format("🔄 Turno cambiado a: ~s~n", [NewTurn]),
            turn_loop(NewTurn);

        {get_turn, Caller} ->
            Caller ! {turn, CurrentTurn},
            turn_loop(CurrentTurn)
    end.

assign_player_name(Pid) ->
    case {whereis(player_a), whereis(player_b)} of
        {undefined, _} ->
            unregister_if_exists(player_a),
            register(player_a, Pid),
            io:format("Jugador A registrado correctamente.~n"),
            "Jugador A";
        {_, undefined} ->
            unregister_if_exists(player_b),
            register(player_b, Pid),
            io:format("Jugador B registrado correctamente.~n"),
            "Jugador B";
        _ ->
            io:format("⚠ Ambos jugadores ya conectados. Rechazando nuevo intento.~n"),
            "Unknown"
    end.

%%% ============================================================
%%% FUNCIONES AUXILIARES
%%% ============================================================
unregister_if_exists(Name) ->
    case whereis(Name) of
        undefined -> ok;
        Pid ->
            unregister(Name),
            exit(Pid, kill)
    end.
