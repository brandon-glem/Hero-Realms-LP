-module(test_game).
-export([run/0]).

run() ->
    JugadorA = player:crear("JugadorA"),
    JugadorB = player:crear("JugadorB"),

    % funciones de player.erl
    io:format("=== Jugadores iniciales ===~n"),
    player:mostrar(JugadorA),
    player:mostrar(JugadorB),

    % funciones de cartas.erl
    Carta1 = cartas:obtener(101), 
    Carta2 = cartas:obtener(102), 
    io:format("Carta 1: ~s~n", [cartas:nombre_carta(101)]),
    io:format("Carta 2: ~s~n", [cartas:nombre_carta(102)]),
    % funciones de player_acciones.erl
    JugadorA1 = player_acciones:jugar_carta(Carta1, JugadorA),
    JugadorA2 = player_acciones:jugar_carta(Carta2, JugadorA1),

    % funcioines de player.erl
    io:format("=== JugadorA después de jugar cartas ===~n"),
    player:mostrar(JugadorA2),


    ok.