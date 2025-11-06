-module(efectos).
-export([aplicar/3]).

aplicar([], Jugador, Oponente) -> {Jugador, Oponente};
aplicar([{atacar, N} | resto], Jugador, Oponente) ->
    Oponente2 = Oponente#{vida := Oponente#{vida} - N},
    aplicar(resto, Jugador, Oponente2);
aplicar([{cobrar, N} | resto], Jugador, Oponente) ->
    Jugador2 = Jugador#{oro := Jugador#{oro} + N},
    aplicar(resto, Jugador2, Oponente);
aplicar([_ | resto], Jugador, Oponente) ->
    aplicar(resto, Jugador, Oponente).





aplicar_efectos(Carta, Jugador, Oponente) ->
    aplicar_efectos_lista(Carta#{efecto}, Jugador, Oponente).

aplicar_efectos_sacrificio(Carta, Jugador, Oponente) ->
    aplicar_efectos_lista(Carta#{efecto_sacrificio}, Jugador, Oponente).

aplicar_efectos_guild(Carta, Jugador, Oponente) ->
    case hay_guild_compatible(Carta, Jugador) of
        true -> aplicar_efectos_lista(Carta#{efecto_guild}, Jugador, Oponente);
        false -> {Jugador, Oponente}
    end.


% ! falta stun, discard/you,extraer 
aplicar_efectos_lista([], Jugador, Oponente) ->
    {Jugador, Oponente};

aplicar_efectos_lista([Efecto | Resto], Jugador, Oponente) ->
    case Efecto of
        {atacar, X} ->
            Ac = maps:get(acumulado, Jugador),
            Ac2 = maps:put(ataque, maps:get(ataque, Ac, 0) + X, Ac),
            Jugador2 = maps:put(acumulado, Ac2, Jugador),
            aplicar_efectos_lista(Resto, Jugador2, Oponente);

        {cobrar, Y} ->
            Ac = maps:get(acumulado, Jugador),
            Ac2 = maps:put(oro, maps:get(oro, Ac, 0) + Y, Ac),
            Jugador2 = maps:put(acumulado, Ac2, Jugador),
            aplicar_efectos_lista(Resto, Jugador2, Oponente);

        _ ->
            io:format("⚠ Efecto no reconocido: ~p~n", [Efecto]),
            aplicar_efectos_lista(Resto, Jugador, Oponente)
    end.