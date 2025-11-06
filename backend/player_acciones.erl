-module(player_acciones).
-export([jugar_carta/2]).


jugar_carta(Carta, Jugador) ->
    Efectos = maps:get(efecto, Carta, []),
    Jugador1 = lists:foldl(fun acumular_un_efecto/2, Jugador, Efectos),

    case {tiene_efecto_guild(Carta), tiene_efecto_sacrificio(Carta)} of
        {true, _} ->
            MesaP = maps:get(mesa_principal, Jugador1, []),
            maps:put(mesa_principal, [Carta | MesaP], Jugador1);
        {_, true} ->
            MesaP = maps:get(mesa_principal, Jugador1, []),
            maps:put(mesa_principal, [Carta | MesaP], Jugador1);
        _ ->
            MesaS = maps:get(mesa_secundaria, Jugador1, []),
            maps:put(mesa_secundaria, [Carta | MesaS], Jugador1)
    end.

% ! falta stun, discard/you,extraer 
acumular_un_efecto({cobrar, X}, Jugador) ->
    Ac = maps:get(acumulado, Jugador),
    maps:put(acumulado, maps:put(oro, maps:get(oro, Ac, 0) + X, Ac), Jugador);
acumular_un_efecto({atacar, X}, Jugador) ->
    Ac = maps:get(acumulado, Jugador),
    maps:put(acumulado, maps:put(ataque, maps:get(ataque, Ac, 0) + X, Ac), Jugador);
acumular_un_efecto(EfectoNoReconocido, Jugador) ->
    io:format("Efecto no reconocido: ~p~n", [EfectoNoReconocido]),
    Jugador.


tiene_efecto_guild(Carta) ->
    case maps:get(efecto_guild, Carta, []) of
        [] -> false;
        _  -> true
    end.

tiene_efecto_sacrificio(Carta) ->
    case maps:get(efecto_sacrificio, Carta, []) of
        [] -> false;
        _  -> true
    end.