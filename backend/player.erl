-module(player).
-export([crear/1, mostrar/1]).

crear(Nombre) ->
    #{
        nombre => Nombre,
        vida => 50,
        mano => [],
        mesaP => [],
        mesaS => [],
        baraja => [],
        descarte => [],
        debuffs => [],

        acumulado => #{oro => 0, ataque => 0} 
    }.

mostrar(Jugador) ->
    Ac = maps:get(acumulado, Jugador),
    io:format("~s -> Vida: ~p, Oro acumulado: ~p, Ataque acumulado: ~p~n",
        [maps:get(nombre, Jugador),
        maps:get(vida, Jugador),
        maps:get(oro, Ac),      
        maps:get(ataque, Ac)]).