-module(cartas).
-export([todas/0, obtener/1, nombre_carta/1]).
todas() ->
    [   % ! no se creo el guardia, efecto compra y arriba de la baraja tanto action como carta cualquiera,
        % ! falta los ataques de guildd y sacrificio
        % * cobrar obtiene dinero, atacar quita vida, curar recupera vida, stun descarta carta en mesa rival, extraer saca carta de bajara
        %100 blue
        %sacrificable,sacar una carta mas y stun heroe. descarta despues de jugar
        #{id => 101,image=>"b1.png", nombre => "fire bomb", tipo => action, costo => 8, ataque => 8,ataque_sacrificio=>5, efecto => [{atacar, 8}, {stun, 1}]},
        % 2 de oro y si otra carta azul  4 ataque
        #{id => 102,image=>"b2.png", nombre => "profit", tipo => action, costo => 1, oro => 2,ataque_guild=>4, efecto => [{cobrar, 2}]},
        %stun y ataque 4
        #{id => 103,image=>"b3.png", nombre => "rake, master assasing", tipo => heroe, costo => 7,vida=>7,ataque=>4, efecto => [{stun, 1},{atacar, 4}]},
        %guardia
        #{id => 104,image=>"b4.png", nombre => "parov, the enforcer", tipo => heroe, costo => 5,vida=>5,ataque=>3, efecto => [{extraer, 1},{atacar, 4}]},
        % falta el efecto de compra y la carta sale en la siguiente ronda
        #{id => 105,image=>"b5.png", nombre => "bribe", tipo => action, costo => 3, efecto => [{cobrar, 3}]},
        %falta efecto de compra y poner la carta arriba de la baraja
        #{id => 106,image=>"b6.png", nombre => "rasmus, the smuggler", tipo => heroe, costo => 4,vida=>5, efecto => [{cobrar, 2}]},
        %guardia
        #{id => 107,image=>"b7.png", nombre => "myros, guild mage", tipo => heroe, costo => 5,vida=>3,ataque_guild=>4, efecto => [{cobrar, 3}]},
        %guild oro falta
        #{id => 108,image=>"b8.png", nombre => "intimidation", tipo => action, costo => 2,oro=>2,oro_guild=>2,ataque=>5, efecto => [{atacar, 5}]},
        %guild stun falta
        #{id => 109,image=>"b9.png", nombre => "death threat", tipo => action, costo => 3, efecto => [{extraer, 1},{atacar, 1}]},
        % se debe ver como hacer para escoger 1 de oro o 2 de ataque
        #{id => 110,image=>"b10.png", nombre => "street thug", tipo => heroe, costo => 3,vida=>4,ataque=>2,oro=>1, efecto => []},
        % guardia
        #{id => 111,image=>"b11.png", nombre => "borg, ogre mercenary", tipo => heroe, costo => 6,vida=>6,ataque=>4, efecto => [{atacar, 4}]},
        % guild stun falta
        #{id => 112,image=>"b12.png", nombre => "hit job", tipo => action, costo => 4,ataque=>7, efecto => [{atacar, 7}]},
        % falta poner una carta de descarte a tu baraja
        #{id => 113,image=>"b13.png", nombre => "smash and grab", tipo =>action , costo => 6,ataque=>6, efecto => [{atacar, 6}]},
        % guild poner la carta que compras en tu mano
        #{id => 114,image=>"b14.png", nombre => "deception", tipo =>action , costo => 5,oro=>2, efecto => [{cobrar, 2}, {extraer, 1}]},
        %200 verde
        % guardia
        #{id => 201,image=>"g1.png", nombre => "torgen, rocksplitter", tipo =>heroe , costo => 7,vida=>7,ataque=>4, efecto => [{atacar, 4}, {discard, 1}]},
        % guild discard falta 
        #{id => 202,image=>"g2.png", nombre => "nature's bounty", tipo =>action , costo => 4,oro=>4,efecto_guild=>discard, efecto => [{cobrar, 4}]},
        % guild ataque falta 
        #{id => 203,image=>"g3.png", nombre => "elven gift", tipo =>action , costo => 2,ataque_guild=>4, efecto => [{cobrar, 2}, {discardyou, 1}]},
        %guild ataque
        #{id => 204,image=>"g4.png", nombre => "spark", tipo => action, costo => 1,vida=>3,ataque_guild=>2, efecto => [{atacar, 1}, {discard, 1}]},
        %guild extraer carta
        #{id => 205,image=>"g5.png", nombre => "orc grunt", tipo =>heroe , costo => 3,vida=>3, efecto => [{atacar, 2}]},
        % extraer y discar una carta tanto personaje y guild
        #{id => 206,image=>"g6.png", nombre => "grak, strom giant", tipo =>heroe , costo => 8,vida=>7,ataque=>6, efecto => [{atacar, 6}]},
        % guild discard card
        #{id => 207,image=>"g7.png", nombre => "broelyn, loreweaver", tipo =>heroe , costo => 5,vida=>6,oro=>2, efecto => [{cobrar, 2}]},
        % guild extraer carta
        #{id => 208,image=>"g8.png", nombre => "cron, the berseker", tipo =>heroe , costo => 6,vida=>6, efecto => [{atacar, 5}]},
        % guild 1 de ataque por cada de guild
        #{id => 209,image=>"g9.png", nombre => "wolf shaman", tipo =>heroe , costo => 2,vida=>4, efecto => [{atacar, 2}]},
        % guild 4 de ataque 
        #{id => 210,image=>"g10.png", nombre => "dire wolf", tipo =>heroe , costo => 5,vida=>5, efecto => [{atacar, 3}]},
        % 
        #{id => 211,image=>"g11.png", nombre => "rampage", tipo =>action , costo => 6, efecto => [{atacar, 5},{extraer,2} ,{discardyou, 2}]},
        % sacrifica y descarta otra carta el openente 
        #{id => 212,image=>"g12.png", nombre => "wolf form", tipo =>action , costo => 5, efecto => [{atacar, 8},{discard, 1}]},
        %guild 3 mas de ataque
        #{id => 213,image=>"g13.png", nombre => "elven curse", tipo =>action , costo => 3, efecto => [{atacar, 6}]},
        %500 primarios
        #{id => 501,image=>"primarydagger1.png", nombre => "dagger", tipo =>action , costo => 0, efecto => [{atacar, 1}]},
        #{id => 502,image=>"primarydagger2.png", nombre => "shortsword", tipo =>action , costo => 0, efecto => [{atacar, 2}]},
        #{id => 503,image=>"primarygold1.png", nombre => "gold", tipo =>action , costo => 0, efecto => [{cobrar, 1}]},    
        #{id => 504,image=>"primarygold2.png", nombre => "ruby", tipo =>action , costo => 0, efecto => [{cobrar, 2}]},
        % sacrificio te da  3 ataque
        #{id => 505,image=>"primarygoldattack.png", nombre => "fire gem", tipo =>action , costo => 2, efecto => [{cobrar, 2}]},
        #{id => 506,image=>"primaryplayer1.png", nombre => "primaryplayer1", tipo =>player },
        #{id => 507,image=>"primaryplayer2.png", nombre => "primaryplayer2", tipo =>player }
].

obtener(Id) ->
    case [C || C <- todas(), maps:get(id, C) =:= Id] of
        [Carta] -> Carta;
        [] -> undefined
    end.

nombre_carta(Id) ->
    case [C || C <- todas(), maps:get(id, C) =:= Id] of
        [Carta] -> maps:get(nombre, Carta);
        [] -> "Carta no encontrada"
    end.