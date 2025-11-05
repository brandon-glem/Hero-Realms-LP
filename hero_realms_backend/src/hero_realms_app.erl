-module(hero_realms_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    % 1. Arranca el supervisor (que a su vez arrancará el Matchmaker)
    hero_realms_backend_sup:start_link(),

    % 2. Define y arranca el servidor web (Cowboy)
    Dispatch = cowboy_router:compile([
        %% Ruta para WebSockets: '/ws' se mapea a nuestro Connection Handler
        {"/", [
            {"ws", cowboy_websocket, {upgrade, {protocol, hero_realms_ws_handler, []}}}
        ]}
    ]),
    
    cowboy:start_http(http_listener, 100, 
                      [{port, 8080}], 
                      [{env, [{dispatch, Dispatch}]}]),

    {ok, self()}.

stop(_State) ->
    ok.