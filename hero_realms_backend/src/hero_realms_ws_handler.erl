-module(hero_realms_ws_handler).
-behaviour(cowboy_websocket).

-export([init/2, websocket_init/1, websocket_handle/2, websocket_info/2, terminate/3]).

%% Registra la conexión en el supervisor para poder enviarle mensajes
init(Req, Opts) ->
    {upgrade, protocol, cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    % Obtiene el PID de este proceso para identificar la conexión
    MyPID = self(),
    io:format("DEBUG: Nuevo cliente conectado con PID ~p~n", [MyPID]),
    
    % Le dice al Matchmaker que hay un nuevo jugador disponible
    matchmaker:new_player(MyPID),
    
    {ok, State}.

%% Maneja los mensajes entrantes del cliente Python (JSON o texto plano)
websocket_handle({text, <<"END_TURN">>}, State) ->
    % Reenvía la acción al proceso Game Room que maneja esta conexión
    matchmaker:end_turn_request(self()),
    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.

%% Maneja los mensajes de Erlang (ej. desde game_room)
websocket_info({game_state_update, TurnoID}, State) ->
    % Convierte el estado de Erlang a un formato que Python pueda entender
    Message = io_lib:format("{\"action\":\"update\",\"turn\":\"~p\"}", [TurnoID]),
    {reply, {text, list_to_binary(Message)}, State};

websocket_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ok.