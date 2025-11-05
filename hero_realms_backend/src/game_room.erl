-module(game_room).
-behaviour(gen_server).

%% API externa
-export([start_link/2, end_turn/1]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Registros de estado (simplificado)
-record(player_state, {
    pid,         % PID del Connection Handler (para enviar mensajes)
    id,          % Identificador (e.g., 'jugador_a', 'jugador_b')
    vida = 50
}).

-record(state, {
    jugadores,   % Mapa: ID -> #player_state{}
    turno_actual % ID del jugador que tiene el turno
}).

%%% API
start_link(PlayerA_PID, PlayerB_PID) ->
    gen_server:start_link({local, game_room}, ?MODULE, [PlayerA_PID, PlayerB_PID], []).

end_turn(GamePID) ->
    gen_server:cast(GamePID, end_turn).

%%% Callbacks
init([PlayerA_PID, PlayerB_PID]) ->
    PlayerA_ID = list_to_atom("PlayerA"),
    PlayerB_ID = list_to_atom("PlayerB"),

    PlayerA = #player_state{pid = PlayerA_PID, id = PlayerA_ID},
    PlayerB = #player_state{pid = PlayerB_PID, id = PlayerB_ID},

    InitialState = #state{
        jugadores = #{
            PlayerA_ID => PlayerA,
            PlayerB_ID => PlayerB
        },
        turno_actual = PlayerA_ID
    },
    
    % Notificar el estado inicial a ambos jugadores
    notificar_estado(InitialState),
    
    {ok, InitialState}.

% Maneja el fin de turno (solicitado desde el cliente Python)
handle_cast(end_turn, #state{turno_actual = CurrentID, jugadores = Jugs} = State) ->
    % Lógica: Cambiar el turno al otro jugador
    NextID = case CurrentID of
        'PlayerA' -> 'PlayerB';
        'PlayerB' -> 'PlayerA'
    end,

    NewState = State#state{turno_actual = NextID},
    
    % Notifica el nuevo estado a los clientes conectados
    notificar_estado(NewState),
    
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

% Función auxiliar para notificar a los clientes
notificar_estado(#state{jugadores = Jugs, turno_actual = TurnoID} = _State) ->
    % Simulación de notificación del estado del juego
    io:format("DEBUG: Nuevo estado, Turno de ~p~n", [TurnoID]),
    
    % Mensaje a ser enviado al cliente Python
    Message = {game_state_update, TurnoID}, 
    
    % Envía el mensaje al PID de conexión de cada jugador
    maps:map(fun(_ID, PState) -> PState#player_state.pid ! Message end, Jugs),
    
    ok.

% Mantenemos los otros callbacks mínimos
handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.