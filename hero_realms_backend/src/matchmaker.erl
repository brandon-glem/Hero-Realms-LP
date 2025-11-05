-module(matchmaker).
-behaviour(gen_server).

-export([start_link/0, new_player/1, end_turn_request/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {waiting_player = none, active_games = #{} }).

%%% API
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

new_player(PlayerPID) ->
    gen_server:cast(?MODULE, {new_player, PlayerPID}).

end_turn_request(PlayerPID) ->
    gen_server:cast(?MODULE, {end_turn, PlayerPID}).

%%% Callbacks
init([]) ->
    {ok, #state{}}.

handle_cast({new_player, NewPlayerPID}, #state{waiting_player = none} = State) ->
    % Pone al primer jugador en espera
    io:format("DEBUG: Jugador ~p en espera.~n", [NewPlayerPID]),
    {noreply, State#state{waiting_player = NewPlayerPID}};

handle_cast({new_player, PlayerB_PID}, #state{waiting_player = PlayerA_PID} = State) ->
    % Empareja al segundo jugador y crea una partida
    io:format("DEBUG: Partida encontrada. Creando Game Room...~n", []),
    
    % NOTA: En un proyecto real, usarías un supervisor para arrancar game_room
    {ok, GamePID} = game_room:start_link(PlayerA_PID, PlayerB_PID),
    
    % Registra la partida y a qué GamePID corresponde cada ConnectionHandler PID
    NewGames = maps:put(PlayerA_PID, GamePID, maps:put(PlayerB_PID, GamePID, State#state.active_games)),

    {noreply, State#state{waiting_player = none, active_games = NewGames}};

handle_cast({end_turn, PlayerPID}, #state{active_games = Games} = State) ->
    case maps:get(PlayerPID, Games, none) of
        GamePID when is_pid(GamePID) ->
            % Reenvía la acción al proceso Game Room
            game_room:end_turn(GamePID);
        _ ->
            io:format("ERROR: Jugador ~p no tiene partida activa.~n", [PlayerPID])
    end,
    {noreply, State}.

handle_call(_Request, _From, State) -> {reply, ok, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.