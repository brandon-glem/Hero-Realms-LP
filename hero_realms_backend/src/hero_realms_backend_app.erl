%%%-------------------------------------------------------------------
%% @doc hero_realms_backend public API
%% @end
%%%-------------------------------------------------------------------

-module(hero_realms_backend_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    hero_realms_backend_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
