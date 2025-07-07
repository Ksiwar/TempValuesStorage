-module(temp_storage).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    temp_storage_sup:start_link().

stop(_State) ->
    ok.