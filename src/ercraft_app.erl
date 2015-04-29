-module(ercraft_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

start() ->
    application:ensure_all_started(ercraft).

start(_StartType, _StartArgs) ->
    ercraft_sup:start_link().

stop(_State) ->
    ok.
