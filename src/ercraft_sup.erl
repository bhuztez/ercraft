-module(ercraft_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    ChildSpecs =
        [
         { ercraft_dns,
           {ercraft_dns, start_link, []},
           permanent, brutal_kill, worker,
           [ercraft_dns]
         },
         { ercraft_http_listener,
           {ercraft_http_listener, start_link, []},
           permanent, brutal_kill, worker,
           [ercraft_http_listener]
         }
        ],

    {ok, {SupFlags, ChildSpecs}}.
