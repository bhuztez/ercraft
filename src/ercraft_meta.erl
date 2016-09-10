-module(ercraft_meta).

-export([start/0]).


start() ->
    {ok, _} = ercraft_meta_http_handler:start_listener(),
    {ok, _} = ercraft_meta_dns_handler:start_listener(),
    ok.
