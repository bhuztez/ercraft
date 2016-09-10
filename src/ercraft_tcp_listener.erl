-module(ercraft_tcp_listener).

-export([start/3, init/3, loop/3]).


start(Port, Options, Handler) ->
    proc_lib:start(?MODULE, init, [Port, Options, Handler]).


init(Port, Options, Handler) ->
    {ok, Socket} = gen_tcp:listen(Port, Options),
    {ok, Ref} = prim_inet:async_accept(Socket, -1),
    proc_lib:init_ack({ok, self()}),
    ?MODULE:loop(Ref, Socket, Handler).


loop(Ref, Socket, Handler = {M,F,A}) ->
    receive
        {inet_async, Socket, Ref, {ok, Conn}} ->
            true = inet_db:register_socket(Conn, inet_tcp),
            {ok, Opts} = prim_inet:getopts(Socket, [active, nodelay, keepalive, delay_send, priority, tos]),
            ok = prim_inet:setopts(Conn, Opts),
            apply(M, F, [Conn|A]);
        Msg ->
            io:format("tcp unknown msg: ~p: ~p~n.", [self(), Msg])
    end,
    {ok, NewRef} = prim_inet:async_accept(Socket, -1),
    ?MODULE:loop(NewRef, Socket, Handler).
