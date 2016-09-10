-module(ercraft_udp_listener).

-export([start/3, init/3, loop/2]).


start(Port, Options, Handler) ->
    proc_lib:start(?MODULE, init, [Port, Options, Handler]).


init(Port, Options, Handler) ->
    {ok, Socket} = gen_udp:open(Port, Options),
    inet:setopts(Socket, [{active, true}]),
    proc_lib:init_ack({ok, self()}),
    ?MODULE:loop(Socket, Handler).


loop(Socket, Handler = {M,F,A}) ->
    receive
        {udp, Socket, Addr, Port, Packet} ->
            apply(M, F, [Socket,Addr,Port,Packet|A]);
        Msg ->
            io:format("udp unknown msg: ~p: ~p~n.", [self(), Msg])
    end,
    ?MODULE:loop(Socket, Handler).
