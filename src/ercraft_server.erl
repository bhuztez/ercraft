-module(ercraft_server).

-export(
   [start/0,
    start_connection/1,
    handle_connection/1]).


start() ->
    ercraft_tcp_listener:start(
      25565,
      [binary,
       {active,false},
       {reuseaddr, true}],
      {?MODULE, start_connection, []}).

start_connection(Conn) ->
    spawn(?MODULE, handle_connection, [Conn]).


recv_length(Conn) ->
    case gen_tcp:recv(Conn, 1) of
        {ok, <<0:1, N:7>>} ->
            N;
        {ok, <<1:1, N:7>>} ->
            (recv_length(Conn) bsl 7) bor N
    end.


recv_packet(Conn) ->
    Length = recv_length(Conn),
    {ok, Bin} = gen_tcp:recv(Conn, Length),
    Bin.


handle_connection(Conn) ->
    link(Conn),
    {connected, Owner} = erlang:port_info(Conn, connected),
    Conn ! {Owner, {connect, self()}},

    {handshake,
     #{next_state := login}} =
        ercraft_c2s:decode_handshaking(
          recv_packet(Conn)),

    {login_start, Name} =
        ercraft_c2s:decode_login(
          recv_packet(Conn)),

    loop(Conn).


loop(Conn) ->
    io:format("~p~n", [recv_packet(Conn)]),
    loop(Conn).
