-module(ercraft_packet).

-export(
   [recv/1, send/2,
    compress/2, decompress/1]).


recv_length(Conn) ->
    case gen_tcp:recv(Conn, 1) of
        {ok, <<0:1, N:7>>} ->
            N;
        {ok, <<1:1, N:7>>} ->
            (recv_length(Conn) bsl 7) bor N
    end.


recv(Conn) ->
    Length = recv_length(Conn),
    {ok, Bin} = gen_tcp:recv(Conn, Length),
    Bin.


send(Conn, Bin) ->
    Length = iolist_size(Bin),
    gen_tcp:send(
      Conn,
      ercraft_datatype:encode_varint(Length)),
    gen_tcp:send(Conn, Bin).


compress(IOList, -1) ->
    [0, IOList];
compress(IOList, Threshold) ->
    case iolist_size(IOList) =< Threshold of
        true ->
            [0, IOList];
        false ->
            Bin = zlib:compress(IOList),
            [ercraft_datatype:encode_varint(byte_size(Bin)),
             Bin]
    end.

decompress(<<0, Bin/binary>>) ->
    Bin;
decompress(Bin) ->
    {Length, Bin1} = ercraft_datatype:decode_varint(Bin),
    <<Bin2:Length/binary>> = zlib:uncompress(Bin1),
    Bin2.
