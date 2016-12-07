-module(ercraft_packet).

-export([encode/3, encode/4, decode/3, decode/4]).


encode(Packet, Mode, State) ->
    iolist_to_binary(do_encode(Packet, Mode, State)).

encode(Packet, Mode, State, Threshold) ->
    compress(do_encode(Packet, Mode, State), Threshold).

decode(Bin, Mode, State) ->
    do_decode(Bin, Mode, State).

decode(Bin, Mode, State, _Threshold) ->
    do_decode(uncompress(Bin), Mode, State).


do_encode(Packet, client, handshaking) ->
    ercraft_c2s:encode_handshaking(Packet);
do_encode(Packet, client, login) ->
    ercraft_c2s:encode_login(Packet);
do_encode(Packet, client, play) ->
    ercraft_c2s:encode_play(Packet);
do_encode(Packet, server, handshaking) ->
    ercraft_s2c:encode_handshaking(Packet);
do_encode(Packet, server, login) ->
    ercraft_s2c:encode_login(Packet);
do_encode(Packet, server, play) ->
    ercraft_s2c:encode_play(Packet).

do_decode(Packet, client, handshaking) ->
    ercraft_s2c:decode_handshaking(Packet);
do_decode(Packet, client, login) ->
    ercraft_s2c:decode_login(Packet);
do_decode(Packet, client, play) ->
    ercraft_s2c:decode_play(Packet);
do_decode(Packet, server, handshaking) ->
    ercraft_c2s:decode_handshaking(Packet);
do_decode(Packet, server, login) ->
    ercraft_c2s:decode_login(Packet);
do_decode(Packet, server, play) ->
    ercraft_c2s:decode_play(Packet).

compress(IOList, Threshold)
  when Threshold < 0 ->
    iolist_to_binary([0,IOList]);
compress(IOList, Threshold) ->
    case iolist_size(IOList) =< Threshold of
        true ->
            iolist_to_binary([0, IOList]);
        false ->
            Bin = zlib:compress(IOList),
            [ercraft_datatype:encode_varint(byte_size(Bin)),
             Bin]
    end.

uncompress(<<0, Bin/binary>>) ->
    Bin;
uncompress(Bin) ->
    {Length, Bin1} = ercraft_datatype:decode_varint(Bin),
    <<Bin2:Length/binary>> = zlib:uncompress(Bin1),
    Bin2.
