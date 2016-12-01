-module(ercraft_datatype).

-export(
   [decode_n/2,
    decode_boolean/1,
    decode_byte/1,
    decode_ubyte/1,
    decode_short/1,
    decode_ushort/1,
    decode_int/1,
    decode_long/1,
    decode_float/1,
    decode_double/1,
    decode_varint/1,
    decode_varlong/1,
    decode_string/1,
    decode_position/1,
    decode_uuid/1,
    decode_nbt/1,
    decode_slot/1,
    decode_entity_metadata/1]).

-export(
   [encode_boolean/1,
    encode_byte/1,
    encode_ubyte/1,
    encode_short/1,
    encode_ushort/1,
    encode_int/1,
    encode_long/1,
    encode_float/1,
    encode_double/1,
    encode_varint/1,
    encode_varlong/1,
    encode_string/1,
    encode_position/1,
    encode_uuid/1,
    encode_nbt/1,
    encode_slot/1,
    encode_entity_metadata/1]).

decode_n(_, 0, Bin) ->
    {[], Bin};
decode_n(Fun, N, Bin) ->
    {H, Bin1} = Fun(Bin),
    {T, Bin2} = decode_n(Fun, N-1, Bin1),
    {[H|T], Bin2}.

decode_n(Fun, {N, Bin}) ->
    decode_n(Fun, N, Bin).


decode_boolean(<<1, Bin/binary>>) ->
    {true, Bin};
decode_boolean(<<0, Bin/binary>>) ->
    {false, Bin}.

decode_byte(<<N:8/signed, Bin/binary>>) ->
    {N, Bin}.

decode_ubyte(<<N:8, Bin/binary>>) ->
    {N, Bin}.

decode_short(<<N:16/signed, Bin/binary>>) ->
    {N, Bin}.

decode_ushort(<<N:16, Bin/binary>>) ->
    {N,Bin}.

decode_int(<<N:32/signed, Bin/binary>>) ->
    {N, Bin}.

decode_long(<<N:64/signed, Bin/binary>>) ->
    {N, Bin}.

decode_float(<<N:32/float, Bin/binary>>) ->
    {N, Bin}.

decode_double(<<N:64/float, Bin/binary>>) ->
    {N, Bin}.

decode_varint(<<0:1, X:7, Bin/binary>>, _) ->
    {X, Bin};
decode_varint(<<1:1, X:7, Bin/binary>>, N) when N > 1 ->
    {Y, Bin1} = decode_varint(Bin, N-1),
    {(Y bsl 7) bor X, Bin1}.

decode_varint(Bin) ->
    {X, Bin1} = decode_varint(Bin, 5),
    <<Y:32/signed>> = <<X:32>>,
    {Y, Bin1}.

decode_varlong(Bin) ->
    {X, Bin1} = decode_varint(Bin, 10),
    <<Y:64/signed>> = <<X:64>>,
    {Y, Bin1}.

decode_string(Bin) ->
    {Len, Bin1} = decode_varint(Bin),
    <<S:Len/binary, Bin2/binary>> = Bin1,
    {S, Bin2}.

decode_position(<<X:26/signed, Y:12/signed, Z:26/signed, Bin/binary>>) ->
    {{pos, X, Y, Z}, Bin}.

decode_uuid(<<N:128, Bin/binary>>) ->
    {N, Bin}.

decode_nbt(<<T, Bin/binary>>) ->
    case T of
        0 ->
            {'End', Bin};
        Type ->
            {Tag, Bin1} = decode_nbt_value(8, Bin),
            {Value, Bin2} = decode_nbt_value(Type, Bin1),
            {{Tag, Value}, Bin2}
    end.


decode_nbt_value(1, <<Byte:8/signed, Bin/binary>>) ->
    {Byte, Bin};
decode_nbt_value(2, <<Short:16/signed, Bin/binary>>) ->
    {Short, Bin};
decode_nbt_value(3, <<Int:32/signed, Bin/binary>>) ->
    {Int, Bin};
decode_nbt_value(4, <<Long:64/signed, Bin/binary>>) ->
    {Long, Bin};
decode_nbt_value(5, <<Float:32/float, Bin/binary>>) ->
    {Float, Bin};
decode_nbt_value(6, <<Double:64/float, Bin/binary>>) ->
    {Double, Bin};
decode_nbt_value(7, <<Length:32/signed, ByteArray:Length/binary, Bin/binary>>) ->
    {list_to_tuple([Byte || <<Byte:8/signed>> <= ByteArray]), Bin};
decode_nbt_value(8, <<Length:16, String:Length/binary, Bin/binary>>) ->
    {String, Bin};
decode_nbt_value(9, <<Type, N:32/signed, Bin/binary>>)
  when Type =/= 0 ->
    decode_nbt_list(Type, N, Bin);
decode_nbt_value(10, Bin) ->
    decode_nbt_compound(Bin, #{});
decode_nbt_value(11, <<Length:32/signed, Bin/binary>>) ->
    Bytes = Length * 4,
    <<IntArray:Bytes/binary, Bin1/binary>> = Bin,
    {list_to_tuple([Int || <<Int:32/signed>> <= IntArray]), Bin1}.

decode_nbt_list(_, 0, Bin) ->
    {[], Bin};
decode_nbt_list(Type, N, Bin) ->
    {H, Bin1} = decode_nbt_value(Type, Bin),
    {T, Bin2} = decode_nbt_list(Type, N-1, Bin1),
    {[H|T], Bin2}.

decode_nbt_compound(Bin, Acc) ->
    case decode_nbt(Bin) of
        {'End', Bin1} ->
            {Acc, Bin1};
        {{Tag, Value}, Bin1} ->
            decode_nbt_compound(Bin1, Acc#{Tag => Value})
    end.


decode_slot(<<(-1):16/signed, Bin/binary>>) ->
    {empty, Bin};
decode_slot(<<ID:16/signed, Count, Damage:16, 0, Bin/binary>>) ->
    {#{block_id => ID,
       item_count => Count,
       item_damage => Damage
      }, Bin};
decode_slot(<<ID:16/signed, Count, Damage:16, 1, Bin/binary>>) ->
    {NBT, Bin1} = decode_nbt(Bin),
    {#{block_id => ID,
       item_count => Count,
       damage => Damage,
       nbt => NBT
      }, Bin1}.


decode_entity_metadata(<<255, Bin/binary>>) ->
    {[], Bin};
decode_entity_metadata(<<Index, 0, Bin/binary>>) ->
    {H, Bin1} = decode_byte(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 1, Bin/binary>>) ->
    {H, Bin1} = decode_varint(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 2, Bin/binary>>) ->
    {H, Bin1} = decode_float(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 3, Bin/binary>>) ->
    {H, Bin1} = decode_string(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 4, Bin/binary>>) ->
    {H, Bin1} = decode_string(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 5, Bin/binary>>) ->
    {H, Bin1} = decode_slot(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 6, Bin/binary>>) ->
    {H, Bin1} = decode_boolean(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 7, X:32/float, Y:32/float, Z:32/float, Bin/binary>>) ->
    H = {rotation, X, Y, Z},
    {T, Bin2} = decode_entity_metadata(Bin),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 8, Bin/binary>>) ->
    {H, Bin1} = decode_position(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 9, 0, Bin/binary>>) ->
    {T, Bin2} = decode_entity_metadata(Bin),
    {[{Index}|T], Bin2};
decode_entity_metadata(<<Index, 9, 1, Bin/binary>>) ->
    {H, Bin1} = decode_position(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 10, Bin/binary>>) ->
    {H, Bin1} = decode_varint(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 11, 0, Bin/binary>>) ->
    {T, Bin2} = decode_entity_metadata(Bin),
    {[{Index}|T], Bin2};
decode_entity_metadata(<<Index, 11, 1, Bin/binary>>) ->
    {H, Bin1} = decode_uuid(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2};
decode_entity_metadata(<<Index, 12, Bin/binary>>) ->
    {H, Bin1} = decode_varint(Bin),
    {T, Bin2} = decode_entity_metadata(Bin1),
    {[{Index,H}|T], Bin2}.


encode_boolean(true) ->
    [1];
encode_boolean(false) ->
    [0].

encode_byte(N) ->
    <<N:8/signed>>.

encode_ubyte(N) ->
    <<N:8>>.

encode_short(N) ->
    <<N:16/signed>>.

encode_ushort(N) ->
    <<N:16>>.

encode_int(N) ->
    <<N:32/signed>>.

encode_long(N) ->
    <<N:64/signed>>.

encode_float(N) ->
    <<N:32/float>>.

encode_double(N) ->
    <<N:64/float>>.

encode_vint(N)
  when N < 16#80 ->
    [N];
encode_vint(N) ->
    [((N band 16#7f) bor 16#80) | encode_vint(N bsr 7)].

encode_varint(N) ->
    <<X:32>> = <<N:32/signed>>,
    encode_vint(X).

encode_varlong(N) ->
    <<X:64>> = <<N:64/signed>>,
    encode_vint(X).

encode_string(S) ->
    [encode_varint(iolist_size(S)), S].

encode_position({pos, X, Y, Z}) ->
    <<X:26/signed, Y:12/signed, Z:26/signed>>.

encode_uuid(N) ->
    <<N:128>>.

encode_nbt(_) ->
    ok.

encode_slot(empty) ->
    <<(-1):16/signed>>;
encode_slot(#{block_id := ID, item_count := Count, item_damage := Damage, nbt := NBT}) ->
    [<<ID:16/signed, Count, Damage:16, 1>>, encode_nbt(NBT)];
encode_slot(#{block_id := ID, item_count := Count, item_damage := Damage}) ->
    <<ID:16/signed, Count, Damage:16, 0>>.

encode_entity_metadata(_) ->
    ok.
