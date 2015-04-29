-module(ercraft_nbt).

-export([parse_file/1]).


parse_file(Filename) ->
    {ok, File} = file:open(Filename, [read, binary]),
    try
        parse_tagged_value(File)
    after
        file:close(File)
    end.


type(<<0>>)  -> 'End';
type(<<1>>)  -> 'Byte';
type(<<2>>)  -> 'Short';
type(<<3>>)  -> 'Int';
type(<<4>>)  -> 'Long';
type(<<5>>)  -> 'Float';
type(<<6>>)  -> 'Double';
type(<<7>>)  -> 'ByteArray';
type(<<8>>)  -> 'String';
type(<<9>>)  -> 'List';
type(<<10>>) -> 'Compound';
type(<<11>>) -> 'IntArray'.


parse_tagged_value(File) ->
    {ok, Tag} = file:read(File, 1),
    case type(Tag) of
        'End' ->
            'End';
        Type ->
            {parse_tag(File), parse_value_payload(File, Type)}
    end.


parse_tag(File) ->
    parse_value_payload(File, 'String').


parse_value_payload(File, 'Byte') ->
    {ok, <<Byte:8/signed>>} = file:read(File, 1),
    Byte;
parse_value_payload(File, 'Short') ->
    {ok, <<Short:16/signed>>} = file:read(File, 2),
    Short;
parse_value_payload(File, 'Int') ->
    {ok, <<Int:32/signed>>} = file:read(File, 4),
    Int;
parse_value_payload(File, 'Long') ->
    {ok, <<Long:64/signed>>} = file:read(File, 8),
    Long;
parse_value_payload(File, 'Float') ->
    {ok, <<Float:32/float>>} = file:read(File, 4),
    Float;
parse_value_payload(File, 'Double') ->
    {ok, <<Double:64/float>>} = file:read(File, 8),
    Double;
parse_value_payload(File, 'ByteArray') ->
    {ok, <<Length:32/signed>>} = file:read(File, 4),
    {ok, ByteArray} = file:read(File, Length),
    list_to_tuple([Byte || <<Byte:8/signed>> <= ByteArray]);
parse_value_payload(File, 'String') ->
    {ok, <<Length:16/unsigned>>} = file:read(File, 2),
    {ok, String} = file:read(File, Length),
    String;
parse_value_payload(File, 'List') ->
    {ok, Tag} = file:read(File, 1),
    {ok, <<N:32/signed>>} = file:read(File, 4),
    case type(Tag) of
        Type when Type =/= 'End' ->
            parse_n_value_payload(File, Type, N)
    end;
parse_value_payload(File, 'Compound') ->
    parse_compound_value(File);
parse_value_payload(File, 'IntArray') ->
    {ok, <<Length:32/signed>>} = file:read(File, 4),
    {ok, ByteArray} = file:read(File, 4*Length),
    list_to_tuple([Int || <<Int:32/signed>> <= ByteArray]).


parse_compound_value(File) ->
    case parse_tagged_value(File) of
        'End' ->
            [];
        Value ->
            io:format("~p~n", [Value]),
            [Value|parse_compound_value(File)]
    end.


parse_n_value_payload(_File, _Type, 0) ->
    [];
parse_n_value_payload(File, Type, N) ->
    [parse_value_payload(File, Type)|parse_n_value_payload(File, Type, N-1)].
