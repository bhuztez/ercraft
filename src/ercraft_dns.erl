-module(ercraft_dns).

-behaviour(supervisor_bridge).

-export([start_link/0]).

-export([init/1, terminate/2]).

-export([loop/1]).


start_link() ->
    supervisor_bridge:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    {ok, Socket} =
        gen_udp:open(
          53,
          [binary,
           {active, false},
           {ip, {127,0,0,1}},
           {reuseaddr, true}]),
    Pid = spawn_link(?MODULE, loop, [Socket]),
    ok = gen_udp:controlling_process(Socket, Pid),
    {ok, Pid, Socket}.


terminate(_Reason, _State) ->
    ok.


loop(Socket) ->
    {ok, {Addr, Port, Packet}} = gen_udp:recv(Socket, 1024),
    case handle_query(Packet) of
        noreply ->
            ok;
        {reply, Data} ->
            gen_udp:send(Socket, Addr, Port, Data)
    end,
    loop(Socket).


handle_query(
  <<ID:16, 0:1, 0:4,
    _AA:1, 0:1, 1:1, _RA:1, 0:1, _AD:1, _CD:1,
    _RCODE:4, 1:16, 0:16, 0:16, 0:16,
    Data/binary>>) ->
    case parse_question(Data) of
        {Name, 1, 1} ->
            {reply, format_response(ID, Name)};
        {Name, 28, 1} ->
            {reply, format_empty_response(ID, Name)};
        _ ->
            noreply
    end;
handle_query(Packet) ->
    io:format("DNS: ~p~n", [Packet]),
    noreply.


parse_name(<<0, Rest/binary>>) ->
    {[], Rest};
parse_name(<<L, Rest/binary>>) ->
    Label = binary:part(Rest, 0, L),
    {Labels, Rest1} =
        parse_name(binary:part(Rest, L, byte_size(Rest)-L)),
    {[Label|Labels], Rest1}.


parse_question(Data) ->
    {Name, <<Type:16,Class:16, _/binary>>} = parse_name(Data),
    {Name, Type, Class}.


format_name([]) ->
    <<0>>;
format_name([H|T]) ->
    <<(byte_size(H)), H/binary, (format_name(T))/binary>>.


format_response(ID, Name) ->
    Header =
        <<ID:16, 1:1, 0:4, 0:1, 0:1, 1:1, 1:1, 0:1, 0:1, 0:1,
          0:4, 1:16, 1:16, 0:16, 0:16>>,
    QName = format_name(Name),
    Question =
        <<QName/binary, 1:16, 1:16>>,
    Answer =
        <<QName/binary, 1:16, 1:16, 3600:32,
          4:16, 127, 0, 0, 1>>,
    [Header, Question, Answer].


format_empty_response(ID, Name) ->
    Header =
        <<ID:16, 1:1, 0:4, 0:1, 0:1, 1:1, 1:1, 0:1, 0:1, 0:1,
          0:4, 1:16, 0:16, 0:16, 0:16>>,
    QName = format_name(Name),
    Question =
        <<QName/binary, 28:16, 1:16>>,
    [Header, Question].
