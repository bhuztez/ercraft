-module(ercraft_proxy).

-behaviour(gen_server).

-export([start/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([start_connection/3]).


start() ->
    start(25566, {127,0,0,1}, 25565).

start(ListenPort, Address, Port) ->
    ercraft_tcp_listener:start(
      ListenPort,
      [binary,
       {active,false},
       {reuseaddr, true}],
      {?MODULE, start_connection, [Address, Port]}
     ).

start_connection(Socket, Address, Port) ->
    spawn(gen_server, start, [?MODULE, [Socket, Address, Port], []]).

init([Socket, Address, Port]) ->
    case ercraft_transport:accept(Socket) of
        {ok, Client} ->
            case ercraft_transport:connect(Address, Port) of
                {ok, Server} ->
                    ercraft_transport:recv(Client),
                    ercraft_transport:recv(Server),
                    {ok,
                     #{server => Server,
                       client => Client,
                       state => handshaking,
                       compression => none}};
                Error ->
                    {stop, Error}
            end;
        Error ->
            {stop, Error}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ercraft, Client, Bin}, State = #{client := Client, server := Server, state := StateName, compression := Threshold}) ->
    ercraft_transport:recv(Client),
    ercraft_transport:send(Server, Bin),
    Packet = ercraft_packet:decode(Bin, server, StateName, Threshold),
    io:format("C->S: ~p~n", [Packet]),
    case Packet of
        {handshake, #{next_state := login}} ->
            {noreply, State#{state := login}};
        _ ->
            {noreply, State}
    end;
handle_info({ercraft, Server, Bin}, State = #{client := Client, server := Server, state := StateName, compression := Threshold}) ->
    ercraft_transport:recv(Server),
    ercraft_transport:send(Client, Bin),
    Packet = ercraft_packet:decode(Bin, client, StateName, Threshold),
    case Packet of
        {chunk_data, Map} ->
            io:format("chunk: ~p~n", [maps:remove(data, Map)]);
        _ ->
            io:format("S->C: ~p~n", [Packet])
    end,
    case Packet of
        {set_compression, #{threshold := Threshold1}} ->
            {noreply, State#{compression := Threshold1}};
        {login_success, _} ->
            {noreply, State#{state := play}};
        _ ->
            {noreply, State}
    end.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
