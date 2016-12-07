-module(ercraft_transport).

-behaviour(gen_server).

-export(
   [connect/2, accept/1, send/2, recv/1, set_shared_secret/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

send(Pid, Bin) ->
    gen_server:call(Pid, {send, Bin}, infinity).

recv(Pid) ->
    gen_server:call(Pid, recv).

set_shared_secret(Pid, SharedSecret) ->
    gen_server:call(Pid, {set_shared_secret, SharedSecret}).


connect(Address, Port) ->
    case gen_tcp:connect(Address, Port, [binary, {active, false}]) of
        {ok, Socket} ->
            case start_link(self(), Socket) of
                {ok, Pid} ->
                    receive
                        {Socket, connected} ->
                            ok
                    end,
                    {ok, Pid};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

accept(Socket) ->
    start_link(self(), Socket).


start_link(Parent, Socket) ->
    gen_server:start_link(?MODULE, [Parent, Socket], []).


init([Parent, Socket]) ->
    link(Socket),
    {connected, Owner} = erlang:port_info(Socket, connected),
    Socket ! {Owner, {connect, self()}},

    {ok, Receiver} = ercraft_transport_receiver:start_link(self(), Parent, Socket),

    {ok,
     #{parent => Parent,
       socket => Socket,
       receiver => Receiver}}.


handle_call({send, Bin}, _From, State = #{socket := Socket, key := Key, iv := IV}) ->
    {CipherText, IV1} = ercraft_cipher:encrypt(Key, IV, add_length(Bin)),
    Reply = gen_tcp:send(Socket, CipherText),
    {reply, Reply, State#{iv := IV1}};
handle_call({send, Bin}, _From, State = #{socket := Socket}) ->
    Reply = gen_tcp:send(Socket, add_length(Bin)),
    {reply, Reply, State};
handle_call({set_shared_secret, SharedSecret}, _From, State = #{receiver := Receiver}) ->
    gen_server:call(Receiver, {set_shared_secret, SharedSecret}),
    {reply, ok, State#{key => SharedSecret, iv => SharedSecret}};
handle_call(recv, _From, State = #{receiver := Receiver}) ->
    gen_server:call(Receiver, recv),
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


add_length(Bin) ->
    Length = iolist_size(Bin),
    [ercraft_datatype:encode_varint(Length), Bin].
