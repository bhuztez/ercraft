-module(ercraft_transport_receiver).

-behaviour(gen_server).

-export([start_link/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Parent, Owner, Socket) ->
    gen_server:start_link(?MODULE, [Parent, Owner, Socket], []).

init([Parent, Owner, Socket]) ->
    {ok,
     #{parent => Parent,
       owner => Owner,
       socket => Socket}
    }.

handle_call({set_shared_secret, SharedSecret}, _From, State) ->
    {reply, ok, State#{key => SharedSecret, iv => SharedSecret}};
handle_call(recv, From, State = #{parent := Parent, owner := Owner, socket := Socket, key := Key, iv := IV}) ->
    gen_server:reply(From, ok),
    case cipher_recv(Socket, Key, IV) of
        {{ok, Bin}, IV1} ->
            Owner ! {ercraft, Parent, Bin},
            {noreply, State#{iv := IV1}};
        {{error, closed}, IV1} ->
            Owner ! {ercraft, Parent, closed},
            {stop, closed, State#{iv := IV1}};
        {Error, IV1} ->
            Owner ! {ercraft, Parent, Error},
            {stop, Error, State#{iv := IV1}}
    end;
handle_call(recv, From, State = #{parent := Parent, owner := Owner, socket := Socket}) ->
    gen_server:reply(From, ok),
    case recv(Socket) of
        {ok, Bin} ->
            Owner ! {ercraft, Parent, Bin},
            {noreply, State};
        {error, closed} ->
            Owner ! {ercraft, Parent, closed},
            {stop, closed, State};
        Error ->
            Owner ! {ercraft, Parent, Error},
            {stop, Error, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


recv_length(Socket) ->
    case gen_tcp:recv(Socket, 1) of
        {ok, <<0:1, N:7>>} ->
            {ok, N};
        {ok, <<1:1, N:7>>} ->
            case recv_length(Socket) of
                {ok, L} ->
                    {ok, (L bsl 7) bor N};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


recv(Socket) ->
    case recv_length(Socket) of
        {ok, Length} ->
            gen_tcp:recv(Socket, Length);
        Error ->
            Error
    end.



cipher_recv_length(Socket, Key, IV) ->
    case gen_tcp:recv(Socket, 1) of
        {ok, Byte} ->
            case ercraft_cipher:decrypt(Key, IV, Byte) of
                {<<0:1, N:7>>, IV1} ->
                    {{ok, N}, IV1};
                {<<1:1, N:7>>, IV1} ->
                    case cipher_recv_length(Socket, Key, IV1) of
                        {{ok, L}, IV2} ->
                            {{ok, (L bsl 7) bor N}, IV2};
                        Error ->
                            Error
                    end
            end;
        Error ->
            {Error, IV}
    end.

cipher_recv(Socket, Key, IV) ->
    case cipher_recv_length(Socket, Key, IV) of
        {{ok, Length}, IV1} ->
            case gen_tcp:recv(Socket, Length) of
                {ok, Bin} ->
                    {PlainText, IV2} = ercraft_cipher:decrypt(Key, IV1, Bin),
                    {{ok, PlainText}, IV2};
                Error ->
                    {Error, IV1}
            end;
        {Error, IV1} ->
            {Error, IV1}
    end.
