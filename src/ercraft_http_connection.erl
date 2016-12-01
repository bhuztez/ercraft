-module(ercraft_http_connection).

-export(
   [start/2,
    handle_connection/2,
    response/5,
    file_response/4,
    file_response/3]).

-include_lib("kernel/include/file.hrl").


start(Conn, Handler) ->
    Pid = spawn(?MODULE, handle_connection, [Conn, Handler]),
    {ok, Pid}.


handle_connection(Conn, {M,F,A}) ->
    link(Conn),
    {connected, Owner} = erlang:port_info(Conn, connected),
    Conn ! {Owner, {connect, self()}},

    {ok, {http_request, Method, Path, Version}} = gen_tcp:recv(Conn, 0),

    Headers = recv_headers(Conn),
    ok = inet:setopts(Conn, [{packet, raw}]),
    try
        Host = proplists:get_value('Host', Headers),
        io:format("HTTP: ~p ~p ~p~n", [Method, Host, Path]),
        apply(M,F,[Conn, Method, Host, Path, Version, Headers|A])
    after
        ok = gen_tcp:close(Conn)
    end.


recv_headers(Conn) ->
    case gen_tcp:recv(Conn, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Conn)];
        {ok, http_eoh} ->
            []
    end.


response(Conn, Code, ContentType, Headers, Body) ->
    ok =
        gen_tcp:send(
          Conn,
          [<<"HTTP/1.1 ">>, integer_to_list(Code), <<" ">>, httpd_util:reason_phrase(Code), <<"\r\n">>,
           <<"Connection: close\r\n">>,
           <<"Content-Type: ">>, ContentType, <<"\r\n">>,
           <<"Content-Length: ">>,
           <<"\r\n">>, integer_to_list(iolist_size(Body)), <<"\r\n">>,
           Headers,
           <<"\r\n">>, Body]).


file_response(Conn, Filename, ContentType, Headers) ->
    case filelib:is_file(Filename) of
        false ->
            io:format("404: ~p~n", [Filename]),
            response(
              Conn,
              404,
              <<"text/html">>,
              [],
              <<"<h1>404 Not Found</h1>\n">>);
        true ->
            {ok, #file_info{size=Size}} = file:read_file_info(Filename, []),
            ok = gen_tcp:send(
                   Conn,
                   [<<"HTTP/1.1 200 OK\r\n">>,
                    <<"Connection: close\r\n">>,
                    <<"Content-Type: ">>, ContentType, <<"\r\n">>,
                    <<"Content-Length: ">>, integer_to_binary(Size), <<"\r\n">>,
                    Headers,
                    <<"\r\n">>]),
            {ok, _} = file:sendfile(Filename, Conn)
    end.


file_response(Conn, Filename, Headers) ->
    file_response(Conn, Filename, <<"application/octet-stream">>, Headers).
