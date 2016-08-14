-module(ercraft_http_listener).

-behaviour(supervisor_bridge).

-export([start_link/0]).

-export([init/1, terminate/2]).

-export([loop/1, handle_connection/1]).

-include_lib("kernel/include/file.hrl").

-define(SERVER, ?MODULE).


start_link() ->
    supervisor_bridge:start_link({local, ?SERVER}, ?MODULE, []).


init([]) ->
    Port = application:get_env(ercraft, port, 80),
    {ok, Socket} =
        gen_tcp:listen(
          Port,
          [binary,
           {packet, http},
           {active, false},
           {reuseaddr, true}]),
    {ok, spawn_link(?MODULE, loop, [Socket]), Socket}.


loop(Socket) ->
    {ok, Conn} = gen_tcp:accept(Socket),
    Pid = spawn(?MODULE, handle_connection, [Conn]),
    gen_tcp:controlling_process(Conn, Pid),
    loop(Socket).


terminate(_Reason, _State) ->
    ok.


handle_connection(Socket) ->
    {ok, {http_request, Method, Path, Version}} = gen_tcp:recv(Socket, 0),
    Headers = recv_headers(Socket),
    ok = inet:setopts(Socket, [{packet, raw}]),
    Host = proplists:get_value('Host', Headers),
    handle_request(Socket, Method, Host, Path, Version, Headers),
    ok = gen_tcp:close(Socket).


recv_headers(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Socket)];
        {ok, http_eoh} ->
            []
    end.


http_response(Socket, Code, ContentType, Headers, Body) ->
    ok =
        gen_tcp:send(
          Socket,
          [<<"HTTP/1.1 ">>, integer_to_list(Code), <<" ">>, httpd_util:reason_phrase(Code), <<"\r\n">>,
           <<"Connection: close\r\n">>,
           <<"Content-Type: ">>, ContentType, <<"\r\n">>,
           <<"Content-Length: ">>,
           <<"\r\n">>, integer_to_list(iolist_size(Body)), <<"\r\n">>,
           Headers,
           <<"\r\n">>, Body]).


file_response(Socket, Filename, ContentType, Headers) ->
    case filelib:is_file(Filename) of
        false ->
            http_response(
              Socket,
              404,
              <<"text/html">>,
              [],
              <<"<h1>404 Not Found</h1>\n">>);
        true ->
            {ok, #file_info{size=Size}} = file:read_file_info(Filename, []),
            ok = gen_tcp:send(
                   Socket,
                   [<<"HTTP/1.1 200 OK\r\n">>,
                    <<"Connection: close\r\n">>,
                    <<"Content-Type: ">>, ContentType, <<"\r\n">>,
                    <<"Content-Length: ">>, integer_to_binary(Size), <<"\r\n">>,
                    Headers,
                    <<"\r\n">>]),
            {ok, _} = file:sendfile(Filename, Socket)
    end.


file_response(Socket, Filename, Headers) ->
    file_response(Socket, Filename, <<"application/octet-stream">>, Headers).


json_response(Socket, Code, Body) ->
    http_response(Socket, Code, "application/json", [], mochijson2:encode(Body)).


read_json_body(Socket, Headers) ->
    case proplists:get_value('Content-Type', Headers) of
        "application/json; charset=utf-8" ->
            case proplists:get_value('Content-Length', Headers) of
                undefined ->
                    error;
                ContentLength ->
                    case string:to_integer(ContentLength) of
                        {N, []} when N > 0 ->
                            {ok, Bytes} = gen_tcp:recv(Socket, N),
                            case catch mochijson2:decode(Bytes) of
                                {'EXIT', _} ->
                                    error;
                                JSON ->
                                    {ok, JSON}
                            end;
                        _ ->
                            error
                    end
            end;
        _ ->
            error
    end.


handle_request(Socket, 'GET', "s3.amazonaws.com", {abs_path, "/Minecraft.Download/launcher/launcher.pack.lzma"}, {1,1}, Headers) ->
    io:format("launcher: ~p~n", [Headers]),
    {ok, LauncherMD5} = application:get_env(ercraft, launcher_md5),
    case proplists:get_value('If-None-Match', Headers) of
        LauncherMD5 ->
            http_response(Socket, 304, "application/octet-stream", [], <<>>);
        _ ->
            file_response(
              Socket,
              ".cache/s3.amazonaws.com/Minecraft.Download/launcher/launcher.pack.lzma",
              [<<"ETag: \"">>, LauncherMD5, <<"\"\r\n">>])
    end;
handle_request(Socket, 'GET', "s3.amazonaws.com", {abs_path, "/Minecraft.Download/" ++ _ = Path}, {1,1}, _Headers) ->
    io:format("download minecraft: ~p ~n", [Path]),
    file_response(
      Socket,
      ".cache/s3.amazonaws.com" ++ Path,
      []);
handle_request(Socket, 'GET', "libraries.minecraft.net", {abs_path, Path}, {1,1}, _Headers) ->
    io:format("download libraries: ~p ~n", [Path]),
    file_response(
      Socket,
      ".cache/libraries.minecraft.net" ++ Path,
      []);
handle_request(Socket, 'GET', "resources.download.minecraft.net", {abs_path, Path}, {1,1}, _Headers) ->
    io:format("download resources: ~p ~n", [Path]),
    file_response(
      Socket,
      ".cache/resources.download.minecraft.net" ++ Path,
      []);

handle_request(Socket, 'GET', "launchermeta.mojang.com", {abs_path, Path}, {1,1}, _Headers) ->
    io:format("download launchermeta: ~p ~n", [Path]),
    file_response(
      Socket,
      ".cache/launchermeta.mojang.com" ++ Path,
      []);

handle_request(Socket, 'GET', "launcher.mojang.com", {abs_path, Path}, {1,1}, _Headers) ->
    io:format("download launcher: ~p ~n", [Path]),
    file_response(
      Socket,
      ".cache/launcher.mojang.com" ++ Path,
      []);


handle_request(Socket, 'POST', "authserver.mojang.com", {abs_path, Path}, {1,1}, Headers) ->
    case read_json_body(Socket, Headers) of
        error ->
            io:format("authserver 415: ~p ~p~n", [Path, Headers]),
            json_response(
              Socket, 415,
              {struct,
               [
                {<<"error">>, <<"Unsupported Media Type">>},
                {<<"errorMessage">>, <<"The server is refusing to service the request because the entity of the request is in a format not supported by the requested resource for the requested method">>}
               ]
              });
        {ok, JSON} ->
            handle_authserver(Socket, Path, Headers, JSON)
    end;
handle_request(Socket, Method, "authserver.mojang.com", Path, {1,1}, _Headers) ->
    io:format("authserver 404: ~p ~p~n", [Method, Path]),
    json_response(
      Socket, 405,
      {struct,
       [
        {<<"error">>, <<"Method Not Allowed">>},
        {<<"errorMessage">>, <<"The method specified in the request is not allowed for the resource identified by the request URI">>}
       ]
      });

handle_request(Socket, 'GET', "mcupdate.tumblr.com", {abs_path, "/"}, {1,1}, _Headers) ->
    http_response(
      Socket, 200, <<"text/html">>, [],
      <<"<h1>mcupdate.tumblr.com</h1>\n<p>N/A</p>\n">>);
handle_request(Socket, Method, Host, Path, _Version, _Headers) ->
    io:format("HTTP: ~p ~p ~p~n", [Method, Host, Path]),
    http_response(Socket, 500, <<"text/html">>, [], <<"<h1>500 Internal Server Error</h1>">>).


handle_authserver(Socket, "/authenticate", _Headers, {struct, Body}) ->
    ClientToken = proplists:get_value(<<"clientToken">>, Body),
    UserName = proplists:get_value(<<"username">>, Body),

    Profile =
        {struct,
         [
          {<<"id">>, ClientToken},
          {<<"name">>, UserName}
         ]
        },

    json_response(
      Socket, 200,
      {struct,
       [
        {<<"accessToken">>, <<"ACCE55">>},
        {<<"clientToken">>, ClientToken},
        {<<"availableProfiles">>, [Profile]},
        {<<"selectedProfile">>, Profile}
       ]
      });

handle_authserver(Socket, "/validate", _Headers, {struct, _Body}) ->
    http_response(Socket, 200, <<"application/json">>, [], <<>>);
handle_authserver(_Socket, "/refresh", _Headers, {struct, Body}) ->
    io:format("refresh: ~p~n", [Body]);
handle_authserver(Socket, Path, _Headers, _JSON) ->
    io:format("authserver 404: ~p~n", [Path]),
    json_response(
      Socket, 404,
      {struct,
       [
        {<<"error">>, <<"Not Found">>},
        {<<"errorMessage">>, <<"The server has not found anything matching the request URI">>}
       ]
      }).
