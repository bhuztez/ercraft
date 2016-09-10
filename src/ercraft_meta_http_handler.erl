-module(ercraft_meta_http_handler).

-export(
   [start_listener/0,
    handle_request/6]).

start_listener() ->
    Dir = filename:dirname(filename:dirname(code:which(?MODULE))) ++ "/.cache/",

    Digest = file_md5([Dir,"s3.amazonaws.com/Minecraft.Download/launcher/launcher.pack.lzma"]),
    MD5 = [ case N < 10 of
                true -> N+48;
                false -> N+87
            end
            || <<N:4>> <= Digest ],

    ok = application:set_env(ercraft, cache_dir, Dir),
    ok = application:set_env(ercraft, launcher_md5, MD5),

    ercraft_tcp_listener:start(
      80,
      [binary,
       {packet, http},
       {active, false},
       {reuseaddr, true}],
      {ercraft_http_connection, start,
       [{?MODULE, handle_request, []}]}).


handle_request(Conn, 'GET', "mcupdate.tumblr.com", {abs_path, "/"}, {1,1}, _Headers) ->
    ercraft_http_connection:response(
      Conn, 200, <<"text/html">>, [],
      <<"<h1>mcupdate.tumblr.com</h1>\n<p>N/A</p>\n">>);
handle_request(Conn, 'GET', Host, {abs_path, Path}, {1,1}, _Headers)
  when Host =:= "libraries.minecraft.net";
       Host =:= "resources.download.minecraft.net";
       Host =:= "launchermeta.mojang.com";
       Host =:= "launcher.mojang.com" ->
    {ok, Dir} = application:get_env(ercraft, cache_dir),
    ercraft_http_connection:file_response(
      Conn,
      [Dir,Host,Path],
      []);
handle_request(Conn, Method, Host, {abs_path, Path}, {1,1}, Headers)
  when Host =:= "authserver.mojang.com";
       Host =:= "sessionserver.mojang.com" ->
    case read_json_body(Conn, Headers) of
        error ->
            Status = 415,
            ResponseHeaders = [],
            Response =
                {struct,
                 [
                  {<<"error">>, <<"Unsupported Media Type">>},
                  {<<"errorMessage">>, <<"The server is refusing to service the request because the entity of the request is in a format not supported by the requested resource for the requested method">>}
                 ]
                };
        {ok, JSON} ->
            {Status, ResponseHeaders, Response} = handle_json_request(Method, Host, Path, Headers, JSON)
    end,
    Body =
        case Status of
            204 ->
                <<>>;
            _ ->
                mochijson2:encode(Response)
        end,
    ercraft_http_connection:response(
      Conn, Status, <<"application/json">>, ResponseHeaders, Body);
handle_request(Conn, 'GET', "s3.amazonaws.com", {abs_path, "/Minecraft.Download/launcher/launcher.pack.lzma"}, {1,1}, Headers) ->
    {ok, Dir} = application:get_env(ercraft, cache_dir),
    {ok, LauncherMD5} = application:get_env(ercraft, launcher_md5),
    case proplists:get_value('If-None-Match', Headers) of
        LauncherMD5 ->
            ercraft_http_connection:response(Conn, 304, "application/octet-stream", [], <<>>);
        _ ->
            ercraft_http_connection:file_response(
              Conn,
              [Dir, "s3.amazonaws.com/Minecraft.Download/launcher/launcher.pack.lzma"],
              [<<"ETag: \"">>, LauncherMD5, <<"\"\r\n">>])
    end;


handle_request(Conn, Method, Host, Path, _Version, _Headers) ->
    io:format("HTTP: ~p ~p ~p~n", [Method, Host, Path]),
    ercraft_http_connection:response(Conn, 500, <<"text/html">>, [], <<"<h1>500 Internal Server Error</h1>">>).


read_json_body(Conn, Headers) ->
    case proplists:get_value('Content-Type', Headers) of
        "application/json; charset=utf-8" ->
            case proplists:get_value('Content-Length', Headers) of
                undefined ->
                    error;
                ContentLength ->
                    case string:to_integer(ContentLength) of
                        {N, []} when N > 0 ->
                            {ok, Bytes} = gen_tcp:recv(Conn, N),
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

handle_json_request('POST', "authserver.mojang.com", "/authenticate", _Headers, JSON) ->
    {struct, Body} = JSON,
    ClientToken = proplists:get_value(<<"clientToken">>, Body),
    UserName = proplists:get_value(<<"username">>, Body),

    Profile =
        {struct,
         [
          {<<"id">>, ClientToken},
          {<<"name">>, UserName}
         ]
        },

    {200, [],
     {struct,
      [
       {<<"accessToken">>, <<"ACCE55">>},
       {<<"clientToken">>, ClientToken},
       {<<"availableProfiles">>, [Profile]},
       {<<"selectedProfile">>, Profile}
      ]
     }
    };
handle_json_request('POST', "authserver.mojang.com", "/validate", _Headers, _JSON) ->
    {204, [], <<>>};
handle_json_request(Method, Host, Path, _Headers, _JSON)
  when Host =:= "authserver.mojang.com", Method =/= 'POST' ->
    io:format("JSON 405: ~p ~p ~p~n", [Method, Host, Path]),
    {405, [],
     {struct,
      [
       {<<"error">>, <<"Method Not Allowed">>},
       {<<"errorMessage">>, <<"The method specified in the request is not allowed for the resource identified by the request URI">>}
      ]
     }
    };
handle_json_request('POST', "authserver.mojang.com", "/validate", _Headers, _JSON) ->
    {204, [], <<>>};
handle_json_request('POST', "sessionserver.mojang.com", "/session/minecraft/join", _Headers, _JSON) ->
    {204, [], <<>>};
handle_json_request(Method, Host, Path, _Headers, _JSON) ->
    io:format("JSON 404: ~p ~p ~p~n", [Method, Host, Path]),
    {404, [],
     {struct,
      [
       {<<"error">>, <<"Not Found">>},
       {<<"errorMessage">>, <<"The server has not found anything matching the request URI">>}
      ]
     }
    }.


file_md5(Filename) ->
    {ok, F} = file:open(Filename, [read]),
    Context = crypto:hash_init(md5),
    file_md5(F, Context).

file_md5(File, Context) ->
    case file:read(File, 1024) of
        {ok, Data} ->
            file_md5(File, crypto:hash_update(Context, Data));
        eof ->
            ok = file:close(File),
            crypto:hash_final(Context)
    end.
