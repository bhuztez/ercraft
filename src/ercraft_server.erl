-module(ercraft_server).

-behaviour(gen_fsm).

-include_lib("public_key/include/public_key.hrl").

-export([start/0]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([start_connection/3]).

-export([handshaking/2, login/2, play/2]).


start() ->
    {ok, Bin} = file:read_file("../share/server.key"),
    Key = public_key:der_decode('RSAPrivateKey', Bin),

    #'RSAPrivateKey'{
       modulus = N,
       publicExponent = E}
        = Key,

    PubKey =
        public_key:der_encode(
          'SubjectPublicKeyInfo',
          #'SubjectPublicKeyInfo'{
             algorithm =
                 #'AlgorithmIdentifier'{
                    algorithm = {1,2,840,113549,1,1,1},
                    parameters = <<5,0>>},
             subjectPublicKey =
                 public_key:der_encode(
                   'RSAPublicKey',
                   #'RSAPublicKey'{
                      modulus = N,
                      publicExponent = E}
                  )
            }
         ),
    start(25565, Key, PubKey).


start(Port, Key, PubKey) ->
    ercraft_tcp_listener:start(
      Port,
      [binary,
       {active,false},
       {reuseaddr, true}],
      {?MODULE, start_connection, [Key, PubKey]}).

start_connection(Socket, Key, PubKey) ->
    spawn(gen_fsm, start, [?MODULE, [Socket, Key, PubKey], []]).


init([Socket, Key, PubKey]) ->
    case ercraft_transport:accept(Socket) of
        {ok, Connection} ->
            ercraft_transport:recv(Connection),
            {ok, handshaking,
             #{connection => Connection,
               key => Key,
               public_key => PubKey}
            };
        Error ->
            {stop, Error}
    end.

handshaking({handshake, #{next_state := login}}, State = #{connection := Connection}) ->
    ercraft_transport:recv(Connection),
    {next_state, login, State}.

login({login_start, #{name := Name}}, State = #{connection := Connection, public_key := PubKey}) ->
    Token = crypto:rand_bytes(4),

    ercraft_transport:send(
      Connection,
      ercraft_packet:encode(
        {encryption_request,
         #{server_id => <<>>,
           public_key => PubKey,
           verify_token => Token
          }
        },
        server,
        login)
     ),

    ercraft_transport:recv(Connection),
    {next_state, login, State#{name => Name, token => Token}};
login({encryption_response, #{shared_secret := EncryptedSecret, verify_token := EncryptedToken}}, State = #{connection := Connection, key := Key, token := Token}) ->
    <<SharedSecret:16/binary>> = public_key:decrypt_private(EncryptedSecret, Key, [{rsa_pad,rsa_pkcs1_padding}]),
    Token = public_key:decrypt_private(EncryptedToken, Key, [{rsa_pad,rsa_pkcs1_padding}]),
    ercraft_transport:recv(Connection),
    {stop, exit, State}.


play(_Event, _State) ->
    ok.

handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.


handle_info({ercraft, Connection, Bin}, StateName, State = #{connection := Connection, compression := Threshold})
  when is_binary(Bin) ->
    ?MODULE:StateName(ercraft_packet:decode(Bin, server, StateName, Threshold), State);
handle_info({ercraft, Connection, Bin}, StateName, State = #{connection := Connection})
  when is_binary(Bin) ->
    ?MODULE:StateName(ercraft_packet:decode(Bin, server, StateName), State).

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
