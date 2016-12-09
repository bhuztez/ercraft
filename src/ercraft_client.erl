-module(ercraft_client).

-behaviour(gen_fsm).

-include_lib("public_key/include/public_key.hrl").

-export([start_link/0]).

-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-export([handshaking/2, login/2, play/2]).


start_link() ->
    start_link({127,0,0,1}, 25565, <<"user">>).

start_link(Address, Port, Name) ->
    gen_fsm:start_link(?MODULE, [Address, Port, Name], []).


init([Address, Port, Name]) ->
    case ercraft_transport:connect(Address, Port) of
        {ok, Connection} ->
            {ok,
             handshaking,
             #{connection => Connection,
               address => Address,
               port => Port,
               name => Name},
             0};
        Error ->
            {stop, Error}
    end.


handshaking(
  timeout,
  State = #{connection := Connection,
            address := Address,
            port := Port,
            name := Name}) ->
    ercraft_transport:send(
      Connection,
      ercraft_packet:encode(
        {handshake,
         #{protocol_version => 210,
           server_address => list_to_binary(inet:ntoa(Address)),
           server_port => Port,
           next_state => login
          }
        },
        client,
        handshaking)
     ),

    ercraft_transport:send(
      Connection,
      ercraft_packet:encode(
        {login_start,
         #{name => Name}
        },
        client,
        login)
     ),

    ercraft_transport:recv(Connection),
    {next_state, login, State}.


login({encryption_request, #{public_key := Key, verify_token := Token}}, State = #{connection := Connection}) ->
    SharedSecret = crypto:rand_bytes(16),
    PubKey =
        public_key:der_decode(
          'RSAPublicKey',
          (public_key:der_decode(
             'SubjectPublicKeyInfo',
             Key))#'SubjectPublicKeyInfo'.subjectPublicKey),

    ercraft_transport:send(
      Connection,
      ercraft_packet:encode(
        {encryption_response,
         #{shared_secret => public_key:encrypt_public(SharedSecret, PubKey, [{rsa_pad,rsa_pkcs1_padding}]),
           verify_token => public_key:encrypt_public(Token, PubKey, [{rsa_pad,rsa_pkcs1_padding}])
          }
        },
        client,
        login
       )
     ),

    ercraft_transport:set_shared_secret(Connection, SharedSecret),
    ercraft_transport:recv(Connection),
    {next_state, login, State};
login({set_compression, #{threshold := Threshold}}, State = #{connection := Connection}) ->
    ercraft_transport:recv(Connection),
    {next_state, login, State#{compression => Threshold}};
login({login_success, _}, State = #{connection := Connection}) ->
    ercraft_transport:recv(Connection),
    {next_state, play, State};
login({disconnect, #{reason := Reason}}, State) ->
    {stop, {disconnect, Reason}, State}.

play({keep_alive, _}, State) ->
    {stop, exit, State};
play({chunk_data, Map}, State = #{connection := Connection}) ->
    io:format("chunk: ~p~n", [maps:remove(data, Map)]),
    ercraft_transport:recv(Connection),
    {next_state, play, State};
play(Event, State = #{connection := Connection}) ->
    io:format("~p~n", [Event]),
    ercraft_transport:recv(Connection),
    {next_state, play, State}.


handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.


handle_info({ercraft, Connection, Bin}, StateName, State = #{connection := Connection, compression := Threshold})
  when is_binary(Bin) ->
    ?MODULE:StateName(ercraft_packet:decode(Bin, client, StateName, Threshold), State);
handle_info({ercraft, Connection, Bin}, StateName, State = #{connection := Connection})
  when is_binary(Bin) ->
    ?MODULE:StateName(ercraft_packet:decode(Bin, client, StateName), State).

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%% loop(Conn, Threshold) ->
%%     case ercraft_s2c:decode_play(
%%           ercraft_packet:decompress(
%%             ercraft_packet:recv(Conn))) of
%%         {join_game, _} -> ok;
%%         {plugin_message, _} -> ok;
%%         {server_difficulty, _} -> ok;
%%         {spawn_position, _} -> ok;
%%         {player_abilities, _} -> ok;
%%         {held_item_change, _} -> ok;
%%         {entity_status, _} -> ok;
%%         {statistics, _} -> ok;
%%         {player_list_item, _}  -> ok;
%%         {chunk_data, _} -> ok;
%%         {player_position_and_look, _} -> ok;
%%         {world_border, _} -> ok;
%%         {time_update, _} -> ok;
%%         {window_items, _} -> ok;
%%         {set_slot, _} -> ok;
%%         {entity_metadata, _} -> ok;
%%         {entity_properties, _} -> ok;
%%         {update_health, _} -> ok;
%%         {set_experience, _} -> ok;
%%         {keep_alive, _} -> ok;
%%         Packet ->
%%             io:format("~p~n", [Packet])
%%     end,
%%     loop(Conn, Threshold).
