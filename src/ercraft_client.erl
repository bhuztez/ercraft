-module(ercraft_client).

-export([start/0, init/0]).

start() ->
    spawn_link(?MODULE, init, []).


init() ->
    {ok, Conn} =
        gen_tcp:connect(
          {127,0,0,1},
          25565,
          [binary, {active,false}]),

    ercraft_packet:send(
      Conn,
      ercraft_c2s:encode_handshaking(
        {handshake,
         #{protocol_version => 210,
           server_address => <<"127.0.0.1">>,
           server_port => 25565,
           next_state => login
          }
        }
       )
     ),

    ercraft_packet:send(
      Conn,
      ercraft_c2s:encode_login(
        {login_start,
         #{name => <<"user">>}
        }
       )
     ),


    {set_compression,
     #{threshold := Threshold}} =
        ercraft_s2c:decode_login(
          ercraft_packet:recv(Conn)),


    {login_success, _} =
        ercraft_s2c:decode_login(
          ercraft_packet:decompress(
            ercraft_packet:recv(Conn))),

    loop(Conn, Threshold).


loop(Conn, Threshold) ->
    case ercraft_s2c:decode_play(
          ercraft_packet:decompress(
            ercraft_packet:recv(Conn))) of
        {join_game, _} -> ok;
        {plugin_message, _} -> ok;
        {server_difficulty, _} -> ok;
        {spawn_position, _} -> ok;
        {player_abilities, _} -> ok;
        {held_item_change, _} -> ok;
        {entity_status, _} -> ok;
        {statistics, _} -> ok;
        {player_list_item, _}  -> ok;
        {chunk_data, _} -> ok;
        {player_position_and_look, _} -> ok;
        {world_border, _} -> ok;
        {time_update, _} -> ok;
        {window_items, _} -> ok;
        {set_slot, _} -> ok;
        {entity_metadata, _} -> ok;
        {entity_properties, _} -> ok;
        {update_health, _} -> ok;
        {set_experience, _} -> ok;
        {keep_alive, _} -> ok;
        Packet ->
            io:format("~p~n", [Packet])
    end,
    loop(Conn, Threshold).
