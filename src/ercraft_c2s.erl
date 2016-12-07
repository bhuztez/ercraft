-module(ercraft_c2s).

-compile({parse_transform, ercraft_packet_trans}).

-packet_type(handshaking).

-handshaking(
   {handshake, 0,
    {record,
     [{protocol_version, varint},
      {server_address, string},
      {server_port, ushort},
      {next_state,
       {union, varint,
        [{status, 1},
         {login, 2}
        ]
       }
      }
     ]
    }
   }
  ).

-packet_type(login).

-login(
   {login_start, 0,
    {record,
     [{name, string}]
    }
   }
  ).

-login(
   {encryption_response, 1,
    {record,
     [{shared_secret, string},
      {verify_token, string}
     ]
    }
   }
  ).
