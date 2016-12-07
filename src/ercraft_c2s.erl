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

-packet_type(play).

-play(
   {teleport_confirm, 16#00,
    {record,
     [{teleport_id, varint}]
    }
   }
  ).

-play(
   {tab_complete, 16#01,
    {record,
     [{text, string},
      {assume_command, boolean},
      {looked_at_block, {optional, position}}
     ]
    }
   }
  ).

-play(
   {chat_message, 16#02,
    {record,
     [{message, string}]
    }
   }
  ).

-play(
   {client_status, 16#03,
    {union, varint,
     [{perform_respawn, 0},
      {request_stats, 1},
      {open_inventory, 2}
     ]
    }
   }
  ).

-play(
   {client_settings, 16#04,
    {record,
     [{locale, string},
      {view_distance, byte},
      {chat_mode,
       {union, varint,
        [{enabled, 0},
         {commands_only, 1},
         {hidden, 2}
        ]
       }
      },
      {chat_colors, boolean},
      {displayed_skin_parts, ubyte},
      {main_hand,
       {union, varint,
        [{left, 0},
         {right, 1}]
       }
      }
     ]
    }
   }
  ).

-play(
   {confirm_transaction, 16#05,
    {record,
     [{window_id, ubyte},
      {action_number, short},
      {accepted, boolean}
     ]
    }
   }
  ).

-play(
   {enchant_item, 16#06,
    {record,
     [{window_id, ubyte},
      {enchantment, byte}
     ]
    }
   }
  ).

-play(
   {click_window, 16#07,
    {record,
     [{window_id, ubyte},
      {slot, short},
      {button, byte},
      {action_number, short},
      {mode, varint},
      {clicked_item, slot}
     ]
    }
   }
  ).

-play(
   {close_window, 16#08,
    {record,
     [{window_id, ubyte}]
    }
   }
  ).

-play(
   {plugin_message, 16#09,
    {record,
     [{channel, string},
      {date, string}
     ]
    }
   }
  ).

-play(
   {use_entity, 16#0A,
    {record,
     [{target, varint},
      {type,
       {union, varint,
        [{interact, 0,
          {record,
           [{hand,
             {union, varint,
              [{main_hand, 0},
               {off_hand, 1}
              ]
             }
            }
           ]
          }
         },
         {attack, 1},
         {interact_at, 2,
          {record,
           [{x, float},
            {y, float},
            {z, float},
            {hand,
             {union, varint,
              [{main_hand, 0},
               {off_hand, 1}
              ]
             }
            }
           ]
          }
         }
        ]
       }
      }
     ]
    }
   }
  ).

-play({keep_alive, 16#0B, varint}).

-play(
   {player_position, 16#0C,
    {record,
     [{x, double},
      {feet_y, double},
      {z, double},
      {on_ground, boolean}
     ]
    }
   }
  ).

-play(
   {player_position_and_look, 16#0D,
    {record,
     [{x, double},
      {feet_y, double},
      {z, double},
      {yaw, float},
      {pitch, float},
      {on_ground, boolean}
     ]
    }
   }
  ).

-play(
   {player_look, 16#0E,
    {record,
     [{yaw, float},
      {pitch, float},
      {on_ground, boolean}
     ]
    }
   }
  ).


-play(
   {player, 16#0F,
    {record,
     [{on_ground, boolean}
     ]
    }
   }
  ).


-play(
   {vehicle_move, 16#10,
    {record,
     [{x, double},
      {y, double},
      {z, double},
      {yaw, float},
      {pitch, float}
     ]
    }
   }
  ).


-play(
   {steer_boat, 16#11,
    {record,
     [{right_paddle_turning, boolean},
      {left_paddle_turning, boolean}
     ]
    }
   }
  ).


-play(
   {player_abilities, 16#12,
    {record,
     [{flags, ubyte},
      {flying_speed, float},
      {walking_speed, float}
     ]
    }
   }
  ).

-play(
   {player_digging, 16#13,
    {record,
     [{status, varint},
      {location, position},
      {face,
       {union, ubyte,
        [{bottom, 0},
         {top, 1},
         {north, 2},
         {south, 3},
         {west, 4},
         {east, 5},
         {special, 255}
        ]
       }
      }
     ]
    }
   }
  ).

-play(
   {entity_action, 16#14,
    {record,
     [{entity_id, varint},
      {action_id, varint},
      {jump_boost, varint}
     ]
    }
   }
  ).


-play(
   {steer_vehicle, 16#15,
    {record,
     [{sideways, float},
      {forward, float},
      {flags, ubyte}
     ]
    }
   }
  ).

-play(
   {resource_pack_status, 16#16,
    {union, varint,
     [{loaded, 0},
      {declined, 1},
      {failed, 2},
      {accepted, 3}
     ]
    }
   }
  ).

-play(
   {held_item_change, 16#17,
    {record,
     [{slot, short}]
    }
   }
  ).


-play(
   {creative_inventory_action, 16#18,
    {record,
     [{slot, short},
      {clicked_item, slot}
     ]
    }
   }
  ).

-play(
   {update_sign, 16#19,
    {record,
     [{location, position},
      {line1, string},
      {line2, string},
      {line3, string},
      {line4, string}
     ]
    }
   }
  ).

-play(
   {animation, 16#1A,
    {record,
     [{hand,
       {union, varint,
        [{main_hand, 0},
         {off_hand, 1}
        ]
       }
      }
     ]
    }
   }
  ).

-play(
   {spectate, 16#1B,
    {record,
     [{target, uuid}]
    }
   }
  ).

-play(
   {player_block_placement, 16#1C,
    {record,
     [{location, position},
      {face,
       {union, ubyte,
        [{bottom, 0},
         {top, 1},
         {north, 2},
         {south, 3},
         {west, 4},
         {east, 5},
         {special, 255}
        ]
       }
      },
      {hand,
       {union, varint,
        [{main_hand, 0},
         {off_hand, 1}
        ]
       }
      },
      {x, float},
      {y, float},
      {z, float}
     ]
    }
   }
  ).

-play(
   {use_item, 16#1D,
    {record,
     [{hand,
       {union, varint,
        [{main_hand, 0},
         {off_hand, 1}
        ]
       }
      }
     ]
    }
   }
  ).
