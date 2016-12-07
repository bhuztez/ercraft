-module(ercraft_s2c).

-compile({parse_transform, ercraft_packet_trans}).


-packet_type(login).

-login(
   {disconnect, 0,
    {record,
     [{reason, string}]
    }
   }
  ).

-login(
   {encryption_request, 1,
    {record,
     [{server_id, string},
      {public_key, string},
      {verify_token, string}
     ]
    }
   }
  ).

-login(
   {login_success, 2,
    {record,
     [{uuid, string},
      {username, string}]
    }
   }
  ).

-login(
   {set_compression, 3,
    {record,
     [{threshold, varint}]
    }
   }
  ).


-packet_type(play).

-play(
   {statistics, 16#07,
    {array, varint,
     {record,
      [{name, string},
       {value, varint}]
     }
    }
   }
  ).

-play(
   {server_difficulty, 16#0d,
    {record,
     [{difficulty, ubyte}]
    }
   }
  ).

-play(
   {window_items, 16#14,
    {record,
     [{window_id, ubyte},
      {slot_data, {array, short, slot}}
     ]
    }
   }
  ).

-play(
   {set_slot, 16#16,
    {record,
     [{window_id, ubyte},
      {slot, short},
      {slot_data, slot}
     ]
    }
   }
  ).

-play(
   {plugin_message, 16#18,
    {record,
     [{channel, string},
      {data, string}
     ]
    }
   }
  ).

-play(
   {entity_status, 16#1b,
    {record,
     [{entity_id, int},
      {entity_status, ubyte}
     ]
    }
   }
  ).

-play(
   {keep_alive, 16#1f,
    {record,
     [{keep_alive_id, varint}]
    }
   }
  ).

-play(
   {chunk_data, 16#20,
    {record,
     [{x, int},
      {z, int},
      {ground_up_continuous, boolean},
      {primary_bit_mask, varint},
      {data, string},
      {block_entities,
       {array, varint, nbt}
      }
     ]
    }
   }
  ).

-play(
   {join_game, 16#23,
    {record,
     [{entity_id, int},
      {game_mode, ubyte},
      {dimension,
       {union, int,
        [{nether, -1},
         {overworld, 0},
         {'end', 1}
        ]
       }
      },
      {difficulty, ubyte},
      {max_players, ubyte},
      {level_type, string},
     {reduced_debug_info, boolean}
     ]
    }
   }
  ).

-play(
   {player_abilities, 16#2b,
    {record,
     [{flags, ubyte},
      {flying_speed, float},
      {field_of_view, float}
     ]
    }
   }
  ).

-play(
   {player_list_item, 16#2d,
    {union, varint,
     [{add_player, 0,
       {array, varint,
        {record,
         [{uuid, uuid},
          {name, string},
          {properties,
           {array, varint,
            {record,
             [{name, string},
              {value, string},
              {signature, {optional, string}}
             ]
            }
           }
          },
          {game_mode, varint},
          {ping, varint},
          {display_name, {optional, string}}
         ]
        }
       }
      },
      {update_game_mode, 1,
       {array, varint,
        {record,
         [{uuid, uuid},
          {game_mode, varint}
         ]
        }
       }
      },
      {update_latency, 2,
       {array, varint,
        {record,
         [{uuid, uuid},
          {ping, varint}
         ]
        }
       }
      },
      {update_display_name, 3,
       {array, varint,
        {record,
         [{uuid, uuid},
          {display_name, {optional, string}}
         ]
        }
       }
      },
      {remove_player, 4,
       {array, varint,
        {record,
         [{uuid, uuid}
         ]
        }
       }
      }
     ]
    }
   }
  ).

-play(
   {player_position_and_look, 16#2e,
    {record,
     [{x, double},
      {y, double},
      {z, double},
      {yaw, float},
      {pitch, float},
      {flags, byte},
      {teleport_id, varint}
     ]
    }
   }
  ).

-play(
   {world_border, 16#35,
    {union, varint,
     [{set_size, 0,
       {record,
        [{diameter, double}]
       }
      },
      {lerp_size, 1,
       {record,
        [{old_diameter, double},
         {new_diameter, double},
         {speed, varlong}
        ]
       }
      },
      {set_center, 2,
       {record,
        [{x, double},
         {z, double}
        ]
       }
      },
      {initialize, 3,
       {record,
        [{x, double},
         {z, double},
         {old_diameter, double},
         {new_diameter, double},
         {speed, varlong},
         {portal_teleport_boundary, varint},
         {warning_time, varint},
         {warning_blocks, varint}
        ]
       }
      },
      {set_warning_time, 4,
       {record,
        [{warning_time, varint}]
       }
      },
      {set_warning_blocks, 5,
       {record,
        [{warning_blocks, varint}]
       }
      }
     ]
    }
   }
  ).

-play(
   {held_item_change, 16#37,
    {record,
     [{slot, ubyte}]
    }
   }
  ).

-play(
   {entity_metadata, 16#39,
    {record,
     [{entity_id, varint},
      {metadata, entity_metadata}
     ]
    }
   }
  ).

-play(
   {set_experience, 16#3d,
    {record,
     [{experience_bar, float},
      {level, varint},
      {total_experience, varint}
     ]
    }
   }
  ).

-play(
   {update_health, 16#3e,
    {record,
     [{health, float},
      {food, varint},
      {food_saturation, float}
     ]
    }
   }
  ).

-play(
   {spawn_position, 16#43,
    {record,
     [{location, position}]
    }
   }
  ).

-play(
   {time_update, 16#44,
    {record,
     [{world_age, long},
      {time_of_day, long}
     ]
    }
   }
  ).

-play(
   {entity_properties, 16#4a,
    {record,
     [{entity_id, varint},
      {properties,
       {array, int,
        {record,
         [{key, string},
          {value, double},
          {modifiers,
           {array, varint,
            {record,
             [{uuid, uuid},
              {amount, double},
              {operation, byte}
             ]
            }
           }
          }
         ]
        }
       }
      }
     ]
    }
   }
  ).
