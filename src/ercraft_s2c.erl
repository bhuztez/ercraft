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
   {spawn_object, 16#00,
    {record,
     [{entity_id, varint},
      {object_uuid, uuid},
      {type, ubyte},
      {x, double},
      {y, double},
      {z, double},
      {pitch, byte},
      {yaw, byte},
      {data, int},
      {velocity_x, short},
      {velocity_y, short},
      {velocity_z, short}
     ]
    }
   }
  ).

-play(
   {spawn_experience_orb, 16#01,
    {record,
     [{entity_id, varint},
      {x, double},
      {y, double},
      {z, double},
      {count, short}
     ]
    }
   }
  ).

-play(
   {spawn_global_entity, 16#02,
    {record,
     [{entity_id, varint},
      {type, ubyte},
      {x, double},
      {y, double},
      {z, double}
     ]
    }
   }
  ).

-play(
   {spawn_mob, 16#03,
    {record,
     [{entity_id, varint},
      {entity_uuid, uuid},
      {type, varint},
      {x, double},
      {y, double},
      {z, double},
      {pitch, byte},
      {yaw, byte},
      {head_pitch, byte},
      {velocity_x, short},
      {velocity_y, short},
      {velocity_z, short},
      {metadata, entity_metadata}
     ]
    }
   }
  ).

-play(
   {spawn_painting, 16#04,
    {record,
     [{entity_id, varint},
      {entity_uuid, uuid},
      {title, string},
      {location, position},
      {direction,
       {union, ubyte,
        [{north, 2},
         {south, 0},
         {west, 1},
         {east, 3}
        ]
       }
      }
     ]
    }
   }
  ).

-play(
   {spawn_player, 16#05,
    {record,
     [{entity_id, varint},
      {player_uuid, uuid},
      {x, double},
      {y, double},
      {z, double},
      {yaw, byte},
      {pitch, byte},
      {metadata, entity_metadata}
     ]
    }
   }
  ).

-play(
   {animation, 16#06,
    {record,
     [{entity_id, varint},
      {animation, ubyte}
     ]
    }
   }
  ).

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
   {block_break_animation, 16#08,
    {record,
     [{entity_id, varint},
      {location, position},
      {destroy_stage, ubyte}
     ]
    }
   }
  ).

-play(
   {update_block_entity, 16#09,
    {record,
     [{location, position},
      {action, ubyte},
      {nbt, nbt}
     ]
    }
   }
  ).

-play(
   {block_action, 16#0A,
    {record,
     [{location, position},
      {action_id, ubyte},
      {action_param, ubyte},
      {block_type, varint}
     ]
    }
   }
  ).

-play(
   {block_change, 16#0B,
    {record,
     [{location, position},
      {block_id, varint}
     ]
    }
   }
  ).

-play(
   {boss_bar, 16#0C,
    {record,
     [{uuid, uuid},
      {action,
       {union, varint,
        [{add, 0,
          {record,
           [{title, string},
            {health, float},
            {color, varint},
            {division, varint},
            {flags, ubyte}
           ]
          }
         },
         {remove, 1},
         {update_health, 2, float},
         {update_title, 3, string},
         {update_style, 4,
          {record,
           [{color, varint},
            {division, varint}
           ]
          }
         },
         {update_flags, 5, ubyte}
        ]
       }
      }
     ]
    }
   }
  ).

-play(
   {server_difficulty, 16#0D,
    {record,
     [{difficulty, ubyte}]
    }
   }
  ).

-play(
   {tab_complete, 16#0E,
    {array, varint, string}
   }
  ).

-play(
   {chat_message, 16#0F,
    {record,
     [{message, string},
      {position, ubyte}
     ]
    }
   }
  ).

-play(
   {multi_block_change, 16#10,
    {record,
     [{x, int},
      {y, int},
      {records,
       {array, varint,
        {record,
         [{horizontal_position, ubyte},
          {y, ubyte},
          {block_id, varint}]
        }
       }
      }
     ]
    }
   }
  ).

-play(
   {confirm_transaction, 16#11,
    {record,
     [{window_id, ubyte},
      {action_number, short},
      {accepted, boolean}
     ]
    }
   }
  ).

-play(
   {close_window, 16#12,
    {record,
     [{window_id, ubyte}]
    }
   }
  ).

%% -play(
%%    {open_window, 16#13,
%%     {record,
%%      [{window_id, ubyte},
%%       {window_type, string},
%%       {window_title, string},
%%       {number_of_slots, ubyte},
%%       {entity_id, int}
%%      ]
%%     }
%%    }
%%   ).

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
   {window_property, 16#15,
    {record,
     [{window_id, ubyte},
      {property, short},
      {value, short}
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
   {set_cooldown, 16#17,
    {record,
     [{item_id, varint},
      {cooldown_ticks, varint}
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
   {named_sound_effect, 16#19,
    {record,
     [{sound_name, string},
      {sound_category, varint},
      {x, int},
      {y, int},
      {z, int},
      {volume, float},
      {pitch, float}
     ]
    }
   }
  ).

-play(
   {disconnect, 16#1A,
    {record,
     [{reason, string}]
    }
   }
  ).

-play(
   {entity_status, 16#1B,
    {record,
     [{entity_id, int},
      {entity_status, ubyte}
     ]
    }
   }
  ).

-play(
   {explosion, 16#1C,
    {record,
     [{x, float},
      {y, float},
      {z, float},
      {radius, float},
      {records,
       {array,int,
        {record,
         [{x, byte},
          {y, byte},
          {z, byte}
         ]
        }
       }
      },
      {player_motion_x, float},
      {player_motion_y, float},
      {player_motion_z, float}
     ]
    }
   }
  ).

-play(
   {unload_chunk, 16#1D,
    {record,
     [{x, int},
      {z, int}
     ]
    }
   }
  ).

-play(
   {change_game_state, 16#1E,
    {record,
     [{reason, ubyte},
      {value, float}
     ]
    }
   }
  ).

-play({keep_alive, 16#1F, varint}).

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
   {effect, 16#21,
    {record,
     [{effect_id, int},
      {location, position},
      {data, int},
      {disable_relative_volume, boolean}
     ]
    }
   }
  ).

%% -play(
%%    {particle, 16#22,
%%     {record,
%%      [{particle_id, int},
%%       {long_distance, boolean},
%%       {x, float},
%%       {y, float},
%%       {z, float},
%%       {offset_x, float},
%%       {offset_y, float},
%%       {offset_z, float},
%%       {particle_data, float},
%%      ]
%%     }
%%    }
%%   ).

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

%% -play(
%%    {map, 16#24,
%%     {record,
%%      [{item_damage, varint},
%%       {scale, ubyte},
%%       {tracking_position, boolean},
%%       {icons,
%%        {array, varint,
%%         {record,
%%          [{direction_and_type, ubyte},
%%           {x, byte},
%%           {z, byte}
%%          ]
%%         }
%%        },
%%       }
%%      ]
%%     }
%%    }
%%   ).

-play(
   {entity_relative_move, 16#25,
    {record,
     [{entity_id, varint},
      {delta_x, short},
      {delta_y, short},
      {delta_z, short},
      {on_ground, boolean}
     ]
    }
   }
  ).

-play(
   {entity_look_and_relative_move, 16#26,
    {record,
     [{entity_id, varint},
      {delta_x, short},
      {delta_y, short},
      {delta_z, short},
      {yaw, byte},
      {pitch, byte},
      {on_ground, boolean}
     ]
    }
   }
  ).

-play(
   {entity_look, 16#27,
    {record,
     [{entity_id, varint},
      {yaw, byte},
      {pitch, byte},
      {on_ground, boolean}
     ]
    }
   }
  ).

-play(
   {entity, 16#28,
    {record,
     [{entity_id, varint}
     ]
    }
   }
  ).

-play(
   {vehicle_move, 16#29,
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
   {open_sign_editor, 16#2A,
    {record,
     [{location, position}
     ]
    }
   }
  ).

-play(
   {player_abilities, 16#2B,
    {record,
     [{flags, ubyte},
      {flying_speed, float},
      {field_of_view, float}
     ]
    }
   }
  ).

-play(
   {combat_event, 16#2C,
    {union, varint,
     [{enter_combat, 0},
      {end_combat, 1,
       {record,
        [{duration, varint},
         {entity_id, int}
        ]
       }
      },
      {entity_dead, 2,
       {record,
        [{player_id, varint},
         {entity_id, int},
         {message, string}
        ]
       }
      }
     ]
    }
   }
  ).

-play(
   {player_list_item, 16#2D,
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
   {player_position_and_look, 16#2E,
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
   {use_bed, 16#2F,
    {record,
     [{entity_id, varint},
      {location, position}
     ]
    }
   }
  ).

-play(
   {destroy_entities, 16#30,
    {array, varint, varint}
   }
  ).


-play(
   {remove_entity_effect, 16#31,
    {record,
     [{entity_id, varint},
      {effect_id, ubyte}
     ]
    }
   }
  ).

-play(
   {resource_pack_send, 16#32,
    {record,
     [{url, string},
      {hash, string}
     ]
    }
   }
  ).

-play(
   {respawn, 16#33,
    {record,
     [{dimension, int},
      {difficulty, ubyte},
      {game_mode, ubyte},
      {level_type, string}
     ]
    }
   }
  ).

-play(
   {entity_head_look, 16#34,
    {record,
     [{entity_id, varint},
      {head_yaw, byte}
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

-play({camara, 16#36, varint}).

-play(
   {held_item_change, 16#37,
    {record,
     [{slot, ubyte}]
    }
   }
  ).

-play(
   {display_scoreboard, 16#38,
    {record,
     [{position, ubyte},
      {score_name, string}
     ]
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
   {attach_entity, 16#3A,
    {record,
     [{attached_entity_id, int},
      {holding_entity_id, int}
     ]
    }
   }
  ).

-play(
   {entity_velocity, 16#3B,
    {record,
     [{entity_id, varint},
      {velocity_x, short},
      {velocity_y, short},
      {velocity_z, short}
     ]
    }
   }
  ).

-play(
   {entity_equipment, 16#3C,
    {record,
     [{entity_id, varint},
      {slot, varint},
      {item, slot}
     ]
    }
   }
  ).

-play(
   {set_experience, 16#3D,
    {record,
     [{experience_bar, float},
      {level, varint},
      {total_experience, varint}
     ]
    }
   }
  ).

-play(
   {update_health, 16#3E,
    {record,
     [{health, float},
      {food, varint},
      {food_saturation, float}
     ]
    }
   }
  ).

%% -play(
%%    {scoreboard_objective, 16#3F,
%%     {record,
%%      [{objective_name, string},
%%       {mode, ubyte}
%%      ]
%%     }
%%    }
%%   ).

-play(
   {set_passengers, 16#40,
    {record,
     [{entity_id, varint},
      {passengers, {array, varint, varint}}
     ]
    }
   }
  ).

-play(
   {teams, 16#41,
    {record,
     [{team_name, string},
      {mode,
       {union, ubyte,
        [{create_team, 0,
          {record,
           [{team_display_name, string},
            {team_prefix, string},
            {team_suffix, string},
            {friendly_flags, ubyte},
            {name_tag_visibility, string},
            {collision_rule, string},
            {color, ubyte},
            {players, {array, varint, string}}
           ]
          }
         },
         {remove_team, 1},
         {update_team_info, 2,
          {record,
           [{team_display_name, string},
            {team_prefix, string},
            {team_suffix, string},
            {friendly_flags, ubyte},
            {name_tag_visibility, string},
            {collision_rule, string},
            {color, ubyte}
           ]
          }
         },
         {add_players_to_team, 3, {array, varint, string}},
         {remove_players_from_team, 4, {array, varint, string}}
        ]
       }
      }
     ]
    }
   }
  ).



%% -play(
%%    {update_score, 16#42,
%%     {record,
%%      [{score_name, string},
%%       {action, ubyte},
%%       {objective_name, string},
%%       {value, varint}
%%      ]
%%     }
%%    }
%%   ).

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
   {title, 16#45,
    {union, varint,
     [{set_title, 0, string},
      {set_subtitle, 1, string},
      {set_action_bar, 2, string},
      {set_times_and_display, 3,
       {record,
        [{fade_in, int},
         {stay, int},
         {fade_out, int}
        ]
       }
      },
      {hide, 4},
      {reset, 5}
     ]
    }
   }
  ).

-play(
   {sound_effect, 16#46,
    {record,
     [{sound_id, varint},
      {sound_category, varint},
      {x, int},
      {y, int},
      {z, int},
      {volume, float},
      {pitch, float}
     ]
    }
   }
  ).

-play(
   {player_list_header_and_footer, 16#47,
    {record,
     [{header, string},
      {footer, string}
     ]
    }
   }
  ).

-play(
   {collect_item, 16#48,
    {record,
     [{collected_entity_id, varint},
      {collector_entity_id, varint},
      {picked_item_count, varint}
     ]
    }
   }
  ).

-play(
   {entity_teleport, 16#49,
    {record,
     [{entity_id, varint},
      {x, double},
      {y, double},
      {z, double},
      {yaw, byte},
      {pitch, byte},
      {on_ground, boolean}
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

-play(
   {entity_effect, 16#4B,
    {record,
     [{entity_id, varint},
      {effect_id, ubyte},
      {amplifier, ubyte},
      {duration, varint},
      {flags, ubyte}
     ]
    }
   }
  ).
