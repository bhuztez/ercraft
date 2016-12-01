-module(ercraft_packet_trans).

-export([parse_transform/2]).


-define(V(X,N), var_(X,N,0)).
-define(V(X,N,L), var_(X,N,L)).


var_(X, N, 0) ->
    {var, 1,
     list_to_atom(
       lists:concat(
         [string:to_upper(atom_to_list(X)),
          N
         ]
        )
      )
    };
var_(X, N, L) ->
    {var, 1,
     list_to_atom(
       lists:concat(
         [string:to_upper(atom_to_list(X)),
          L,
          "_",
          N
         ]
        )
      )
    }.


decode_(Atom) ->
    list_to_atom("decode_"++atom_to_list(Atom)).

encode_(Atom) ->
    list_to_atom("encode_"++atom_to_list(Atom)).

call_(F,A)
  when is_atom(F) ->
    {call, 1, {atom, 1, F}, A};
call_(F,A) ->
    {call, 1, F, A}.

call_(M,F,A)
  when is_atom(M), is_atom(F) ->
    {call, 1,
     {remote, 1, {atom, 1, M}, {atom, 1, F}},
     A}.


decode_union_field_({Tag, ID}, Level) ->
    {clause, 1,
     [{tuple, 1, [{integer, 1, ID}, ?V(bin, 0, Level)]}],
     [],
     [{tuple, 1, [{atom, 1, Tag}, ?V(bin, 0, Level)]}]
    };
decode_union_field_({Tag, ID, Field}, Level) ->
    {clause, 1,
     [{tuple, 1, [{integer, 1, ID}, ?V(bin, 0, Level)]}],
     [],
     [{match, 1,
       {tuple, 1, [?V(r, Level), ?V(bin, 1, Level)]},
       decode_field_(Field, 0, Level)
      },
      {tuple, 1,
       [{tuple, 1,
         [{atom, 1, Tag},
          ?V(r, Level)
         ]
        },
        ?V(bin, 1, Level)
       ]
      }
     ]
    }.

decode_record_field_({Name, {optional, Type}}, N, Level) ->
    {[{match, 1,
       {tuple, 1, [?V(has, Name, Level), ?V(bin, N+1, Level)]},
       decode_field_(boolean, N, Level)
      },
      {'case', 1,
       ?V(has,Name,Level),
       [{clause, 1,
         [{atom,1,true}],
         [],
         [{match, 1,
           {tuple, 1, [?V(v, Name, Level), ?V(bin, N+2, Level)]},
           decode_field_(Type, N+1, Level)
          },
          {match, 1,
           ?V(r, N+2, Level),
           {map, 1,
            ?V(r, N, Level),
            [{map_field_assoc, 1, {atom, 1, Name}, ?V(v, Name, Level)}]
           }
          }
         ]
        },
        {clause, 1,
         [{atom,1,false}],
         [],
         [{match, 1, ?V(r, N+2, Level), ?V(r, N, Level)},
          {match, 1, ?V(bin, N+2, Level), ?V(bin, N+1, Level)}
         ]
        }
       ]
      }
     ], N+2
    };
decode_record_field_({Name, Type}, N, Level) ->
    {[{match, 1,
      {tuple, 1, [?V(v, Name, Level), ?V(bin, N+1, Level)]},
      decode_field_(Type, N, Level)
     },
     {match, 1,
      ?V(r, N+1, Level),
      {map, 1,
       ?V(r, N, Level),
       [{map_field_assoc, 1, {atom, 1, Name}, ?V(v, Name, Level)}]
      }
     }
    ], N+1}.

decode_field_({union, Type, Fields}, N, Level) ->
    call_(
      {'fun',1,
       {clauses,
        [decode_union_field_(Field, Level+1)
         || Field <- Fields]
       }
      },
      [call_(ercraft_datatype, decode_(Type), [?V(bin, N,Level)])]
     );
decode_field_({record, Fields}, N, Level) ->
    {Exprs, N1} =
        lists:mapfoldl(
          fun(Field, M) ->
                  decode_record_field_(Field, M, Level+1)
          end,
          0,
          Fields
         ),

    call_(
      {'fun', 1,
       {clauses,
        [{clause,1,
          [?V(bin, 0,Level+1)],
          [],
          [{match, 1, ?V(r, 0, Level+1), {map,1,[]}}]
           ++ lists:append(Exprs)
          ++[{tuple,1,
              [?V(r, N1, Level+1),
               ?V(bin, N1, Level+1)
              ]
             }
            ]
         }
        ]
       }
      },
      [?V(bin, N,Level)]);
decode_field_({array, Type, Field}, N, Level) ->
    call_(
      ercraft_datatype, decode_n,
      [{'fun', 1,
        {clauses,
         [{clause, 1,
           [?V(bin, 0, Level+1)],
           [],
           [decode_field_(Field, 0, Level+1)]
          }
         ]
        }
       },
       decode_field_(Type, N, Level)]
     );
decode_field_(Type, N, Level) when is_atom(Type) ->
    call_(ercraft_datatype, decode_(Type), [?V(bin, N,Level)]).


decode_packet_({Type, Field}) ->
    {function, 1,
     decode_(Type), 1,
     [{clause, 1,
       [?V(bin, 0)],
       [[call_(is_binary, [?V(bin, 0)])]],
       [{match, 1,
         {tuple, 1, [{var, 1, 'R'}, {bin, 1, []}]},
         decode_field_(Field, 0, 0)
        },
        {var, 1, 'R'}
       ]
      }
     ]
    }.



encode_union_field_(Type, {Tag, ID}, _N) ->
    {clause, 1,
     [{atom, 1, Tag}],
     [],
     [call_(ercraft_datatype, encode_(Type), [{integer, 1, ID}])]
    };
encode_union_field_(Type, {Tag, ID, Field}, N) ->
    {clause, 1,
     [{tuple, 1, [{atom, 1, Tag}, ?V(v, N)]}],
     [],
     [{cons,1,
       call_(ercraft_datatype, encode_(Type), [{integer, 1, ID}]),
       {cons, 1,
        call_(encode_field_(Field, N+1), [?V(v, N)]),
        {nil,1}
       }
      }
     ]
    }.


encode_record_fields_([], _) ->
    {nil, 1};
encode_record_fields_([{Name, {optional, Type}}|T], N) ->
    {cons, 1,
     call_(ercraft_datatype, encode_boolean,
           [call_(maps,is_key,[{atom,1,Name}, ?V(v,N)])]),
     {cons, 1,
      {'case', 1,
       call_(maps,is_key,[{atom,1,Name}, ?V(v,N)]),
       [{clause, 1,
         [{atom, 1, true}],
         [],
         [call_(encode_field_(Type, N+1), [call_(maps,get,[{atom,1,Name}, ?V(v,N)])])]
        },
       {clause, 1,
        [{atom, 1, false}],
        [],
        [{nil, 1}]
       }
       ]
      },
      encode_record_fields_(T, N)
     }
    };
encode_record_fields_([{Name, Type}|T], N) ->
    {cons,
     1,
     call_(encode_field_(Type, N+1), [call_(maps,get,[{atom,1,Name}, ?V(v,N)])]),
     encode_record_fields_(T, N)}.



encode_field_({union, Type, Fields}, N) ->
    {'fun', 1,
     {clauses,
      [ encode_union_field_(Type, Field, N) || Field <- Fields ]
     }
    };
encode_field_({array, Type, Field}, N) ->
    {'fun',1,
     {clauses,
      [{clause, 1,
        [?V(v,N)],
        [],
        [{cons, 1,
          call_(
            ercraft_datatype,
            encode_(Type),
            [call_(length, [?V(v, N)])]
           ),
          {cons, 1,
           call_(
             lists,map,
             [case encode_field_(Field,N+1) of
                  {remote, 1, M, F} ->
                      {'fun',1,{function, M, F, {integer,1,1}}};
                  Other ->
                      Other
              end,
              ?V(v,N)]),
           {nil, 1}
          }
         }
        ]
       }
      ]
     }
    };
encode_field_({record, Fields}, N) ->
    {'fun', 1,
     {clauses,
      [{clause, 1,
        [?V(v, N)],
        [],
        [encode_record_fields_(Fields, N)]
       }
      ]
     }
    };
encode_field_(Type, _N) when is_atom(Type) ->
    {remote, 1, {atom, 1, ercraft_datatype}, {atom, 1, encode_(Type)}}.



encode_packet_({Type, Field}) ->
    {function, 1,
     encode_(Type), 1,
     [{clause, 1,
       [?V(v,0)],
       [],
       [call_(encode_field_(Field,1), [?V(v,0)])]
      }
     ]
    }.


generate_for_packet_type(Type, Forms) ->
    Attribs =
        [ Value
          || {attribute, _, T, Value} <- Forms,
             T =:= Type ],
    [decode_packet_({Type, {union, varint, Attribs}}),
     encode_packet_({Type, {union, varint, Attribs}})].


parse_transform([File, Module|Forms], _Options) ->
    PacketTypes =
        [ Type
          || {attribute, _, packet_type, Type} <- Forms,
             is_atom(Type)
        ],

    Forms1 =
        [ Form
          || Form <- Forms,
             case Form of
                 {attribute, _, T, _} ->
                     not lists:member(T, [packet_type|PacketTypes]);
                 _ ->
                     true
             end
        ],

    Forms2 =
        [ File, Module,
          {attribute, 1, export,
           [{decode_(Type), 1} || Type <- PacketTypes]
          },
          {attribute, 1, export,
           [{encode_(Type), 1} || Type <- PacketTypes]
          } ]
        ++ Forms1 ++
        lists:append(
          [ generate_for_packet_type(Type, Forms)
            || Type <- PacketTypes
          ]),

    %% io:format("~p~n", [Forms2]),
    %% Tree = erl_syntax:form_list(Forms2),
    %% io:format("~s~n", [erl_prettypr:format(Tree)]),

    Forms2.
