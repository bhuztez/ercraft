-module(ercraft).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export([start/0]).


start() ->
    Chunk =
        << 1,1,1,1,
           1,1,1,1,
           1,1,1,1,
           1,1,1,1,

           1,0,0,1,
           0,0,0,0,
           0,1,0,0,
           0,0,0,0  >>,

    WX = wx:new(),
    Frame = wxFrame:new(WX, 1, "Ercraft", [{size, {640,480}}]),
    ok = wxFrame:connect(Frame, close_window),

    Canvas =
        wxGLCanvas:new(
          Frame, [{attribList, [?WX_GL_RGBA, ?WX_GL_DOUBLEBUFFER, 0]}]),

    wxWindow:show(Frame),
    wxGLCanvas:setCurrent(Canvas),

    {Width, Height} = wxWindow:getClientSize(Canvas),
    gl:viewport(0,0,Width,Height),

    Program = glutil:load_program("block.vs", "block.fs"),
    {Faces, VerticesBuffer, TexCoordBuffer} = init_buffers(Chunk),


    ok = gl:clearColor(0.0,0.0,0.0,1.0),
    ok = gl:clearDepth(1.0),
    ok = gl:enable(?GL_DEPTH_TEST),
    ok = gl:enable(?GL_CULL_FACE),
    ok = gl:depthFunc(?GL_LEQUAL),
    ok = gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),

    Texture = load_texture("texture.png"),

    ok = gl:useProgram(Program),

    PerspectiveMatrix = matrix:perspective(45.0, Width/Height, 0.1, 100.0),
    PUniform = gl:getUniformLocation(Program, "perspectiveMatrix"),
    ok = gl:uniformMatrix4fv(PUniform, ?GL_FALSE, [PerspectiveMatrix]),

    TranslateMatrix = matrix:translate(-2.5, -8.0, -14.0),
    TranslateUniform = gl:getUniformLocation(Program, "translateMatrix"),
    ok = gl:uniformMatrix4fv(TranslateUniform, ?GL_FALSE, [TranslateMatrix]),

    RotateMatrix = matrix:rotate(0*math:pi()/180.0, {0,1,0}),
    RotateUniform = gl:getUniformLocation(Program, "rotateMatrix"),
    ok = gl:uniformMatrix4fv(RotateUniform, ?GL_FALSE, [RotateMatrix]),

    ok = gl:bindBuffer(?GL_ARRAY_BUFFER, VerticesBuffer),

    VertexPositionAttrib = gl:getAttribLocation(Program, "position"),
    ok = gl:enableVertexAttribArray(VertexPositionAttrib),
    ok = gl:vertexAttribPointer(VertexPositionAttrib, 3, ?GL_FLOAT, ?GL_FALSE, 0, 0),

    ok = gl:bindBuffer(?GL_ARRAY_BUFFER, TexCoordBuffer),
    TexCoordAttrib = gl:getAttribLocation(Program, "aTextureCoord"),
    ok = gl:enableVertexAttribArray(TexCoordAttrib),
    ok = gl:vertexAttribPointer(TexCoordAttrib, 2, ?GL_FLOAT, ?GL_FALSE, 0, 0),

    Sampler = gl:getUniformLocation(Program, "sampler"),
    ok = gl:activeTexture(?GL_TEXTURE0),
    ok = gl:bindTexture(?GL_TEXTURE_2D, Texture),
    ok = gl:uniform1i(Sampler, 0),

    ok = gl:drawArrays(?GL_TRIANGLES, 0,  Faces*6),

    wxGLCanvas:swapBuffers(Canvas),
    loop(Canvas, 0, RotateUniform, Faces),
    ok = wx:destroy().


loop(Canvas, Angle, RotateUniform, Faces) ->
    receive
        #wx{event=#wxClose{}} ->
            closed;
        _ ->
            loop(Canvas, Angle, RotateUniform, Faces)
    after 25 ->
            ok = gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
            RotateMatrix = matrix:rotate(Angle*math:pi()/180.0, {0,1,0}),
            ok = gl:uniformMatrix4fv(RotateUniform, ?GL_FALSE, [RotateMatrix]),

            ok = gl:drawArrays(?GL_TRIANGLES, 0,  Faces*6),
            wxGLCanvas:swapBuffers(Canvas),

            Angle1 =
                case Angle + 1 of
                    360 ->
                        0;
                    N ->
                        N
                end,
            loop(Canvas, Angle1, RotateUniform, Faces)
    end.


block_type(X,Z,Y, Chunk) ->
    binary:at(Chunk, Y * 16 + Z * 4 + X).


neighbor(front, X, Z, Y) ->
    {X, Z+1, Y};
neighbor(back, X, Z, Y) ->
    {X, Z-1, Y};
neighbor(left, X, Z, Y) ->
    {X-1, Z, Y};
neighbor(right, X, Z, Y) ->
    {X+1, Z, Y};
neighbor(top, X, Z, Y) ->
    {X, Z, Y+1};
neighbor(bottom, X, Z, Y) ->
    {X, Z, Y-1}.

empty({X,_,_}, _)
  when X < 0 ->
    true;
empty({X,_,_}, _)
  when X > 3 ->
    true;
empty({_,Z,_}, _)
  when Z < 0 ->
    true;
empty({_,Z,_}, _)
  when Z > 3 ->
    true;
empty({_,_,Y}, _)
  when Y < 0 ->
    true;
empty({_,_,Y}, _)
  when Y > 1 ->
    true;
empty({X,Z,Y}, Chunk) ->
    block_type(X,Z,Y,Chunk) == 0.



vertex(X, Z, Y, front) ->
    [ X,   Y+1, Z+1,
      X,   Y,   Z+1,
      X+1, Y,   Z+1,

      X+1, Y,   Z+1,
      X+1, Y+1, Z+1,
      X,   Y+1, Z+1
    ];
vertex(X, Z, Y, back) ->
    [ X+1, Y+1, Z,
      X+1, Y,   Z,
      X,   Y,   Z,

      X,   Y,   Z,
      X,   Y+1, Z,
      X+1, Y+1, Z
    ];
vertex(X, Z, Y, left) ->
    [ X, Y+1, Z,
      X, Y,   Z,
      X, Y,   Z+1,

      X, Y,   Z+1,
      X, Y+1, Z+1,
      X, Y+1, Z
    ];
vertex(X, Z, Y, right) ->
    [ X+1, Y+1, Z+1,
      X+1, Y,   Z+1,
      X+1, Y,   Z,

      X+1, Y,   Z,
      X+1, Y+1, Z,
      X+1, Y+1, Z+1
    ];
vertex(X, Z, Y, top) ->
    [ X,   Y+1, Z,
      X,   Y+1, Z+1,
      X+1, Y+1, Z+1,

      X+1, Y+1, Z+1,
      X+1, Y+1, Z,
      X,   Y+1, Z
    ];
vertex(X, Z, Y, bottom) ->
    [ X,   Y, Z,
      X+1, Y, Z,
      X+1, Y, Z+1,

      X+1, Y, Z+1,
      X,   Y, Z+1,
      X,   Y, Z
    ].


texcoord(_, top) ->
    [ 0.0,    0.8125,
      0.0,    0.875,
      0.0625, 0.875,

      0.0625, 0.875,
      0.0625, 0.8125,
      0.0,    0.8125
    ];
texcoord(_, bottom) ->
    [ 0.0,    0.9375,
      0.0,    1.0,
      0.0625, 1.0,

      0.0625, 1.0,
      0.0625, 0.9375,
      0.0,    0.9375
    ];
texcoord(_, _) ->
    [ 0.0,    0.875,
      0.0,    0.9375,
      0.0625, 0.9375,

      0.0625, 0.9375,
      0.0625, 0.875,
      0.0,    0.875
    ].



float32(List) ->
    << <<X:32/float-native>> || X <- List >>.


%% 4x4x2 for now
compute_chunk(Chunk) ->
    Faces =
        [{float32(vertex(X,Z,Y,F)),float32(texcoord(block_type(X,Z,Y,Chunk), F))}
         || Y <- lists:seq(0,1),
            Z <- lists:seq(0,3),
            X <- lists:seq(0,3),
            F <- [front, back, top, bottom, left, right],
            not empty({X,Z,Y}, Chunk),
            empty(neighbor(F,X,Z,Y),Chunk)
        ],
    Vertices = << <<X/binary>> || {X,_} <- Faces >>,
    TexCoord = << <<X/binary>> || {_,X} <- Faces >>,
    {length(Faces), Vertices, TexCoord}.



load_texture(Filename) ->
    {Format, Width, Height, Data} = glutil:load_rgba_image(Filename),
    [Texture] = gl:genTextures(1),
    ok = gl:bindTexture(?GL_TEXTURE_2D, Texture),
    ok = gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MAG_FILTER, ?GL_NEAREST),
    ok = gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_NEAREST),

    ok = gl:texImage2D(?GL_TEXTURE_2D, 0, Format, Width, Height, 0, Format, ?GL_UNSIGNED_BYTE, Data),
    Texture.



init_buffers(Chunk) ->
    {Faces, Vertices, TexCoord} = compute_chunk(Chunk),

    [VerticesBuffer] = gl:genBuffers(1),
    ok = gl:bindBuffer(?GL_ARRAY_BUFFER, VerticesBuffer),

    ok = gl:bufferData(?GL_ARRAY_BUFFER, size(Vertices), Vertices, ?GL_STATIC_DRAW),

    [TexCoordBuffer] = gl:genBuffers(1),
    ok = gl:bindBuffer(?GL_ARRAY_BUFFER, TexCoordBuffer),
    ok = gl:bufferData(?GL_ARRAY_BUFFER, size(TexCoord), TexCoord, ?GL_STATIC_DRAW),

    {Faces, VerticesBuffer, TexCoordBuffer}.
