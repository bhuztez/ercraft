-module(glutil).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export(
   [
    load_program/2,
    load_rgb_image/1,
    load_rgba_image/1
   ]).


create_shader(Type, Source) ->
    Shader = gl:createShader(Type),
    ok = gl:shaderSource(Shader, [Source]),
    ok = gl:compileShader(Shader),
    case gl:getShaderiv(Shader, ?GL_COMPILE_STATUS) of
        ?GL_TRUE ->
            {ok, Shader};
        ?GL_FALSE ->
            Length = gl:getShaderiv(Shader, ?GL_INFO_LOG_LENGTH),
            Log = gl:getShaderInfoLog(Shader, Length+1),
            {error, Log}
    end.


create_program(Shaders) ->
    Program = gl:createProgram(),
    lists:foreach(
      fun (Shader) ->
              ok = gl:attachShader(Program, Shader)
      end,
      Shaders),
    ok = gl:linkProgram(Program),

    case gl:getProgramiv(Program, ?GL_LINK_STATUS) of
        ?GL_TRUE ->
            {ok, Program};
        ?GL_FALSE ->
            Length = gl:getProgramiv(Program, ?GL_INFO_LOG_LENGTH),
            Log = gl:getProgramInfoLog(Program, Length+1),
            {error, Log}
    end.


load_program(VSFilename, FSFilename) ->
    {ok, VsSource} = file:read_file(VSFilename),
    {ok, FsSource} = file:read_file(FSFilename),
    {ok, VS} = create_shader(?GL_VERTEX_SHADER, VsSource),
    {ok, FS} = create_shader(?GL_FRAGMENT_SHADER, FsSource),
    {ok, Program} = create_program([VS, FS]),
    Program.


load_rgb_image(Filename) ->
    Image = wxImage:new(Filename),
    Width = wxImage:getWidth(Image),
    Height = wxImage:getHeight(Image),
    Data = wxImage:getData(Image),
    {?GL_RGB, Width, Height, Data}.


load_rgba_image(Filename) ->
    Image = wxImage:new(Filename),
    Width = wxImage:getWidth(Image),
    Height = wxImage:getHeight(Image),
    RawData = wxImage:getData(Image),

    A =
        case wxImage:hasAlpha(Image) of
            true ->
                wxImage:getAlpha(Image);
            false ->
                255
        end,
    Data = << <<R,G,B,A>> || <<R,G,B>> <= RawData >>,
    {?GL_RGBA, Width, Height, Data}.
