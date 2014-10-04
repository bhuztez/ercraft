-module(matrix).

-export(
   [
    identity/0,
    frustum/6,
    perspective/4,
    ortho/6,
    scale/3,
    translate/3,
    rotate/2
   ]).


identity() ->
    {
      1.0, 0.0, 0.0, 0.0,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      0.0, 0.0, 0.0, 1.0
    }.


frustum(Left, Right, Bottom, Top, Near, Far) ->
    DX = Right - Left,
    DY = Top - Bottom,
    DZ = Far - Near,
    Width = Left + Right,
    Height = Top + Bottom,
    Depth = Far + Near,

    {
      2.0*Near/DX, 0.0,          0.0,                0.0,
      0.0,         2.0*Near/DY,  0.0,                0.0,
      Width/DX,    Height/DY,   -Depth/DZ,          -1.0,
      0.0,         0.0,         -(Far*Near*2.0)/DZ,  0.0
    }.


perspective(Fovy, Aspect, Near, Far) ->
    Top = Near*math:tan(Fovy*math:pi()/180.0),
    Right = Top * Aspect,
    frustum(-Right, Right, -Top, Top, Near, Far).


ortho(Left, Right, Bottom, Top, Near, Far) ->
    DX = Right - Left,
    DY = Top - Bottom,
    DZ = Far - Near,
    Width = Left + Right,
    Height = Top + Bottom,
    Depth = Far + Near,

    {
       2.0/DX,    0.0,        0.0,      0.0,
       0.0,       2.0/DY,     0.0,      0.0,
       0.0,       0.0,       -2.0/DZ,   0.0,
      -Width/DX, -Height/DY, -Depth/DZ, 1.0
    }.


scale(X, Y, Z) ->
    {
      X,   0.0, 0.0, 0.0,
      0.0, Y,   0.0, 0.0,
      0.0, 0.0, Z,   0.0,
      0.0, 0.0, 0.0, 1.0
    }.


translate(X, Y, Z) ->
    {
      1.0, 0.0, 0.0, 0.0,
      0.0, 1.0, 0.0, 0.0,
      0.0, 0.0, 1.0, 0.0,
      X,   Y,   Z,   1.0
    }.


rotate(Theta, {X0, Y0, Z0}) ->
    C = math:cos(Theta),
    S = math:sin(Theta),
    T = 1-C,
    M = math:sqrt(X0*X0+Y0*Y0+Z0*Z0),
    {X,Y,Z} = {X0/M, Y0/M, Z0/M},
    {
      X*X*T+C,   X*Y*T+Z*S, X*Z*T-Y*S, 0.0,
      X*Y*T-Z*S, Y*Y*T+C,   Y*Z*T+X*S, 0.0,
      X*Z*T+Y*S, Y*Z*T-X*S, Z*Z*T+C,   0.0,
      0.0,       0.0,       0.0,       1.0
    }.
