module Day25.Vector exposing (Vector, add, sub, mul)


type alias Vector =
    ( Float, Float )


add : Vector -> Vector -> Vector
add ( a1, a2 ) ( b1, b2 ) =
    ( a1 + b1, a2 + b2 )


sub : Vector -> Vector -> Vector
sub ( a1, a2 ) ( b1, b2 ) =
    ( a1 - b1, a2 - b2 )


mul : Float -> Vector -> Vector
mul a ( v1, v2 ) =
    ( a * v1, a * v2 )


norm2 : Vector -> Float
norm2 ( v1, v2 ) =
    v1 * v1 + v2 * v2


dist2 : Vector -> Vector -> Float
dist2 u v =
    sub u v |> norm2
