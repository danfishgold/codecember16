module Day25.Vector exposing (Vector, add, arg, capMagnitude, dist2, mul, neg, norm2, normalizeOrZero, onZero, polar, sub, sum, vecMul)


type alias Vector =
    ( Float, Float )


add : Vector -> Vector -> Vector
add ( a1, a2 ) ( b1, b2 ) =
    ( a1 + b1, a2 + b2 )


sum : List Vector -> Vector
sum vecs =
    List.foldl add ( 0, 0 ) vecs


sub : Vector -> Vector -> Vector
sub ( a1, a2 ) ( b1, b2 ) =
    ( a1 - b1, a2 - b2 )


neg : Vector -> Vector
neg ( x, y ) =
    ( -x, -y )


mul : Float -> Vector -> Vector
mul a ( v1, v2 ) =
    ( a * v1, a * v2 )


norm2 : Vector -> Float
norm2 ( v1, v2 ) =
    v1 * v1 + v2 * v2


dist2 : Vector -> Vector -> Float
dist2 u v =
    sub u v |> norm2


arg : Vector -> Float
arg ( x, y ) =
    atan2 -x y


normalizeOrZero : Float -> Vector -> Vector
normalizeOrZero mag ( x, y ) =
    if ( x, y ) == ( 0, 0 ) then
        ( 0, 0 )

    else
        let
            scale =
                mag / (sqrt <| norm2 ( x, y ))
        in
        ( x * scale, y * scale )


onZero : Vector -> Vector -> Vector
onZero default vec =
    if vec == ( 0, 0 ) then
        default

    else
        vec


polar : Float -> Float -> Vector
polar r theta =
    ( r * cos theta, r * sin theta )


capMagnitude : Float -> Vector -> Vector
capMagnitude mag vec =
    if norm2 vec > mag ^ 2 then
        normalizeOrZero mag vec

    else
        vec


vecMul : Vector -> Vector -> Vector
vecMul ( a1, a2 ) ( b1, b2 ) =
    ( a1 * b1, a2 * b2 )
