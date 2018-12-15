module Pointer exposing (Position, click, down, move, up)

import Html exposing (Attribute, Html)
import Html.Events exposing (on)
import Json.Decode as Json



--


type alias Position =
    ( Float, Float )


position : Json.Decoder Position
position =
    Json.map2 (\a b -> ( a, b ))
        (Json.field "offsetX" Json.float)
        (Json.field "offsetY" Json.float)


click : (Position -> msg) -> Attribute msg
click msg =
    on "click" (position |> Json.map msg)


down : (Position -> msg) -> Attribute msg
down msg =
    on "mousedown" (position |> Json.map msg)


up : (Position -> msg) -> Attribute msg
up msg =
    on "mouseup" (position |> Json.map msg)


move : (Position -> msg) -> Attribute msg
move msg =
    on "mousemove" (position |> Json.map msg)
