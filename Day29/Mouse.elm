module Day29.Mouse exposing (..)

import Json.Decode as Json
import Html.Events exposing (on)
import Html exposing (Html, Attribute)


--


type alias Position =
    ( Float, Float )


position : Json.Decoder Position
position =
    Json.map2 (,)
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)


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
