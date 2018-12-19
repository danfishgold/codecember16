module Pointer exposing (Position, onDown, onMove, onUp)

import Html exposing (Attribute, Html)
import Html.Events exposing (on)
import Html.Events.Extra.Pointer as P
import Json.Decode as Json



--


type alias Position =
    ( Float, Float )


on : String -> (Position -> msg) -> Attribute msg
on event positionToMsg =
    P.onWithOptions event
        { stopPropagation = True, preventDefault = True }
        (\e -> positionToMsg e.pointer.offsetPos)


onDown : (Position -> msg) -> Attribute msg
onDown toMsg =
    on "pointerdown" toMsg


onUp : (Position -> msg) -> Attribute msg
onUp toMsg =
    on "pointerup" toMsg


onMove : (Position -> msg) -> Attribute msg
onMove toMsg =
    on "pointermove" toMsg
