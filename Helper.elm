module Helper exposing (filled, onEnter, onEnterOrSpace, outlined)

import Array exposing (Array)
import Browser
import Browser.Events
import Collage exposing (Collage, LineJoin, Shape, defaultLineStyle)
import Color exposing (Color)
import Html exposing (Html, a, div, h1, text)
import Html.Attributes exposing (href, style)
import Json.Decode as Json
import Markdown



-- SVG COLORS


filled color =
    Collage.filled <| Collage.uniform <| avh4ToTheSetColor color


outlined : Float -> Color -> LineJoin -> Shape -> Collage msg
outlined thickness color join =
    Collage.outlined
        { defaultLineStyle
            | thickness = thickness
            , join = join
            , fill = Collage.uniform <| avh4ToTheSetColor color
        }


avh4ToTheSetColor : Color -> { red : Int, green : Int, blue : Int, alpha : Float }
avh4ToTheSetColor c =
    let
        { red, green, blue, alpha } =
            Color.toRgba c
    in
    { red = floor <| red * 255
    , green = floor <| green * 255
    , blue = floor <| blue * 255
    , alpha = alpha
    }



-- KEYBOARD EVENTS


onEnter toMsg =
    Browser.Events.onKeyUp
        (Json.field "key" Json.string
            |> Json.andThen
                (\key ->
                    if key == "Enter" then
                        Json.succeed toMsg

                    else
                        Json.fail "Other key"
                )
        )


onEnterOrSpace enterMsg spaceMsg =
    Browser.Events.onKeyUp
        (Json.field "key" Json.string
            |> Json.andThen
                (\key ->
                    case key of
                        "Enter" ->
                            Json.succeed enterMsg

                        " " ->
                            Json.succeed spaceMsg

                        _ ->
                            Json.fail "Other key pressed"
                )
        )
