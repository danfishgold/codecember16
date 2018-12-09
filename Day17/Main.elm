module Headache exposing (main)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Helper exposing (project)
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, transform, width)
import Time


type alias Model =
    { width : Float
    , height : Float
    , angle : Float
    }


type Msg
    = Tick Time.Posix


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , angle = 0
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrame Tick



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            let
                time =
                    Time.posixToMillis posix |> toFloat
            in
            ( { model
                | angle = time / 100 |> (\t -> t - 360 * toFloat (floor (t / 360)))
              }
            , Cmd.none
            )



--


ring : Float -> Float -> Int -> Float -> Color -> Svg msg
ring ringRadius circleRadius n angle color =
    let
        circle theta =
            Svg.circle
                [ cx <| String.fromFloat <| ringRadius * cos theta
                , cy <| String.fromFloat <| ringRadius * sin theta
                , r <| String.fromFloat circleRadius
                , fill <| Color.toCssString color
                ]
                []
    in
    List.range 0 n
        |> List.map (\k -> 360 * toFloat k / toFloat n |> degrees)
        |> List.map circle
        |> g [ transform <| "rotate(" ++ String.fromFloat angle ++ ")" ]


view : Model -> Svg Msg
view model =
    let
        n i =
            2 * pi * ringR i / (5 + 2 * circR i) |> ceiling

        ringR i =
            toFloat i * 15

        circR i =
            if modBy 2 i == 0 then
                4

            else
                8

        angle i =
            model.angle * toFloat (modBy 5 i * 2 - 3)

        color i =
            Color.red

        aRing i =
            ring (ringR i) (circR i) (n i) (angle i) (color i)
    in
    List.range 1 15
        |> List.map aRing
        |> g
            [ transform <|
                "translate("
                    ++ String.fromFloat (model.width / 2)
                    ++ ","
                    ++ String.fromFloat (model.height / 2)
                    ++ ")"
            ]
        |> (\g -> svg [ width <| String.fromFloat model.width, height <| String.fromFloat model.height ] [ g ])



--


description : String
description =
    """
This was a much needed rest from the previous days' projects,
which were a little intense.

It was inspired mostly by optical illusions.
"""


main : Program () Model Msg
main =
    document
        { init = always <| init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view |> project 17 description
        }
