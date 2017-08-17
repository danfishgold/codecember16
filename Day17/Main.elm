module Headache exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (width, height, transform, cx, cy, r, fill)
import AnimationFrame
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)


type alias Model =
    { width : Float
    , height : Float
    , angle : Float
    }


type Msg
    = Tick Float


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
    AnimationFrame.times Tick



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model
                | angle = time / 100 |> \t -> t - 360 * toFloat (floor (t / 360))
              }
            , Cmd.none
            )



--


ring : Float -> Float -> Int -> Float -> Color -> Svg msg
ring ringRadius circleRadius n angle color =
    let
        circle theta =
            Svg.circle
                [ cx <| toString <| ringRadius * cos theta
                , cy <| toString <| ringRadius * sin theta
                , r <| toString circleRadius
                , fill <| colorToCssRgb color
                ]
                []
    in
        List.range 0 n
            |> List.map (\k -> 360 * toFloat k / toFloat n |> degrees)
            |> List.map circle
            |> g [ transform <| "rotate(" ++ toString angle ++ ")" ]


view : Model -> Svg Msg
view model =
    let
        n i =
            2 * pi * ringR i / (5 + 2 * circR i) |> ceiling

        ringR i =
            toFloat i * 15

        circR i =
            if i % 2 == 0 then
                4
            else
                8

        angle i =
            model.angle * toFloat (i % 5 * 2 - 3)

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
                        ++ toString (model.width / 2)
                        ++ ","
                        ++ toString (model.height / 2)
                        ++ ")"
                ]
            |> \g -> svg [ width <| toString model.width, height <| toString model.height ] [ g ]



--


description : String
description =
    """
This was a much needed rest from the previous days' projects,
which were a little intense.

It was inspired mostly by optical illusions.
"""


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view |> project 17 description
        }
