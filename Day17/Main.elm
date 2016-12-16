module Headache exposing (..)

import Html exposing (Html, program)
import Collage exposing (collage, move, rotate, group, filled)
import Element
import AnimationFrame
import Color exposing (Color)


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


ring : Float -> Float -> Int -> Float -> Color -> Collage.Form
ring ringRadius circleRadius n angle color =
    let
        circle theta =
            Collage.circle circleRadius
                |> filled color
                |> move ( ringRadius * cos theta, ringRadius * sin theta )
    in
        List.range 0 n
            |> List.map (\k -> 360 * toFloat k / toFloat n |> degrees)
            |> List.map circle
            |> group
            |> rotate angle


view : Model -> Html Msg
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
                7

        angle i =
            model.angle * toFloat (i % 3 * 2 - 1) |> degrees

        color i =
            Color.red

        aRing i =
            ring (ringR i) (circR i) (n i) (angle i) (color i)
    in
        List.range 1 15
            |> List.map aRing
            |> collage (floor model.width) (floor model.height)
            |> Element.toHtml



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
