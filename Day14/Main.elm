module Moiré exposing (..)

import Html exposing (Html, program)
import Svg exposing (Svg, svg, circle, g)
import Svg.Attributes exposing (cx, cy, r, width, height, fill, transform)
import Day14.Clip exposing (clip, clipPath)
import Mouse
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)


type alias Point =
    { x : Float, y : Float }


type alias Model =
    { width : Float
    , height : Float
    , mouse : Point
    }


type Msg
    = Mouse Point


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , mouse = Point 0 0
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        update { x, y } =
            Point
                (toFloat x |> max 0 |> min model.width)
                (toFloat y |> max 0 |> min model.height)
                |> Mouse
    in
        Mouse.moves update



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mouse mouse ->
            ( { model | mouse = mouse }, Cmd.none )



--


circles : Float -> Float -> Float -> Float -> Color -> List (Svg msg)
circles width height rad spacing color =
    let
        circle x y =
            Svg.circle
                [ cx <| toString x
                , cy <| toString y
                , r <| toString rad
                , fill <| colorToCssRgb color
                ]
                []

        d =
            2 * rad + spacing

        dx y =
            if y % 2 == 0 then
                d / 2
            else
                0

        row y =
            List.range 0 (width / d |> ceiling)
                |> List.map (\i -> circle (dx y + toFloat i * d) (toFloat y * d))
    in
        List.range 0 (height / (d * sqrt 3 / 2) |> ceiling)
            |> List.concatMap row


view : Model -> Html Msg
view model =
    let
        circles2 =
            circles (2 * model.width) (2 * model.height) 10 3 Color.red

        circles1 =
            circles model.width model.height 8.3 5 Color.red
    in
        svg
            [ width <| toString model.width
            , height <| toString model.height
            ]
            [ clip "circles1" circles1
            , g
                [ clipPath "circles1" ]
                [ g [ transform <| "translate(" ++ toString (model.mouse.x - model.width) ++ "," ++ toString (model.mouse.y - model.height) ++ ")" ]
                    circles2
                ]
            ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
