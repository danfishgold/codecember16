module Moiré exposing (..)

import Html exposing (Html, program)
import Svg exposing (Svg, svg, circle, g)
import Svg.Attributes exposing (cx, cy, r, width, height, fill)
import Day14.Clip exposing (clip, clipPath)
import Mouse


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
    Mouse.moves <| \{ x, y } -> Mouse <| Point (toFloat x) (toFloat y)



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mouse mouse ->
            ( { model | mouse = mouse }, Cmd.none )



--


view : Model -> Html Msg
view model =
    let
        circ x y =
            circle [ cx <| toString x, cy <| toString y, r "50", fill "red" ] []

        circles1 =
            List.range 0 10 |> List.map (\x -> circ (x * 150) 250)

        circles2 =
            List.range 0 10 |> List.map (\x -> circ (x * 150 + floor model.mouse.x) (280 + floor model.mouse.y))
    in
        svg
            [ width <| toString model.width
            , height <| toString model.height
            ]
            [ clip "circles1" circles1, g [ clipPath "circles1" ] circles2 ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
