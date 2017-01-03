module Polyomino exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (transform, width, height)
import Day15.Polyomino as Poly
import Day15.View as View
import Random
import Keyboard exposing (KeyCode)
import Color exposing (Color)
import Day2.Random exposing (ryb1)


type alias Model =
    { width : Float, height : Float, polyominos : List ( Color, Poly.Word ) }


type Msg
    = SetPolyominos (List ( Color, Poly.Word ))
    | Key KeyCode


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , polyominos = []
      }
    , randomize
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups Key



--


randomize : Cmd Msg
randomize =
    Random.map2 (,)
        (ryb1 1 0.5)
        (Poly.randomBN 2 6 |> Random.map Poly.bn)
        |> Random.list 9
        |> Random.generate SetPolyominos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPolyominos polyominos ->
            ( { model | polyominos = polyominos }, Cmd.none )

        Key 32 ->
            ( model, randomize )

        Key _ ->
            ( model, Cmd.none )



--


view : Float -> Model -> Svg Msg
view scale model =
    let
        x i =
            model.width / 6 + toFloat (i % 3) * model.width / 3

        y i =
            model.height / 6 + toFloat (i // 3) * model.height / 3

        translate i =
            "translate(" ++ toString (x i) ++ "," ++ toString (y i) ++ ")"

        poly i ( color, word ) =
            g [ transform <| translate i ] [ View.polygon scale color ( 0, 0 ) word ]
    in
        model.polyominos
            |> List.indexedMap poly
            |> svg
                [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view 8 |> project 15
        }
