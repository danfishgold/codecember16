module Polyomino exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (points, fill, stroke, strokeWidth, transform, width, height)
import Day15.Polyomino as Poly
import Random
import Random.Extra
import Keyboard exposing (KeyCode)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
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


polygon : Float -> ( Color, Poly.Word ) -> Svg msg
polygon scale ( color, word ) =
    let
        pts =
            Poly.points ( 0, 0 ) word
                |> List.map (\( x, y ) -> ( scale * toFloat x, scale * toFloat y ))

        pointsValue =
            pts
                |> List.map (\( px, py ) -> toString px ++ "," ++ toString py)
                |> String.join " "

        n =
            List.length pts |> toFloat

        ( cx, cy ) =
            pts
                |> List.foldr (\( px, py ) ( sx, sy ) -> ( sx + px, sy + py )) ( 0, 0 )
                |> \( sumX, sumY ) ->
                    ( sumX / n, sumX / n )
    in
        Svg.polygon
            [ points pointsValue
            , fill <| colorToCssRgb color
            , stroke "black"
            , strokeWidth "1"
            ]
            []


view : Float -> Model -> Svg Msg
view scale model =
    let
        x i =
            model.width / 6 + toFloat (i % 3) * model.width / 3

        y i =
            model.height / 6 + toFloat (i // 3) * model.height / 3

        translate i =
            "translate(" ++ toString (x i) ++ "," ++ toString (y i) ++ ")"

        poly i polyomino =
            g [ transform <| translate i ] [ polygon scale polyomino ]
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
        , view = view 8
        }
