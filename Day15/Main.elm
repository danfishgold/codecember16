module Polyomino exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (points, fill, stroke, strokeWidth, transform, width, height)
import Day15.Polyomino as Poly
import Random
import Random.Extra


type alias Model =
    { width : Float, height : Float, polyominos : List Poly.Word }


type Msg
    = SetPolyominos (List Poly.Word)


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
    Sub.none



--


randomize : Cmd Msg
randomize =
    Random.list 9 (Poly.randomBN 2 6 |> Random.map Poly.bn)
        |> Random.generate SetPolyominos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPolyominos polyominos ->
            ( { model | polyominos = polyominos }, Cmd.none )



--


polygon : Float -> Poly.Word -> Svg msg
polygon scale word =
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
            , fill "none"
            , stroke "black"
            , strokeWidth "1"
            ]
            []


view : Model -> Svg Msg
view model =
    let
        scale =
            8

        x i =
            model.width / 6 + toFloat (i % 3) * model.width / 3

        y i =
            model.height / 6 + toFloat (i // 3) * model.height / 3

        translate i =
            "translate(" ++ toString (x i) ++ "," ++ toString (y i) ++ ")"

        poly i word =
            g [ transform <| translate i ] [ polygon scale word ]
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
        , view = view
        }
