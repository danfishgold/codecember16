module Palette exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Svg exposing (svg, g, rect, text, text_)
import Svg.Attributes exposing (x, y, width, height, fill)
import Svg.Attributes exposing (fontFamily, fontSize)
import Day2.Random exposing (ryb1, ryb1v1, ryb1v2, ryb1v3, ryb2v2)
import Random exposing (generate, map)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb, colorToHex)


type alias Model =
    List Color


type Msg
    = Set Model
    | One
    | OneVOne
    | OneVTwo
    | OneVThree
    | TwoVTwo


t1 : a -> List a
t1 a =
    [ a ]


t2 : ( a, a ) -> List a
t2 ( a, b ) =
    [ a, b ]


t3 : ( a, a, a ) -> List a
t3 ( a, b, c ) =
    [ a, b, c ]


t4 : ( a, a, a, a ) -> List a
t4 ( a, b, c, d ) =
    [ a, b, c, d ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Set newModel ->
            ( newModel, Cmd.none )

        One ->
            ( model, generate Set (ryb1 1 0.5 |> map t1) )

        OneVOne ->
            ( model, generate Set (ryb1v1 1 0.5 |> map t2) )

        OneVTwo ->
            ( model, generate Set (ryb1v2 1 0.5 45 |> map t3) )

        OneVThree ->
            ( model, generate Set (ryb1v3 1 0.5 45 |> map t4) )

        TwoVTwo ->
            ( model, generate Set (ryb2v2 1 0.5 45 |> map t4) )


view : Model -> Html Msg
view colors =
    let
        d =
            250

        n =
            List.length colors

        properties k n =
            case ( k, n ) of
                ( 0, 1 ) ->
                    ( 0, 0, d * 2, d * 2 )

                ( i, 2 ) ->
                    ( 0, d * toFloat i, 2 * d, d )

                ( 0, 3 ) ->
                    ( 0, 0, 2 * d, d )

                ( i, 3 ) ->
                    ( toFloat (i - 1) * d, d, d, d )

                ( i, _ ) ->
                    ( d * toFloat (i % 2), d * toFloat (i // 2), d, d )

        square ( x0, y0, w, h ) color =
            g []
                [ rect
                    [ x <| toString <| x0
                    , y <| toString <| y0
                    , width <| toString w
                    , height <| toString h
                    , fill <| colorToCssRgb color
                    ]
                    []
                , text_
                    [ x <| toString <| x0 + 0.05 * d
                    , y <| toString <| y0 + 0.1 * d
                    , fontSize "24"
                    , fontFamily "sans serif"
                    ]
                    [ Svg.text <| colorToHex color ]
                ]
    in
        div []
            [ colors
                |> List.indexedMap (\i c -> square (properties i n) c)
                |> svg
                    [ width <| toString <| d * 2
                    , height <| toString <| d * 2
                    ]
            , div []
                [ button [ onClick One ] [ Html.text "1" ]
                , button [ onClick OneVOne ] [ Html.text "1:1" ]
                , button [ onClick OneVTwo ] [ Html.text "1:2" ]
                , button [ onClick OneVThree ] [ Html.text "1:3" ]
                , button [ onClick TwoVTwo ] [ Html.text "2:2" ]
                ]
            ]


main : Program Never Model Msg
main =
    program
        { init = [] |> update OneVThree
        , update = update
        , view = view |> project 2
        , subscriptions = always Sub.none
        }
