module Main exposing (..)

import Html exposing (program)
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (x, y, width, height, fill)
import Day2.Ryb exposing (ryb)
import Day2.Random exposing (ryb1, ryb1v1, ryb1v2, ryb1v3, ryb2v2)
import Random exposing (generate, map)
import Color exposing (Color)


type alias Model =
    List Color


type Msg
    = Set Model
    | One
    | OneVOne
    | OneVTwo
    | OneVThree
    | TwoVTwo


t1 a =
    [ a, a, a, a ]


t2 ( a, b ) =
    [ a, a, b, b ]


t3 ( a, b, c ) =
    [ a, a, b, c ]


t4 ( a, b, c, d ) =
    [ a, b, c, d ]


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


main : Program Never Model Msg
main =
    program
        { init = ( [], Cmd.none )
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }


colorToString : Color -> String
colorToString color =
    color
        |> Color.toRgb
        |> (\{ red, green, blue } ->
                "rgb("
                    ++ toString red
                    ++ ", "
                    ++ toString green
                    ++ ", "
                    ++ toString blue
                    ++ ")"
           )


view : Model -> Html Msg
view colors =
    let
        d =
            300

        n =
            List.length colors

        square i color =
            rect
                [ x <| toString <| d * toFloat (i % 2)
                , y <| toString <| d * toFloat (i // 2)
                , width <| toString d
                , height <| toString d
                , fill <| colorToString color
                ]
                []
    in
        div []
            [ div []
                [ button [ onClick One ] [ text "1" ]
                , button [ onClick OneVOne ] [ text "1:1" ]
                , button [ onClick OneVTwo ] [ text "1:2" ]
                , button [ onClick OneVThree ] [ text "1:3" ]
                , button [ onClick TwoVTwo ] [ text "2:2" ]
                ]
            , colors
                |> List.indexedMap square
                |> svg
                    [ width <| toString <| d * 2
                    , height <| toString <| d * toFloat (ceiling (d / 2))
                    ]
            ]
