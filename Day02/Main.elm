module Palette exposing (main)

import Browser exposing (document)
import Color exposing (Color)
import Color.Convert exposing (colorToHex)
import Day2.Random exposing (ryb1, ryb1v1, ryb1v2, ryb1v3, ryb2v2)
import Helper exposing (project)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Random exposing (generate, map)
import Svg exposing (g, rect, svg, text, text_)
import Svg.Attributes exposing (fill, fontFamily, fontSize, height, width, x, y)


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


t2 : { c1 : Color, c2 : Color } -> List Color
t2 { c1, c2 } =
    [ c1, c2 ]


t3 : { c1 : Color, c2 : Color, c3 : Color } -> List Color
t3 { c1, c2, c3 } =
    [ c1, c2, c3 ]


t4 : { c1 : Color, c2 : Color, c3 : Color, c4 : Color } -> List Color
t4 { c1, c2, c3, c4 } =
    [ c1, c2, c3, c4 ]


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

        properties k =
            case ( k, List.length colors ) of
                ( 0, 1 ) ->
                    ( ( 0, 0 ), ( d * 2, d * 2 ) )

                ( i, 2 ) ->
                    ( ( 0, d * toFloat i ), ( 2 * d, d ) )

                ( 0, 3 ) ->
                    ( ( 0, 0 ), ( 2 * d, d ) )

                ( i, 3 ) ->
                    ( ( toFloat (i - 1) * d, d ), ( d, d ) )

                ( i, _ ) ->
                    ( ( d * toFloat (modBy 2 i), d * toFloat (i // 2) ), ( d, d ) )

        square ( ( x0, y0 ), ( w, h ) ) color =
            g []
                [ rect
                    [ x <| String.fromFloat <| x0
                    , y <| String.fromFloat <| y0
                    , width <| String.fromFloat w
                    , height <| String.fromFloat h
                    , fill <| Color.toCssString color
                    ]
                    []
                , text_
                    [ x <| String.fromFloat <| x0 + 0.05 * d
                    , y <| String.fromFloat <| y0 + 0.1 * d
                    , fontSize "24"
                    , fontFamily "sans serif"
                    ]
                    [ Svg.text <| colorToHex color ]
                ]
    in
    div []
        [ colors
            |> List.indexedMap (\i c -> square (properties i) c)
            |> svg
                [ width <| String.fromFloat <| d * 2
                , height <| String.fromFloat <| d * 2
                ]
        , div []
            [ button [ onClick One ] [ Html.text "1" ]
            , button [ onClick OneVOne ] [ Html.text "1:1" ]
            , button [ onClick OneVTwo ] [ Html.text "1:2" ]
            , button [ onClick OneVThree ] [ Html.text "1:3" ]
            , button [ onClick TwoVTwo ] [ Html.text "2:2" ]
            ]
        ]


description : String
description =
    """
**Originally from May 2014**

I came across the RYB color space in [Paletton](http://paletton.com) and thought it was pretty.

I wanted to make one of my own. The weird and unintuitive equation in the Elm file comes
from the python file, which is a slight modification of the one I wrote originally,
based on [Paint Inspired Color Compositing](http://bahamas10.github.io/ryb/assets/ryb.pdf) by N. Gossett and B. Chen.

I used this day's project in many other days.
"""


main : Program () Model Msg
main =
    document
        { init = always ([] |> update OneVThree)
        , update = update
        , view = view |> project 2 description
        , subscriptions = always Sub.none
        }
