module Automaton exposing (main)

import Browser exposing (document)
import Color exposing (Color)
import Dict exposing (Dict)
import Helper exposing (project)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, height, stroke, strokeWidth, transform, width, x, y)
import Svg.Events exposing (onClick)
import Task


type alias Model =
    { levelCount : Int
    , colors : Int
    , rule : Dict (List Int) Int
    , levels : List (List Int)
    }


type Msg
    = ShiftRule (List Int)
    | SetNumColors Int


init : Int -> ( Model, Cmd Msg )
init levelCount =
    ( { levelCount = levelCount
      , colors = 3
      , rule = Dict.empty
      , levels = [ [ 1 ] ]
      }
        |> addRowIfNeeded
    , Cmd.none
    )


kthRule : Int -> Int -> Int -> List Int
kthRule c n k =
    if n == 0 then
        []

    else
        kthRule c (n - 1) (k // c) ++ [ modBy c k ]



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


update : Msg -> Model -> Model
update msg model =
    case msg of
        ShiftRule idx ->
            let
                shift ruleN =
                    case ruleN of
                        Just n ->
                            Just <| modBy model.colors (n + 1)

                        Nothing ->
                            Just 1
            in
            { model
                | rule = Dict.update idx shift model.rule
                , levels = [ [ 1 ] ]
            }
                |> addRowIfNeeded

        SetNumColors n ->
            { model
                | colors = n
                , rule = Dict.filter (\_ c -> c < n) model.rule
            }


addRowIfNeeded : Model -> Model
addRowIfNeeded model =
    if List.length model.levels < model.levelCount then
        let
            lastLevel =
                model.levels
                    |> List.head
                    |> Maybe.withDefault [ 1 ]

            levels =
                nextLevel model.rule lastLevel :: model.levels
        in
        addRowIfNeeded { model | levels = levels }

    else
        model



--


nextLevel : Dict (List Int) Int -> List Int -> List Int
nextLevel rule lvl =
    let
        fn x ( prevs, tupl ) =
            let
                tuple =
                    tupl ++ [ x ]
            in
            ( (Dict.get tuple rule |> Maybe.withDefault 0) :: prevs
            , List.drop 1 tuple
            )

        initial =
            List.repeat 2 0
    in
    List.foldl fn ( [], initial ) (List.append lvl initial)
        |> Tuple.first
        |> List.reverse


color : Int -> Color
color i =
    case i of
        0 ->
            Color.white

        1 ->
            Color.black

        _ ->
            Color.red


pyramid : Float -> Model -> Html Msg
pyramid res model =
    let
        j0 i =
            model.levelCount - i

        pixel i j c =
            rect
                [ x <| String.fromFloat <| res * toFloat j
                , y <| String.fromFloat <| res * toFloat i
                , width <| String.fromFloat res
                , height <| String.fromFloat res
                , fill <| Color.toCssString <| color c
                ]
                []

        row i pxls =
            pxls |> List.indexedMap (\j -> pixel i (j0 i + j))
    in
    model.levels
        |> List.reverse
        |> List.indexedMap row
        |> List.concat
        |> svg
            [ width <| String.fromFloat <| res * toFloat (2 * model.levelCount + 1)
            , height <| String.fromFloat <| res * toFloat model.levelCount
            ]


rules : Float -> Model -> Html Msg
rules res { rule, colors, levelCount } =
    let
        pixel i j c =
            rect
                [ x <| String.fromFloat <| res * toFloat j
                , y <| String.fromFloat <| res * toFloat i
                , width <| String.fromFloat res
                , height <| String.fromFloat res
                , fill <| Color.toCssString <| color c
                , stroke <| "gray"
                , strokeWidth "1"
                ]
                []

        n =
            3

        allRules =
            List.range 0 (colors ^ n - 1)
                |> List.reverse
                |> List.map (kthRule colors n)

        bottom rl =
            rule |> Dict.get rl |> Maybe.withDefault 0 |> pixel 1 1

        top rl =
            List.indexedMap (pixel 0) rl

        tetris i rl =
            svg
                [ width <| String.fromFloat <| res * toFloat n + 2
                , height <| String.fromFloat <| res * 2 + 2
                , onClick (ShiftRule rl)
                ]
                [ g [ transform "translate(1, 1)" ] (bottom rl :: top rl) ]

        insideDiv rsvg =
            div
                [ style "flex" "0 0 auto"
                , style "padding" "20px 20px"
                ]
                [ rsvg ]
    in
    allRules
        |> List.indexedMap tetris
        |> List.map insideDiv
        |> div
            [ style "display" "flex"
            , style "flex-wrap" "wrap"
            , style "width" "95%"
            , style "max-width" "600px"
            ]


view : Float -> Model -> Html Msg
view res model =
    div
        [ style "margin" "0 auto"
        , style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ pyramid (res / 3) model
        , rules res model
        , div []
            [ button [ onClick <| SetNumColors 2 ] [ text "2 colors" ]
            , button [ onClick <| SetNumColors 3 ] [ text "3 colors" ]
            ]
        ]



--


description : String
description =
    """
I wanted to make this ever since I saw Stephen Wolfram's
[TED talk](https://www.ted.com/talks/stephen_wolfram_computing_a_theory_of_everything).

## Instructions
This works in rows. The first row is white everywhere except in the middle, where it's black.

Each row is determined pixel by pixel:
The pixel checks the three pixels above it (above to the left, just above it, and above to the right).
Based on the colors of these pixels its own color is determined by the tetris pieces.

When you click one of the tetris pieces it changes the color of the bottom pixel and the rule is applied.

### Example
If you click the tetris piece that is

    |white|black|white|
          |white|

then the bottom pixel (currently white) will turn black.
Now the rule means that whenever a pixel is black
and the pixel to the left and the one to the right are white,
then the pixel below it will be black.

Stephen Wolfram explains it better.

## Discussion

This is *very* slow.
"""


main : Program () Model Msg
main =
    document
        { init = always <| init 120
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view 10 |> project 5 description
        }
