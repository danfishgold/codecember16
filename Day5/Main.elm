module Automaton exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Html exposing (Html, div, button, text)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (x, y, width, height, fill, stroke, strokeWidth, transform)
import Svg.Events exposing (onClick)
import Dict exposing (Dict)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Task


type alias Model =
    { levelCount : Int
    , ruleRadius : Int
    , colors : Int
    , rule : Dict (List Int) Int
    , levels : List (List Int)
    }


message : msg -> Cmd msg
message msg =
    Task.perform identity (Task.succeed msg)


type Msg
    = ShiftRule (List Int)
    | AddRowIfNeeded
    | SetNumColors Int


init : Int -> ( Model, Cmd Msg )
init levelCount =
    ( { levelCount = levelCount
      , ruleRadius = 1
      , colors = 3
      , rule = Dict.empty
      , levels = [ [ 1 ] ]
      }
    , message AddRowIfNeeded
    )


kthRule : Int -> Int -> Int -> List Int
kthRule c n k =
    if n == 0 then
        []
    else
        kthRule c (n - 1) (k // c) ++ [ k % c ]



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShiftRule idx ->
            let
                shift ruleN =
                    case ruleN of
                        Just n ->
                            Just <| (n + 1) % model.colors

                        Nothing ->
                            Just 1
            in
                ( { model
                    | rule = Dict.update idx shift model.rule
                    , levels = [ model.levels |> List.head |> Maybe.withDefault [ 1 ] ]
                  }
                , message AddRowIfNeeded
                )

        AddRowIfNeeded ->
            if List.length model.levels < model.levelCount then
                let
                    lastLevel =
                        model.levels
                            |> List.drop (List.length model.levels - 1)
                            |> List.head
                            |> Maybe.withDefault [ 1 ]

                    levels =
                        model.levels ++ [ nextLevel model.rule model.ruleRadius lastLevel ]
                in
                    ( { model | levels = levels }, message AddRowIfNeeded )
            else
                ( model, Cmd.none )

        SetNumColors n ->
            ( { model | colors = n }, Cmd.none )



--


nextLevel : Dict (List Int) Int -> Int -> List Int -> List Int
nextLevel rule rad lvl =
    let
        fn x ( prevs, tupl ) =
            let
                tuple =
                    tupl ++ [ x ]
            in
                ( prevs ++ [ Dict.get tuple rule |> Maybe.withDefault 0 ], List.drop 1 tuple )

        initial =
            List.repeat (2 * rad) 0
    in
        List.foldl fn ( [], initial ) (List.append lvl initial) |> Tuple.first


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
                [ x <| toString <| res * toFloat j
                , y <| toString <| res * toFloat i
                , width <| toString res
                , height <| toString res
                , fill <| colorToCssRgb <| color c
                ]
                []

        row i pxls =
            pxls |> List.indexedMap (\j -> pixel i (j0 i + j))
    in
        model.levels
            |> List.indexedMap row
            |> List.concat
            |> svg
                [ width <| toString <| res * toFloat (2 * model.levelCount + 1)
                , height <| toString <| res * toFloat model.levelCount
                ]


rules : Float -> Model -> Html Msg
rules res { rule, ruleRadius, colors, levelCount } =
    let
        pixel i j c =
            rect
                [ x <| toString <| res * toFloat j
                , y <| toString <| res * toFloat i
                , width <| toString res
                , height <| toString res
                , fill <| colorToCssRgb <| color c
                , stroke <| "gray"
                , strokeWidth "1"
                ]
                []

        n =
            2 * ruleRadius + 1

        allRules =
            List.range 0 (colors ^ n - 1)
                |> List.reverse
                |> List.map (kthRule colors n)

        bottom rl =
            rule |> Dict.get rl |> Maybe.withDefault 0 |> pixel 1 ruleRadius

        top rl =
            List.indexedMap (pixel 0) rl

        tetris i rl =
            svg
                [ width <| toString <| res * toFloat n + 2
                , height <| toString <| res * 2 + 2
                , onClick (ShiftRule rl)
                ]
                [ g [ transform "translate(1, 1)" ] (bottom rl :: top rl) ]

        insideDiv rsvg =
            div
                [ style
                    [ ( "flex", "0 0 auto" )
                    , ( "padding", "20px 20px" )
                    ]
                ]
                [ rsvg ]
    in
        allRules
            |> List.indexedMap tetris
            |> List.map insideDiv
            |> div
                [ style
                    [ ( "display", "flex" )
                    , ( "flex-wrap", "wrap" )
                    , ( "width", "95%" )
                    , ( "max-width", "600px" )
                    ]
                ]


view : Float -> Model -> Html Msg
view res model =
    div
        [ style
            [ ( "margin", "0 auto" )
            , ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
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


main : Program Never Model Msg
main =
    program
        { init = init 120
        , subscriptions = subscriptions
        , update = update
        , view = view 10 |> project 5 description
        }
