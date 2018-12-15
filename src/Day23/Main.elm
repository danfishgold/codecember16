module Day23.Main exposing (Model, Msg, page)

import Array exposing (Array)
import Color exposing (Color)
import Color.Gradient exposing (linearGradient)
import Color.Interpolate as Space
import Html exposing (Html, div)
import Html.Attributes exposing (style)
import Svg exposing (Svg, g, line, svg)
import Svg.Attributes exposing (height, stroke, strokeLinecap, strokeWidth, width, x1, x2, y1, y2)


type alias Model =
    ()


type alias Msg =
    ()


type alias Braid =
    { count : Int
    , transitions : List (List ( Index, Index ))
    }


type alias Index =
    Int


type alias Step =
    Int


type Z
    = Over
    | Under


type alias Segment =
    { first : Step
    , last : Step
    , column : Index
    , nextColumn : Index
    , z : Z
    }


braid3 : Braid
braid3 =
    { count = 3
    , transitions =
        List.repeat 5
            [ [ ( 0, 1 ) ]
            , [ ( 2, 0 ) ]
            , [ ( 1, 2 ) ]
            ]
            |> List.concat
    }


braid4 : Braid
braid4 =
    { count = 4
    , transitions =
        List.repeat 2
            [ [ ( 2, 1 ) ]
            , [ ( 1, 3 ), ( 0, 2 ) ]
            , [ ( 3, 0 ) ]
            , [ ( 0, 1 ), ( 2, 3 ) ]
            , [ ( 1, 2 ) ]
            , [ ( 3, 1 ), ( 2, 0 ) ]
            , [ ( 0, 3 ) ]
            , [ ( 1, 0 ), ( 3, 2 ) ]
            ]
            |> List.concat
    }


braid2 : Braid
braid2 =
    { count = 2
    , transitions =
        List.repeat 7
            [ [ ( 0, 1 ) ]
            , [ ( 1, 0 ) ]
            ]
            |> List.concat
    }



--


segmentZ : Segment -> Int
segmentZ { z } =
    if z == Over then
        1

    else
        -1



--


last : Array a -> Maybe a
last arr =
    Array.get (Array.length arr - 1) arr


update : Int -> (Maybe a -> a) -> Array a -> Array a
update i fn arr =
    arr
        |> Array.get i
        |> fn
        |> (\new -> Array.set i new arr)


push : Int -> a -> Array (Array a) -> Array (Array a)
push i x array =
    let
        extend x_ maybeArr =
            maybeArr
                |> Maybe.withDefault Array.empty
                |> Array.push x_
    in
    update i (extend x) array


itemColumns : Braid -> Array (Array ( Step, Index, Z ))
itemColumns { count, transitions } =
    let
        -- initially, every column at zero
        initial =
            List.range 0 (count - 1)
                |> List.map (\i -> Array.fromList [ ( 0, i, Over ) ])
                |> Array.fromList

        -- get the last column of the ith braid
        lastCol i columns =
            Array.get i columns
                |> Maybe.andThen last
                |> Maybe.map (\( _, col, _ ) -> col)
                |> Maybe.withDefault i

        -- swap braids i and j at step
        swap ( step, ( i, j ) ) columns =
            columns
                |> push i ( step, lastCol j columns, Over )
                |> push j ( step, lastCol i columns, Under )
    in
    transitions
        |> List.indexedMap (\step -> List.map (\transition -> ( step + 1, transition )))
        |> List.concat
        |> List.foldl swap initial


segments : Int -> Int -> List ( Step, Index, Z ) -> List Segment
segments offset maxStep itemColumns_ =
    case itemColumns_ of
        ( t1, i1, _ ) :: ( t2, i2, z2 ) :: rest ->
            { first = t1 - offset
            , last = t2 - 1
            , column = i1
            , nextColumn = i2
            , z = z2
            }
                :: segments 0 maxStep (( t2, i2, z2 ) :: rest)

        [ ( t, i, z ) ] ->
            [ { first = t - offset
              , last = maxStep
              , column = i
              , nextColumn = i
              , z = z
              }
            ]

        [] ->
            []



--


view : Float -> Float -> Braid -> Svg Msg
view braidWidth stepHeight ({ count, transitions } as model) =
    let
        spacing =
            0

        steps =
            List.length transitions

        ht =
            braidWidth * toFloat count + spacing * toFloat (count + 1)

        wd =
            toFloat (steps + 4) * stepHeight

        y column =
            braidWidth / 2 + (braidWidth + spacing) * toFloat column

        x step =
            stepHeight * toFloat (step + 2)

        colors =
            linearGradient Space.HSL [ Color.red, Color.green ] count

        line c s1 s2 i1 i2 =
            Svg.line
                [ stroke <| Color.toCssString c
                , strokeWidth <| String.fromFloat braidWidth
                , strokeLinecap "round"
                , y1 <| String.fromFloat <| y i1
                , y2 <| String.fromFloat <| y i2
                , x1 <| String.fromFloat <| x s1
                , x2 <| String.fromFloat <| x s2
                ]
                []

        columnSegments : List ( Color, Segment )
        columnSegments =
            itemColumns model
                |> Array.toList
                |> List.map Array.toList
                |> List.map (segments 1 steps)
                |> List.map2 (\c segs -> List.map (\b -> ( c, b )) segs) colors
                |> List.concat

        vertical ( color, seg ) =
            line color seg.first seg.last seg.column seg.column

        diagonal ( color, seg ) =
            line color seg.last (seg.last + 1) seg.column seg.nextColumn
    in
    (columnSegments
        |> List.sortBy (\( c, segment ) -> segmentZ segment)
        |> (\segs ->
                (segs
                    |> List.filter (\( _, seg ) -> seg.first /= seg.last)
                    |> List.map vertical
                )
                    ++ List.map diagonal segs
           )
    )
        |> svg
            [ width <| String.fromFloat wd
            , height <| String.fromFloat ht
            , style "padding" "20px"
            ]



--


description : String
description =
    """
Inspired by [this](http://sortvis.org), which was mentioned in
[Mike Bostock's talk about visualizing algorithms](https://bost.ocks.org/mike/algorithms/).

This was fun but also annoying.
"""


mainHtml =
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        ]
        [ braid4 |> view 15 30
        , braid3 |> view 15 30
        , braid2 |> view 15 30
        ]


page =
    { init = always ( (), Cmd.none )
    , subscriptions = always Sub.none
    , update = \_ _ -> ( (), Cmd.none )
    , title = "Braids"
    , body = always mainHtml
    , description = description
    }
