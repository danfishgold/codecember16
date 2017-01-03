module Braids exposing (..)

import Html exposing (Html, div)
import Helper exposing (project)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, g, line)
import Svg.Attributes exposing (width, height, x1, y1, x2, y2, stroke, strokeWidth, strokeLinecap)
import Color exposing (Color)
import Color.Gradient exposing (gradient)
import Color.Interpolate as Space
import Color.Convert exposing (colorToCssRgb)
import Array exposing (Array)


type alias Model =
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


braid3 : Model
braid3 =
    { count = 3
    , transitions =
        List.repeat 5
            ([ [ ( 0, 1 ) ]
             , [ ( 2, 0 ) ]
             , [ ( 1, 2 ) ]
             ]
            )
            |> List.concat
    }


braid4 : Model
braid4 =
    { count = 4
    , transitions =
        List.repeat 2
            ([ [ ( 2, 1 ) ]
             , [ ( 1, 3 ), ( 0, 2 ) ]
             , [ ( 3, 0 ) ]
             , [ ( 0, 1 ), ( 2, 3 ) ]
             , [ ( 1, 2 ) ]
             , [ ( 3, 1 ), ( 2, 0 ) ]
             , [ ( 0, 3 ) ]
             , [ ( 1, 0 ), ( 3, 2 ) ]
             ]
            )
            |> List.concat
    }


braid2 : Model
braid2 =
    { count = 2
    , transitions =
        List.repeat 7
            ([ [ ( 0, 1 ) ]
             , [ ( 1, 0 ) ]
             ]
            )
            |> List.concat
    }



--


segmentZ : Segment -> comparable
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
        |> \new -> Array.set i new arr


push : Int -> a -> Array (Array a) -> Array (Array a)
push i x array =
    let
        extend x maybeArr =
            maybeArr
                |> Maybe.withDefault Array.empty
                |> Array.push x
    in
        update i (extend x) array


itemColumns : Model -> Array (Array ( Step, Index, Z ))
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
            |> List.foldl (swap) initial


segments : Int -> Int -> List ( Step, Index, Z ) -> List Segment
segments offset maxStep itemColumns =
    case itemColumns of
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


view : Float -> Float -> Model -> Svg Never
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
            gradient Space.HSL [ Color.red, Color.green ] count

        line c s1 s2 i1 i2 =
            Svg.line
                [ stroke <| colorToCssRgb c
                , strokeWidth <| toString braidWidth
                , strokeLinecap "round"
                , y1 <| toString <| y i1
                , y2 <| toString <| y i2
                , x1 <| toString <| x s1
                , x2 <| toString <| x s2
                ]
                []

        columnSegments : List ( Color, Segment )
        columnSegments =
            itemColumns model
                |> Array.toList
                |> List.map Array.toList
                |> List.map (segments 1 steps)
                |> List.map2 (\c segs -> List.map ((,) c) segs) colors
                |> List.concat

        vertical ( color, seg ) =
            line color seg.first seg.last seg.column seg.column

        diagonal ( color, seg ) =
            line color seg.last (seg.last + 1) seg.column seg.nextColumn
    in
        (columnSegments
            |> List.sortBy (\( c, segment ) -> segmentZ segment)
            |> \segs ->
                (segs |> List.filter (\( _, { first, last } ) -> first /= last) |> List.map vertical)
                    ++ List.map diagonal segs
        )
            |> svg
                [ width <| toString wd
                , height <| toString ht
                , style [ ( "padding", "20px" ) ]
                ]



--


main : Html Never
main =
    div
        [ style
            [ ( "display", "flex" )
            , ( "flex-direction", "column" )
            , ( "align-items", "center" )
            ]
        ]
        [ braid4 |> view 15 30
        , braid3 |> view 15 30
        , braid2 |> view 15 30
        ]
        |> \html -> project 23 (always html) <| ()
