module Braids exposing (..)

import Svg exposing (Svg, svg, g, line)
import Svg.Attributes exposing (width, height, x1, y1, x2, y2, stroke, strokeWidth)
import Color exposing (Color)
import Color.Gradient exposing (gradient)
import Color.Interpolate as Space
import Color.Convert exposing (colorToCssRgb)
import Array exposing (Array)


type alias Model =
    { count : Int
    , transitions : List ( Index, Index )
    }


type alias Index =
    Int


type alias Step =
    Int


type Z
    = Over
    | Under


braid3 : Model
braid3 =
    { count = 3
    , transitions = List.repeat 5 ([ ( 0, 1 ), ( 2, 0 ), ( 1, 2 ) ]) |> List.concat
    }


braid2 : Model
braid2 =
    { count = 2
    , transitions = List.repeat 5 ([ ( 0, 1 ), ( 1, 0 ) ]) |> List.concat
    }



--


rangeZ : ( Step, Step, Index, Index, Z ) -> comparable
rangeZ ( _, _, _, _, z ) =
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
        initial =
            List.range 0 (count - 1)
                |> List.map (\i -> Array.fromList [ ( 0, i, Over ) ])
                |> Array.fromList

        lastCol i columns =
            Array.get i columns
                |> Maybe.andThen last
                |> Maybe.map (\( _, col, _ ) -> col)
                |> Maybe.withDefault i

        swap ( step, ( i, j ) ) columns =
            columns
                |> push i ( step, lastCol j columns, Over )
                |> push j ( step, lastCol i columns, Under )
    in
        transitions
            |> List.indexedMap (\i transition -> ( i + 1, transition ))
            |> List.foldl swap initial


ranges : Int -> Int -> List ( Step, Index, Z ) -> List ( Step, Step, Index, Index, Z )
ranges offset maxStep itemColumns =
    case itemColumns of
        ( t1, i1, _ ) :: ( t2, i2, z2 ) :: rest ->
            ( t1 - offset, t2 - 1, i1, i2, z2 ) :: ranges 0 maxStep (( t2, i2, z2 ) :: rest)

        [ ( t, i, z ) ] ->
            [ ( t - offset, maxStep, i, i, z ) ]

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

        wd =
            braidWidth * toFloat count + spacing * toFloat (count + 1)

        ht =
            toFloat (steps + 4) * stepHeight

        x column =
            braidWidth / 2 + (braidWidth + spacing) * toFloat column

        y step =
            stepHeight * toFloat (step + 2)

        colors =
            -- gradient Space.RGB [ Color.red, Color.green ] count
            [ Color.red, Color.blue, Color.green ]

        line c s1 s2 i1 i2 =
            Svg.line
                [ stroke <| colorToCssRgb c
                , strokeWidth <| toString braidWidth
                , Svg.Attributes.strokeLinecap <| "round"
                , x1 <| toString <| x i1
                , x2 <| toString <| x i2
                , y1 <| toString <| y s1
                , y2 <| toString <| y s2
                ]
                []

        columnRanges : List ( Color, ( Int, Int, Int, Int, Z ) )
        columnRanges =
            itemColumns model
                |> Array.toList
                |> List.map Array.toList
                |> List.map (ranges 1 steps)
                |> List.map2 (\c ranges -> List.map ((,) c) ranges) colors
                |> List.concat

        vertical ( color, ( s1, s2, i, _, _ ) ) =
            line color s1 s2 i i

        diagonal ( color, ( _, s, i1, i2, _ ) ) =
            line color s (s + 1) i1 i2
    in
        (columnRanges
            |> List.sortBy (\( c, range ) -> rangeZ range)
            |> \ranges -> List.map vertical ranges ++ List.map diagonal ranges
        )
            |> svg
                [ width <| toString wd
                , height <| toString ht
                , style [ ( "padding", "20px" ) ]
                ]



--


main : Svg Never
main =
    braid3 |> view 10 20
