module Day28.Border exposing (..)

import Day27.Area as Area exposing (Area, Center)
import Svg exposing (Svg, g, polygon)
import Svg.Attributes exposing (points, stroke, strokeWidth, fill)
import Color
import Color.Convert exposing (colorToCssRgba)
import Set exposing (Set)
import Dict exposing (Dict)


type alias Corner =
    ( Int, Int )


type Orientation
    = Positive
    | Negative
    | Undetermined


type Direction
    = Up
    | Right
    | Down
    | Left


type alias DirectionNum =
    Int


directionToInt : Direction -> DirectionNum
directionToInt dir =
    case dir of
        Up ->
            0

        Right ->
            1

        Down ->
            2

        Left ->
            3


directionFromInt : DirectionNum -> Direction
directionFromInt i =
    case i of
        0 ->
            Up

        1 ->
            Right

        2 ->
            Down

        3 ->
            Left

        _ ->
            Debug.crash "wrong direction encoding"


unitVector : DirectionNum -> ( Int, Int )
unitVector dirNum =
    case directionFromInt dirNum of
        Up ->
            ( 0, 1 )

        Right ->
            ( 1, 0 )

        Down ->
            ( 0, -1 )

        Left ->
            ( -1, 0 )


edgeFromCenter : DirectionNum -> Center -> ( Corner, Corner )
edgeFromCenter dirNum ( x, y ) =
    case directionFromInt dirNum of
        Up ->
            ( ( x, y ), ( x + 1, y ) )

        Right ->
            ( ( x + 1, y ), ( x + 1, y - 1 ) )

        Down ->
            ( ( x + 1, y - 1 ), ( x, y - 1 ) )

        Left ->
            ( ( x, y - 1 ), ( x, y ) )


nextDir : Orientation -> DirectionNum -> DirectionNum
nextDir orient dirNum =
    if orient == Positive || orient == Undetermined then
        (dirNum + 1) % 4
    else
        (dirNum - 1) % 4



--


innerBorder : Set Center -> Set ( Center, DirectionNum )
innerBorder points =
    let
        borders dirNum =
            let
                ( dx, dy ) =
                    unitVector dirNum
            in
                points
                    |> Set.map (\( x, y ) -> ( x + dx, y + dy ))
                    |> (\shifted -> Set.diff shifted points)
                    |> Set.map (\( x, y ) -> ( ( x - dx, y - dy ), dirNum ))
    in
        [ Up, Right, Down, Left ]
            |> List.map directionToInt
            |> List.map borders
            |> List.foldl Set.union Set.empty


borderEdges : Set ( Center, DirectionNum ) -> Dict ( Corner, DirectionNum ) Corner
borderEdges innerBorderPoints =
    let
        edge ( pt, dirNum ) =
            edgeFromCenter dirNum pt
                |> (\( p1, p2 ) -> ( ( p1, dirNum ), p2 ))
    in
        innerBorderPoints
            |> Set.map edge
            |> Set.toList
            |> Dict.fromList


borders : Set Center -> List (List Corner)
borders points =
    []



--


view : Int -> Area -> Svg msg
view scale area =
    let
        pointString ( x, y ) =
            ""
                ++ toString (toFloat scale * (toFloat x + 0.5))
                ++ ","
                ++ toString (toFloat scale * (toFloat y + 0.5))

        borderView border =
            polygon
                [ fill "none"
                , stroke <| colorToCssRgba area.color
                , points <| String.join " " <| List.map pointString <| border
                ]
                []

        areaView =
            Area.view scale { area | color = Color.rgba 0 0 0 0 }
    in
        g []
            (areaView :: List.map borderView (borders area.points))
