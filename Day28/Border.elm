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


type Direction
    = Up
    | Right
    | Down
    | Left


type alias DirectionNum =
    Int


type alias BorderDict =
    Dict ( Corner, DirectionNum ) Corner


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


rotateDir : Orientation -> DirectionNum -> DirectionNum
rotateDir orient dirNum =
    case orient of
        Positive ->
            (dirNum + 1) % 4

        Negative ->
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


borderEdges : Set ( Center, DirectionNum ) -> BorderDict
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


type alias StepData =
    { orientation : Maybe Orientation
    , point : Corner
    , dirNum : DirectionNum
    , dict : BorderDict
    }


aBorder : StepData -> Result Corner StepData
aBorder ({ orientation, point, dirNum, dict } as step) =
    let
        nextPoint =
            case Dict.get ( point, dirNum ) dict of
                Just pt ->
                    pt

                Nothing ->
                    Debug.crash <| "current point (" ++ toString point ++ ") not on border"

        newDirAndOrientation =
            if Dict.member ( nextPoint, dirNum ) dict then
                Just ( dirNum, orientation )
            else
                case orientation of
                    Just orient ->
                        if Dict.member ( nextPoint, rotateDir orient dirNum ) dict then
                            Just ( rotateDir orient dirNum, Just orient )
                        else
                            Nothing

                    Nothing ->
                        let
                            positiveDirNum =
                                rotateDir Positive dirNum

                            negativeDirNum =
                                rotateDir Negative dirNum
                        in
                            if Dict.member ( nextPoint, positiveDirNum ) dict then
                                Just ( positiveDirNum, Just Positive )
                            else if Dict.member ( nextPoint, positiveDirNum ) dict then
                                Just ( negativeDirNum, Just Negative )
                            else
                                Nothing

        makeNextStep ( nextDirNum, newOrientation ) =
            { step
                | orientation = newOrientation
                , point = nextPoint
                , dirNum = nextDirNum
                , dict = Dict.remove ( point, dirNum ) dict
            }
    in
        newDirAndOrientation
            |> Maybe.map makeNextStep
            |> Result.fromMaybe nextPoint


borders : Set Center -> List (List Corner)
borders points =
    let
        borderDict =
            points |> innerBorder |> borderEdges
    in
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
