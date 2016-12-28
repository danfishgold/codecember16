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
            ( 0, -1 )

        Right ->
            ( 1, 0 )

        Down ->
            ( 0, 1 )

        Left ->
            ( -1, 0 )


edgeFromCenter : DirectionNum -> Center -> ( Corner, Corner )
edgeFromCenter dirNum ( x, y ) =
    case directionFromInt dirNum of
        Up ->
            ( ( x, y ), ( x + 1, y ) )

        Right ->
            ( ( x + 1, y ), ( x + 1, y + 1 ) )

        Down ->
            ( ( x + 1, y + 1 ), ( x, y + 1 ) )

        Left ->
            ( ( x, y + 1 ), ( x, y ) )


rotateDir : Orientation -> DirectionNum -> DirectionNum
rotateDir orient dirNum =
    case orient of
        Positive ->
            (dirNum + 1) % 4

        Negative ->
            (dirNum - 1) % 4



--


either : Maybe a -> Maybe a -> Maybe a
either x y =
    if x == Nothing then
        y
    else
        x



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
    }


borderStep : BorderDict -> StepData -> Maybe StepData
borderStep dict ({ orientation, point, dirNum } as step) =
    let
        nextPoint =
            case Dict.get ( point, dirNum ) dict of
                Just pt ->
                    pt

                Nothing ->
                    Debug.crash <| "current point (" ++ toString point ++ ") not on border"

        nextStep =
            { step | point = nextPoint }

        handleOrientation orient =
            let
                nextDirNum =
                    rotateDir orient dirNum
            in
                if Dict.member ( nextPoint, nextDirNum ) dict then
                    Just { nextStep | dirNum = nextDirNum, orientation = Just orient }
                else
                    Nothing
    in
        if Dict.member ( nextPoint, dirNum ) dict then
            Just nextStep
        else
            case orientation of
                Just orient ->
                    handleOrientation orient

                Nothing ->
                    either (handleOrientation Positive) (handleOrientation Negative)


border : StepData -> BorderDict -> ( List Corner, BorderDict )
border initial dict =
    let
        newDict =
            dict |> Dict.remove ( initial.point, initial.dirNum )
    in
        case borderStep dict initial of
            Nothing ->
                ( [ initial.point ], newDict )

            Just step ->
                let
                    ( nextBorderPoints, nextDict ) =
                        border step newDict
                in
                    ( initial.point :: nextBorderPoints, nextDict )


borders : BorderDict -> List (List Corner)
borders borderDict =
    case borderDict |> Dict.keys |> List.head of
        Nothing ->
            []

        Just ( point, dirNum ) ->
            let
                step =
                    { point = point
                    , dirNum = dirNum
                    , orientation = Nothing
                    }

                ( aBorder, newDict ) =
                    border step borderDict
            in
                aBorder :: borders newDict



--


view : Int -> Color.Color -> Area -> Svg msg
view scale fillColor area =
    let
        allBorders =
            area.points
                |> innerBorder
                |> borderEdges
                |> borders

        pointString ( x, y ) =
            ""
                ++ toString (scale * x)
                ++ ","
                ++ toString (scale * y)

        borderView border =
            polygon
                [ fill "none"
                , stroke <| colorToCssRgba area.color
                , points <| String.join " " <| List.map pointString <| border
                ]
                []

        areaView =
            Area.view scale { area | color = fillColor }
    in
        g []
            (areaView :: List.map borderView (allBorders))
