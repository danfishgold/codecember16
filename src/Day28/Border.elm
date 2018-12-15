module Day28.Border exposing (view)

import Color
import Day27.Area as Area exposing (Area, Center)
import Dict exposing (Dict)
import Set exposing (Set)
import Svg exposing (Svg, g, polygon)
import Svg.Attributes exposing (fill, points, stroke, strokeWidth)


type alias Corner =
    ( Int, Int )


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

        -- This should be 3, but since this is an internal function, I know it
        -- won't ever be anything other than 0,1,2,3
        _ ->
            Left


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


possibleDirections : DirectionNum -> List DirectionNum
possibleDirections dirNum =
    [ dirNum + 1, dirNum + 3, dirNum ] |> List.map (\i -> modBy 4 i)



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
        borders_ dirNum =
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
        |> List.map borders_
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


borderStep : BorderDict -> ( Corner, DirectionNum ) -> Maybe ( Corner, DirectionNum )
borderStep dict ( point, dirNum ) =
    let
        nextPoint =
            case Dict.get ( point, dirNum ) dict of
                Just pt ->
                    pt

                Nothing ->
                    Debug.todo <| "current point (" ++ Debug.toString point ++ ") not on border"

        handleDirection nextDirNum =
            if Dict.member ( nextPoint, nextDirNum ) dict then
                Just ( nextPoint, nextDirNum )

            else
                Nothing
    in
    possibleDirections dirNum
        |> List.map handleDirection
        |> List.foldl either Nothing


border : ( Corner, DirectionNum ) -> BorderDict -> ( List Corner, BorderDict )
border ( point, dirNum ) dict =
    let
        newDict =
            dict |> Dict.remove ( point, dirNum )
    in
    case borderStep dict ( point, dirNum ) of
        Nothing ->
            ( [ point ], newDict )

        Just step ->
            let
                ( nextBorderPoints, nextDict ) =
                    border step newDict
            in
            ( point :: nextBorderPoints, nextDict )


borders : BorderDict -> List (List Corner)
borders borderDict =
    case borderDict |> Dict.keys |> List.head of
        Nothing ->
            []

        Just key ->
            let
                ( aBorder, newDict ) =
                    border key borderDict
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
                ++ String.fromInt (scale * x)
                ++ ","
                ++ String.fromInt (scale * y)

        borderView border_ =
            polygon
                [ fill "none"
                , stroke <| Color.toCssString area.color
                , strokeWidth "2"
                , points <| String.join " " <| List.map pointString <| border_
                ]
                []

        areaView =
            Area.view scale { area | color = fillColor }
    in
    g []
        (areaView :: List.map borderView allBorders)
