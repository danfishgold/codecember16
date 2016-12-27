module Day27.Area exposing (..)

import Color exposing (Color)
import Svg exposing (Svg, polygon)
import Svg.Attributes exposing (fill, stroke, strokeWidth, points)
import Color.Convert exposing (colorToCssRgb)
import Dict exposing (Dict)
import Set exposing (Set)
import Array exposing (Array)


colors : Array Color
colors =
    Array.fromList
        [ Color.red
        , Color.blue
        , Color.green
        , Color.purple
        , Color.brown
        ]


type alias Area =
    { inner : Set Corner
    , border : Dict Corner ( Corner, Corner )
    , color : Color
    }


type alias Border =
    Dict Corner ( Corner, Corner )


type alias Corner =
    ( Int, Int )


type alias Center =
    ( Int, Int )


type Shape
    = Square
    | Cross



-- BORDER BUSINESS


triplets : (a -> a -> a -> b) -> List a -> List b
triplets fn xs =
    let
        withoutOverflow xs =
            case xs of
                a1 :: a2 :: a3 :: rest ->
                    fn a1 a2 a3 :: withoutOverflow (a2 :: a3 :: rest)

                _ ->
                    []
    in
        case xs of
            a1 :: a2 :: a3 :: rest ->
                withoutOverflow (xs ++ [ a1, a2 ])

            _ ->
                []


borderFromList : List Corner -> Border
borderFromList pts =
    pts
        |> triplets (\a b c -> ( b, ( a, c ) ))
        |> Dict.fromList


crashOnNothing : String -> Maybe a -> a
crashOnNothing msg x =
    case x of
        Nothing ->
            Debug.crash msg

        Just val ->
            val


listFromBorder : Border -> List Corner
listFromBorder border =
    let
        p0 =
            border
                |> Dict.keys
                |> List.head
                |> crashOnNothing "empty area border"

        next p =
            border
                |> Dict.get p
                |> crashOnNothing "point not in area"
                |> Tuple.second

        fn p =
            next p
                |> \p1 ->
                    if p1 == p0 then
                        [ p ]
                    else
                        p :: fn p1
    in
        fn p0


shapeAround : Color -> Shape -> Center -> Area
shapeAround color shape ( x, y ) =
    let
        borderPoints =
            case shape of
                Square ->
                    [ ( x, y ), ( x, y + 1 ), ( x + 1, y + 1 ), ( x + 1, y ) ]

                Cross ->
                    [ ( x, y ), ( x, y - 1 ), ( x + 1, y - 1 ), ( x + 1, y ), ( x + 2, y ), ( x + 2, y + 1 ), ( x + 1, y + 1 ), ( x + 1, y + 2 ), ( x, y + 2 ), ( x, y + 1 ), ( x - 1, y + 1 ), ( x - 1, y ) ]
    in
        { inner = Set.empty
        , border = borderFromList borderPoints
        , color = color
        }



-- MERGING


reverseBorder : Border -> Border
reverseBorder border =
    border |> Dict.map (\_ ( before, after ) -> ( after, before ))


isEncased : Area -> Area -> Bool
isEncased a b =
    Dict.diff a.border b.border
        |> Dict.keys
        |> Set.fromList
        |> \notInBorder -> Set.diff notInBorder b.inner |> Set.isEmpty


addToAreas : List Area -> Area -> List Area
addToAreas areas newArea =
    case areas of
        [] ->
            [ newArea ]

        area :: rest ->
            case merge area newArea of
                Just merged ->
                    addToAreas rest merged

                Nothing ->
                    area :: addToAreas rest newArea


merge : Area -> Area -> Maybe Area
merge a b =
    if isEncased a b then
        Just b
    else if isEncased b a then
        Just a
    else
        let
            rangesInA =
                intersectionRangesInA a b
        in
            if List.isEmpty rangesInA then
                Nothing
            else
                Nothing



-- a, b, startInA, endInA, joint -> jointWithFixedBorder


intersectionRangesInA : Area -> Area -> List ( Corner, Corner )
intersectionRangesInA a b =
    let
        isStart p =
            a.border
                |> Dict.get p
                |> crashOnNothing "point not in first area"
                |> Tuple.first
                |> \aBefore ->
                    not (Dict.member aBefore b.border)

        findEnd start =
            let
                next =
                    Dict.get start a.border |> crashOnNothing "point not in first area" |> Tuple.second
            in
                if Dict.member next b.border then
                    findEnd next
                else
                    next
    in
        Dict.intersect a.border b.border
            |> Dict.keys
            |> List.filter isStart
            |> List.map (\start -> ( start, findEnd start ))


mergeIntersection : Corner -> Area -> Area -> ( Area, Area )
mergeIntersection p a b =
    let
        ( aBefore, aAfter ) =
            Dict.get p a.border |> crashOnNothing "point not in first area"

        ( bBefore, bAfter ) =
            Dict.get p b.border |> crashOnNothing "point not in second area"
    in
        if Dict.member aBefore b.border then
            mergeIntersection aBefore a b
        else if aAfter == bAfter then
            mergeIntersection p a { b | border = reverseBorder b.border }
        else if aAfter == bBefore then
            ( a, b )
        else if Set.member aAfter b.inner then
            ( a, b )
        else
            ( a, b )


glueMutualBorders : Area -> Area -> List Corner -> Area
glueMutualBorders a b mutualBorder =
    let
        c =
            { inner = Set.union a.inner b.inner
            , color = a.color
            , border = Dict.empty
            }
    in
        a



-- VIEW


view : Int -> Color -> Area -> Svg msg
view scale fillColor { border, color } =
    Svg.polygon
        [ fill <| colorToCssRgb fillColor
        , stroke <| colorToCssRgb color
        , strokeWidth "2"
        , border
            |> listFromBorder
            |> List.map (\( x, y ) -> toString (scale * x) ++ "," ++ toString (scale * y))
            |> String.join " "
            |> points
        ]
        []
