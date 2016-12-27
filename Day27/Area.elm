module Day27.Area exposing (..)

import Color exposing (Color)
import Svg exposing (Svg, polygon)
import Svg.Attributes exposing (fill, x, y, width, height)
import Color.Convert exposing (colorToCssRgb)
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
    { points : Set Center
    , color : Color
    }


type alias Center =
    ( Int, Int )


type Shape
    = Square
    | Cross


shapeAround : Color -> Shape -> Center -> Area
shapeAround color shape ( x, y ) =
    let
        points =
            case shape of
                Square ->
                    [ ( x, y ) ]

                Cross ->
                    [ ( x, y ), ( x - 1, y ), ( x + 1, y ), ( x, y - 1 ), ( x, y + 1 ) ]
    in
        { points = Set.fromList points
        , color = color
        }



-- MERGING


addToAreas : List Area -> Area -> List Area
addToAreas areas newArea =
    let
        colorI =
            List.length areas % Array.length colors

        color =
            Array.get colorI colors |> Maybe.withDefault newArea.color

        tryMerging existing area =
            case existing of
                [] ->
                    [ area ]

                first :: rest ->
                    case merge first area of
                        Just merged ->
                            tryMerging rest merged

                        Nothing ->
                            first :: tryMerging rest area
    in
        tryMerging areas { newArea | color = color }


merge : Area -> Area -> Maybe Area
merge a b =
    let
        deltas =
            [ ( 0, 0 ), ( 0, 1 ), ( 0, -1 ), ( 1, 0 ), ( -1, 0 ) ]

        moveBy ( dx, dy ) set =
            set |> Set.map (\( x, y ) -> ( x + dx, y + dy ))

        ( setToShift, setToStay ) =
            if Set.size a.points < Set.size b.points then
                ( a.points, b.points )
            else
                ( b.points, a.points )

        doesIntersect delta =
            Set.intersect (moveBy delta setToShift) setToStay |> Set.isEmpty |> not

        anyIntersection =
            List.any doesIntersect deltas
    in
        if anyIntersection then
            Just <| { points = Set.union setToShift setToStay, color = a.color }
        else
            Nothing



-- VIEW


view : Int -> Color -> Area -> Svg msg
view scale fillColor { points, color } =
    let
        pixel ( x0, y0 ) =
            Svg.rect
                [ x <| toString <| scale * x0
                , y <| toString <| scale * y0
                , width <| toString scale
                , height <| toString scale
                , fill <| colorToCssRgb color
                ]
                []
    in
        points
            |> Set.toList
            |> List.map pixel
            |> Svg.g []
