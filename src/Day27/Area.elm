module Day27.Area exposing (Area, Center, Shape(..), addToAreas, colors, merge, shapeAround, view)

import Array exposing (Array)
import Color exposing (Color)
import Set exposing (Set)
import Svg exposing (Svg, polygon)
import Svg.Attributes exposing (fill, height, width, x, y)


colors : Array Color
colors =
    Array.fromList
        [ Color.red
        , Color.blue
        , Color.green
        , Color.purple
        , Color.brown
        , Color.orange
        , Color.yellow
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


shapeAround : Shape -> Center -> Set Center
shapeAround shape ( x, y ) =
    case shape of
        Square ->
            Set.fromList [ ( x, y ) ]

        Cross ->
            Set.fromList
                [ ( x, y )
                , ( x - 1, y )
                , ( x + 1, y )
                , ( x, y - 1 )
                , ( x, y + 1 )
                ]



-- MERGING


addToAreas : List Area -> Set Center -> List Area
addToAreas areas points =
    let
        existingColors =
            areas |> List.map .color

        colorI =
            modBy (Array.length colors) (List.length areas)

        color =
            colors
                |> Array.filter (\c -> List.member c existingColors |> not)
                |> Array.get 0
                |> Maybe.withDefault
                    (Array.get colorI colors
                        |> Maybe.withDefault Color.black
                    )

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
    tryMerging areas { points = points, color = color }


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


view : Int -> Area -> Svg msg
view scale { points, color } =
    let
        pixel ( x0, y0 ) =
            Svg.rect
                [ x <| String.fromInt <| scale * x0
                , y <| String.fromInt <| scale * y0
                , width <| String.fromInt scale
                , height <| String.fromInt scale
                , fill <| Color.toCssString color
                ]
                []
    in
    points
        |> Set.toList
        |> List.map pixel
        |> Svg.g []
