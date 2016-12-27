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
    newArea :: areas



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
