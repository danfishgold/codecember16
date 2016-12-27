module Day27.Area exposing (..)

import Color exposing (Color)
import Svg exposing (Svg, polygon)
import Svg.Attributes exposing (fill, stroke, strokeWidth, points)
import Color.Convert exposing (colorToCssRgb)
import Dict exposing (Dict)
import Set exposing (Set)


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



--


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



--


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
