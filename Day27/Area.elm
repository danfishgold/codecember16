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


makeBorder : List Corner -> Border
makeBorder pts =
    pts
        |> triplets (\a b c -> ( b, ( a, c ) ))
        |> Dict.fromList



--


view : Int -> Color -> Area -> Svg msg
view scale fillColor { border, color } =
    Svg.polygon
        [ fill <| colorToCssRgb fillColor
        , stroke <| colorToCssRgb color
        , strokeWidth "2"
        , border
            |> Dict.keys
            |> List.map (\( x, y ) -> toString (scale * x) ++ "," ++ toString (scale * y))
            |> String.join " "
            |> points
        ]
        []
