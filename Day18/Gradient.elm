module Day18.Gradient exposing (gradient, gradientStroke)

import Svg exposing (Svg, linearGradient, defs, stop)
import Svg.Attributes exposing (id, x1, y1, x2, y2, stopColor, offset)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)


gradient : String -> List Color -> Svg msg
gradient name colors =
    let
        n =
            List.length colors

        percentiles =
            List.range 0 n |> List.map (\k -> 100 * toFloat k / toFloat n)

        stops : Float -> Color -> Svg msg
        stops prcnt color =
            stop [ offset <| toString prcnt ++ "%", stopColor <| colorToCssRgb color ] []
    in
        List.map2 stops percentiles colors
            |> linearGradient [ id name, x1 <| "0%", y1 <| "0%", x2 <| "100%", y2 <| "0%" ]
            |> \grad -> defs [] [ grad ]


gradientStroke : String -> Svg.Attribute msg
gradientStroke name =
    Svg.Attributes.stroke <| "url(#" ++ name ++ ")"
