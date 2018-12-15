module Day18.Gradient exposing (gradient, gradientStroke)

import Color exposing (Color)
import Svg exposing (Svg, defs, linearGradient, stop)
import Svg.Attributes exposing (gradientUnits, id, offset, stopColor, x1, x2, y1, y2)


gradient : String -> ( ( Float, Float, Color ), ( Float, Float, Color ) ) -> Svg msg
gradient name ( ( xa, ya, ca ), ( xb, yb, cb ) ) =
    let
        stops =
            [ stop [ offset "0%", stopColor <| Color.toCssString ca ] []
            , stop [ offset "100%", stopColor <| Color.toCssString cb ] []
            ]
    in
    linearGradient
        [ id name
        , x1 <| String.fromFloat xa
        , y1 <| String.fromFloat ya
        , x2 <| String.fromFloat xb
        , y2 <| String.fromFloat yb
        , gradientUnits "userSpaceOnUse"
        ]
        stops


gradientStroke : String -> Svg.Attribute msg
gradientStroke name =
    Svg.Attributes.stroke <| "url(#" ++ name ++ ")"
