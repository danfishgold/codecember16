module Day15.View exposing (polygon)

import Color exposing (Color)
import Day15.Polyomino as Poly
import Svg exposing (Svg, polygon)
import Svg.Attributes exposing (fill, points, stroke, strokeWidth)


polygon : Float -> Color -> Poly.Point -> Poly.Word -> Svg msg
polygon scale color p0 word =
    let
        pts =
            Poly.points p0 word
                |> List.map (\( x, y ) -> ( scale * toFloat x, scale * toFloat y ))

        pointsValue =
            pts
                |> List.map (\( px, py ) -> String.fromFloat px ++ "," ++ String.fromFloat py)
                |> String.join " "

        n =
            List.length pts |> toFloat
    in
    Svg.polygon
        [ points pointsValue
        , fill <| Color.toCssString color
        , stroke "black"
        , strokeWidth "1"
        ]
        []
