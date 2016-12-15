module Day15.View exposing (polygon)

import Svg exposing (Svg, polygon)
import Svg.Attributes exposing (points, fill, stroke, strokeWidth)
import Day15.Polyomino as Poly
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)


polygon : Float -> Color -> Poly.Point -> Poly.Word -> Svg msg
polygon scale color p0 word =
    let
        pts =
            Poly.points p0 word
                |> List.map (\( x, y ) -> ( scale * toFloat x, scale * toFloat y ))

        pointsValue =
            pts
                |> List.map (\( px, py ) -> toString px ++ "," ++ toString py)
                |> String.join " "

        n =
            List.length pts |> toFloat
    in
        Svg.polygon
            [ points pointsValue
            , fill <| colorToCssRgb color
            , stroke "black"
            , strokeWidth "1"
            ]
            []
