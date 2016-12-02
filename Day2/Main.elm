module Main exposing (..)

import Html exposing (beginnerProgram)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height, fill)
import Day2.Ryb exposing (ryb)
import Color exposing (Color)


main : Program Never {} msg
main =
    beginnerProgram
        { model = {}
        , update = flip always
        , view = view
        }


colorToString : Color -> String
colorToString color =
    color
        |> Color.toRgb
        |> (\{ red, green, blue } ->
                "rgb("
                    ++ toString red
                    ++ ", "
                    ++ toString green
                    ++ ", "
                    ++ toString blue
                    ++ ")"
           )


view : {} -> Svg msg
view _ =
    let
        n =
            24

        colors =
            List.range 0 n
                |> List.map (\i -> ryb (toFloat (360 // n * i)) 1 0.5)
                |> List.map colorToString

        square i color =
            rect [ x <| toString <| 50 * i, y <| "0", width "50", height "50", fill color ] []
    in
        colors
            |> List.indexedMap square
            |> svg [ width "100%", height "100%" ]
