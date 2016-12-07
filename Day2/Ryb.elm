module Day2.Ryb exposing (ryb, ryba)

import Color exposing (Color)


type alias Ryba =
    { red : Int
    , yellow : Int
    , blue : Int
    , alpha : Float
    }


type alias Rgba =
    { red : Int
    , green : Int
    , blue : Int
    , alpha : Float
    }


ryba : Float -> Float -> Float -> Float -> Color
ryba h s l alpha =
    ryb h s l
        |> Color.toRgb
        |> \c -> Color.rgba c.red c.green c.blue alpha


ryb : Float -> Float -> Float -> Color
ryb h s l =
    Color.hsl (degrees h) s l
        |> Color.toRgb
        |> rgbaAsRyba
        |> toRgba
        |> \{ red, green, blue, alpha } -> Color.rgba red green blue alpha


rgbaAsRyba : Rgba -> Ryba
rgbaAsRyba { red, green, blue, alpha } =
    Ryba red green blue alpha


toRgba : Ryba -> Rgba
toRgba { red, yellow, blue, alpha } =
    let
        r =
            toFloat red / 255

        y =
            toFloat yellow / 255

        b =
            toFloat blue / 255
    in
        { red = floor <| -34.935 * b * r * y + 85.935 * b * r - 41.565 * b * y - 213.435 * b + 255.0
        , green = floor <| -176.715 * b * r * y + 159.885 * b * r + 73.185 * b * y - 159.885 * b + 127.5 * r * y - 255.0 * r + 255.0
        , blue = floor <| -280.5 * b * r * y + 229.5 * b * r + 153.0 * b * y - 102.0 * b + 255.0 * r * y - 255.0 * r - 255.0 * y + 255.0
        , alpha = alpha
        }
