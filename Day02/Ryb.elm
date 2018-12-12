module Day2.Ryb exposing (ryb, ryba)

import Color exposing (Color)


type alias Ryba =
    { red : Float
    , yellow : Float
    , blue : Float
    , alpha : Float
    }


type alias Rgba =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


ryba : Float -> Float -> Float -> Float -> Color
ryba h s l alpha =
    ryb h s l
        |> Color.toRgba
        |> (\c -> Color.rgba c.red c.green c.blue alpha)


ryb : Float -> Float -> Float -> Color
ryb h s l =
    Color.hsl (h / 360) s l
        |> Color.toRgba
        |> rgbaAsRyba
        |> toRgba
        |> (\{ red, green, blue, alpha } -> Color.rgba red green blue alpha)


rgbaAsRyba : Rgba -> Ryba
rgbaAsRyba { red, green, blue, alpha } =
    Ryba red green blue alpha


toRgba : Ryba -> Rgba
toRgba { red, yellow, blue, alpha } =
    let
        r =
            red

        y =
            yellow

        b =
            blue
    in
    { red = (-34.935 * b * r * y + 85.935 * b * r - 41.565 * b * y - 213.435 * b + 255.0) / 255
    , green = (-176.715 * b * r * y + 159.885 * b * r + 73.185 * b * y - 159.885 * b + 127.5 * r * y - 255.0 * r + 255.0) / 255
    , blue = (-280.5 * b * r * y + 229.5 * b * r + 153.0 * b * y - 102.0 * b + 255.0 * r * y - 255.0 * r - 255.0 * y + 255.0) / 255
    , alpha = alpha
    }
