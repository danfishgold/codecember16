module Day2.Ryb exposing (ryb)

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


ryb : Float -> Float -> Float -> Color
ryb h s l =
    Color.hsl (degrees h) s l
        |> Debug.log "ryb"
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
            toFloat red

        y =
            toFloat yellow

        b =
            toFloat blue
    in
        { red = floor <| -2.10688196847366e-6 * b * r * y + 0.00132156862745098 * b * r - 0.00063921568627451 * b * y - 0.837 * b + 255.0
        , green = floor <| -1.06574394463668e-5 * b * r * y + 0.00245882352941176 * b * r + 0.00112549019607843 * b * y - 0.627 * b + 0.00196078431372549 * r * y - 1.0 * r + 255.0
        , blue = floor <| -1.69165705497885e-5 * b * r * y + 0.00352941176470588 * b * r + 0.00235294117647059 * b * y - 0.4 * b + 0.00392156862745098 * r * y - 1.0 * r - 1.0 * y + 255.0
        , alpha = alpha
        }
