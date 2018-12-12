module Day2.Random exposing (ryb, ryb1, ryb1v1, ryb1v2, ryb1v3, ryb2v2)

import Color exposing (Color)
import Day2.Ryb as Ryb
import Random exposing (Generator, float, map, map3)


hue : Generator Float
hue =
    float 0 360


{-| A random color in ryb space.
-}
ryb : Generator Color
ryb =
    map3 Ryb.ryb
        (float 0 360)
        (float 0 1)
        (float 0 1)


{-| One random color in ryb space with predetermined saturation and lightness.
-}
ryb1 : Float -> Float -> Generator Color
ryb1 s l =
    hue |> map (\h -> Ryb.ryb h s l)


{-| Two random colors in ryb space that have opposite hues
with given saturation and lightness
-}
ryb1v1 : Float -> Float -> Generator { c1 : Color, c2 : Color }
ryb1v1 s l =
    let
        clr h =
            Ryb.ryb h s l

        colors h =
            { c1 = clr h
            , c2 = clr (h + 180)
            }
    in
    map colors hue


{-| Three random colors in ryb space such that the first is opposite to the second and third
in terms of their hues.
The second and third colors' hues are separated by `spacing`.
-}
ryb1v2 : Float -> Float -> Float -> Generator { c1 : Color, c2 : Color, c3 : Color }
ryb1v2 s l spacing =
    let
        clr h =
            Ryb.ryb h s l

        colors h =
            { c1 = clr h
            , c2 = clr (h + 180 + spacing / 2)
            , c3 = clr (h + 180 - spacing / 2)
            }
    in
    map colors hue


{-| Four random colors in ryb space such that the first is opposite to the second one
in terms of hue.
The hue of the third and fourth colors is +-`spacing` from the second color.
-}
ryb1v3 : Float -> Float -> Float -> Generator { c1 : Color, c2 : Color, c3 : Color, c4 : Color }
ryb1v3 s l spacing =
    let
        clr h =
            Ryb.ryb h s l

        colors h =
            { c1 = clr h
            , c2 = clr (h + 180 + spacing)
            , c3 = clr (h + 180)
            , c4 = clr (h + 180 - spacing)
            }
    in
    map colors hue


{-| Four random colors in ryb space.
The first is opposite to the third. The second is opposite to the fourth.
The distance between the first and the second is equal to
the distance between the third and fourth, which is equal to `spacing`.
-}
ryb2v2 : Float -> Float -> Float -> Generator { c1 : Color, c2 : Color, c3 : Color, c4 : Color }
ryb2v2 s l spacing =
    let
        clr h =
            Ryb.ryb h s l

        colors h =
            { c1 = clr (h + spacing / 2)
            , c2 = clr (h + -spacing / 2)
            , c3 = clr (h + 180 + spacing / 2)
            , c4 = clr (h + 180 - spacing / 2)
            }
    in
    map colors hue
