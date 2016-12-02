module Day2.Random exposing (ryb1, ryb1v1, ryb1v2, ryb1v3, ryb2v2)

import Day2.Ryb exposing (ryb)
import Random exposing (Generator, float, map)
import Color exposing (Color)


hue : Generator Float
hue =
    float 0 360


{-|
   One random color in ryb space.
-}
ryb1 : Float -> Float -> Generator Color
ryb1 s l =
    hue |> map (\h -> ryb h s l)


{-|
   Two random colors in ryb space that have opposite hues
-}
ryb1v1 : Float -> Float -> Generator ( Color, Color )
ryb1v1 s l =
    let
        clr h =
            ryb h s l

        colors h =
            ( clr h
            , clr (h + 180)
            )
    in
        map colors hue


{-|
   Three random colors in ryb space such that the first is opposite to the second and third
   in terms of their hues.
   The second and third colors' hues are separated by `spacing`.
-}
ryb1v2 : Float -> Float -> Float -> Generator ( Color, Color, Color )
ryb1v2 s l spacing =
    let
        clr h =
            ryb h s l

        colors h =
            ( clr h
            , clr (h + 180 + spacing / 2)
            , clr (h + 180 - spacing / 2)
            )
    in
        map colors hue


{-|
Four random colors in ryb space such that the first is opposite to the second one
in terms of hue.
The hue of the third and fourth colors is +-`spacing` from the second color.
-}
ryb1v3 : Float -> Float -> Float -> Generator ( Color, Color, Color, Color )
ryb1v3 s l spacing =
    let
        clr h =
            ryb h s l

        colors h =
            ( clr h
            , clr (h + 180 + spacing)
            , clr (h + 180)
            , clr (h + 180 - spacing)
            )
    in
        map colors hue


{-|
Four random colors in ryb space.
The first is opposite to the third. The second is opposite to the fourth.
The distance between the first and the second is equal to
the distance between the third and fourth, which is equal to `spacing`.
-}
ryb2v2 : Float -> Float -> Float -> Generator ( Color, Color, Color, Color )
ryb2v2 s l spacing =
    let
        clr h =
            ryb h s l

        colors h =
            ( clr (h + spacing / 2)
            , clr (h + -spacing / 2)
            , clr (h + 180 + spacing / 2)
            , clr (h + 180 - spacing / 2)
            )
    in
        map colors hue
