module Day19.Trail exposing (Trail, random, update, view)

import Color exposing (Color)
import Random exposing (constant)
import Random.Char
import Random.Extra exposing (combine, oneIn)
import Svg exposing (Svg, g, text, text_)
import Svg.Attributes exposing (fill, fontFamily, fontSize, x, y)


characters : List Char
characters =
    [ 'a', 'b', 'c', 'd' ]


randomLetter : Random.Generator Char
randomLetter =
    Random.Char.latinExtendedA


type alias Trail =
    { letters : List Char
    , pace : Int
    , x : Int
    , y : Int
    }


random : Int -> Int -> Random.Generator Trail
random width letterCount =
    let
        letters =
            Random.list letterCount randomLetter

        pace =
            Random.int 5 10

        x =
            Random.int 0 (width - 1)

        trail letters_ pace_ x_ =
            { letters = letters_, pace = pace_, x = x_, y = -1 }
    in
    Random.map3 trail
        letters
        pace
        x


update : Int -> Trail -> Random.Generator Trail
update time trail =
    if modBy trail.pace time == 0 then
        let
            movedTrail =
                { trail | y = trail.y + 1 }

            maybeChangeLetter k letter =
                oneIn (k + 5)
                    |> Random.andThen
                        (\change ->
                            if change then
                                randomLetter

                            else
                                constant letter
                        )

            addNewLetter letters =
                randomLetter
                    |> Random.map (\new -> new :: letters |> List.take (List.length letters))
        in
        trail.letters
            |> List.indexedMap maybeChangeLetter
            |> combine
            |> Random.andThen addNewLetter
            |> Random.map (\letters -> { movedTrail | letters = letters })

    else
        constant trail


letterColor : Int -> Int -> Color
letterColor n k =
    Color.hsl (1 / 3) 1 (toFloat (n - k) / toFloat n)


view : String -> Float -> Trail -> Svg msg
view font letterSize trail =
    let
        n =
            List.length trail.letters

        letterView x0 y0 k letter =
            text_
                [ fontFamily font
                , fontSize <| String.fromFloat letterSize
                , fill <| Color.toCssString <| letterColor n k
                , x <| String.fromFloat <| toFloat x0 * letterSize
                , y <| String.fromFloat <| toFloat (y0 - k) * letterSize
                ]
                [ text <| String.fromChar letter ]
    in
    trail.letters
        |> List.indexedMap (letterView trail.x trail.y)
        |> g []
