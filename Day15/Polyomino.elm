module Day15.Polyomino
    exposing
        ( Letter(..)
        , Word
        , Point
        , bn
        , isRegular
        , points
        , random
        , randomBN
        )

import Set
import Random
import Random.Extra exposing (sample, constant)


type Letter
    = U
    | D
    | L
    | R


type alias Word =
    List Letter


type alias Point =
    ( Int, Int )


co : Letter -> Letter
co letter =
    case letter of
        U ->
            D

        D ->
            U

        L ->
            R

        R ->
            L


delta : Letter -> Point
delta letter =
    case letter of
        U ->
            ( 0, 1 )

        D ->
            ( 0, -1 )

        L ->
            ( -1, 0 )

        R ->
            ( 1, 0 )


complement : Word -> Word
complement word =
    word
        |> List.map co
        |> List.reverse


add : Point -> Point -> Point
add ( x1, y1 ) ( x2, y2 ) =
    ( x1 + x2, y1 + y2 )


points : Point -> Word -> List Point
points origin word =
    case word of
        [] ->
            [ origin ]

        hd :: tl ->
            origin :: points (add origin (delta hd)) tl


bn : ( Word, Word, Word ) -> Word
bn ( a, b, c ) =
    a ++ b ++ c ++ complement a ++ complement b ++ complement c


isRegular : Word -> Bool
isRegular word =
    let
        pts =
            points ( 0, 0 ) word

        list =
            pts |> List.length

        set =
            Set.fromList pts |> Set.size

        ( first, last ) =
            ( List.take 1 pts, List.drop (list - 1) pts )
    in
        list == set + 1 && first == last



--


randomLetter : Random.Generator Letter
randomLetter =
    sample [ U, D, L, R ] |> Random.map (Maybe.withDefault U)


randomWord : Int -> Int -> Random.Generator Word
randomWord min max =
    Random.Extra.rangeLengthList min max randomLetter


random : Int -> Int -> Random.Generator Word
random min max =
    randomWord min max
        |> Random.Extra.filter isRegular


randomBN : Int -> Int -> Random.Generator ( Word, Word, Word )
randomBN min max =
    Random.map3 (,,)
        (randomWord min max)
        (randomWord min max)
        (randomWord min max)
        |> Random.Extra.filter (bn >> isRegular)
