module Day15.Polyomino exposing (..)

import Html exposing (text)
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


isRegular : Word -> Word -> Word -> Bool
isRegular a b c =
    let
        word =
            bn ( a, b, c )

        pts =
            points ( 0, 0 ) word

        list =
            pts |> List.length

        set =
            Set.fromList pts |> Set.size
    in
        list == set + 1


bn : Word -> Word -> Word -> Word
bn a b c =
    a ++ b ++ c ++ complement a ++ complement b ++ complement c



--


randomLetter : Random.Generator Letter
randomLetter =
    sample [ U, D, L, R ] |> Random.map (Maybe.withDefault U)


randomWord : Int -> Int -> Random.Generator Word
randomWord min max =
    Random.Extra.rangeLengthList min max randomLetter


randomBN : Int -> Int -> Random.Generator ( Word, Word, Word )
randomBN min max =
    Random.map3 (,,)
        (randomWord min max)
        (randomWord min max)
        (randomWord min max)
        |> Random.Extra.filter (\( a, b, c ) -> isRegular a b c)



--


word : Word
word =
    Random.step (randomBN 1 3) (Random.initialSeed 2) |> Tuple.first |> bn


main =
    text <| toString <| points ( 0, 0 ) word
