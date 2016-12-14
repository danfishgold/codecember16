module Day15.Polyomino exposing (..)

import Html exposing (text)
import Set


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


isRegular : Word -> Bool
isRegular word =
    let
        pts =
            points ( 0, 0 ) word

        list =
            pts |> List.length

        set =
            Set.fromList pts |> Set.size
    in
        list == set + 1


main =
    text <| toString <| points ( 0, 0 ) [ U, U, L, D, L, D, R, R ]
