module Day06.Array2D exposing (Array2D, elementsInSubArray, empty, get, set)

import Array exposing (Array)


type alias Array2D a =
    Array (Array (Maybe a))


empty : Int -> Int -> Array2D a
empty n m =
    Array.repeat n (Array.repeat m Nothing)


get : Int -> Int -> Array2D a -> Maybe a
get i j arr =
    Array.get i arr
        |> Maybe.andThen (Array.get j)
        |> Maybe.andThen identity


set : Int -> Int -> Maybe a -> Array2D a -> Array2D a
set i j a arr =
    case Array.get i arr of
        Nothing ->
            arr

        Just row ->
            Array.set i (Array.set j a row) arr


elementsInSubArray : Array2D a -> List Int -> List Int -> List a
elementsInSubArray arr rows cols =
    case rows of
        [] ->
            []

        i :: otherRows ->
            elementsInSubArray arr otherRows cols
                ++ List.filterMap (\j -> get i j arr) cols
