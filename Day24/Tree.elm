module Day24.Tree exposing
    ( Index
    , Tree(..)
    , get
    , indexedMap
    , leafIndexes
    , map
    , rootIndexes
    , set
    , size
    , update
    )


type Tree a
    = Leaf a
    | Tree (Tree a) (Tree a) (Tree a) (Tree a)


type alias Index =
    List Int


size : Tree a -> Int
size tree =
    case tree of
        Tree a b c d ->
            [ a, b, c, d ] |> List.map size |> List.sum

        Leaf _ ->
            1


get : Index -> Tree a -> Maybe a
get idx tree =
    case ( idx, tree ) of
        ( [], Leaf x ) ->
            Just x

        ( i, Leaf x ) ->
            Nothing

        ( 0 :: rest, Tree x _ _ _ ) ->
            get rest x

        ( 1 :: rest, Tree _ x _ _ ) ->
            get rest x

        ( 2 :: rest, Tree _ _ x _ ) ->
            get rest x

        ( 3 :: rest, Tree _ _ _ x ) ->
            get rest x

        _ ->
            Nothing


update : Index -> (Tree a -> Tree a) -> Tree a -> Maybe (Tree a)
update idx fn tree =
    case ( idx, tree ) of
        ( [], node ) ->
            Just (fn node)

        ( i, Leaf x ) ->
            Nothing

        ( 0 :: rest, Tree x b c d ) ->
            update rest fn x
                |> Maybe.map (\a -> Tree a b c d)

        ( 1 :: rest, Tree a x c d ) ->
            update rest fn x
                |> Maybe.map (\b -> Tree a b c d)

        ( 2 :: rest, Tree a b x d ) ->
            update rest fn x
                |> Maybe.map (\c -> Tree a b c d)

        ( 3 :: rest, Tree a b c x ) ->
            update rest fn x
                |> Maybe.map (\d -> Tree a b c d)

        _ ->
            Nothing


set : Index -> Tree a -> Tree a -> Maybe (Tree a)
set idx node tree =
    update idx (always node) tree


map : (a -> b) -> Tree a -> List b
map fn tree =
    case tree of
        Leaf x ->
            [ fn x ]

        Tree a b c d ->
            [ a, b, c, d ] |> List.concatMap (map fn)


indexedMap : (Index -> a -> b) -> Tree a -> List b
indexedMap fn tree =
    let
        sub fn_ idx tree_ =
            case tree_ of
                Leaf x ->
                    [ fn_ idx x ]

                Tree a b c d ->
                    [ a, b, c, d ]
                        |> List.indexedMap (\i t -> sub fn_ (idx ++ [ i ]) t)
                        |> List.concatMap identity
    in
    sub fn [] tree


leafIndexes : Tree a -> List Index
leafIndexes tree =
    case tree of
        Leaf _ ->
            [ [] ]

        Tree a b c d ->
            [ a, b, c, d ]
                |> List.indexedMap (\i sub -> leafIndexes sub |> List.map ((::) i))
                |> List.concat


isLeaf : Tree a -> Bool
isLeaf tree =
    case tree of
        Leaf _ ->
            True

        _ ->
            False


rootIndexes : Tree a -> List Index
rootIndexes tree =
    case tree of
        Leaf _ ->
            []

        Tree a b c d ->
            [ a, b, c, d ]
                |> List.indexedMap (\i sub -> rootIndexes sub |> List.map ((::) i))
                |> List.concat
                |> (++) []
