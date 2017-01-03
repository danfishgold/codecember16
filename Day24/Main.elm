module Koalas exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height, fill)
import Random
import Random.Extra exposing (sample)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Day2.Random exposing (ryb1, ryb2v2)
import Time exposing (Time, every, second)
import Day24.Tree as Tree exposing (Tree(..))


type alias Model =
    { width : Float
    , height : Float
    , tree : Tree Color
    , maxDepth : Int
    }


type Msg
    = Tick Time
    | Expand Tree.Index
    | Retract Tree.Index
    | SetNode ( Tree.Index, Tree Color )


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , tree = Leaf Color.gray
      , maxDepth = 6
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    every (0.1 * second) Tick



--


randomAction : Model -> Cmd Msg
randomAction model =
    let
        randomLeafIdx =
            Tree.leafIndexes model.tree
                |> List.filter (\idx -> List.length idx <= model.maxDepth)
                |> sample
                |> Random.map (Maybe.withDefault [])

        oneLevelUp idx =
            List.take (List.length idx - 1) idx
    in
        Random.Extra.frequency
            [ ( 1, randomLeafIdx |> Random.map oneLevelUp |> Random.map Retract )
            , ( 12, randomLeafIdx |> Random.map Expand )
            ]
            |> Random.generate identity


setNodes : List ( Tree.Index, Tree a ) -> Tree a -> Tree a
setNodes nodes tree =
    nodes
        |> List.foldl
            (\( idx, node ) tr ->
                Tree.set idx node tr
                    |> Maybe.withDefault tr
            )
            tree


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model, randomAction model )

        Expand idx ->
            let
                node ( c1, c2, c3, c4 ) =
                    Tree (Leaf c1) (Leaf c2) (Leaf c3) (Leaf c4)
                        |> \node -> ( idx, node )
            in
                ( model, ryb2v2 1 0.5 45 |> Random.map node |> Random.generate SetNode )

        Retract idx ->
            ( model, ryb1 1 0.5 |> Random.map (\c -> ( idx, Leaf c )) |> Random.generate SetNode )

        SetNode ( idx, node ) ->
            ( { model | tree = model.tree |> Tree.set idx node |> Maybe.withDefault model.tree }, Cmd.none )



--


view : Model -> Svg Msg
view model =
    let
        location idxs =
            case idxs of
                [] ->
                    ( 0, 0 )

                i :: rest ->
                    location rest
                        |> \( x, y ) ->
                            ( 0.5 * toFloat (i % 2) + 0.5 * x
                            , 0.5 * toFloat (i // 2) + 0.5 * y
                            )

        nodeRect i c =
            let
                ( x0, y0 ) =
                    location i

                side =
                    List.length i |> toFloat |> \n -> 0.5 ^ n
            in
                rect
                    [ x <| toString <| model.width * x0
                    , y <| toString <| model.height * y0
                    , width <| toString <| model.width * side
                    , height <| toString <| model.height * side
                    , fill <| colorToCssRgb c
                    ]
                    []
    in
        model.tree
            |> Tree.indexedMap nodeRect
            |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view |> project 24
        }
