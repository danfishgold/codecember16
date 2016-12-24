module Koalas exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height, fill)
import Random
import Random.Extra exposing (sample)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Day2.Random exposing (ryb2v2)
import Time exposing (Time, every, second)
import Day24.Tree as Tree exposing (Tree)


type alias Model =
    { width : Float
    , height : Float
    , tree : Tree Color
    }


type Msg
    = Tick Time
    | Expand Tree.Index
    | Retract Tree.Index
    | SetNodes (List ( Tree.Index, Tree Color ))


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , tree = Tree.Leaf Color.gray
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


randomAction : Model -> Cmd Msg
randomAction model =
    sample
        [ \() ->
            Tree.rootIndexes model.tree
                |> sample
                |> Random.map (Maybe.withDefault [])
                |> Random.map Retract
        , \() ->
            Tree.leafIndexes model.tree
                |> sample
                |> Random.map (Maybe.withDefault [])
                |> Random.map Expand
        ]
        |> Random.map (Maybe.withDefault <| Debug.crash "UGHH")
        |> Random.andThen (\lazy -> lazy ())
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
            ( model, Cmd.none )

        Retract idx ->
            ( model, Cmd.none )

        SetNodes nodes ->
            ( { model | tree = setNodes nodes model.tree }, Cmd.none )



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
        , view = view
        }
