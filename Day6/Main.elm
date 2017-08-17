module Poisson exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Set exposing (Set)
import Array exposing (Array)
import Random exposing (Generator)
import Random.Set
import Random.Extra
import Color exposing (Color, black, red)
import Color.Convert exposing (colorToCssRgb)
import Time exposing (Time, every, second)
import AnimationFrame
import Day6.Array2D as Array2D exposing (Array2D)
import Helper exposing (project)


type alias Point =
    ( Float, Float )


type alias Model =
    { activeList : Set Int
    , grid : Array2D Int
    , background : Array ( Point, Time )
    , r : Float
    , k : Int
    , time : Time
    , animationTime : Time
    }


type Msg
    = Tick Time
    | HandleActivePoint (Maybe Int)
    | HandleCandidates Int (List Point)
    | MakeAlgorithmStep


init : Int -> Float -> ( Model, Cmd Msg )
init k r =
    let
        gridCellSize =
            r / sqrt 2

        gridCellCount =
            ceiling (1 / gridCellSize)
    in
        ( { activeList = Set.empty
          , grid = Array2D.empty gridCellCount gridCellCount
          , background = Array.empty
          , r = r
          , k = k
          , time = 0
          , animationTime = 0.2 * second
          }
            |> addActive ( 0, 0 )
        , Cmd.none
        )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    if Set.isEmpty model.activeList then
        if
            model.background
                |> Array.map Tuple.second
                |> Array.toList
                |> List.any (\t -> model.time < t + model.animationTime + 0.1 * second)
        then
            AnimationFrame.diffs Tick
        else
            Sub.none
    else
        Sub.batch
            [ AnimationFrame.diffs Tick
            , Time.every (0.001 * second) (always MakeAlgorithmStep)
            ]



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ r, k, activeList, background, grid } as model) =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt }, Cmd.none )

        MakeAlgorithmStep ->
            ( model
            , Random.generate HandleActivePoint
                (Random.Set.sample activeList)
            )

        HandleActivePoint maybePointIndex ->
            let
                pointFromIndex idx =
                    Array.get idx background
                        |> Maybe.map Tuple.first

                idxAndPoint =
                    Maybe.map2 (,)
                        maybePointIndex
                        (maybePointIndex |> Maybe.andThen pointFromIndex)
            in
                case idxAndPoint of
                    Nothing ->
                        ( model, Cmd.none )

                    Just ( pIdx, p ) ->
                        ( model
                        , Random.generate
                            (HandleCandidates pIdx)
                            (Random.list k (randomPointAround r p))
                        )

        HandleCandidates pIdx qs ->
            case findFirst (gridIdxsIfFarEnough model) qs of
                Nothing ->
                    ( { model | activeList = Set.remove pIdx activeList }
                    , Cmd.none
                    )

                Just q ->
                    ( model |> addActive q, Cmd.none )


gridIdxs : Model -> Point -> ( Int, Int )
gridIdxs { r } ( x, y ) =
    ( floor (x / r * sqrt 2), floor (y / r * sqrt 2) )


addActive : Point -> Model -> Model
addActive pt ({ background, activeList, grid, time } as model) =
    let
        idx =
            Array.length background

        ( i, j ) =
            gridIdxs model pt
    in
        { model
            | background = Array.push ( pt, time ) background
            , activeList = Set.insert idx activeList
            , grid = Array2D.set i j (Just idx) grid
        }


findFirst : (a -> Maybe b) -> List a -> Maybe a
findFirst fn lst =
    case lst of
        [] ->
            Nothing

        hd :: tl ->
            case fn hd of
                Nothing ->
                    findFirst fn tl

                Just ans ->
                    Just hd


isPointInBox : Point -> Bool
isPointInBox ( x, y ) =
    0 <= x && x <= 1 && 0 <= y && y <= 1


randomPointAround : Float -> Point -> Generator Point
randomPointAround r ( x, y ) =
    let
        rad =
            Random.float 1 4 |> Random.map (\f -> r * sqrt f)

        arg =
            Random.float 0 (2 * pi)

        pt r t =
            ( x + r * cos t, y + r * sin t )
    in
        Random.map2 pt rad arg
            |> Random.Extra.filter isPointInBox


gridIdxsIfFarEnough : Model -> Point -> Maybe ( Int, Int )
gridIdxsIfFarEnough ({ r, grid, background } as model) q =
    let
        ( i, j ) =
            gridIdxs model q

        pointsAreFar ( x1, y1 ) ( x2, y2 ) =
            (x1 - x2) ^ 2 + (y1 - y2) ^ 2 > r ^ 2
    in
        case Array2D.get i j grid of
            Just _ ->
                Nothing

            Nothing ->
                if
                    Array2D.elementsInSubArray grid
                        (List.range (i - 2) (i + 2))
                        (List.range (j - 2) (j + 2))
                        |> List.filterMap (flip Array.get background)
                        |> List.all (\( pt, _ ) -> pointsAreFar pt q)
                then
                    Just ( i, j )
                else
                    Nothing



--


view : Float -> Float -> Model -> Html Msg
view wd ht model =
    let
        circle c ( ( x, y ), t0 ) =
            Svg.circle
                [ cx <| toString (wd * x)
                , cy <| toString (ht * y)
                , r <| toString <| min 2 <| (model.time - t0) / model.animationTime
                , fill <| colorToCssRgb c
                ]
                []
    in
        svg
            [ width <| toString wd
            , height <| toString ht
            ]
            [ model.background
                |> Array.toList
                |> List.map (circle black)
                |> g []
            , model.activeList
                |> Set.toList
                |> List.filterMap (flip Array.get model.background)
                |> List.map (circle red)
                |> g []
            ]



--


description : String
description =
    """
This was probably the most difficult project I made. I rewrote it about four times.

This is an example of the Poisson Disk sampling algorithm, which produces
points which are randomly distributed *and* aren't too densly packed.
This produces a more natural pattern than random sampling, where there could be
points which are very close to other points and some which are relatively isolated.

It's based on [Mike Bostock's version](https://bl.ocks.org/mbostock/19168c663618b7f07158),
which is based on [Jason Davies's version](https://www.jasondavies.com/poisson-disc/).
I first encountered this algorithm in
[Mike's very good talk](https://bost.ocks.org/mike/algorithms/) about visualizing alorithms.

## Discussion

This is *so much slower* than the two linked versions.
Probably because Elm doesn't handle random stuff very well.
I tried using ports, which helped a little.

This might also be due to some inefficient code, but I don't think an Elm version could
produce results as impressive as Jason's.
"""


main : Program Never Model Msg
main =
    program
        { init = init 30 0.05
        , subscriptions = subscriptions
        , update = update
        , view = view 500 500 |> project 6 description
        }
