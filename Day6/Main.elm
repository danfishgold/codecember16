module Poisson exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Svg exposing (Svg, svg, g)
import Svg.Keyed exposing (node)
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Dict exposing (Dict)
import Set exposing (Set)
import Random exposing (Generator)
import Random.Set
import Color exposing (Color, black, red)
import Color.Convert exposing (colorToCssRgb)
import Time exposing (Time, every, second)


type alias Point =
    ( Float, Float )


type alias GridPoint =
    ( Int, Int )


type alias Model =
    { activeList : Set Point
    , grid : Dict GridPoint Point
    , finalPoints : List Point
    , width : Float
    , height : Float
    , k : Int
    , r : Float
    , seed : Random.Seed
    }


type Alg
    = MoveToFinal Point
    | AddActive Point
    | Finished


type Msg
    = Step Alg
    | Tick Time
    | SetModel Model


init : Float -> Float -> Int -> Float -> ( Model, Cmd Msg )
init width height k r =
    ( { activeList = Set.empty
      , grid = Dict.empty
      , finalPoints = []
      , width = width
      , height = height
      , k = k
      , r = r
      , seed = Random.initialSeed 0
      }
        |> updateAlgorithm (AddActive ( width / 2, height / 2 ))
    , Cmd.none
    )



{-
   choose a point p at random.
   try k random points around it
   if all those points q are near another point, remove p from active list and add it to finalPoints.
   if some point q is near an existing point in grid, add q to the active list.
   repeat until activePoints is empty
-}
--
--


subscriptions : Model -> Sub Msg
subscriptions model =
    if Set.isEmpty model.activeList then
        Sub.none
    else
        Time.every (second / 100) Tick



--


pointAround : Float -> Point -> Generator Point
pointAround r ( x, y ) =
    let
        rad =
            Random.float 0 1 |> Random.map (\f -> r * sqrt (1 + 3 * f))

        arg =
            Random.float 0 (2 * pi)

        pt r t =
            ( x + r * cos t, y + r * sin t )
    in
        Random.map2 pt rad arg


isFar : Float -> Dict GridPoint Point -> Point -> Bool
isFar r grid ( x, y ) =
    let
        ( i0, j0 ) =
            ( floor (2 * x / r), floor (2 * y / r) )

        keysX dy =
            [ ( i0 - 1, j0 + dy ), ( i0, j0 + dy ), ( i0 + 1, j0 + dy ) ]

        keys =
            List.range -1 1 |> List.concatMap keysX

        isFarFrom k =
            grid
                |> Dict.get k
                |> \pt ->
                    case pt of
                        Nothing ->
                            True

                        Just ( x1, y1 ) ->
                            (x - x1) ^ 2 + (y - y1) ^ 2 >= r ^ 2
    in
        List.all isFarFrom keys


isWithin : Float -> Float -> Point -> Bool
isWithin width height ( x, y ) =
    0 <= x && x <= width && 0 <= y && y <= height


step : Model -> ( Alg, Random.Seed )
step { seed, activeList, k, r, grid, width, height } =
    let
        processPoint ( pnt, seed1 ) =
            case pnt of
                Nothing ->
                    ( Finished, seed1 )

                Just p ->
                    stepFor seed1 p k

        stepFor seed1 p kp =
            let
                dealWith ( q, seed2 ) =
                    if isFar r grid q && isWithin width height q then
                        ( AddActive q, seed2 )
                    else
                        stepFor seed2 p (kp - 1)
            in
                if kp == 0 then
                    ( MoveToFinal p, seed1 )
                else
                    Random.step (pointAround r p) seed1 |> dealWith
    in
        Random.step (Random.Set.sample activeList) seed |> processPoint



--


updateAlgorithm : Alg -> Model -> Model
updateAlgorithm alg model =
    case alg of
        MoveToFinal p ->
            { model
                | activeList = model.activeList |> Set.remove p
                , finalPoints = p :: model.finalPoints
            }

        AddActive ( x, y ) ->
            { model
                | grid =
                    model.grid
                        |> Dict.update
                            ( floor (2 * x / model.r)
                            , floor (2 * y / model.r)
                            )
                            (always <| Just ( x, y ))
                , activeList = Set.insert ( x, y ) model.activeList
            }

        Finished ->
            model


makeStep : Model -> Model
makeStep model =
    let
        ( alg, seed1 ) =
            step model
    in
        updateAlgorithm alg { model | seed = seed1 }


makeSteps : Int -> Model -> Model
makeSteps n model =
    if n > 0 then
        makeSteps (n - 1) (makeStep model)
    else
        model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Step step ->
            ( updateAlgorithm step model, Cmd.none )

        Tick _ ->
            ( model |> makeSteps 10, Cmd.none )

        SetModel newModel ->
            ( newModel, Cmd.none )



--


circle : Color -> Point -> Svg msg
circle c ( x, y ) =
    Svg.circle
        [ cx <| toString x
        , cy <| toString y
        , r <| "1"
        , fill <| colorToCssRgb c
        ]
        []


view : Model -> Html Msg
view ({ activeList, finalPoints } as model) =
    svg
        [ width <| toString model.width
        , height <| toString model.height
        ]
        [ node "g" [] (finalPoints |> List.map (\pt -> ( toString pt, circle black pt )))
        , node "g" [] (activeList |> Set.toList |> List.map (\pt -> ( toString pt, circle red pt )))
        ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500 30 10
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
