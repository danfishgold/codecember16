module Poisson exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Svg exposing (Svg, svg, g)
import Svg.Keyed exposing (node)
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Dict exposing (Dict)
import Set exposing (Set)
import Random exposing (Generator)
import Random.Extra exposing (constant)
import Random.Set
import Color exposing (Color, black, red)
import Color.Convert exposing (colorToCssRgb)
import Time exposing (Time, every, second)


type alias Point =
    ( Float, Float )


type alias GridPoint =
    ( Int, Int )


type alias Components =
    { activeList : Set Point
    , grid : Dict GridPoint Point
    , finalPoints : List Point
    }


type alias Model =
    { components : Components
    , width : Float
    , height : Float
    , step : Components -> Generator Alg
    , r : Float
    }


type Alg
    = MoveToFinal Point
    | AddActive Point
    | Finished


type Msg
    = Step Alg
    | Tick Time


init : Float -> Float -> Int -> Float -> ( Model, Cmd Msg )
init width height k r =
    ( { components =
            { activeList = Set.empty
            , grid = Dict.empty
            , finalPoints = []
            }
      , width = width
      , height = height
      , step = stepFunction k r width height
      , r = r
      }
        |> updateAlgorithm (AddActive ( width / 2, height / 2 ))
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    if Set.isEmpty model.components.activeList then
        Sub.none
    else
        Time.every (second / 100) Tick



--


randomPointAround : Float -> Point -> Generator Point
randomPointAround r ( x, y ) =
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


{-|
Step 1: Take a random point p from activeList
Step 2: Choose a random q around p
Step 3: If q is near any existing point, remove it. If it's far enough, add it to activeList
Step 4: Repeat steps 2-3 k times. If no q was added, move p from activeList to finalPoints.
-}
stepFunction : Int -> Float -> Float -> Float -> Components -> Generator Alg
stepFunction k r width height components =
    let
        isOk grid q =
            isFar r grid q && isWithin width height q

        pointAround p =
            randomPointAround r p

        stepsFor grid p kp =
            if kp > 0 then
                pointAround p
                    |> Random.andThen
                        (\q ->
                            if isOk grid q then
                                constant (AddActive q)
                            else
                                stepsFor grid p (kp - 1)
                        )
            else
                constant (MoveToFinal p)
    in
        Random.Set.sample components.activeList
            |> Random.andThen
                (\choice ->
                    case choice of
                        Nothing ->
                            constant Finished

                        Just p ->
                            stepsFor components.grid p k
                )



--


updateAlgorithm : Alg -> Model -> Model
updateAlgorithm alg ({ components, r } as model) =
    case alg of
        MoveToFinal p ->
            { model
                | components =
                    { components
                        | activeList = components.activeList |> Set.remove p
                        , finalPoints = p :: components.finalPoints
                    }
            }

        AddActive ( x, y ) ->
            { model
                | components =
                    { components
                        | grid =
                            components.grid
                                |> Dict.update
                                    ( floor (2 * x / model.r)
                                    , floor (2 * y / model.r)
                                    )
                                    (always <| Just ( x, y ))
                        , activeList = Set.insert ( x, y ) components.activeList
                    }
            }

        Finished ->
            model


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            ( model, Random.generate Step (model.step model.components) )

        Step alg ->
            ( updateAlgorithm alg model, Cmd.none )



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
view ({ components } as model) =
    svg
        [ width <| toString model.width
        , height <| toString model.height
        ]
        [ node "g" [] (components.finalPoints |> List.map (\pt -> ( toString pt, circle black pt )))
        , node "g" [] (components.activeList |> Set.toList |> List.map (\pt -> ( toString pt, circle red pt )))
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
