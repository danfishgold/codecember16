module Waves exposing (..)

import Html exposing (Html, program)
import Helper exposing (project)
import Collage exposing (collage, rect, circle, move, filled, outlined, solid)
import Element
import Color
import Color.Manipulate exposing (fadeOut)
import Pointer exposing (Position)
import Time exposing (Time, second)
import AnimationFrame


type alias Model =
    { width : Float
    , height : Float
    , mouseSource : Maybe ( Position, Time )
    , reflectors : List Position
    , waves : List ( Position, Time, Float )
    , time : Time
    , generatorFrequency : Time
    , waveSpeed : Float
    , waveLifetime : Time
    }


type Msg
    = MouseDown Position
    | MouseUp Position
    | Tick Time


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , mouseSource = Nothing
      , reflectors = []
      , waves = []
      , time = 0
      , generatorFrequency = second
      , waveSpeed = 100 / second
      , waveLifetime = 4 * second
      }
    , Cmd.none
    )


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view |> project 29
        }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown ( x, y ) ->
            let
                pos =
                    ( x - model.width / 2, -y + model.height * 0.5 )
            in
                { model | mouseSource = Just ( pos, model.time ) }

        MouseUp ( x, y ) ->
            let
                endPos =
                    ( x - model.width / 2, -y + model.height * 0.5 )

                newModel =
                    { model | mouseSource = Nothing }
            in
                case model.mouseSource of
                    Nothing ->
                        -- This is also probably a bug because that makes absolutely no sense
                        newModel

                    Just ( startPos, startTime ) ->
                        if model.time - startTime < model.generatorFrequency then
                            { newModel | reflectors = startPos :: model.reflectors }
                        else
                            newModel

        Tick dt ->
            let
                newTime =
                    model.time + dt

                filteredWaves =
                    model.waves |> List.filter (\( _, t0, lifetime ) -> newTime - t0 <= lifetime)

                reflectedWaves =
                    filteredWaves
                        |> List.map (reflect model.time newTime model.waveSpeed)
                        |> List.map (model.reflectors |> flip List.filterMap)
                        |> List.concat

                newWaves =
                    case model.mouseSource of
                        Nothing ->
                            []

                        Just ( mouse, t0 ) ->
                            let
                                cycle =
                                    (model.time - t0)
                                        / model.generatorFrequency
                                        |> ceiling
                                        |> toFloat

                                newWaveTime =
                                    t0 + cycle * model.generatorFrequency
                            in
                                if cycle >= 1 && newTime >= newWaveTime then
                                    [ ( mouse, newWaveTime, model.waveLifetime ) ]
                                else
                                    []
            in
                { model
                    | time = newTime
                    , waves = newWaves ++ filteredWaves ++ reflectedWaves
                }



--


dist : Position -> Position -> Float
dist ( x1, y1 ) ( x2, y2 ) =
    sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2


reflect : Time -> Time -> Float -> ( Position, Time, Float ) -> Position -> Maybe ( Position, Time, Float )
reflect t1 t2 waveSpeed ( origin, t0, lifetime ) reflector =
    let
        reflectionTime =
            dist origin reflector / waveSpeed
    in
        if reflectionTime > t1 - t0 && reflectionTime <= t2 - t0 then
            Just ( reflector, t0 + reflectionTime, lifetime - reflectionTime )
        else
            Nothing



--


view : Model -> Html Msg
view model =
    let
        reflector ( x, y ) =
            circle 5
                |> filled Color.red
                |> move ( x, y )

        wave ( ( x, y ), t0, lifetime ) =
            let
                fo =
                    1 - (lifetime - (model.time - t0)) / model.waveLifetime
            in
                (model.time - t0)
                    * model.waveSpeed
                    |> circle
                    |> outlined (Color.red |> fadeOut fo |> solid)
                    |> move ( x, y )

        bg =
            rect model.width model.height
                |> filled (Color.rgb 250 250 250)
    in
        Html.div
            [ Pointer.down MouseDown
            , Pointer.up MouseUp
            ]
            [ collage
                (floor model.width)
                (floor model.height)
                (bg :: List.map reflector model.reflectors ++ List.map wave model.waves)
                |> Element.toHtml
            ]
