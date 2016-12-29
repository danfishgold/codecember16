module Waves exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, circle, rect)
import Svg.Attributes exposing (x, y, width, height, cx, cy, r, fill, stroke, strokeWidth)
import Color
import Color.Manipulate exposing (fadeOut)
import Color.Convert exposing (colorToCssRgba)
import Day29.Mouse as Mouse exposing (Position)
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
        , view = view
        }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--


update : Msg -> Model -> Model
update msg model =
    case msg of
        MouseDown pos ->
            { model | mouseSource = Just ( pos, model.time ) }

        MouseUp endPos ->
            let
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


view : Model -> Svg Msg
view model =
    let
        reflector ( x, y ) =
            circle
                [ cx <| toString x
                , cy <| toString y
                , r "5"
                , fill <| colorToCssRgba Color.red
                ]
                []

        wave ( ( x, y ), t0, lifetime ) =
            let
                fo =
                    1 - (lifetime - (model.time - t0)) / model.waveLifetime
            in
                circle
                    [ cx <| toString x
                    , cy <| toString y
                    , r <| toString <| (model.time - t0) * model.waveSpeed
                    , fill "none"
                    , stroke <| colorToCssRgba (Color.red |> fadeOut fo)
                    , strokeWidth "1"
                    ]
                    []

        bg =
            rect
                [ x "0"
                , y "0"
                , width <| toString model.width
                , height <| toString model.height
                , fill "#fafafa"
                , Mouse.down MouseDown
                , Mouse.up MouseUp
                ]
                []
    in
        (bg :: List.map reflector model.reflectors ++ List.map wave model.waves)
            |> svg
                [ width <| toString model.width
                , height <| toString model.height
                ]
