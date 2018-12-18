module Day29.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Collage exposing (circle, group, rectangle, shift, solid)
import Collage.Render
import Color
import Color.Manipulate exposing (fadeOut)
import Helper exposing (filled, outlined, projectCollage)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Pointer exposing (Position)


type alias Model =
    { width : Float
    , height : Float
    , mouseSource : Maybe ( Position, Float )
    , reflectors : List Position
    , waves : List ( Position, Float, Float )
    , time : Float
    , generatorFrequency : Float
    , waveSpeed : Float
    , waveLifetime : Float
    }


type Msg
    = MouseDown Position
    | MouseUp Position
    | Tick Float
    | Reset


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


second =
    1000


description : String
description =
    """
**Originally from May 2015**

Physics is fun.

## Instructions

Click an empty space to place a "reflector".
Click and hold an empty space to generate a wave.
The wave will progress and its intensity will decrease.
When it hits a reflector, a secondary wave will be emitted from it.
"""


page =
    { init = always <| init 500 500
    , subscriptions = subscriptions
    , update = \msg model -> ( update msg model, Cmd.none )
    , title = "Waves"
    , body = view
    , description = description
    }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Tick



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
                        |> List.map (model.reflectors |> (\b a -> List.filterMap a b))
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

        Reset ->
            { model
                | reflectors = []
                , waves = []
                , mouseSource = Nothing
            }



--


dist : Position -> Position -> Float
dist ( x1, y1 ) ( x2, y2 ) =
    sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2


reflect : Float -> Float -> Float -> ( Position, Float, Float ) -> Position -> Maybe ( Position, Float, Float )
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
                |> shift ( x, y )

        wave ( ( x, y ), t0, lifetime ) =
            let
                fo =
                    1 - (lifetime - (model.time - t0)) / model.waveLifetime
            in
            (model.time - t0)
                * model.waveSpeed
                |> circle
                |> outlined 1 (Color.red |> fadeOut fo) Collage.Clipped
                |> shift ( x, y )

        bg =
            rectangle model.width model.height
                |> filled (Color.rgb 250 250 250)
    in
    div []
        [ div
            [ Pointer.down MouseDown
            , Pointer.up MouseUp
            ]
            [ projectCollage
                ( model.width, model.height )
              <|
                group <|
                    List.concat
                        [ List.map wave model.waves
                        , List.map reflector model.reflectors
                        , [ bg ]
                        ]
            ]
        , div [] [ button [ onClick Reset ] [ text "Reset" ] ]
        ]
