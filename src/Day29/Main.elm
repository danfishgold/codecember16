module Day29.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Collage exposing (circle, group, rectangle, shift, solid)
import Collage.Render
import Color
import Color.Manipulate exposing (fadeOut)
import Helper exposing (Size, filled, getViewport, outlined, projectCollage)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (class, id)
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
    | SetSize Size
    | GetViewport


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
    , getSvgViewport
    )


getSvgViewport =
    getViewport SetSize Reset "day29"


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
    , update = update
    , title = "Waves"
    , body = view
    , description = description
    }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , Browser.Events.onResize (\_ _ -> GetViewport)
        ]



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseDown ( x, y ) ->
            let
                pos =
                    ( x - model.width / 2, -y + model.height * 0.5 )
            in
            ( { model | mouseSource = Just ( pos, model.time ) }, Cmd.none )

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
                    ( newModel, Cmd.none )

                Just ( startPos, startTime ) ->
                    if model.time - startTime < model.generatorFrequency then
                        ( { newModel | reflectors = startPos :: model.reflectors }, Cmd.none )

                    else
                        ( newModel, Cmd.none )

        Tick dt ->
            let
                newTime =
                    model.time + min 100 dt

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
            ( { model
                | time = newTime
                , waves = newWaves ++ filteredWaves ++ reflectedWaves
              }
            , Cmd.none
            )

        Reset ->
            ( { model
                | reflectors = []
                , waves = []
                , mouseSource = Nothing
              }
            , Cmd.none
            )

        SetSize { width, height } ->
            ( { model | width = width, height = height }, Cmd.none )

        GetViewport ->
            ( model, getSvgViewport )



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
        [ projectCollage
            ( model.width, model.height )
            [ id "day29"
            , Pointer.onDown MouseDown
            , Pointer.onUp MouseUp
            ]
          <|
            group <|
                List.concat
                    [ List.map wave model.waves
                    , List.map reflector model.reflectors
                    , [ bg ]
                    ]
        , div [] [ button [ onClick Reset ] [ text "Reset" ] ]
        ]
