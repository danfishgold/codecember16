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
    , waves : List ( Position, Time )
    , time : Time
    , generatorFrequency : Time
    , waveSpeed : Float
    , waveDecayTime : Time
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
      , waveDecayTime = 4 * second
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
                newModel =
                    { model | time = model.time + dt }
            in
                newModel



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

        wave ( ( x, y ), t0 ) =
            let
                opac =
                    (model.time - t0) / model.waveDecayTime
            in
                circle
                    [ cx <| toString x
                    , cy <| toString y
                    , fill "none"
                    , stroke <| colorToCssRgba (Color.red |> fadeOut opac)
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
