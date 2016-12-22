module Spirograph exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, button, text)
import Html.Events exposing (onInput, onClick)
import Svg exposing (svg, polyline, circle)
import Svg.Attributes exposing (width, height, cx, cy, r, stroke, strokeWidth, fill)
import AnimationFrame


type alias Point =
    ( Float, Float )


type alias Model =
    { width : Float
    , height : Float
    , bigR : Float
    , smallR : Float
    , points : List Point
    }


type Msg
    = SetBigR Float
    | SetSmallR Float
    | Reset
    | Tick Float


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , bigR = 0.5
      , smallR = 0.5
      , points = []
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view model =
    let
        onValue msg =
            onInput (String.toFloat >> Result.withDefault 0 >> msg)
    in
        div []
            [ input [ onValue SetBigR ] []
            , input [ onValue SetSmallR ] []
            , button [ onClick Reset ] [ text "Reset" ]
            , svg [ width <| toString model.width, height <| toString model.height ] []
            ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
