module Automaton exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Svg exposing (Svg, svg, rect)
import Svg.Events exposing (onClick)
import Dict exposing (Dict)


type alias Model =
    { levels : Int
    , ruleWidth : Int
    , colors : Int
    , rule : Dict (List Int) Int
    }


type Msg
    = ShiftRule (List Int)


init : ( Model, Cmd Msg )
init =
    ( { levels = 10
      , ruleWidth = 3
      , colors = 2
      , rule = Dict.empty
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShiftRule idx ->
            let
                shift ruleN =
                    case ruleN of
                        Just n ->
                            Just <| (n + 1) % model.colors

                        Nothing ->
                            Just 1
            in
                ( { model | rule = Dict.update idx shift model.rule }, Cmd.none )



--


view : Float -> Model -> Html Msg
view res model =
    svg [] []



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view 10
        }
