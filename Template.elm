module Main exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)


type alias Model =
    {}


type Msg
    = Msg


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view model =
    div [] []



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
