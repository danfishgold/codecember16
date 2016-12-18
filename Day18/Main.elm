module Main exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height)


type alias Model =
    { width : Float
    , height : Float
    }


type Msg
    = Msg


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
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
        _ ->
            ( model, Cmd.none )



--


view : Model -> Svg Msg
view model =
    []
        |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
