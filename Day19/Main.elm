module Matrix exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g, text, text_)
import Svg.Attributes exposing (width, height)
import Time exposing (Time, second, every)
import Day19.Trail exposing (Trail)


type alias Model =
    { width : Float
    , height : Float
    , trails : List Trail
    }


type Msg
    = Tick Time
    | AddTrail Trail


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , trails = []
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    every second Tick



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
