module Day3.Anatoly exposing (..)

import Html exposing (Html, program)
import Collage exposing (..)
import Element
import Time exposing (second, every)
import Color exposing (Color)


type alias Model =
    { points : List ( Int, Int, Color )
    , width : Int
    , height : Int
    }


init : Model
init =
    { points = [], width = 750, height = 500 }


type Msg
    = Tick
    | Add ( Int, Int, Color )



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view model =
    collage 750 500 [ rect 750 500 |> filled Color.black ]
        |> Element.toHtml



--


subscriptions : Model -> Sub Msg
subscriptions _ =
    every (0.3 * second) (always Tick)



--


main : Program Never Model Msg
main =
    program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
