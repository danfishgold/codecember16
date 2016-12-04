module Frequency exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Html.Attributes exposing (style)
import AnimationFrame exposing (diffs)
import Time exposing (Time)


type alias Model =
    { events : List { title : String, duration : Float, current : Time, fired : Bool } }


type Msg
    = Tick Time


init : List ( String, Float ) -> ( Model, Cmd Msg )
init events =
    let
        addInitials ( title, duration ) =
            { title = title, duration = duration, current = 0, fired = False }
    in
        ( { events = events |> List.map addInitials
          }
        , Cmd.none
        )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    diffs Tick



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


events : List ( String, Float )
events =
    [ ( "Heartbeat", 1 )
    ]


main : Program Never Model Msg
main =
    program
        { init = init events
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
