module UrlParallax exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Window
import Mouse
import Navigation exposing (Location)
import Task


type alias Model =
    { width : Float
    , fraction : Float
    }


type Msg
    = SetWidth Float
    | SetFraction Float
    | None


widthFromWindow : Window.Size -> Msg
widthFromWindow { width } =
    SetWidth (toFloat width)


fractionFromMouse : Model -> Mouse.Position -> Msg
fractionFromMouse { width } { x } =
    SetFraction (toFloat x / width)


init : Location -> ( Model, Cmd Msg )
init _ =
    ( { width = 1
      , fraction = 0
      }
    , Task.perform widthFromWindow Window.size
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves (fractionFromMouse model)
        , Window.resizes widthFromWindow
        ]



--


simpleUrl : Int -> Float -> String
simpleUrl n fraction =
    let
        before =
            floor <| (toFloat n * fraction)

        after =
            n - before
    in
        "http://localhost:8000/Day12/" ++ String.join "|‾‾‾|" [ String.repeat before "_", String.repeat after "_" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetWidth width ->
            ( { model | width = width }, Cmd.none )

        SetFraction f ->
            ( { model | fraction = f }, Navigation.modifyUrl (simpleUrl 70 f) )

        None ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view model =
    div [] [ text "hey" ]



--


main : Program Never Model Msg
main =
    Navigation.program (always None)
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
