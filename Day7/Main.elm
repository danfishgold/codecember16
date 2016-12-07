port module Loops exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Color exposing (Color)
import Day2.Ryb exposing (ryba)


type alias Model =
    { minLength : Int
    , count : Int
    , loops : List Loop
    , width : Int
    , height : Int
    }


type Msg
    = SetLoops (List Loop)


type alias Point =
    ( Int, Int )


type alias Loop =
    { points : List Point, center : Point, color : Color }


init : ( Model, Cmd Msg )
init =
    ( { minLength = 10
      , count = 1
      , loops = []
      , width = 500
      , height = 500
      }
    , requestLoops ( 500, 500, 20, 200, 1 )
    )



--


type alias JSLoop =
    ( List Point, Point, Float )


{-| width, height, minLength, maxLength, count
-}
port requestLoops : ( Int, Int, Int, Int, Int ) -> Cmd msg


port getLoops : (List JSLoop -> msg) -> Sub msg


parseLoop : JSLoop -> Loop
parseLoop ( pts, center, hue ) =
    { points = pts
    , center = center
    , color = ryba (hue |> degrees) 1 0.5 0.5
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    getLoops (List.map parseLoop >> SetLoops)



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetLoops loops ->
            ( { model | loops = loops }, Cmd.none )



--


view : Model -> Html Msg
view model =
    div [] [ text <| toString model ]



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
