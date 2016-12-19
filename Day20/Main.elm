module Gravity exposing (..)

import Html exposing (Html, program)
import Collage exposing (collage, circle, filled)
import Element
import Color exposing (Color)
import Mouse
import AnimationFrame


type alias Model =
    { width : Float
    , height : Float
    , g : Float
    , balls : List Ball
    }


type alias Ball =
    { x : Float
    , y : Float
    , v : Float
    , bounced : Bool
    , color : Color
    }


type Msg
    = Tick Float
    | Add Ball
    | Mouse Mouse.Position


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , g = 1
      , balls = []
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Mouse.moves Mouse
        ]



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view model =
    []
        |> collage (floor model.width) (floor model.height)
        |> Element.toHtml



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
