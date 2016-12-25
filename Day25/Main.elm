module Waves exposing (..)

import Html exposing (Html, program)
import Collage exposing (collage, polygon, filled, move, rotate)
import Element
import AnimationFrame
import Color exposing (Color)


type alias Point =
    ( Float, Float )


type alias Boid =
    { r : Point, v : Point, c : Color }


type alias Model =
    { width : Float
    , height : Float
    , boids : List Boid
    }


type Msg
    = SetBoids (List Boid)
    | Tick Float


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , boids = []
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            { model | boids = List.map (updateBoid dt model.boids) model.boids }

        SetBoids boids ->
            { model | boids = boids }


updateBoid : Float -> List Boid -> Boid -> Boid
updateBoid dt boids ({ r, v } as boid) =
    boid



--


view : Model -> Html Msg
view { width, height } =
    []
        |> collage (floor width) (floor height)
        |> Element.toHtml



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        }
