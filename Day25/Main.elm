module Waves exposing (..)

import Html exposing (Html, program)
import Collage exposing (collage, polygon, filled, move, rotate)
import Element
import AnimationFrame
import Color exposing (Color)
import Day25.Vector as Vector exposing (..)


type alias Boid =
    { r : Vector, v : Vector, c : Color }


type alias Rule =
    List Boid -> Boid -> Boid


rules : List ( Rule, Float, Int )
rules =
    []


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
            { model | boids = model.boids |> updateVelocities |> updateLocations dt }

        SetBoids boids ->
            { model | boids = boids }


updateVelocities : List Boid -> List Boid
updateVelocities boids =
    boids


updateLocations : Float -> List Boid -> List Boid
updateLocations dt boids =
    boids |> List.map (\boid -> { boid | r = add boid.r (mul dt boid.v) })



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
