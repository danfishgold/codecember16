module Starfield exposing (..)

import Html exposing (Html, program)
import Helper
import Collage exposing (collage, rect, circle, move, filled)
import Element
import Color exposing (black, white)
import AnimationFrame
import Time exposing (Time)
import Random
import Random.Float


type alias Model =
    { points : List Point3D
    , count : Int
    , speed : Float
    , zFar : Float
    , width : Float
    , height : Float
    }


type alias Point3D =
    ( Float, Float, Float )


type alias Point2D =
    ( Float, Float )


type Msg
    = AddPoint Point3D
    | Delta Time


init : ( Model, Cmd Msg )
init =
    ( { points = []
      , count = 400
      , zFar = 20
      , speed = 1 / 200
      , width = 500
      , height = 500
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Delta



--


randomPoint : Float -> Random.Generator Point3D
randomPoint z =
    let
        pt x y =
            ( x, y, z )
    in
        Random.map2 pt
            (Random.Float.normal 0 4)
            (Random.Float.normal 0 4)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Delta dt ->
            let
                newPoints =
                    model.points
                        |> List.map (\( x, y, z ) -> ( x, y, z - dt * model.speed ))
                        |> List.filter inFrame
            in
                ( { model | points = newPoints }
                , if List.length newPoints < model.count then
                    Random.generate AddPoint (randomPoint model.zFar)
                  else
                    Cmd.none
                )

        AddPoint pt ->
            ( { model | points = pt :: model.points }, Cmd.none )



--


project : Point3D -> Point2D
project ( x, y, z ) =
    ( x / z, y / z )


inFrame : Point3D -> Bool
inFrame ( x, y, z ) =
    (z > 0)
        && (-0.5 <= x / z)
        && (x / z <= 0.5)
        && (-0.5 <= y / z)
        && (y / z <= 0.5)


view : Model -> Html Msg
view { width, height, points } =
    let
        point ( x, y, z ) =
            circle (5 / z)
                |> filled white
                |> move (project ( x * width, y * height, z ))
    in
        [ rect width height |> filled black
        , points
            |> List.map point
            |> Collage.group
        ]
            |> collage (floor width) (floor height)
            |> Element.toHtml



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view |> Helper.project 10
        }
