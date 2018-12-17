module Day10.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Collage exposing (circle, group, rectangle, shift)
import Collage.Render
import Color exposing (black, white)
import Helper exposing (filled, projectCollage)
import Html exposing (Html)
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
    | Delta Float


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
    Browser.Events.onAnimationFrameDelta Delta



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
                |> shift (project ( x * width, y * height, z ))
    in
    [ points
        |> List.map point
        |> Collage.group
    , rectangle width height |> filled black
    ]
        |> group
        |> projectCollage ( width, height )



--


description : String
description =
    """
This is a Windows screen saver from my childhood.
I couldn't find a lot of information about it so I'm not even sure if it's
from Windows 98 or 95.

Anyway, this was a snack in preparation for tomorrow's project.
"""


page =
    { init = always <| init
    , subscriptions = subscriptions
    , update = update
    , title = "Starfield"
    , body = view
    , description = description
    }
