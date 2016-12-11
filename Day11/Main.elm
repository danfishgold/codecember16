module Parallax exposing (..)

import Html exposing (Html, program)
import Collage exposing (collage, rect, circle, move, filled)
import Element
import Day2.Random
import Random
import Mouse
import Color exposing (Color)


type alias Model =
    { rects : List Rect
    , count : Int
    , width : Float
    , height : Float
    , eye : Point2D
    }


type alias Point3D =
    ( Float, Float, Float )


type alias Point2D =
    ( Float, Float )


type alias Rect =
    { center : Point3D
    , width : Float
    , height : Float
    , color : Color
    }


type Msg
    = SetRects (List Rect)
    | Mouse Mouse.Position


init : Int -> ( Model, Cmd Msg )
init count =
    ( { rects = []
      , count = count
      , width = 500
      , height = 500
      , eye = ( 0, 0 )
      }
    , Random.generate SetRects (Random.list count randomRect)
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves Mouse



--


randomRect : Random.Generator Rect
randomRect =
    let
        randomZ =
            Random.map (\f -> 1 / f) (Random.float 0 1)

        pt x y z =
            ( x * z, y * z, z )

        center =
            Random.map3 pt
                (Random.float -0.5 0.5)
                (Random.float -0.5 0.5)
                randomZ
    in
        Random.map4 Rect
            (center)
            (Random.float 0.1 0.2)
            (Random.float 0.1 0.2)
            (Day2.Random.ryb1 1 0.5)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRects rects ->
            ( { model | rects = rects }, Cmd.none )

        Mouse { x, y } ->
            let
                eye =
                    ( toFloat x - model.width / 2
                    , -(toFloat y - model.height / 2)
                    )
            in
                ( { model | eye = eye }, Cmd.none )



--


z : Rect -> Float
z { center } =
    center |> \( _, _, z ) -> z


edges : Rect -> List Point3D
edges { center, width, height } =
    center
        |> \( x, y, z ) ->
            [ ( x - width / 2, y - height / 2, z )
            , ( x - width / 2, y + height / 2, z )
            , ( x + width / 2, y + height / 2, z )
            , ( x + width / 2, y - height / 2, z )
            ]


project : Point2D -> Point3D -> Point2D
project ( x0, y0 ) ( x, y, z ) =
    ( (x - x0) / z, (y - y0) / z )


view : Model -> Html Msg
view { eye, width, height, rects } =
    let
        aRect ({ color } as rect) =
            rect
                |> edges
                |> List.map (\( x, y, z ) -> project eye ( x * width, y * height, z ))
                |> Collage.polygon
                |> filled color
    in
        rects
            |> List.sortBy (negate << z)
            |> List.map aRect
            |> Collage.collage (floor width) (floor height)
            |> Element.toHtml



--


main : Program Never Model Msg
main =
    program
        { init = init 40
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
