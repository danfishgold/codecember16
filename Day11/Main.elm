module Parallax exposing (main)

import Browser exposing (document)
import Collage exposing (circle, group)
import Collage.Render
import Color exposing (Color)
import Color.Manipulate exposing (darken, lighten)
import Day2.Random
import Helper exposing (filled)
import Html exposing (Html, div)
import Pointer
import Random


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
    , depth : Float
    , color : Color
    }


type Msg
    = SetRects (List Rect)
    | Mouse Pointer.Position


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
    Sub.none



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
    Random.map5 Rect
        center
        (Random.float 0.1 0.2)
        (Random.float 0.1 0.2)
        (Random.float 0.1 0.2)
        (Day2.Random.ryb1 1 0.5)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetRects rects ->
            ( { model | rects = rects }, Cmd.none )

        Mouse ( x, y ) ->
            let
                eye =
                    ( x / model.width - 0.5
                    , -(y / model.height - 0.5)
                    )
            in
            ( { model | eye = eye }, Cmd.none )



--


rectZ : Rect -> Float
rectZ { center } =
    center |> (\( _, _, z ) -> z)


faces : Point2D -> Rect -> List ( Color, List Point3D )
faces ( x0, y0 ) { center, width, height, depth, color } =
    let
        ( x, y, z ) =
            center

        ( dx, dy, dz ) =
            ( width / 2, height / 2, depth )

        ( cFront, cTop, cBottom ) =
            ( color, lighten 0.1 color, darken 0.1 color )

        pt i j k =
            ( x + i * dx, y + j * dy, z + k * dz )

        front =
            [ pt -1 -1 0, pt -1 1 0, pt 1 1 0, pt 1 -1 0 ]

        top =
            [ pt -1 1 0, pt -1 1 1, pt 1 1 1, pt 1 1 0 ]

        bottom =
            [ pt -1 -1 0, pt -1 -1 1, pt 1 -1 1, pt 1 -1 0 ]

        left =
            [ pt -1 1 0, pt -1 -1 0, pt -1 -1 1, pt -1 1 1 ]

        right =
            [ pt 1 1 0, pt 1 -1 0, pt 1 -1 1, pt 1 1 1 ]
    in
    [ if y0 > y then
        ( cTop, top )

      else
        ( cBottom, bottom )
    , if x0 > x then
        ( cTop, right )

      else
        ( cBottom, left )
    , ( cFront, front )
    ]


project : Point2D -> Point3D -> Point2D
project ( x0, y0 ) ( x, y, z ) =
    ( (x - x0) / z, (y - y0) / z )


view : Model -> Html Msg
view { eye, width, height, rects } =
    let
        polygon ( color, points ) =
            points
                |> List.map (project eye)
                |> List.map (\( x, y ) -> ( x * width, y * height ))
                |> Collage.polygon
                |> filled color
    in
    rects
        |> List.sortBy rectZ
        |> List.concatMap (faces eye)
        |> List.map polygon
        |> group
        |> Collage.Render.svgBox ( width, height )
        |> (\canvas -> div [ Pointer.move Mouse ] [ canvas ])



--


description : String
description =
    """
The year was 2015. I took a Python course even though I already knew Python.
During the lectures I mostly played
[VVVVVV](http://thelettervsixtim.es)
and [Luftrausers](http://luftrausers.com).

At some point I tried to find something on GitHub and I was hypnotized by the
[parallax](https://en.wikipedia.org/wiki/Parallax) in the 404 page.
Faking 3D in CG is something I wanted to do, so here we are.

These could have been just squares, but I made them cubes because of
[xkcd](https://xkcd.com/8/)
and because it looks so much better than just squares.

This was super fun.

## Instructions

Move the mouse ¯\\\\\\_(ツ)\\_/¯
"""


main : Program () Model Msg
main =
    document
        { init = always <| init 40
        , subscriptions = subscriptions
        , update = update
        , view = view |> Helper.project 11 description
        }
