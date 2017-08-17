module Nicky exposing (..)

import Html exposing (Html, program, div)
import Helper exposing (project)
import Collage exposing (polygon, group, filled, outlined, defaultLine)
import Element
import Color
import Pointer


type alias Point =
    ( Float, Float )


type alias Model =
    { polygons : List (List Point)
    , frame : List Point
    , mouse : Point
    }


type Msg
    = Mouse Point


init : ( Model, Cmd Msg )
init =
    ( { polygons =
            [ [ ( -150, -130 ), ( -150, -80 ), ( -50, -50 ), ( -50, -150 ) ]
            , [ ( 80, 220 ), ( -50, 180 ), ( -40, 100 ), ( 50, 140 ) ]
            , [ ( 90, -80 ), ( 160, -150 ), ( 150, -60 ) ]
            , [ ( -60, -190 ), ( -70, -210 ), ( -190, -170 ), ( -160, -160 ) ]
            , [ ( 50, 50 ), ( 70, 90 ), ( 130, 20 ) ]
            ]
      , frame = [ ( -250, -250 ), ( -250, 250 ), ( 250, 250 ), ( 250, -250 ) ]
      , mouse = ( 0, 0 )
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


pairs : List a -> List ( a, a )
pairs xs =
    let
        withoutOverflow xs =
            case xs of
                fst :: snd :: rest ->
                    ( fst, snd ) :: withoutOverflow (snd :: rest)

                _ ->
                    []
    in
        case xs of
            fst :: rest ->
                withoutOverflow (xs ++ [ fst ])

            [] ->
                []


intersection : Point -> Float -> ( Point, Point ) -> Maybe Float
intersection ( a, b ) theta ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        ( dx, dy, dx1, dy1 ) =
            ( x2 - x1, y2 - y1, a - x1, b - y1 )

        ( c, s ) =
            ( cos theta, sin theta )

        ( nom, denom ) =
            ( dx1 * s - dy1 * c, dx * s - dy * c )

        t =
            if abs denom > 0.05 then
                Just (nom / denom)
            else
                Nothing

        d t =
            if 0 <= t && t <= 1 then
                (dx * t - dx1)
                    / c
                    |> \dist ->
                        if dist > 0 then
                            Just dist
                        else
                            Nothing
            else
                Nothing
    in
        t |> Maybe.andThen d


angle : Point -> Point -> Float
angle ( x0, y0 ) ( x, y ) =
    atan2 (y - y0) (x - x0)


cartesian : Point -> ( Float, Float ) -> Point
cartesian ( x0, y0 ) ( d, angle ) =
    ( x0 + d * cos angle, y0 + d * sin angle )


destinations : Point -> List (List Point) -> List Point
destinations mouse polygons =
    let
        lines =
            polygons |> List.concatMap pairs

        rayAngles =
            polygons
                |> List.concatMap (List.map (angle mouse))
                |> List.concatMap (\t -> [ t - 0.001, t + 0.001 ])
                |> List.sort

        rayIntersection angle =
            lines
                |> List.filterMap (intersection mouse angle)
                |> List.minimum
                |> Maybe.map (\d -> ( d, angle ))
    in
        rayAngles
            |> List.filterMap rayIntersection
            |> List.map (cartesian mouse)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mouse mouse ->
            { model | mouse = mouse }



--


view : Model -> Html Msg
view { mouse, polygons, frame } =
    let
        actualMouse =
            ( Tuple.first mouse - 250, 250 - Tuple.second mouse )

        lightAreas source =
            (frame :: polygons)
                |> destinations source
                |> pairs
                |> List.map (\( p1, p2 ) -> [ source, p1, p2 ])
                |> List.map polygon
                |> List.map (filled <| Color.rgba 255 255 255 0.3)

        sources dist n =
            List.range 0 n
                |> List.map (\i -> degrees <| toFloat i / toFloat n * 360)
                |> List.map (\ang -> ( dist, ang ))
                |> (\pts -> ( 0, 0 ) :: pts)
                |> List.map (cartesian actualMouse)

        shapes =
            polygons |> List.map polygon
    in
        [ frame |> polygon |> filled Color.black
        , shapes |> List.map (filled Color.white) |> group
        , shapes |> List.map (outlined defaultLine) |> group
        , sources 5 10 |> List.concatMap lightAreas |> group
        ]
            |> Collage.collage 500 500
            |> Element.toHtml
            |> \canvas -> div [ Pointer.move Mouse ] [ canvas ]



--


description : String
description =
    """
This is an homage to [Nicky Case](http://ncase.me)'s
[Sight and Light](http://ncase.me/sight-and-light/).

It's more of a reimplementation than an homage, but still <3

Here's a link to [Bret Victor](http://worrydream.com), because why not.
"""


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view |> project 9 description
        }
