module Day09.Main exposing (Model, Msg, page)

import Collage exposing (defaultLineStyle, group, polygon, uniform)
import Collage.Render
import Color exposing (Color)
import Helper exposing (filled, outlined, projectCollage)
import Html exposing (Html, div)
import Pointer


type alias Point =
    ( Float, Float )


type alias Model =
    { polygons : List (List Point)
    , frame : List Point
    , mouse : Maybe Point
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
      , mouse = Nothing
      }
    , Cmd.none
    )



--


pairs : List a -> List ( a, a )
pairs xs =
    let
        withoutOverflow xs_ =
            case xs_ of
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
        ( ( dx, dy ), ( dx1, dy1 ) ) =
            ( ( x2 - x1, y2 - y1 ), ( a - x1, b - y1 ) )

        ( c, s ) =
            ( cos theta, sin theta )

        ( nom, denom ) =
            ( dx1 * s - dy1 * c, dx * s - dy * c )

        t =
            if abs denom > 0.05 then
                Just (nom / denom)

            else
                Nothing

        d t_ =
            if 0 <= t_ && t_ <= 1 then
                (dx * t_ - dx1)
                    / c
                    |> (\dist ->
                            if dist > 0 then
                                Just dist

                            else
                                Nothing
                       )

            else
                Nothing
    in
    t |> Maybe.andThen d


angle : Point -> Point -> Float
angle ( x0, y0 ) ( x, y ) =
    atan2 (y - y0) (x - x0)


cartesian : Point -> ( Float, Float ) -> Point
cartesian ( x0, y0 ) ( d, angle_ ) =
    ( x0 + d * cos angle_, y0 + d * sin angle_ )


destinations : Point -> List (List Point) -> List Point
destinations mouse polygons =
    let
        lines : List ( Point, Point )
        lines =
            polygons |> List.concatMap pairs

        rayAngles : List Float
        rayAngles =
            polygons
                |> List.concatMap (List.map (angle mouse))
                |> List.concatMap (\t -> [ t - 0.001, t + 0.001 ])
                |> List.sort

        rayIntersection : Float -> Maybe ( Float, Float )
        rayIntersection angle_ =
            lines
                |> List.filterMap (intersection mouse angle_)
                |> List.minimum
                |> Maybe.map (\d -> ( d, angle_ ))
    in
    rayAngles
        |> List.filterMap rayIntersection
        |> List.map (cartesian mouse)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Mouse mouse ->
            { model
                | mouse = Just mouse
            }



--


view : Model -> Html Msg
view { mouse, polygons, frame } =
    let
        actualMouse ( x, y ) =
            ( x - 250, 250 - y )

        lightAreas source =
            (frame :: polygons)
                |> destinations source
                |> pairs
                |> List.map (\( p1, p2 ) -> [ source, p1, p2 ])
                |> List.map polygon
                |> List.map (filled <| Color.rgba 1 1 1 0.15)

        sources dist n =
            case mouse of
                Nothing ->
                    []

                Just mouse_ ->
                    List.range 0 n
                        |> List.map (\i -> degrees <| toFloat i / toFloat n * 360)
                        |> List.map (\ang -> ( dist, ang ))
                        |> (\pts -> ( 0, 0 ) :: pts)
                        |> List.map (cartesian (actualMouse mouse_))

        shapes =
            polygons |> List.map polygon
    in
    [ shapes |> List.map (Helper.outlined 1 Color.black Collage.Clipped) |> group
    , sources 5 10 |> List.concatMap lightAreas |> group
    , shapes |> List.map (filled Color.lightGray) |> group
    , frame |> polygon |> filled Color.black
    ]
        |> Collage.group
        |> projectCollage ( 500, 500 )
        |> (\canvas -> div [ Pointer.move Mouse ] [ canvas ])



--


description : String
description =
    """
This is a recreation of [Nicky Case](http://ncase.me)'s
[Sight and Light](http://ncase.me/sight-and-light/).

It's more of a reimplementation than an homage, but still <3

Here's a link to [Bret Victor](http://worrydream.com), because why not.
"""


page =
    { init = always init
    , subscriptions = always Sub.none
    , update = \msg model -> ( update msg model, Cmd.none )
    , title = "Nicky"
    , body = view
    , description = description
    }
