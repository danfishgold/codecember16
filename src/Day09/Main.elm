module Day09.Main exposing (Model, Msg, page)

import Browser.Events
import Collage exposing (defaultLineStyle, group, polygon, uniform)
import Collage.Render
import Color exposing (Color)
import Helper exposing (Size, filled, getViewport, outlined, projectCollage)
import Html exposing (Html, div)
import Html.Attributes exposing (class, id)
import Pointer


type alias Point =
    ( Float, Float )


type alias Model =
    { polygons : List (List Point)
    , frame : List Point
    , mouse : Maybe Point
    , size : Size
    }


type Msg
    = Mouse Point
    | MouseUp
    | SetSize Size
    | GetViewport
    | NoOp


init : ( Model, Cmd Msg )
init =
    ( { polygons =
            [ [ ( -100 * 0.3, -100 * 0.26 ), ( -100 * 0.3, -100 * 0.16 ), ( -100 * 0.1, -100 * 0.1 ), ( -100 * 0.1, -100 * 0.3 ) ]
            , [ ( 100 * 0.16, 100 * 0.44 ), ( -100 * 0.1, 100 * 0.36 ), ( -100 * 0.08, 100 * 0.2 ), ( 100 * 0.1, 100 * 0.28 ) ]
            , [ ( 100 * 0.18, -100 * 0.16 ), ( 100 * 0.32, -100 * 0.3 ), ( 100 * 0.3, -100 * 0.12 ) ]
            , [ ( -100 * 0.12, -100 * 0.38 ), ( -100 * 0.14, -100 * 0.42 ), ( -100 * 0.38, -100 * 0.34 ), ( -100 * 0.32, -100 * 0.32 ) ]
            , [ ( 100 * 0.1, 100 * 0.1 ), ( 100 * 0.14, 100 * 0.18 ), ( 100 * 0.26, 100 * 0.04 ) ]
            ]
      , frame = [ ( -100 * 0.5, -100 * 0.5 ), ( -100 * 0.5, 100 * 0.5 ), ( 100 * 0.5, 100 * 0.5 ), ( 100 * 0.5, -100 * 0.5 ) ]
      , mouse = Nothing
      , size = { width = 500, height = 500 }
      }
    , getSvgViewport
    )


getSvgViewport =
    getViewport SetSize NoOp "day09"



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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Mouse ( x, y ) ->
            ( { model
                | mouse =
                    Just
                        ( (x / model.size.width - 0.5) * 100
                        , (0.5 - y / model.size.height) * 100
                        )
              }
            , Cmd.none
            )

        MouseUp ->
            ( { model | mouse = Nothing }, Cmd.none )

        SetSize sz ->
            ( { model | size = sz }, Cmd.none )

        GetViewport ->
            ( model, getSvgViewport )

        NoOp ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view { size, mouse, polygons, frame } =
    let
        polygon_ pts =
            pts
                |> List.map (\( x, y ) -> ( x * size.width / 100, y * size.height / 100 ))
                |> polygon

        lightAreas source =
            (frame :: polygons)
                |> destinations source
                |> pairs
                |> List.map (\( p1, p2 ) -> [ source, p1, p2 ])
                |> List.map polygon_
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
                        |> List.map (cartesian mouse_)

        shapes =
            polygons |> List.map polygon_
    in
    [ shapes |> List.map (Helper.outlined 1 Color.black Collage.Clipped) |> group
    , sources 1 10 |> List.concatMap lightAreas |> group
    , shapes |> List.map (filled Color.lightGray) |> group
    , frame |> polygon_ |> filled Color.black
    ]
        |> Collage.group
        |> projectCollage ( size.width, size.height )
            [ id "day09"
            , Pointer.onMove Mouse
            , Pointer.onTouchUp MouseUp
            ]



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
    , subscriptions = subscriptions
    , update = update
    , title = "Nicky"
    , body = view
    , description = description
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize (\_ _ -> GetViewport)
