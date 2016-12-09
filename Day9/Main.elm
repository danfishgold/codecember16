module Nicky exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, polygon)
import Svg.Attributes exposing (width, height)
import Svg.Attributes exposing (points, x1, y1, x2, y2, stroke, strokeWidth, fill)
import Mouse


type alias Point =
    ( Float, Float )


type alias Model =
    { polygons : List (List Point)
    , mouse : Point
    }


type Msg
    = Mouse Point


init : ( Model, Cmd Msg )
init =
    ( { polygons =
            [ [ ( 100, 120 ), ( 100, 170 ), ( 200, 200 ), ( 200, 100 ) ]
            , [ ( 330, 470 ), ( 200, 430 ), ( 210, 350 ), ( 300, 390 ) ]
            , [ ( 0, 0 ), ( 0, 500 ), ( 500, 500 ), ( 500, 0 ) ]
            ]
      , mouse = ( 0, 0 )
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Mouse.moves <| \{ x, y } -> Mouse ( toFloat x, toFloat y )



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


destinations : List (List Point) -> Point -> List Point
destinations polygons mouse =
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


view : Model -> Svg Msg
view model =
    let
        line ( xm, ym ) ( x, y ) =
            Svg.line
                [ x1 <| toString xm
                , y1 <| toString ym
                , x2 <| toString x
                , y2 <| toString y
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        poly pts =
            Svg.polygon
                [ pts
                    |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                    |> String.join " "
                    |> points
                , fill "none"
                , stroke "black"
                , strokeWidth "1"
                ]
                []
    in
        ((destinations model.polygons model.mouse
            |> pairs
            |> List.map (\( p1, p2 ) -> [ model.mouse, p1, p2 ])
            |> List.map poly
          -- |> List.map (line model.mouse)
         )
            ++ (model.polygons |> List.map poly)
        )
            |> svg [ width "500", height "500" ]



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        }
