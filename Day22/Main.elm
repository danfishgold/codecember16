module Spirograph exposing (..)

import Html exposing (Html, program)
import Helper exposing (project)
import Html exposing (div, input, button, text)
import Html.Attributes as Attrs exposing (type_, style, defaultValue)
import Html.Events exposing (onInput, onClick)
import Svg exposing (Svg, svg, polyline, circle)
import Svg.Attributes exposing (width, height, cx, cy, r, stroke, strokeWidth, fill, points)
import AnimationFrame


radiusResolution : Float
radiusResolution =
    0.025


type alias Point =
    ( Float, Float )


type alias Model =
    { width : Float
    , height : Float
    , bigR : Float
    , smallR : Float
    , points : List Point
    , time : Float
    , maxTime : Float
    , v : Float
    , live : Bool
    }


type Speed
    = Instantaneous
    | Fast
    | Medium
    | Slow


type Msg
    = SetBigR Float
    | SetSmallR Float
    | SetSpeed Speed
    | Tick Float
    | Reset


init : Float -> Float -> Model
init width height =
    update Reset
        { width = width
        , height = height
        , bigR = 0.35
        , smallR = 0.35
        , points = []
        , time = 0
        , maxTime = 0
        , v = 1 / 200
        , live = False
        }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.live then
        AnimationFrame.diffs Tick
    else
        Sub.none



--


centers : Model -> Float -> ( Point, Point )
centers { bigR, smallR, v } time =
    let
        ( t, t_ ) =
            ( time * v, -(1 - bigR) / bigR * time * v )

        ( xCirc, yCirc ) =
            ( (1 - bigR) * cos t
            , (1 - bigR) * sin t
            )

        ( xDot, yDot ) =
            ( xCirc + bigR * smallR * cos t_
            , yCirc + bigR * smallR * sin t_
            )
    in
        ( ( xCirc, yCirc ), ( xDot, yDot ) )


{-|
   solve for lowest n:
     n * 6/4 = int
   => n = 4 / 2 = 4 / gcd(6, 4)
   The resulting integer would be 4 / gcd(6, 4) * 6/4 = 6 / gcd(6, 4) = 3
   Meaning, nominator / gcd (nominator, denominator)

   We want both theta1 = 2pi k and theta2 = 2pi m
     time = 2pi k / v = 2pi m / v * r/(1-r)
   this means we're looking for integers k, m, s.t. k = m * r/(1-r)

   therefore, k = r / gcd (r, 1-r)
   and t = 2pi / v * r / gcd (r, 1-r)

-}
maxTime : { r : Float, v : Float } -> Float
maxTime { r, v } =
    let
        gcd n m =
            if m == 0 then
                n
            else
                gcd m (n % m)

        ( nom, denom ) =
            ( round <| r / radiusResolution, round <| (1 - r) / radiusResolution )

        k =
            nom // gcd nom denom
    in
        2 * pi * toFloat k / v


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            if model.live then
                { model
                    | time = model.time + min 32 dt
                    , points =
                        if model.time <= model.maxTime then
                            centers model model.time |> Tuple.second |> \pt -> pt :: model.points
                        else
                            model.points
                }
            else
                model

        SetSpeed speed ->
            case speed of
                Instantaneous ->
                    update Reset { model | live = False }

                Fast ->
                    update Reset { model | live = True, v = 1 / 200 }

                Medium ->
                    update Reset { model | live = True, v = 1 / 300 }

                Slow ->
                    update Reset { model | live = True, v = 1 / 400 }

        SetBigR r ->
            update Reset { model | bigR = r }

        SetSmallR r ->
            update Reset { model | smallR = r }

        Reset ->
            let
                maxT =
                    maxTime { r = model.bigR, v = model.v }
            in
                if model.live then
                    { model | time = 0, points = [], maxTime = maxT }
                else
                    { model
                        | points =
                            maxT
                                / 16
                                |> ceiling
                                |> List.range 0
                                |> List.map ((*) 16 >> toFloat >> centers model >> Tuple.second)
                        , maxTime = maxT
                    }



--


svgView : Model -> Svg Msg
svgView ({ time, bigR, smallR } as model) =
    let
        ( xCenter, yCenter, scale ) =
            ( model.width / 2, model.height / 2, min model.width model.height / 2.2 )

        ( ( xCirc, yCirc ), ( xDot, yDot ) ) =
            centers model model.time

        path =
            polyline
                [ model.points
                    |> List.map (\( x, y ) -> toString (xCenter + scale * x) ++ "," ++ toString (yCenter + scale * y))
                    |> String.join " "
                    |> points
                , fill "none"
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        container =
            circle
                [ cx <| toString xCenter
                , cy <| toString yCenter
                , r <| toString <| 1 * scale
                , fill "none"
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        circ =
            circle
                [ cx <| toString <| xCenter + scale * xCirc
                , cy <| toString <| xCenter + scale * yCirc
                , r <| toString <| bigR * scale
                , fill "none"
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        dot =
            circle
                [ cx <| toString <| xCenter + scale * xDot
                , cy <| toString <| xCenter + scale * yDot
                , r <| "2"
                , fill "black"
                ]
                []
    in
        svg [ width <| toString model.width, height <| toString model.height ]
            (if model.live then
                [ container, circ, dot, path ]
             else
                [ path ]
            )


view : Model -> Html Msg
view model =
    let
        onValue msg =
            onInput (String.toFloat >> Result.withDefault 0 >> msg)

        radio msg name title checked =
            div [ style [ ( "display", "flex" ), ( "align-items", "center" ) ] ]
                [ input
                    [ type_ "radio"
                    , Attrs.name name
                    , onClick msg
                    , Attrs.checked checked
                    ]
                    []
                , text title
                ]
    in
        div
            [ style
                [ ( "display", "flex" )
                , ( "flex-direction", "column" )
                , ( "align-items", "center" )
                , ( "margin", "20px auto" )
                ]
            ]
            [ div [ style [ ( "display", "flex" ) ] ]
                [ radio (SetSpeed Instantaneous) "v" "Instantaneous" True
                , radio (SetSpeed Fast) "v" "Fast" False
                , radio (SetSpeed Medium) "v" "Medium" False
                , radio (SetSpeed Slow) "v" "Slow" False
                ]
            , text "Big radius"
            , input
                [ onValue SetBigR
                , type_ "range"
                , Attrs.min "0"
                , Attrs.max "1"
                , Attrs.step <| toString radiusResolution
                , defaultValue "0.8"
                ]
                []
            , text "Small radius"
            , input
                [ onValue SetSmallR
                , type_ "range"
                , Attrs.min "0"
                , Attrs.max "1"
                , Attrs.step <| toString radiusResolution
                , defaultValue "0.8"
                ]
                []
            , svgView model
            ]



--


description : String
description =
    """
[Spirographs](https://en.wikipedia.org/wiki/Spirograph) are neat!
I got one in a Kinder Surprise in 2014.

I thought I'd have a lot to say about every project but I was wrong.
"""


main : Program Never Model Msg
main =
    program
        { init = ( init 500 500, Cmd.none )
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view |> project 22 description
        }
