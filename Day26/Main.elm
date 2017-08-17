module Jam exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (width, height, cx, cy, r, fill, stroke, strokeWidth)
import Svg.Events exposing (onMouseOver, onMouseOut)
import AnimationFrame
import Time exposing (Time, second)


type alias Car =
    { x : Float
    , v : Float
    , len : Float
    , s0 : Float
    , v0 : Float
    , aMax : Float
    , reactionTime : Float
    , deceleration : Float
    , delta : Float
    }


type alias Model =
    { width : Float
    , height : Float
    , cars : List Car
    , obstacle : Bool
    }


type Msg
    = Tick Time
    | AddObstacle
    | RemoveObstacle


init : Float -> Float -> Int -> ( Model, Cmd Msg )
init width height count =
    ( { width = width
      , height = height
      , cars = List.range 1 count |> List.map (\i -> toFloat i / toFloat count) |> List.map car
      , obstacle = False
      }
    , Cmd.none
    )


car : Float -> Car
car x =
    let
        sec =
            second

        m =
            1 / 500
    in
        { x = x
        , v = 0
        , len = 2 * m
        , s0 = 3 * m
        , v0 = 60 * m / sec
        , aMax = 24 * m / sec ^ 2
        , reactionTime = 0.5 * sec
        , deceleration = 12 * m / sec ^ 2
        , delta = 4
        }


description : String
description =
    """
"""


main : Program Never Model Msg
main =
    program
        { init = init 500 500 35
        , subscriptions = subscriptions
        , update = update
        , view = view |> project 26 descriptionI like systems where local changes cause a global trend.
Among all these projects, four are examples of such systems, including this one.

At first I tried to come up with a model for trafic, but that proved to be much
harder than I expected. I did some googling and found
[this beautiful project](https://github.com/volkhin/RoadTrafficSimulator)
which uses the
[Intelligent Driver Model](https://en.wikipedia.org/wiki/Intelligent_driver_model).

This was a lot of fun.

## Instructions

Hover over the ring road to create a trafic jam.

        }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | cars =
                    let
                        carSortValue { x, v, reactionTime } =
                            if x + v * reactionTime > 1 then
                                x - 1
                            else
                                x

                        cars =
                            List.sortBy carSortValue model.cars
                    in
                        case cars of
                            [] ->
                                []

                            first :: rest ->
                                if model.obstacle then
                                    cars ++ [ car 0 ] |> mapPairs (updateCar dt)
                                else
                                    cars
                                        ++ [ first ]
                                        |> mapPairs (updateCar dt)
              }
            , Cmd.none
            )

        AddObstacle ->
            ( { model | obstacle = True }, Cmd.none )

        RemoveObstacle ->
            ( { model | obstacle = False }, Cmd.none )


updateCar : Time -> Car -> { a | x : Float, v : Float, len : Float } -> Car
updateCar dt ({ x, v, v0, aMax, s0, reactionTime, deceleration, delta } as car) next =
    if dt /= 0 then
        let
            modAfter a b =
                if b >= a + 1 then
                    modAfter a (b - 1)
                else if b < a then
                    modAfter a (b + 1)
                else
                    b

            s =
                modAfter x (next.x - next.len) - x

            s_ =
                s0 + v * reactionTime + v * (v - next.v) / (2 * sqrt (aMax * deceleration))

            a =
                aMax
                    * (1 - (v / v0) ^ delta - (s_ / s) ^ 2)

            v1 =
                v + dt * a

            x1 =
                x + v * dt |> modAfter 0
        in
            { car | x = x1, v = v1 }
    else
        car



--


mapPairs : (a -> a -> b) -> List a -> List b
mapPairs fn xs =
    case xs of
        fst :: snd :: rest ->
            fn fst snd :: mapPairs fn (snd :: rest)

        _ ->
            []



--


view : Model -> Svg Msg
view model =
    let
        ( centerX, centerY ) =
            ( model.width / 2, model.height / 2 )

        circle x y rad fillColor strokeColor width =
            Svg.circle
                [ cx <| toString <| centerX + x
                , cy <| toString <| centerY + y
                , r <| toString rad
                , fill fillColor
                , stroke strokeColor
                , strokeWidth <| toString width
                ]
                []

        ( ringRad, ringWidth ) =
            ( min model.width model.height |> \l -> l / 3, 15 )

        ring =
            circle 0 0 ringRad "none" "lightgray" ringWidth

        car { x } =
            circle (ringRad * cos (x * 2 * pi)) (ringRad * sin (x * 2 * pi)) (ringWidth / 3) "black" "none" 0

        obstacle =
            circle ringRad 0 3 "red" "none" 0
    in
        [ g [ onMouseOver AddObstacle, onMouseOut RemoveObstacle ]
            [ ring
            , model.cars |> List.map car |> g []
            , if model.obstacle then
                obstacle
              else
                Svg.text ""
            ]
        ]
            |> svg [ width <| toString model.width, height <| toString model.height ]
