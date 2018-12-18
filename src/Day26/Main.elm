module Day26.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Helper exposing (projectSvg)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (on, onMouseDown, onMouseUp)
import Json.Decode
import Svg exposing (Svg, g, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, width)


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
    = Tick Float
    | AddObstacle
    | RemoveObstacle


init : Float -> Float -> Int -> ( Model, Cmd Msg )
init width height count =
    ( { width = width
      , height = height
      , cars =
            List.range 1 count
                |> List.map (\i -> toFloat i / toFloat count)
                |> List.map carAtPosition
      , obstacle = False
      }
    , Cmd.none
    )


carAtPosition : Float -> Car
carAtPosition x =
    let
        sec =
            1000

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
I like systems where local changes cause a global trend.
Among all these projects, four are examples of such systems, including this one.

At first I tried to come up with a model for traffic, but that proved to be much
harder than I expected. I did some googling and found
[this beautiful project](https://github.com/volkhin/RoadTrafficSimulator)
which uses the
[Intelligent Driver Model](https://en.wikipedia.org/wiki/Intelligent_driver_model).

This was a lot of fun.
"""


page =
    { init = always <| init 500 500 35
    , subscriptions = subscriptions
    , update = update
    , title = "Jam"
    , body = view
    , description = description
    }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Tick



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
                                cars ++ [ carAtPosition 0 ] |> mapPairs (updateCar dt)

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


updateCar : Float -> Car -> { a | x : Float, v : Float, len : Float } -> Car
updateCar dt ({ x, v, v0, aMax, s0, reactionTime, deceleration, delta } as car) next =
    if dt /= 0 then
        let
            modAfter a_ b_ =
                if b_ >= a_ + 1 then
                    modAfter a_ (b_ - 1)

                else if b_ < a_ then
                    modAfter a_ (b_ + 1)

                else
                    b_

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
                [ cx <| String.fromFloat <| centerX + x
                , cy <| String.fromFloat <| centerY + y
                , r <| String.fromFloat rad
                , fill fillColor
                , stroke strokeColor
                , strokeWidth <| String.fromFloat width
                ]
                []

        ( ringRad, ringWidth ) =
            ( min model.width model.height |> (\l -> l / 3), 15 )

        ring =
            circle 0 0 ringRad "none" "lightgray" ringWidth

        car { x } =
            circle (ringRad * cos (x * 2 * pi)) (ringRad * sin (x * 2 * pi)) (ringWidth / 3) "black" "none" 0

        obstacle =
            circle ringRad 0 3 "red" "none" 0
    in
    div []
        [ projectSvg ( model.width, model.height )
            []
            [ g []
                [ ring
                , model.cars |> List.map car |> g []
                , if model.obstacle then
                    obstacle

                  else
                    Svg.text ""
                ]
            ]
        , div []
            [ button
                [ style "-webkit-user-select" "none"
                , on "touchstart" (Json.Decode.succeed AddObstacle)
                , on "touchend" (Json.Decode.succeed RemoveObstacle)
                , onMouseDown AddObstacle
                , onMouseUp RemoveObstacle
                ]
                [ text "Hold for red light" ]
            ]
        ]
