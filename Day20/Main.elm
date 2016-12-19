module Gravity exposing (..)

import Html exposing (Html, program)
import Collage exposing (collage, circle, filled, move)
import Element
import Color exposing (Color)
import Mouse
import AnimationFrame
import Random
import Day2.Random exposing (ryb1)
import Time exposing (second)


type alias Model =
    { width : Float
    , height : Float
    , g : Float
    , balls : List Ball
    }


type alias Ball =
    { x : Float
    , y : Float
    , v : Float
    , radius : Float
    , bounced : Bool
    , color : Color
    }


type Msg
    = Tick Float
    | Add Ball
    | Mouse ( Float, Float )


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , g = 0.001
      , balls = []
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Mouse.moves (\{ x, y } -> Mouse ( toFloat x, toFloat y ))
        , Time.every (0.01 * second)
            (\t ->
                Mouse
                    ( model.width / 2 + model.width / 8 * cos (t / 200)
                    , model.height / 2 + model.height / 8 * sin (t / 200)
                    )
            )
        ]



--


randomBall : Float -> Float -> Random.Generator Ball
randomBall x y =
    let
        ball r c v =
            { x = x, y = y, v = v, radius = r, color = c, bounced = False }
    in
        Random.map3 ball
            (Random.float 2 5)
            (ryb1 1 0.5)
            (Random.float 0 0.2)


updateBall : Float -> Float -> Ball -> Maybe Ball
updateBall g dt ({ v, y } as ball) =
    let
        yNew =
            y + dt * v - 1 / 2 * dt * dt * g

        vNew =
            v - dt * g
    in
        if yNew > ball.radius then
            Just { ball | y = yNew, v = vNew }
        else if not ball.bounced then
            Just { ball | v = -vNew * 0.5, y = 2 * ball.radius - yNew, bounced = True }
        else if yNew > -ball.radius then
            Just { ball | y = yNew }
        else
            Nothing


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | balls =
                    model.balls
                        |> List.filterMap (updateBall model.g dt)
              }
            , Cmd.none
            )

        Mouse ( x, y ) ->
            ( model
            , if 0 <= x && x <= model.width && 0 <= y && y <= model.height then
                Random.generate Add (randomBall x (model.height - y))
              else
                Cmd.none
            )

        Add ball ->
            ( { model | balls = ball :: model.balls }, Cmd.none )



--


view : Model -> Html Msg
view model =
    let
        ball { x, y, radius, color } =
            circle radius |> filled color |> move ( x - model.width / 2, y - model.height / 2 )
    in
        List.map ball model.balls
            |> collage (floor model.width) (floor model.height)
            |> Element.toHtml



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
