module Gravity exposing (..)

import Html exposing (Html, program)
import Svg exposing (Svg, svg, circle)
import Svg.Keyed exposing (node)
import Svg.Attributes exposing (width, height, cx, cy, r, fill)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
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
    , id : Int
    }


type alias Ball =
    { x : Float
    , y : Float
    , v : Float
    , radius : Float
    , bounced : Bool
    , color : Color
    , id : String
    }


type Msg
    = Tick Float
    | Add Ball
    | Mouse ( Float, Float )


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , g = 0.0005
      , balls = []
      , id = 0
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
                    ( model.width / 2 + model.width / 6 * cos (t / 300)
                    , model.height / 2 + model.height / 6 * sin (t / 300)
                    )
            )
        ]



--


randomBall : String -> Float -> Float -> Random.Generator Ball
randomBall id x y =
    let
        ball r c v =
            { x = x, y = y, v = v, radius = r, color = c, bounced = False, id = id }
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
            ( { model | id = model.id + 1 }
            , if 0 <= x && x <= model.width && 0 <= y && y <= model.height then
                Random.generate Add (randomBall (toString model.id) x (model.height - y))
              else
                Cmd.none
            )

        Add ball ->
            ( { model | balls = ball :: model.balls }, Cmd.none )



--


view : Model -> Svg Msg
view model =
    let
        ball { id, x, y, radius, color } =
            ( id
            , circle
                [ cx <| toString <| x
                , cy <| toString <| model.height - y
                , r <| toString radius
                , fill <| colorToCssRgb color
                ]
                []
            )
    in
        List.map ball model.balls
            |> node "svg"
                [ width <| toString model.width
                , height <| toString model.height
                ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
