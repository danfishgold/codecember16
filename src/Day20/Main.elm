module Day20.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Day02.Random exposing (ryb1)
import Helper exposing (projectSvgAttrs)
import Html exposing (Html)
import Pointer
import Random
import Svg exposing (Svg, circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, width)
import Svg.Keyed exposing (node)
import Time


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
        [ Browser.Events.onAnimationFrameDelta Tick
        , Time.every 10
            (\posix ->
                let
                    t =
                        toFloat <| Time.posixToMillis posix
                in
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
                Random.generate Add (randomBall (String.fromInt model.id) x (model.height - y))

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
                [ cx <| String.fromFloat <| x
                , cy <| String.fromFloat <| model.height - y
                , r <| String.fromFloat radius
                , fill <| Color.toCssString color
                ]
                []
            )
    in
    List.map ball model.balls
        |> node "svg"
            (Pointer.onMove Mouse :: projectSvgAttrs False ( model.width, model.height ))



--


description : String
description =
    """
**Originally from March 2009**

In 2009 or so I saved up enough money to buy an iPod Touch.
I started getting into Objective-C.
I'm not sure if I tried writing apps before I actually got an iPod,
but that's not important, because over the years I started learning Obj-C (and
quit soon after) about five times. Swift is what finally made it stick :)

Anyway, my first project was this:
you tap the screen and a square appers and starts growing.
When you let go, it jumps up a little and drops below the screen.
It was very fun.
Version 1.1 included fart sounds when you let the square go.

## Instructions

Use the mouse to make tiny circles or just look at the ones in the big circle.
"""


page =
    { init = always <| init 500 500
    , subscriptions = subscriptions
    , update = update
    , title = "Gravity"
    , body = view
    , description = description
    }
