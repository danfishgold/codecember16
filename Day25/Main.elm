module Day25.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Collage exposing (group, polygon, rotate, shift)
import Collage.Render
import Color exposing (Color)
import Day25.Vector as Vector exposing (..)
import Helper exposing (filled)
import Html exposing (Html)
import Random


type alias Boid =
    { r : Vector, v : Vector, c : Color }


type alias Rule =
    { rule : Boid -> List Boid -> Vector
    , weight : Float
    , radius : Float
    }


rules : List Rule
rules =
    [ Rule cohesion 3.0 100
    , Rule alignment 0.01 20
    , Rule separation 0.2 50

    -- , Rule center 4 0
    ]


type alias Model =
    { width : Float
    , height : Float
    , boids : List Boid
    , maxSpeed : Float
    }


type Msg
    = SetBoids (List Boid)
    | Tick Float


init : Float -> Float -> Float -> ( Model, Cmd Msg )
init maxSpeed width height =
    ( { width = width
      , height = height
      , boids = []
      , maxSpeed = maxSpeed
      }
    , Random.generate SetBoids (Random.list 50 (randomBoid maxSpeed))
    )



--


randomBoid : Float -> Random.Generator Boid
randomBoid maxSpeed =
    let
        v =
            Random.map2 Vector.polar
                (Random.float 0 maxSpeed)
                (Random.float 0 360 |> Random.map degrees)

        r =
            Random.map2 (\a b -> ( a, b ))
                (Random.float -0.5 0.5)
                (Random.float -0.5 0.5)

        boid r_ v_ =
            Boid r_ v_ Color.black
    in
    Random.map2 boid r v



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Tick



--


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            { model | boids = model.boids |> updateBoids dt model.maxSpeed }

        SetBoids boids ->
            { model | boids = boids }


updateBoids : Float -> Float -> List Boid -> List Boid
updateBoids dt maxSpeed boids =
    let
        velocityFromRule boid { rule, weight, radius } =
            boids
                |> neighborsWithin boid radius
                |> rule boid
                |> Vector.mul weight

        applyRules boid =
            rules
                |> List.map (velocityFromRule boid)
                |> Vector.sum
                |> Vector.normalizeOrZero (maxSpeed / 100)
                |> (\v -> { boid | v = Vector.add boid.v v |> Vector.capMagnitude maxSpeed })

        updateLocation dt_ boid =
            { boid | r = add boid.r (mul dt_ boid.v) }
    in
    boids |> List.map applyRules |> List.map (updateLocation dt) |> List.map modPosition


modPosition : Boid -> Boid
modPosition boid =
    let
        mod x_ =
            if x_ > 0.5 then
                mod (x_ - 1)

            else if x_ < -0.5 then
                mod (x_ + 1)

            else
                x_

        ( x, y ) =
            boid.r
    in
    { boid | r = ( mod x, mod y ) }


neighborsWithin : Boid -> Float -> List Boid -> List Boid
neighborsWithin this rad boids =
    boids
        |> List.filter ((/=) this)
        |> List.filter (\other -> Vector.dist2 other.r this.r <= rad ^ 2)


separation : Boid -> List Boid -> Vector
separation { r, v } boids =
    if List.isEmpty boids then
        ( 0, 0 )

    else
        boids
            |> List.map (.r >> Vector.sub r)
            |> Vector.sum
            |> Vector.normalizeOrZero 1


alignment : Boid -> List Boid -> Vector
alignment { r, v } boids =
    if List.isEmpty boids then
        ( 0, 0 )

    else
        boids
            |> List.map .v
            |> Vector.sum
            |> Vector.normalizeOrZero 1


cohesion : Boid -> List Boid -> Vector
cohesion { r, v } boids =
    if List.isEmpty boids then
        ( 0, 0 )

    else
        boids
            |> List.map .r
            |> Vector.sum
            |> Vector.mul (1 / (toFloat <| List.length boids))
            |> Vector.normalizeOrZero 1


center : Boid -> List Boid -> Vector
center { r } _ =
    Vector.sub ( 0, 0 ) r



--


view : Model -> Html Msg
view { width, height, boids } =
    let
        boidPolygon { r, v, c } =
            polygon [ ( -4, -4 ), ( 4, -4 ), ( 0, 8 ) ]
                |> filled c
                |> shift (vecMul r ( width, height ))
                |> rotate (Vector.arg v)
    in
    boids
        |> List.map boidPolygon
        |> group
        |> Collage.Render.svgBox ( width, height )



--


description : String
description =
    """
[Boids](https://en.wikipedia.org/wiki/Boids) are cool.
I don't know when I first encountered them, but they sure are cool.
"""


page =
    { init = always <| init 0.001 500 500
    , subscriptions = subscriptions
    , update = \msg model -> ( update msg model, Cmd.none )
    , title = "Boids"
    , body = view
    , description = description
    }
