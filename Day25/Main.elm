module Boids exposing (..)

import Html exposing (Html, program)
import Collage exposing (collage, polygon, filled, move, rotate)
import Element
import AnimationFrame
import Color exposing (Color)
import Day25.Vector as Vector exposing (..)
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
    [ Rule separation 0.2 30
    , Rule alignment 1 30
    , Rule cohesion 30 30
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
      , boids =
            [ Boid ( 0, 0 ) ( 0.2, 0 ) Color.black
            , Boid ( 0.2, 0.2 ) ( 0.1, 0.0 ) Color.black
            , Boid ( -0.2, 0.2 ) ( 0.13, 0.0 ) Color.black
            ]
      , maxSpeed = maxSpeed
      }
    , Random.generate SetBoids (Random.list 30 (randomBoid maxSpeed))
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
            Random.map2 (,)
                (Random.float -0.5 0.5)
                (Random.float -0.5 0.5)

        boid r v =
            Boid r v Color.black
    in
        Random.map2 boid r v



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            { model
                | boids =
                    model.boids
                        |> updateBoids dt model.maxSpeed
            }

        SetBoids boids ->
            { model | boids = boids }


updateBoids : Float -> Float -> List Boid -> List Boid
updateBoids dt maxSpeed boids =
    let
        velocityFromRule boid { rule, weight, radius } =
            boids
                |> neighborsWithin boid radius
                |> \bs ->
                    if List.isEmpty bs then
                        ( 0, 0 )
                    else
                        bs
                            |> rule boid
                            |> Vector.mul weight

        applyRules boid =
            rules
                |> List.map (velocityFromRule boid)
                |> Vector.sum
                |> \v -> { boid | v = Vector.add boid.v v |> Vector.capMagnitude maxSpeed }

        updateLocation dt boid =
            { boid | r = add boid.r (mul dt boid.v) }
    in
        boids |> List.map applyRules |> List.map (updateLocation dt) |> List.map modPosition


modPosition : Boid -> Boid
modPosition boid =
    let
        mod x =
            if x > 0.5 then
                mod (x - 1)
            else if x < -0.5 then
                mod (x + 1)
            else
                x

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
    boids
        |> List.map (.r >> Vector.sub r)
        |> Vector.sum
        |> Vector.normalizeOrZero 1


alignment : Boid -> List Boid -> Vector
alignment { r, v } boids =
    boids
        |> List.map .v
        |> Vector.sum
        |> Vector.normalizeOrZero 1


cohesion : Boid -> List Boid -> Vector
cohesion { r, v } boids =
    boids
        |> List.map .r
        |> Vector.sum
        |> Vector.mul (1 / (toFloat <| List.length boids))
        |> Vector.normalizeOrZero 1



--


view : Model -> Html Msg
view { width, height, boids } =
    let
        boidPolygon { r, v, c } =
            polygon [ ( -4, -4 ), ( 4, -4 ), ( 0, 8 ) ]
                |> filled c
                |> move (vecMul r ( width, height ))
                |> rotate (Vector.arg v)
    in
        boids
            |> List.map boidPolygon
            |> collage (floor width) (floor height)
            |> Element.toHtml



--


main : Program Never Model Msg
main =
    program
        { init = init 0.002 500 500
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        }
