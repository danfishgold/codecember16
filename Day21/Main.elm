module StarSystem exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g, circle)
import Svg.Attributes exposing (width, height, cx, cy, r, stroke, strokeWidth, fill, transform)
import AnimationFrame
import Random
import Random.Extra
import Keyboard exposing (KeyCode)


type alias Model =
    { width : Float
    , height : Float
    , planets : List Planet
    , sunMass : Float
    , count : Int
    , time : Float
    }


type alias Moon =
    -- mass and distance from axis
    { m : Float, r : Float }


type alias Planet =
    -- mass, distance from axis, and moons
    { m : Float
    , r : Float
    , moons : List Moon
    }


type Msg
    = SetPlanets (List Planet)
    | Key KeyCode
    | Tick Float


init : Float -> Float -> Int -> Float -> ( Model, Cmd Msg )
init width height count sunMass =
    ( { width = width
      , height = height
      , count = count
      , planets = []
      , sunMass = sunMass
      , time = 0
      }
    , makePlanets count sunMass
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs Tick
        , Keyboard.ups Key
        ]



--


makePlanets : Int -> Float -> Cmd Msg
makePlanets count sunMass =
    let
        planet =
            Random.map3 Planet
                (Random.float 0.1 0.2 |> Random.map ((*) sunMass))
                (Random.float (3 * sqrt sunMass) 1)
                (Random.Extra.rangeLengthList 0 10 moon)

        moon =
            Random.map2 Moon
                (Random.float 0.01 0.02 |> Random.map ((*) sunMass))
                (Random.float 0.01 0.02)
    in
        Random.list count planet
            |> Random.generate SetPlanets



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlanets planets ->
            ( { model | planets = planets }, Cmd.none )

        Tick dt ->
            ( { model | time = model.time + dt }, Cmd.none )

        Key 32 ->
            ( model, makePlanets model.count model.sunMass )

        Key _ ->
            ( model, Cmd.none )



--


circle : String -> Float -> Float -> Float -> Svg msg
circle color x y rad =
    Svg.circle
        [ fill color
        , stroke "black"
        , strokeWidth "1"
        , cx <| toString x
        , cy <| toString y
        , r <| toString rad
        ]
        []


view : Model -> Svg Msg
view ({ time, planets, sunMass } as model) =
    let
        ( centerX, centerY, scale ) =
            ( model.width / 2, model.height / 2, min model.width model.height / 2 )

        circ color x y rad =
            circle color (centerX + scale * x) (centerY + scale * y) (scale * rad)

        sun =
            circ "white" 0 0 (sqrt sunMass)

        tracks =
            planets |> List.map (\p -> circ "none" 0 0 (p.r)) |> g []
    in
        [ sun, tracks ]
            |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500 4 0.01
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
