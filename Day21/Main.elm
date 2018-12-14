module Day21.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Helper exposing (onEnter)
import Random
import Random.Extra
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, stroke, strokeWidth, transform, width)


type alias Model =
    { width : Float
    , height : Float
    , planets : List Planet
    , sunRadius : Float
    , count : Int
    , time : Float
    }


type alias Object =
    { r : Float
    , d : Float
    , w : Float
    , phase : Float
    }


type alias Planet =
    { props : Object
    , moons : List Object
    }


type Msg
    = SetPlanets (List Planet)
    | Randomize
    | Tick Float


init : Float -> Float -> Int -> Float -> ( Model, Cmd Msg )
init width height count sunRadius =
    ( { width = width
      , height = height
      , count = count
      , planets = []
      , sunRadius = sunRadius
      , time = 0
      }
    , makePlanets count sunRadius
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrameDelta Tick
        , onEnter Randomize
        ]



--


diffs : List Float -> List Float
diffs xs =
    case xs of
        hd :: nk :: tl ->
            (nk - hd) :: diffs (nk :: tl)

        _ ->
            []


makePlanets : Int -> Float -> Cmd Msg
makePlanets count sunRadius =
    let
        planetProps =
            Random.map4 Object
                -- rad, dist, w, phase
                (Random.float 0.15 0.3 |> Random.map ((*) sunRadius))
                (Random.float (1.5 * sunRadius) 0.9)
                (Random.float 0.0005 0.0015)
                (Random.float 0 360 |> Random.map degrees)

        moon { r } =
            Random.map4 Object
                (Random.float 0.02 0.07 |> Random.map ((*) sunRadius))
                (Random.float 1.7 2.3 |> Random.map ((*) r))
                (Random.float 0.005 0.001)
                (Random.float 0 360 |> Random.map degrees)

        planet props =
            moon props
                |> Random.Extra.rangeLengthList 0 4
                |> Random.map (Planet props)

        minRad =
            List.map (.props >> .r)
                >> List.minimum
                >> Maybe.withDefault 0

        farEnough planets =
            planets
                |> List.map (.props >> .d)
                |> diffs
                |> List.all (\d -> d > 2 * minRad planets)
    in
    Random.list count (planetProps |> Random.andThen planet)
        |> Random.Extra.filter farEnough
        |> Random.generate SetPlanets



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPlanets planets ->
            ( { model | planets = planets }, Cmd.none )

        Tick dt ->
            ( { model | time = model.time + dt }, Cmd.none )

        Randomize ->
            ( model, makePlanets model.count model.sunRadius )



--


circle : String -> Float -> Float -> Float -> Svg msg
circle color x y rad =
    Svg.circle
        [ fill color
        , stroke "black"
        , strokeWidth "1"
        , cx <| String.fromFloat x
        , cy <| String.fromFloat y
        , r <| String.fromFloat rad
        ]
        []


view : Model -> Svg Msg
view ({ time, planets, sunRadius } as model) =
    let
        ( centerX, centerY, scale ) =
            ( model.width / 2, model.height / 2, min model.width model.height / 2 )

        circ color x y rad =
            circle color (centerX + scale * x) (centerY + scale * y) (scale * rad)

        bg =
            Svg.rect
                [ width <| String.fromFloat model.width
                , height <| String.fromFloat model.height
                , fill "#ddd"
                ]
                []

        sun =
            circ "#bbb" 0 0 sunRadius

        tracks =
            planets |> List.map (\p -> circ "none" 0 0 p.props.d) |> g []

        xy { d, w, phase } =
            ( d * cos (w * time + phase)
            , d * sin (w * time + phase)
            )

        planet p =
            let
                ( px, py ) =
                    xy p.props

                moon m =
                    let
                        ( mx, my ) =
                            xy m
                    in
                    circ "#ddd" (px + mx) (py + my) m.r
            in
            g [] [ circ "#888" px py p.props.r, List.map moon p.moons |> g [] ]
    in
    [ bg, sun, tracks, List.map planet planets |> g [] ]
        |> svg [ width <| String.fromFloat model.width, height <| String.fromFloat model.height ]



--


description : String
description =
    """
**Originally from May 2015**

I think I saw something similar to this on [bl.ocks.org](https://bl.ocks.org)
and wanted to make this. Space is nice, but this isn't physically accurate at all.

## Instructions

Hit enter to randomize.
"""


page =
    { init = always <| init 500 500 3 0.2
    , subscriptions = subscriptions
    , update = update
    , title = "Star System"
    , body = view
    , description = description
    }
