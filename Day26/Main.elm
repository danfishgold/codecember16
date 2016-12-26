module Jam exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (width, height, cx, cy, r, fill, stroke, strokeWidth)
import Svg.Events exposing (onMouseOver, onMouseOut)
import AnimationFrame


type alias Car =
    { x : Float
    , v : Float
    , alpha : Float
    , vMax : Float
    , aMax : Float
    , aMin : Float
    }


car : Int -> Float -> Car
car i x =
    let
        min =
            1 / 60

        sec =
            min / 60
    in
        { x = x
        , v = 0
        , alpha = 2 + 0.3 * sin (toFloat i)
        , vMax = 0.0002
        , aMax = 7 / sec
        , aMin = -14 / sec
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
      , cars = List.range 1 count |> List.map (\i -> toFloat i / toFloat count) |> List.indexedMap car
      , obstacle = False
      }
    , Cmd.none
    )



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
                | cars = model.cars |> List.sortBy .x |> mapPairs (updateCar dt)
              }
            , Cmd.none
            )

        AddObstacle ->
            ( { model | obstacle = True }, Cmd.none )

        RemoveObstacle ->
            ( { model | obstacle = False }, Cmd.none )


updateCar : Float -> Car -> { a | x : Float } -> Car
updateCar dt ({ x, v, alpha } as car) next =
    if dt /= 0 then
        let
            modAfter a b =
                if b >= a + 1 then
                    modAfter a (b - 1)
                else if b < a then
                    modAfter a (b + 1)
                else
                    b

            xDesired =
                modAfter x next.x - alpha * v - 0.05 |> min (x + dt * car.vMax) |> max x

            a =
                2 / (dt ^ 2) * (xDesired - x - v * dt) |> min car.aMax |> max car.aMin

            v1 =
                v + dt * a |> min car.vMax |> max 0

            x1 =
                x + v * dt + a * dt ^ 2 / 2 |> modAfter 0
        in
            { car | x = x1, v = v1 }
    else
        car



--


mapPairs : (a -> a -> b) -> List a -> List b
mapPairs fn xs =
    let
        withoutOverflow xs =
            case xs of
                fst :: snd :: rest ->
                    fn fst snd :: withoutOverflow (snd :: rest)

                _ ->
                    []
    in
        case xs of
            fst :: rest ->
                withoutOverflow ((List.drop (List.length xs - 1) xs) ++ xs)

            [] ->
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
            ( min model.width model.height |> \l -> l / 3, 30 )

        ring =
            g [ onMouseOver AddObstacle, onMouseOut RemoveObstacle ] [ circle 0 0 ringRad "none" "gray" ringWidth ]

        car { x } =
            circle (ringRad * cos (x * 2 * pi)) (ringRad * sin (x * 2 * pi)) (ringWidth / 3) "black" "none" 0
    in
        [ ring
        , model.cars |> List.map car |> g []
        ]
            |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500 10
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
