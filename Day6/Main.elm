port module Poisson exposing (..)

import Html exposing (programWithFlags)
import Html exposing (Html, div, text)
import Svg exposing (Svg, svg, g)
import Svg.Keyed exposing (node)
import Svg.Attributes exposing (cx, cy, r, fill, width, height)
import Set exposing (Set)
import Color exposing (Color, black, red)
import Color.Convert exposing (colorToCssRgb)
import Time exposing (Time, second)
import AnimationFrame exposing (times)


type alias Point =
    ( Float, Float )


type alias Flags =
    { width : Float
    , height : Float
    , r : Float
    , k : Int
    }


type alias Model =
    { candidates : Set Point
    , finals : List ( Point, Time )
    , width : Float
    , height : Float
    , finished : Bool
    , time : Time
    }


type Msg
    = AddCandidate Point
    | MoveToFinal Point
    | Finished
    | Tick Time


init : Flags -> ( Model, Cmd Msg )
init { width, height } =
    ( { candidates = Set.empty
      , finals = []
      , width = width
      , height = height
      , finished = False
      , time = 0
      }
    , Cmd.none
    )



--


port checkRandomCandidate : () -> Cmd msg


port candidates : (Point -> msg) -> Sub msg


port finals : (Point -> msg) -> Sub msg


port finished : (() -> msg) -> Sub msg



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ candidates AddCandidate
        , finals MoveToFinal
        , finished (always Finished)
        , if model.finished then
            Sub.none
          else
            times Tick
        ]



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick time ->
            ( { model | time = time }, checkRandomCandidate () )

        MoveToFinal p ->
            ( { model
                | candidates = model.candidates |> Set.remove p
                , finals = ( p, model.time ) :: model.finals
              }
            , Cmd.none
            )

        AddCandidate ( x, y ) ->
            ( { model
                | candidates = Set.insert ( x, y ) model.candidates
              }
            , Cmd.none
            )

        Finished ->
            ( { model | finished = True }, Cmd.none )



--


circle : Color -> Float -> Point -> Svg msg
circle c rad ( x, y ) =
    Svg.circle
        [ cx <| toString x
        , cy <| toString y
        , r <| toString rad
        , fill <| colorToCssRgb c
        ]
        []


view : Model -> Html Msg
view model =
    let
        radius dt =
            dt / (0.2 * second) |> min 1 |> ((*) 2)

        point ( pt, t ) =
            ( toString pt, circle black (radius <| model.time - t) pt )

        finals =
            model.finals |> List.map point
    in
        svg
            [ width <| toString model.width
            , height <| toString model.height
            ]
            [ node "g" [] finals
            ]



--


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
