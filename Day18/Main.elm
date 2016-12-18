module Pentagram exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g, defs, circle, line)
import Svg.Attributes exposing (width, height, cx, cy, r, x1, y1, x2, y2, strokeWidth, fill)
import Day18.Gradient exposing (gradient, gradientStroke)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Random
import AnimationFrame


type alias Model =
    { width : Float
    , height : Float
    , t : Float
    , vertexes : List Vertex
    }


type Msg
    = Tick Float
    | SetVertexes (List Vertex)


type alias Vertex =
    { cx : Float
    , cy : Float
    , r : Float
    , w : Float
    }


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , t = 0
      , vertexes =
            [ Vertex 200 200 50 0.001
            , Vertex 230 190 100 0.0025
            , Vertex 150 300 20 0.002
            ]
      }
    , randomizeVertexes 5
    )


randomizeVertexes : Int -> Cmd Msg
randomizeVertexes n =
    let
        radius =
            Random.float

        edge =
            3
    in
        Cmd.none



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.times Tick



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { model | t = t }, Cmd.none )

        SetVertexes vertexes ->
            ( { model | vertexes = vertexes }, Cmd.none )



--


color : Float -> Color
color theta =
    Color.hsl theta 1 0.5


vertexParameters : Float -> Vertex -> ( Float, Float, Color )
vertexParameters t v =
    let
        theta =
            v.w * t
    in
        ( v.cx + v.r * cos theta, v.cx + v.r * sin theta, Color.hsl theta 1 0.5 )


pairs : List a -> List ( a, a )
pairs xs =
    let
        withoutOverflow xs =
            case xs of
                fst :: snd :: rest ->
                    ( fst, snd ) :: withoutOverflow (snd :: rest)

                _ ->
                    []
    in
        case xs of
            fst :: rest ->
                withoutOverflow (xs ++ [ fst ])

            [] ->
                []


view : Model -> Svg Msg
view model =
    let
        vertexes =
            List.map (vertexParameters model.t) model.vertexes

        edges =
            pairs vertexes

        gradients =
            edges
                |> List.indexedMap
                    (\i edge ->
                        gradient (toString i) edge
                    )

        point ( x, y, c ) =
            Svg.circle
                [ cx <| toString x
                , cy <| toString y
                , r <| "3"
                , fill <| colorToCssRgb c
                ]
                []

        line i ( ( xa, ya, _ ), ( xb, yb, _ ) ) =
            Svg.line
                [ x1 <| toString xa
                , y1 <| toString ya
                , x2 <| toString xb
                , y2 <| toString yb
                , gradientStroke <| toString i
                , strokeWidth "3"
                ]
                []
    in
        [ Svg.defs [] gradients
        , edges |> List.indexedMap line |> g []
        , vertexes |> List.map point |> g []
        ]
            |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
