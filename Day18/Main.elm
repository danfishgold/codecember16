module RotatingPolygon exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g, defs, circle, line)
import Svg.Attributes exposing (width, height, cx, cy, r, x1, y1, x2, y2, strokeWidth, fill)
import Day18.Gradient exposing (gradient, gradientStroke)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Random
import AnimationFrame
import Random.Extra
import Keyboard exposing (KeyCode)


type alias Model =
    { width : Float
    , height : Float
    , t : Float
    , vertexes : List Vertex
    }


type Msg
    = Tick Float
    | SetVertexes (List Vertex)
    | Key KeyCode


type alias Vertex =
    { cx : Float
    , cy : Float
    , r : Float
    , w : Float
    , phase : Float
    }


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , t = 0
      , vertexes = []
      }
    , randomizeVertexes width height 9
    )


randomizeVertexes : Float -> Float -> Int -> Cmd Msg
randomizeVertexes wd ht n =
    let
        radius =
            Random.float (min wd ht / 6) (min wd ht / 4)

        x r =
            Random.float (2 * r) (wd - 2 * r)

        y r =
            Random.float (2 * r) (ht - 2 * r)

        w =
            Random.float 0.001 0.002
                |> Random.andThen
                    (\absValue ->
                        Random.Extra.sample [ absValue, -absValue ]
                            |> Random.map (Maybe.withDefault absValue)
                    )

        phase =
            Random.float 0 (degrees 360)

        makeVertex r x y w ph =
            Vertex x y r w ph

        vertex =
            radius
                |> Random.andThen
                    (\r ->
                        Random.map4
                            (makeVertex r)
                            (x r)
                            (y r)
                            w
                            phase
                    )
    in
        Random.list n vertex
            |> Random.generate SetVertexes



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.times Tick
        , Keyboard.ups Key
        ]



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick t ->
            ( { model | t = t }, Cmd.none )

        SetVertexes vertexes ->
            ( { model | vertexes = vertexes }, Cmd.none )

        Key 32 ->
            ( model, randomizeVertexes model.width model.height 9 )

        Key _ ->
            ( model, Cmd.none )



--


vertexParameters : Float -> Vertex -> ( Float, Float, Color )
vertexParameters t v =
    let
        theta =
            v.w * t + v.phase
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
                , r <| "2"
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
                , strokeWidth "4"
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
