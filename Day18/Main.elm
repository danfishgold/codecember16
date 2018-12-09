module RotatingPolygon exposing (main)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Day18.Gradient exposing (gradient, gradientStroke)
import Helper exposing (onEnter, project)
import Random
import Random.Extra
import Svg exposing (Svg, circle, defs, g, line, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, strokeWidth, width, x1, x2, y1, y2)
import Time


type alias Model =
    { width : Float
    , height : Float
    , t : Float
    , vertices : List Vertex
    }


type Msg
    = Tick Time.Posix
    | SetVertices (List Vertex)
    | Randomize


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
      , vertices = []
      }
    , randomizeVertices width height 9
    )


randomizeVertices : Float -> Float -> Int -> Cmd Msg
randomizeVertices wd ht n =
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

        makeVertex r x_ y_ w_ ph =
            Vertex x_ y_ r w_ ph

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
        |> Random.generate SetVertices



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , onEnter Randomize
        ]



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            ( { model | t = toFloat <| Time.posixToMillis posix }, Cmd.none )

        SetVertices vertices ->
            ( { model | vertices = vertices }, Cmd.none )

        Randomize ->
            ( model, randomizeVertices model.width model.height 9 )



--


vertexParameters : Float -> Vertex -> ( Float, Float, Color )
vertexParameters t v =
    let
        theta =
            v.w * t + v.phase

        hue =
            theta / degrees 360 - toFloat (floor (theta / degrees 360))
    in
    ( v.cx + v.r * cos theta, v.cx + v.r * sin theta, Color.hsl hue 1 0.5 )


pairs : List a -> List ( a, a )
pairs xs =
    let
        withoutOverflow xs_ =
            case xs_ of
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
        vertices =
            List.map (vertexParameters model.t) model.vertices

        edges =
            pairs vertices

        gradients =
            edges
                |> List.indexedMap
                    (\i edge ->
                        gradient (String.fromInt i) edge
                    )

        point ( x, y, c ) =
            Svg.circle
                [ cx <| String.fromFloat x
                , cy <| String.fromFloat y
                , r <| "2"
                , fill <| Color.toCssString c
                ]
                []

        line i ( ( xa, ya, _ ), ( xb, yb, _ ) ) =
            Svg.line
                [ x1 <| String.fromFloat xa
                , y1 <| String.fromFloat ya
                , x2 <| String.fromFloat xb
                , y2 <| String.fromFloat yb
                , gradientStroke <| String.fromInt i
                , strokeWidth "4"
                ]
                []
    in
    [ Svg.defs [] gradients
    , edges |> List.indexedMap line |> g []
    , vertices |> List.map point |> g []
    ]
        |> svg [ width <| String.fromFloat model.width, height <| String.fromFloat model.height ]



--


description : String
description =
    """
**Originally from May 2015**

I'm not sure what inspired me in May 2015. It probably had something to do with
spirographs (more on that later) and Monument Valley.
"""


main : Program () Model Msg
main =
    document
        { init = always <| init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view |> project 18 description
        }
