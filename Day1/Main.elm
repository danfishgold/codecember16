module Argyle exposing (..)

import Svg exposing (Svg, svg, g, line, polygon)
import Svg.Attributes exposing (points, fill, transform)
import Svg.Attributes exposing (strokeWidth, stroke, strokeDasharray, x1, x2, y1, y2)
import Svg.Attributes exposing (width, height)
import Html exposing (programWithFlags)
import Keyboard exposing (KeyCode)
import Random
import Random.Color
import String
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)


type alias Model =
    { color1 : Color
    , color2 : Color
    , color3 : Color
    , lineColor : Color
    , shift : ( Float, Float )
    , width : Float
    , aspectRatio : Float
    , window : WindowSize
    }


type alias Flags =
    { width : Float
    , height : Float
    , randomize : Bool
    }


type alias WindowSize =
    { width : Float, height : Float }


init : Flags -> ( Model, Cmd Msg )
init { width, height, randomize } =
    ( { color1 = Color.blue
      , color2 = Color.green
      , color3 = Color.red
      , lineColor = Color.gray
      , shift = ( 0, 0 )
      , width = 0.15
      , aspectRatio = 1.5
      , window = WindowSize width height
      }
    , if randomize then
        Random.generate SetModel (randomModel <| WindowSize width height)
      else
        Cmd.none
    )


pattern : WindowSize -> Int -> Model
pattern window seed =
    seed
        |> Random.initialSeed
        |> Random.step (randomModel window)
        |> Tuple.first


type Msg
    = KeyPressed KeyCode
    | SetModel Model



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "message" msg) of
        KeyPressed 32 ->
            ( model, Random.generate SetModel (randomModel model.window) )

        KeyPressed _ ->
            ( model, Cmd.none )

        SetModel newModel ->
            ( newModel
            , Cmd.none
            )



--


randomModel : WindowSize -> Random.Generator Model
randomModel window =
    let
        shift =
            Random.map2 (,)
                (Random.float 0 1)
                (Random.float 0 1)

        colors =
            Random.map4 (,,,)
                Random.Color.hsl
                Random.Color.hsl
                Random.Color.hsl
                Random.Color.hsl

        width =
            Random.float 0.08 0.2

        aspectRatio =
            Random.float 1.2 1.7

        model ( c1, c2, c3, lc ) shift wd ratio =
            { color1 = c1
            , color2 = c2
            , color3 = c3
            , lineColor = lc
            , shift = shift
            , width = wd
            , aspectRatio = ratio
            , window = window
            }
    in
        Random.map4 model
            colors
            shift
            width
            aspectRatio



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyPressed



--


parallelogramColor : Model -> Int -> Int -> Color
parallelogramColor model i j =
    case ( i % 4, j % 2 ) of
        ( 0, 0 ) ->
            model.color2

        ( 0, 1 ) ->
            model.color3

        ( 2, 0 ) ->
            model.color3

        ( 2, 1 ) ->
            model.color2

        _ ->
            model.color1


translation : Model -> Int -> Int -> Svg.Attribute Msg
translation { width, aspectRatio, shift, window } i j =
    let
        wd =
            width * window.width

        dx =
            -wd * (Tuple.first shift) + (toFloat j - toFloat (i % 2) / 2) * wd

        dy =
            -wd * (Tuple.second shift) + toFloat (i - 1) * wd * aspectRatio / 2
    in
        "translate("
            ++ toString dx
            ++ ", "
            ++ toString dy
            ++ ")"
            |> transform


parallelogram : Model -> Int -> Int -> Svg Msg
parallelogram ({ width, aspectRatio, window } as model) i j =
    let
        color =
            parallelogramColor model i j
                |> colorToCssRgb

        wd =
            width * window.width

        pts =
            [ ( wd / 2, 0 )
            , ( wd, wd / 2 * aspectRatio )
            , ( wd / 2, wd * aspectRatio )
            , ( 0, wd / 2 * aspectRatio )
            ]
                |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                |> String.join " "
    in
        polygon
            [ points pts
            , fill color
            , translation model i j
            ]
            []


lineGroup : Model -> Int -> Int -> Svg Msg
lineGroup ({ width, aspectRatio, lineColor, window } as model) i j =
    let
        ( wd, ht ) =
            ( window.width * width / 4, window.width * width * aspectRatio / 4 )

        lineProps =
            [ stroke <| colorToCssRgb lineColor
            , strokeWidth "1.5"
            , translation model i j
            , strokeDasharray "10, 5"
            ]

        pts1 =
            [ x1 <| toString wd
            , y1 <| toString ht
            , x2 <| toString (3 * wd)
            , y2 <| toString (3 * ht)
            ]

        pts2 =
            [ x1 <| toString (3 * wd)
            , y1 <| toString ht
            , x2 <| toString wd
            , y2 <| toString (3 * ht)
            ]
    in
        g []
            [ Svg.line (lineProps ++ pts1) []
            , Svg.line (lineProps ++ pts2) []
            ]


view : Model -> Svg Msg
view ({ window, width, aspectRatio } as model) =
    let
        ( n, m ) =
            ( ceiling (1 / width) + 2
            , ceiling (1 / (width * aspectRatio / 2)) + 2
            )

        row shape i =
            List.range 0 n
                |> List.map (shape model i)

        repeat shape =
            List.range 0 m
                |> List.concatMap (row shape)
                |> g []
    in
        svg
            [ Svg.Attributes.width <| toString <| window.width
            , Svg.Attributes.height <| toString <| window.height
            ]
            [ g []
                [ repeat parallelogram
                , repeat lineGroup
                ]
            ]



--


main : Program Flags Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
