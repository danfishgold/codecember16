module Argyle exposing (..)

import Svg exposing (Svg, svg, g, line, polygon)
import Svg.Attributes exposing (points, fill, transform)
import Svg.Attributes exposing (strokeWidth, stroke, strokeDasharray, x1, x2, y1, y2)
import Svg.Attributes exposing (width, height)
import Html exposing (program)
import Window
import Keyboard exposing (KeyCode)
import Random
import Random.Color
import String
import Task
import Color exposing (Color)


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


type alias WindowSize =
    { width : Float, height : Float }


init : ( Model, Cmd Msg )
init =
    ( { color1 = Color.blue
      , color2 = Color.green
      , color3 = Color.red
      , lineColor = Color.gray
      , shift = ( 0, 0 )
      , width = 200
      , aspectRatio = 1.5
      , window = WindowSize 0 0
      }
    , Cmd.batch
        [ Task.perform WindowResize Window.size
        , Random.generate SetParameters randomModelParameters
        ]
    )


type Msg
    = WindowResize Window.Size
    | KeyPressed KeyCode
    | SetParameters ( ( Color, Color, Color, Color ), ( Float, Float ), Float, Float )



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "message" msg) of
        WindowResize { width, height } ->
            ( { model
                | window =
                    { width = toFloat width
                    , height = toFloat height
                    }
              }
            , Cmd.none
            )

        KeyPressed 32 ->
            ( model, Random.generate SetParameters randomModelParameters )

        KeyPressed _ ->
            ( model, Cmd.none )

        SetParameters ( ( c1, c2, c3, lc ), shift, wd, ratio ) ->
            ( { model
                | color1 = c1
                , color2 = c2
                , color3 = c3
                , lineColor = lc
                , shift = shift
                , width = wd
                , aspectRatio = ratio
              }
            , Cmd.none
            )



--


randomModelParameters : Random.Generator ( ( Color, Color, Color, Color ), ( Float, Float ), Float, Float )
randomModelParameters =
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
            Random.int 100 300 |> Random.map toFloat

        aspectRatio =
            Random.float 1.2 1.7
    in
        Random.map4 (,,,)
            colors
            shift
            width
            aspectRatio



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes WindowResize
        , Keyboard.ups KeyPressed
        ]



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


colorToString : Color -> String
colorToString color =
    color
        |> Color.toRgb
        |> (\{ red, green, blue } ->
                "rgb("
                    ++ toString red
                    ++ ", "
                    ++ toString green
                    ++ ", "
                    ++ toString blue
                    ++ ")"
           )


translation : Model -> Int -> Int -> Svg.Attribute Msg
translation { width, aspectRatio, shift } i j =
    let
        dx =
            -width * (Tuple.first shift) + (toFloat j - toFloat (i % 2) / 2) * width

        dy =
            -width * (Tuple.second shift) + toFloat (i - 1) * width * aspectRatio / 2
    in
        "translate("
            ++ toString dx
            ++ ", "
            ++ toString dy
            ++ ")"
            |> transform


parallelogram : Model -> Int -> Int -> Svg Msg
parallelogram ({ width, aspectRatio } as model) i j =
    let
        color =
            parallelogramColor model i j
                |> colorToString

        pts =
            [ ( width / 2, 0 )
            , ( width, width / 2 * aspectRatio )
            , ( width / 2, width * aspectRatio )
            , ( 0, width / 2 * aspectRatio )
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
lineGroup ({ width, aspectRatio, lineColor } as model) i j =
    let
        lineProps =
            [ stroke <| colorToString lineColor
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

        ( wd, ht ) =
            ( width / 4, width * aspectRatio / 4 )
    in
        g []
            [ Svg.line (lineProps ++ pts1) []
            , Svg.line (lineProps ++ pts2) []
            ]


repeat : Model -> (Model -> Int -> Int -> Svg Msg) -> Svg Msg
repeat model shape =
    let
        ( n, m ) =
            counts model

        row i =
            List.range 0 n
                |> List.map (shape model i)
    in
        List.range 0 m
            |> List.concatMap row
            |> g []


counts : Model -> ( Int, Int )
counts ({ window, width, aspectRatio } as model) =
    ( ceiling (window.width / width) + 2
    , ceiling (window.height / (width * aspectRatio / 2)) + 2
    )


view : Model -> Svg Msg
view ({ window, width, aspectRatio } as model) =
    svg
        [ Svg.Attributes.width <| toString <| window.width
        , Svg.Attributes.height <| toString <| window.height
        ]
        [ g []
            [ repeat model parallelogram
            , repeat model lineGroup
            ]
        ]



--


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
