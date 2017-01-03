module Argyle exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Svg exposing (Svg, svg, g, line, polygon)
import Svg.Attributes exposing (points, fill, transform)
import Svg.Attributes exposing (strokeWidth, stroke, strokeDasharray, x1, x2, y1, y2)
import Svg.Attributes exposing (width, height)
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
    , shapeWidth : Float
    , aspectRatio : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { color1 = Color.blue
      , color2 = Color.green
      , color3 = Color.red
      , lineColor = Color.gray
      , shift = ( 0, 0 )
      , shapeWidth = 0.15
      , aspectRatio = 1.5
      }
    , Random.generate SetModel randomModel
    )


type Msg
    = KeyPressed KeyCode
    | SetModel Model



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case (Debug.log "message" msg) of
        KeyPressed 32 ->
            ( model, Random.generate SetModel randomModel )

        KeyPressed _ ->
            ( model, Cmd.none )

        SetModel newModel ->
            ( newModel
            , Cmd.none
            )



--


randomModel : Random.Generator Model
randomModel =
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
            , shapeWidth = wd
            , aspectRatio = ratio
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


translation : Model -> Float -> Float -> Int -> Int -> Svg.Attribute Msg
translation { shapeWidth, aspectRatio, shift } width height i j =
    let
        wd =
            shapeWidth * width

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


parallelogram : Float -> Float -> Model -> Int -> Int -> Svg Msg
parallelogram width height ({ shapeWidth, aspectRatio } as model) i j =
    let
        color =
            parallelogramColor model i j
                |> colorToCssRgb

        wd =
            shapeWidth * width

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
            , translation model width height i j
            ]
            []


lineGroup : Float -> Float -> Model -> Int -> Int -> Svg Msg
lineGroup width height ({ shapeWidth, aspectRatio, lineColor } as model) i j =
    let
        ( wd, ht ) =
            ( width * shapeWidth / 4, height * shapeWidth * aspectRatio / 4 )

        lineProps =
            [ stroke <| colorToCssRgb lineColor
            , strokeWidth "1.5"
            , translation model width height i j
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


view : Float -> Float -> Model -> Svg Msg
view width height ({ shapeWidth, aspectRatio } as model) =
    let
        ( n, m ) =
            ( ceiling (1 / shapeWidth) + 2
            , ceiling (1 / (shapeWidth * aspectRatio / 2)) + 2
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
            [ Svg.Attributes.width <| toString <| width
            , Svg.Attributes.height <| toString <| height
            ]
            [ g []
                [ repeat (parallelogram width height)
                , repeat (lineGroup width height)
                ]
            ]



--


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view 500 500 |> project 1
        , update = update
        , subscriptions = subscriptions
        }
