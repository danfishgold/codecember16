module Day01.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Helper exposing (projectSvg)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Json
import Random
import String
import Svg exposing (Svg, g, line, polygon, svg)
import Svg.Attributes exposing (fill, height, points, stroke, strokeDasharray, strokeWidth, transform, width, x1, x2, y1, y2)


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
    = Randomize
    | SetModel Model



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            ( model, Random.generate SetModel randomModel )

        SetModel newModel ->
            ( newModel
            , Cmd.none
            )



--


randomModel : Random.Generator Model
randomModel =
    let
        randomShift =
            Random.map2 (\a b -> ( a, b ))
                (Random.float 0 1)
                (Random.float 0 1)

        randomColor =
            Random.map3 Color.hsl
                (Random.float 0 1)
                (Random.float 0 1)
                (Random.float 0 1)

        colors =
            Random.map4 (\c1 c2 c3 lc -> { c1 = c1, c2 = c2, c3 = c3, lc = lc })
                randomColor
                randomColor
                randomColor
                randomColor

        width =
            Random.float 0.08 0.2

        aspectRatio =
            Random.float 1.2 1.7

        model { c1, c2, c3, lc } shift wd ratio =
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
        randomShift
        width
        aspectRatio



--


parallelogramColor : Model -> Int -> Int -> Color
parallelogramColor model i j =
    case ( modBy 4 i, modBy 2 j ) of
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
            -wd * Tuple.first shift + (toFloat j - toFloat (modBy 2 i) / 2) * wd

        dy =
            -wd * Tuple.second shift + toFloat (i - 1) * wd * aspectRatio / 2
    in
    "translate("
        ++ String.fromFloat dx
        ++ ", "
        ++ String.fromFloat dy
        ++ ")"
        |> transform


parallelogram : Float -> Float -> Model -> Int -> Int -> Svg Msg
parallelogram width height ({ shapeWidth, aspectRatio } as model) i j =
    let
        color =
            parallelogramColor model i j
                |> Color.toCssString

        wd =
            shapeWidth * width

        pts =
            [ ( wd / 2, 0 )
            , ( wd, wd / 2 * aspectRatio )
            , ( wd / 2, wd * aspectRatio )
            , ( 0, wd / 2 * aspectRatio )
            ]
                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
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
            [ stroke <| Color.toCssString lineColor
            , strokeWidth "1.5"
            , translation model width height i j
            , strokeDasharray "10, 5"
            ]

        pts1 =
            [ x1 <| String.fromFloat wd
            , y1 <| String.fromFloat ht
            , x2 <| String.fromFloat (3 * wd)
            , y2 <| String.fromFloat (3 * ht)
            ]

        pts2 =
            [ x1 <| String.fromFloat (3 * wd)
            , y1 <| String.fromFloat ht
            , x2 <| String.fromFloat wd
            , y2 <| String.fromFloat (3 * ht)
            ]
    in
    g []
        [ Svg.line (lineProps ++ pts1) []
        , Svg.line (lineProps ++ pts2) []
        ]


view : Float -> Float -> Model -> Html Msg
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
    div []
        [ projectSvg ( width, height )
            []
            [ g []
                [ repeat (parallelogram width height)
                , repeat (lineGroup width height)
                ]
            ]
        , div [] [ button [ onClick Randomize ] [ text "Randomize" ] ]
        ]



--


description : String
description =
    """
This was my first project.
I wanted to make something similar to [this](https://avh4.github.io/codevember-2016/day-3/) by Aaron VonderHar and I like argyle.
"""


page =
    { init = always init
    , title = "Argyle"
    , body = view 500 500
    , description = description
    , update = update
    , subscriptions = always Sub.none
    }
