module Spirograph exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, button, text)
import Html.Events exposing (onInput, onClick)
import Svg exposing (Svg, svg, polyline, circle)
import Svg.Attributes exposing (width, height, cx, cy, r, stroke, strokeWidth, fill, points)
import AnimationFrame


type alias Point =
    ( Float, Float )


type alias Model =
    { width : Float
    , height : Float
    , bigR : Float
    , smallR : Float
    , points : List Point
    , time : Float
    }


type Msg
    = SetBigR Float
    | SetSmallR Float
    | Reset
    | Tick Float


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , bigR = 0.55
      , smallR = 0.85
      , points = []
      , time = 0
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--


centers : Model -> ( Point, Point )
centers { time, bigR, smallR } =
    let
        v =
            1 / 300

        ( t, t_ ) =
            ( time * v, -(1 - bigR) / bigR * time * v )

        ( xCirc, yCirc ) =
            ( (1 - bigR) * cos t
            , (1 - bigR) * sin t
            )

        ( xDot, yDot ) =
            ( xCirc + bigR * smallR * cos t_
            , yCirc + bigR * smallR * sin t_
            )
    in
        ( ( xCirc, yCirc ), ( xDot, yDot ) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model
                | time = model.time + min 32 dt
                , points = centers model |> Tuple.second |> \pt -> pt :: model.points
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



--


svgView : Model -> Svg Msg
svgView ({ time, bigR, smallR } as model) =
    let
        ( xCenter, yCenter, scale ) =
            ( model.width / 2, model.height / 2, min model.width model.height / 3 )

        ( ( xCirc, yCirc ), ( xDot, yDot ) ) =
            centers model

        path =
            polyline
                [ model.points
                    |> List.map (\( x, y ) -> toString (xCenter + scale * x) ++ "," ++ toString (yCenter + scale * y))
                    |> String.join " "
                    |> points
                , fill "none"
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        container =
            circle
                [ cx <| toString xCenter
                , cy <| toString yCenter
                , r <| toString <| 1 * scale
                , fill "none"
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        circ =
            circle
                [ cx <| toString <| xCenter + scale * xCirc
                , cy <| toString <| xCenter + scale * yCirc
                , r <| toString <| bigR * scale
                , fill "none"
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        dot =
            circle
                [ cx <| toString <| xCenter + scale * xDot
                , cy <| toString <| xCenter + scale * yDot
                , r <| "2"
                , fill "black"
                ]
                []
    in
        [ container, circ, dot, path ]
            |> svg [ width <| toString model.width, height <| toString model.height ]


view : Model -> Html Msg
view model =
    let
        onValue msg =
            onInput (String.toFloat >> Result.withDefault 0 >> msg)
    in
        div []
            [ input [ onValue SetBigR ] []
            , input [ onValue SetSmallR ] []
            , button [ onClick Reset ] [ text "Reset" ]
            , svgView model
            ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
