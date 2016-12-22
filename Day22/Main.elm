module Spirograph exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, button, text)
import Html.Attributes as Attrs exposing (type_)
import Html.Events exposing (onInput, onClick)
import Svg exposing (Svg, svg, polyline, circle)
import Svg.Attributes exposing (width, height, cx, cy, r, stroke, strokeWidth, fill, points)
import AnimationFrame


radiusResolution : Float
radiusResolution =
    0.05


type alias Point =
    ( Float, Float )


type alias Model =
    { width : Float
    , height : Float
    , bigR : Float
    , smallR : Float
    , points : List Point
    , time : Float
    , v : Float
    , live : Bool
    }


type Msg
    = SetBigR Float
    | SetSmallR Float
    | ChangeLive
    | Tick Float
    | Reset


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , bigR = 0.8
      , smallR = 0.8
      , points = []
      , time = 0
      , v = 1 / 300
      , live = True
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.live then
        AnimationFrame.diffs Tick
    else
        Sub.none



--


centers : Model -> Float -> ( Point, Point )
centers { bigR, smallR, v } time =
    let
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


maxTime : Model -> Float
maxTime { bigR, v } =
    2 * pi * 10 / v


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            if model.live then
                ( { model
                    | time = model.time + min 32 dt
                    , points =
                        if model.time < maxTime model then
                            centers model model.time |> Tuple.second |> \pt -> pt :: model.points
                        else
                            model.points
                  }
                , Cmd.none
                )
            else
                ( model, Cmd.none )

        ChangeLive ->
            update Reset { model | live = not model.live }

        SetBigR r ->
            update Reset { model | bigR = r }

        SetSmallR r ->
            update Reset { model | smallR = r }

        Reset ->
            if model.live then
                ( { model | time = 0, points = [] }, Cmd.none )
            else
                ( { model
                    | points =
                        maxTime model
                            / 16
                            |> ceiling
                            |> List.range 0
                            |> List.map ((*) 16 >> toFloat >> centers model >> Tuple.second)
                  }
                , Cmd.none
                )



--


svgView : Model -> Svg Msg
svgView ({ time, bigR, smallR } as model) =
    let
        ( xCenter, yCenter, scale ) =
            ( model.width / 2, model.height / 2, min model.width model.height / 3 )

        ( ( xCirc, yCirc ), ( xDot, yDot ) ) =
            centers model model.time

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
        svg [ width <| toString model.width, height <| toString model.height ]
            (if model.live then
                [ container, circ, dot, path ]
             else
                [ path ]
            )


view : Model -> Html Msg
view model =
    let
        onValue msg =
            onInput (String.toFloat >> Result.withDefault 0 >> msg)
    in
        div []
            [ text "live"
            , input [ type_ "checkbox", onInput (always ChangeLive) ] []
            , text "Big radius"
            , input [ onValue SetBigR, type_ "range", Attrs.min "0", Attrs.max "1", Attrs.step <| toString radiusResolution ] []
            , text "Small radius"
            , input [ onValue SetSmallR, type_ "range", Attrs.min "0", Attrs.max "1", Attrs.step <| toString radiusResolution ] []
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
