module Day22.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Html exposing (Html, button, div, input, text)
import Html.Attributes as Attrs exposing (style, type_, value)
import Html.Events exposing (onClick, onInput)
import Svg exposing (Svg, circle, polyline, svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, strokeWidth, width)


radiusResolution : Float
radiusResolution =
    0.01


type alias Point =
    ( Float, Float )


type alias Model =
    { width : Float
    , height : Float
    , bigR : Float
    , smallR : Float
    , points : List Point
    , time : Float
    , speed : Speed
    }


type Speed
    = Instantaneous
    | Fast
    | Medium
    | Slow


isLive : Speed -> Bool
isLive speed =
    velocity speed /= Nothing


velocity : Speed -> Maybe Float
velocity speed =
    case speed of
        Fast ->
            Just <| 1 / 200

        Medium ->
            Just <| 1 / 300

        Slow ->
            Just <| 1 / 400

        Instantaneous ->
            Nothing


type Msg
    = SetBigR Float
    | SetSmallR Float
    | SetSpeed Speed
    | Tick Float
    | Reset


init : Float -> Float -> Model
init width height =
    update Reset
        { width = width
        , height = height
        , bigR = 0.6
        , smallR = 0.6
        , points = []
        , time = 0
        , speed = Fast
        }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    if isLive model.speed then
        Browser.Events.onAnimationFrameDelta Tick

    else
        Sub.none



--


{-| The position of the circle and the dot inside at the given time
-}
centers : Model -> Float -> Float -> ( Point, Point )
centers { bigR, smallR } v time =
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


{-| solve for lowest n:
n \* 6/4 = int
=> n = 4 / 2 = 4 / gcd(6, 4)
The resulting integer would be 4 / gcd(6, 4) \* 6/4 = 6 / gcd(6, 4) = 3
Meaning, nominator / gcd (nominator, denominator)

We want both theta1 = 2pi k and theta2 = 2pi m
theta = 2pi k = 2pi m \* r/(1-r)
this means we're looking for integers k, m, s.t. k = m \* r/(1-r)

therefore, k = r / gcd (r, 1-r)
and theta = 2pi \* r / gcd (r, 1-r)

-}
maxTheta : Float -> Float
maxTheta bigR =
    let
        gcd n m =
            if m == 0 then
                n

            else
                gcd m (modBy m n)

        ( nom, denom ) =
            ( round <| bigR / radiusResolution, round <| (1 - bigR) / radiusResolution )

        k =
            nom // gcd nom denom
    in
    2 * pi * toFloat k


update : Msg -> Model -> Model
update msg model =
    case msg of
        Tick dt ->
            case velocity model.speed of
                Just v ->
                    { model
                        | time = model.time + min 32 dt
                        , points =
                            if model.time <= maxTheta model.bigR / v then
                                centers model v model.time
                                    |> Tuple.second
                                    |> (\pt -> pt :: model.points)

                            else
                                model.points
                    }

                Nothing ->
                    model

        SetSpeed speed ->
            update Reset { model | speed = speed }

        SetBigR r ->
            update Reset { model | bigR = r }

        SetSmallR r ->
            update Reset { model | smallR = r }

        Reset ->
            if isLive model.speed then
                { model | time = 0, points = [] }

            else
                let
                    max =
                        maxTheta model.bigR

                    length =
                        ceiling max * 30

                    pt idx =
                        Tuple.second <| centers model 1 (max * toFloat idx / toFloat length)
                in
                { model | points = List.map pt (List.range 0 length) }



--


svgView : Model -> Svg Msg
svgView ({ time, bigR, smallR } as model) =
    let
        ( xCenter, yCenter, scale ) =
            ( model.width / 2, model.height / 2, min model.width model.height / 2.2 )

        path =
            polyline
                [ model.points
                    |> List.map (\( x, y ) -> String.fromFloat (xCenter + scale * x) ++ "," ++ String.fromFloat (yCenter + scale * y))
                    |> String.join " "
                    |> points
                , fill "none"
                , stroke "black"
                , strokeWidth "1"
                ]
                []

        svg_ =
            svg [ width <| String.fromFloat model.width, height <| String.fromFloat model.height ]
    in
    case velocity model.speed of
        Just v ->
            let
                ( ( xCirc, yCirc ), ( xDot, yDot ) ) =
                    centers model v model.time

                container =
                    circle
                        [ cx <| String.fromFloat xCenter
                        , cy <| String.fromFloat yCenter
                        , r <| String.fromFloat <| 1 * scale
                        , fill "none"
                        , stroke "black"
                        , strokeWidth "1"
                        ]
                        []

                circ =
                    circle
                        [ cx <| String.fromFloat <| xCenter + scale * xCirc
                        , cy <| String.fromFloat <| xCenter + scale * yCirc
                        , r <| String.fromFloat <| bigR * scale
                        , fill "none"
                        , stroke "black"
                        , strokeWidth "1"
                        ]
                        []

                dot =
                    circle
                        [ cx <| String.fromFloat <| xCenter + scale * xDot
                        , cy <| String.fromFloat <| xCenter + scale * yDot
                        , r <| "2"
                        , fill "black"
                        ]
                        []
            in
            svg_ [ container, circ, dot, path ]

        Nothing ->
            svg_ [ path ]


view : Model -> Html Msg
view model =
    let
        onValue msg =
            onInput (String.toFloat >> Maybe.withDefault 0 >> msg)

        radio toMsg name title val checked =
            div [ style "display" "flex", style "align-items" "center" ]
                [ input
                    [ type_ "radio"
                    , Attrs.name name
                    , onClick <| toMsg val
                    , Attrs.checked checked
                    ]
                    []
                , text title
                ]

        radios toMsg name titlesAndValues selectedValue =
            titlesAndValues
                |> List.map
                    (\( title, value ) ->
                        radio toMsg name title value (value == selectedValue)
                    )
                |> div [ style "display" "flex" ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "margin" "20px auto"
        ]
        [ radios SetSpeed
            "v"
            [ ( "Instantaneous", Instantaneous )
            , ( "Fast", Fast )
            , ( "Medium", Medium )
            , ( "Slow", Slow )
            ]
            model.speed
        , text "Big radius"
        , input
            [ onValue SetBigR
            , type_ "range"
            , Attrs.min "0"
            , Attrs.max "1"
            , Attrs.step <| String.fromFloat radiusResolution
            ]
            []
        , text "Small radius"
        , input
            [ onValue SetSmallR
            , type_ "range"
            , Attrs.min "0"
            , Attrs.max "1"
            , Attrs.step <| String.fromFloat radiusResolution
            ]
            []
        , svgView model
        ]



--


description : String
description =
    """
[Spirographs](https://en.wikipedia.org/wiki/Spirograph) are neat!
I got one in a Kinder Surprise in 2014.

I thought I'd have a lot to say about every project but I was wrong.
"""


page =
    { init = always <| ( init 500 500, Cmd.none )
    , subscriptions = subscriptions
    , update = \msg model -> ( update msg model, Cmd.none )
    , title = "Spirograph"
    , body = view
    , description = description
    }
