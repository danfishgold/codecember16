module Day14.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Day14.Clip exposing (clip, clipPath)
import Html exposing (Html)
import Svg exposing (Svg, circle, g, svg)
import Svg.Attributes as Attrs exposing (cx, cy, fill, r, transform)
import Time exposing (Posix)


type alias Model =
    { width : Float
    , height : Float
    , dx : Float
    , dy : Float
    }


type Msg
    = Tick Posix


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , dx = 0
      , dy = 0
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrame Tick



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick posix ->
            let
                t =
                    toFloat <| Time.posixToMillis posix
            in
            ( { model
                | dx = cos (t * 0.001) * 10
                , dy = sin (t * 0.001) * 10
              }
            , Cmd.none
            )



--


circles : Float -> Float -> Float -> Float -> Color -> List (Svg msg)
circles width height rad spacing color =
    let
        d =
            2 * rad + spacing

        x i j =
            if modBy 2 i == 0 then
                d * j + d / 2

            else
                d * j

        y i j =
            d * toFloat i

        circle i j =
            Svg.circle
                [ cx <| String.fromFloat <| x i j
                , cy <| String.fromFloat <| y i j
                , r <| String.fromFloat rad
                , fill <| Color.toCssString color
                ]
                []

        ( rows, columns ) =
            ( height / (d * sqrt 3 / 2) |> ceiling
            , width / d |> ceiling
            )

        row i =
            List.range 0 columns
                |> List.map toFloat
                |> List.map (\j -> circle i j)
    in
    List.range 0 rows
        |> List.concatMap row


view : Model -> Html Msg
view { width, height, dx, dy } =
    let
        circles1 =
            circles width height 8 5 Color.red

        circles2 =
            circles width height 10 2.5 Color.red
    in
    svg
        [ Attrs.width <| String.fromFloat width
        , Attrs.height <| String.fromFloat height
        ]
        [ clip "circles1" circles1
        , g
            [ clipPath "circles1" ]
            [ g [ transform <| "translate(" ++ String.fromFloat dx ++ "," ++ String.fromFloat dy ++ ")" ]
                circles2
            ]
        ]



--


description : String
description =
    """
I love [Moiré](https://en.wikipedia.org/wiki/Moiré_pattern).

Especially on the chairs in the conference room where I work
(they're made of a perforated fabric that stretches around a metal frame,
so there are holes on the front and on the back and they make cool patterns.)

Here's a link to
[the Sampling Theorem](https://en.wikipedia.org/wiki/Nyquist–Shannon_sampling_theorem)
which is relevant, and to
[Seeing Circles, Sines, and Signals](https://jackschaedler.github.io/circles-sines-signals/sincos.html),
which is important to me because it led me to
[Mike Bostock's talk](https://bost.ocks.org/mike/algorithms/)
about visualizing alorithms, which was mentioned in day 6.
"""


page =
    { init = always <| init 500 500
    , subscriptions = subscriptions
    , update = update
    , title = "Moiré"
    , body = view
    , description = description
    }
