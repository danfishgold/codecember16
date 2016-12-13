module Main exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g, rect)
import Svg.Attributes exposing (x, y, width, height, fill, transform)
import Day13.Dvd as Dvd
import Color exposing (Color)
import AnimationFrame
import Time exposing (Time)
import Random.Extra
import Random


colors : List Color
colors =
    [ Color.hsl (degrees 0) 1 0.5
    , Color.hsl (degrees 60) 1 0.5
    , Color.hsl (degrees 120) 1 0.5
    , Color.hsl (degrees 180) 1 0.5
    , Color.hsl (degrees 240) 1 0.5
    , Color.hsl (degrees 300) 1 0.5
    ]


type alias Model =
    { x : Float
    , y : Float
    , vx : Float
    , vy : Float
    , color : Color
    , width : Float
    , height : Float
    }


type Msg
    = SetColor Color
    | Tick Time


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { x = 0
      , y = 0
      , vx = 0.1
      , vy = 0.1
      , color = Color.white
      , width = width
      , height = height
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    AnimationFrame.diffs Tick



--


move : Float -> Float -> Float -> Float -> { p : Float, v : Float, flip : Bool }
move p v dt len =
    let
        p1 =
            p + v * dt
    in
        if p1 < 0 then
            { p = -p1, v = -v, flip = True }
        else if p1 > len then
            { p = 2 * len - p1, v = -v, flip = True }
        else
            { p = p1, v = v, flip = False }


randomColor : Color -> Random.Generator Color
randomColor color =
    colors
        |> List.filter ((/=) color)
        |> Random.Extra.sample
        |> Random.map (Maybe.withDefault Color.white)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetColor c ->
            ( { model | color = c }, Cmd.none )

        Tick dt ->
            let
                x =
                    move model.x model.vx dt (model.width - Dvd.width)

                y =
                    move model.y model.vy dt (model.height - Dvd.height)
            in
                ( { model
                    | x = x.p
                    , y = y.p
                    , vx = x.v
                    , vy = y.v
                  }
                , if x.flip || y.flip then
                    Random.generate SetColor
                        (randomColor model.color)
                  else
                    Cmd.none
                )



--


dvd : Float -> Float -> Color -> Svg msg
dvd x y c =
    g [ transform <| "translate(" ++ toString x ++ "," ++ toString y ++ ")" ] [ Dvd.dvd c ]


bg : Model -> Svg msg
bg model =
    rect
        [ x "0"
        , y "0"
        , width <| toString model.width
        , height <| toString model.height
        , fill "black"
        ]
        []


view : Model -> Svg Msg
view model =
    svg [ width <| toString model.width, height <| toString model.height ]
        [ bg model
        , dvd model.x model.y model.color
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
