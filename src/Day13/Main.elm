module Day13.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Day13.Dvd as Dvd
import Helper exposing (projectSvg)
import Random
import Random.Extra
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, height, transform, width, x, y)


colors : List Color
colors =
    [ Color.hsl (0 / 360) 1 0.5
    , Color.hsl (60 / 360) 1 0.5
    , Color.hsl (120 / 360) 1 0.5
    , Color.hsl (180 / 360) 1 0.5
    , Color.hsl (240 / 360) 1 0.5
    , Color.hsl (300 / 360) 1 0.5
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
    | Tick Float


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { x = 0
      , y = 0
      , vx = 0.1
      , vy = 0.1
      , color = Color.red
      , width = width
      , height = height
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onAnimationFrameDelta Tick



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
    g [ transform <| "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")" ] [ Dvd.dvd c ]


bg : Model -> Svg msg
bg model =
    rect
        [ x "0"
        , y "0"
        , width <| String.fromFloat model.width
        , height <| String.fromFloat model.height
        , fill "black"
        ]
        []


view : Model -> Svg Msg
view model =
    projectSvg ( model.width, model.height )
        []
        [ bg model
        , dvd model.x model.y model.color
        ]



--


description : String
description =
    """
CHILDHOOD MEMORIES
"""


page =
    { init = always <| init 500 500
    , subscriptions = subscriptions
    , update = update
    , title = "DVD"
    , body = view
    , description = description
    }
