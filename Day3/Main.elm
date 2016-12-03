module Anatoly exposing (..)

import Html exposing (Html, program)
import Time exposing (second, every, Time)
import Color exposing (Color)
import Random exposing (Generator, generate)
import Random.Extra
import Keyboard exposing (KeyCode)
import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (x, y, width, height, fill)
import Svg.Keyed exposing (node)
import Color.Convert exposing (colorToCssRgb)
import Day2.Random exposing (ryb1)


type alias Model =
    { previous : List Point
    , current : Point
    , side : Int
    , paused : Bool
    }


type alias Point =
    ( Int, Int, Color )


init : Model
init =
    { previous = []
    , current = ( 0, 0, Color.white )
    , side = 100
    , paused = False
    }


initWithShape : Int -> Float -> ( Model, Cmd Msg )
initWithShape side res =
    ( { previous = []
      , current = ( 0, 0, Color.white )
      , side = side
      , paused = True
      }
    , generate SetShape (randomShape side)
    )


type Msg
    = ResetPoints
    | Tick Time
    | Add Point
    | SetShape (List Point)
    | Key KeyCode



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.paused then
            Sub.none
          else
            every (0.0001 * second) Tick
        , Keyboard.ups Key
        ]



--


randomNeighbor : Point -> Generator Point
randomNeighbor ( x0, y0, _ ) =
    let
        neighbor ( x, y ) =
            let
                up =
                    Random.Extra.constant ( x, y + 1 )

                down =
                    Random.Extra.constant ( x, y - 1 )

                left =
                    Random.Extra.constant ( x - 1, y )

                right =
                    Random.Extra.constant ( x + 1, y )
            in
                Random.Extra.frequency
                    [ ( 0.4, up )
                    , ( 0.3, down )
                    , ( 0.3, left )
                    , ( 0.5, right )
                    ]

        color =
            ryb1 1 0.35
    in
        Random.map2 (\( x1, y1 ) c -> ( x1, y1, c ))
            (neighbor ( x0, y0 ))
            (color)


randomShape : Int -> Generator (List Point)
randomShape side =
    let
        process previous (( x, y, _ ) as pt) =
            if abs x > side || abs y > side then
                Random.Extra.constant previous
            else
                randomNeighbor pt
                    |> Random.andThen (process (pt :: previous))
    in
        process [] ( 0, 0, Color.white )



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetPoints ->
            ( { model | current = ( 0, 0, Color.white ), previous = [] }, Cmd.none )

        Add (( x, y, _ ) as pt) ->
            if abs x > model.side // 2 || abs y > model.side // 2 then
                update ResetPoints model
            else
                ( { model
                    | previous = model.previous ++ [ model.current ]
                    , current = pt
                  }
                , Cmd.none
                )

        Tick _ ->
            ( model, generate Add (randomNeighbor model.current) )

        Key 32 ->
            ( { model | paused = not model.paused }, Cmd.none )

        Key 13 ->
            update ResetPoints { model | paused = False }

        Key _ ->
            ( model, Cmd.none )

        SetShape [] ->
            update ResetPoints model

        SetShape (current :: previous) ->
            ( { model
                | current = current
                , previous = previous
                , paused = True
              }
            , Cmd.none
            )



--


view : Float -> Model -> Svg Msg
view res { side, current, previous } =
    let
        bg =
            rect
                [ x "0"
                , y "0"
                , width <| toString <| toFloat side * res
                , height <| toString <| toFloat side * res
                , fill <| colorToCssRgb Color.black
                ]
                []

        pts =
            (previous ++ [ current ])
                |> List.indexedMap (\i pt -> ( toString i, mirroredPoint pt ))
                |> node "g" []

        mirroredPoint ( x, y, c ) =
            g []
                [ point ( x, y, c )
                , point ( y, x, c )
                , point ( -x, y, c )
                , point ( -y, x, c )
                , point ( x, -y, c )
                , point ( y, -x, c )
                , point ( -x, -y, c )
                , point ( -y, -x, c )
                ]

        point ( x0, y0, c ) =
            rect
                [ x <| toString <| toFloat (x0 + side // 2) * res
                , y <| toString <| toFloat (y0 + side // 2) * res
                , width <| toString res
                , height <| toString res
                , fill <| colorToCssRgb c
                ]
                []
    in
        svg
            [ width <| toString <| side * ceiling res
            , height <| toString <| side * ceiling res
            ]
            [ bg, pts ]



--


main : Program Never Model Msg
main =
    program
        { init = ( init, Cmd.none )
        , update = update
        , view = view 5
        , subscriptions = subscriptions
        }
