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
    { previous : List ( Int, Int, Color )
    , current : ( Int, Int, Color )
    , side : Int
    , res : Float
    , symmetry : Symmetry
    , paused : Bool
    }


init : Model
init =
    { previous = []
    , current = ( 0, 0, Color.white )
    , side = 100
    , res = 5
    , symmetry = Mirror
    , paused = False
    }


type Msg
    = ResetPoints
    | Tick Time
    | Add ( Int, Int, Color )
    | Key KeyCode


type Symmetry
    = Mirror
    | Rotation



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


randomColor : Generator Color
randomColor =
    ryb1 1 0.35


randomNeighbor : ( Int, Int ) -> Generator ( Int, Int )
randomNeighbor ( x, y ) =
    let
        forward =
            Random.Extra.sample [ ( x + 1, y ), ( x, y + 1 ) ]

        backward =
            Random.Extra.sample [ ( x - 1, y ), ( x, y - 1 ) ]
    in
        Random.Extra.frequency [ ( 0.57, forward ), ( 0.43, backward ) ]
            |> Random.map (Maybe.withDefault ( x, y ))



-- |> Random.map (\( x, y ) -> ( max x 0, max y 0 ))


randomPoint : Model -> Generator ( Int, Int, Color )
randomPoint { current } =
    current
        |> \( x0, y0, _ ) ->
            Random.map2 (\( x1, y1 ) c -> ( x1, y1, c ))
                (randomNeighbor ( x0, y0 ))
                (randomColor)



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
            ( model, generate Add (randomPoint model) )

        Key 32 ->
            ( { model | paused = not model.paused }, Cmd.none )

        Key 49 ->
            update ResetPoints { model | symmetry = Mirror }

        Key 50 ->
            update ResetPoints { model | symmetry = Rotation }

        Key _ ->
            ( model, Cmd.none )



--


view : Model -> Svg Msg
view { side, current, previous, res, symmetry } =
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
            case symmetry of
                Rotation ->
                    g [] [ point ( x, y, c ), point ( -y, x, c ), point ( y, -x, c ), point ( -x, -y, c ) ]

                Mirror ->
                    g [] [ point ( x, y, c ), point ( -x, y, c ), point ( x, -y, c ), point ( -x, -y, c ) ]

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
        , view = view
        , subscriptions = subscriptions
        }
