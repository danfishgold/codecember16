module Cradle exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, polyline)
import Svg.Attributes exposing (width, height, d)
import Mouse
import Array exposing (Array)


type alias Point =
    ( Float, Float )


type MouseState
    = Up
    | Down Point
    | Moved Point Point


type alias Model =
    { line : Array Point
    , previousClose : Maybe Int
    , mouse : MouseState
    }


type Msg
    = ChangedState MouseState


init : ( Model, Cmd Msg )
init =
    ( { line = Array.fromList [ ( 50, 250 ), ( 450, 250 ) ]
      , previousClose = Nothing
      , mouse = Up
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        lastPoint =
            case model.mouse of
                Up ->
                    Nothing

                Down p ->
                    Just p

                Moved _ p ->
                    Just p
    in
        case lastPoint of
            Nothing ->
                Mouse.downs <| \{ x, y } -> ChangedState <| Down ( toFloat x, toFloat y )

            Just p ->
                Sub.batch
                    [ Mouse.ups <| always (ChangedState Up)
                    , Mouse.moves <| \{ x, y } -> ChangedState <| Moved p ( toFloat x, toFloat y )
                    ]



--


areClose : Point -> Point -> Bool
areClose ( x1, y1 ) ( x2, y2 ) =
    max
        (abs (x1 - x2))
        (abs (y1 - y2))
        <= 10


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedState _ ->
            ( model, Cmd.none )



--


view : Model -> Svg Msg
view model =
    svg [ width "500", height "500" ] []



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
