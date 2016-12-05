module Automaton exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height, fill)
import Svg.Events exposing (onClick)
import Dict exposing (Dict)
import Array
import Color
import Color.Convert exposing (colorToCssRgb)


type alias Model =
    { levels : Int
    , ruleRadius : Int
    , colors : Int
    , rule : Dict (List Int) Int
    }


type Msg
    = ShiftRule (List Int)


init : ( Model, Cmd Msg )
init =
    ( { levels = 10
      , ruleRadius = 1
      , colors = 2
      , rule = Dict.empty |> Dict.update [ 0, 1, 0 ] (always (Just 1))
      }
    , Cmd.none
    )



-- binaryRule : Int -> Dict (List Int) Int
-- binaryRule n =
--
--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShiftRule idx ->
            let
                shift ruleN =
                    case ruleN of
                        Just n ->
                            Just <| (n + 1) % model.colors

                        Nothing ->
                            Just 1
            in
                ( { model | rule = Dict.update idx shift model.rule }, Cmd.none )



--


paddedNs : Int -> List Int -> List (List Int)
paddedNs r xs =
    let
        len =
            List.length xs

        arr =
            Array.fromList xs

        around k =
            List.range (k - r) (k + r) |> List.map (\i -> Array.get i arr |> Maybe.withDefault 0)
    in
        List.range 0 len
            |> List.map around


levels : Model -> List (List Int)
levels { levels, colors, ruleRadius, rule } =
    let
        width =
            2 * levels + 1

        initial =
            List.range 0 width
                |> List.map
                    (\i ->
                        if i == width // 2 then
                            1
                        else
                            0
                    )

        next _ level =
            paddedNs ruleRadius level |> List.map (\k -> Dict.get k rule |> Maybe.withDefault 0)
    in
        List.scanl next initial (List.range 1 levels)


view : Float -> Model -> Html Msg
view res model =
    let
        color i =
            case i of
                0 ->
                    Color.white

                1 ->
                    Color.black

                _ ->
                    Color.red

        pixel i j c =
            rect
                [ x <| toString <| res * toFloat j
                , y <| toString <| res * toFloat i
                , width <| toString res
                , height <| toString res
                , fill <| colorToCssRgb <| color c
                ]
                []

        row i pxls =
            pxls |> List.indexedMap (pixel i)

        rows =
            levels model
    in
        levels model
            |> List.indexedMap row
            |> List.concatMap identity
            |> svg [ width "100%", height "100%" ]



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view 10
        }
