module Automaton exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (x, y, width, height, fill, stroke, strokeWidth, transform)
import Svg.Events exposing (onClick)
import Dict exposing (Dict)
import Array
import Color exposing (Color)
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


kthRule : Int -> Int -> Int -> List Int
kthRule c n k =
    if n == 0 then
        []
    else
        (k % c) :: kthRule c (n - 1) (k // c)



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


color : Int -> Color
color i =
    case i of
        0 ->
            Color.white

        1 ->
            Color.black

        _ ->
            Color.red


pyramid : Float -> Model -> Svg Msg
pyramid res model =
    let
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
            |> g []


rules : Float -> Model -> Svg Msg
rules res { rule, ruleRadius, colors } =
    let
        pixel i j c =
            rect
                [ x <| toString <| res * toFloat j
                , y <| toString <| res * toFloat i
                , width <| toString res
                , height <| toString res
                , fill <| colorToCssRgb <| color c
                , stroke <| "gray"
                , strokeWidth "1"
                ]
                []

        n =
            2 * ruleRadius + 1

        allRules =
            List.range 0 (colors ^ n)
                |> List.map (kthRule colors n)

        bottom rl =
            rule |> Dict.get rl |> Maybe.withDefault 0 |> pixel 1 ruleRadius

        top rl =
            List.indexedMap (pixel 0) rl

        tetris i rl =
            g
                [ transform <| "translate(" ++ toString (50 + 50 * i) ++ ", 300)"
                , onClick (ShiftRule rl)
                ]
                (bottom rl :: top rl)
    in
        allRules
            |> Debug.log "rules"
            |> List.indexedMap tetris
            |> g []


view : Float -> Model -> Svg Msg
view res model =
    svg [ width "600px", height "600px" ] [ pyramid res model, rules res model ]



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view 10
        }
