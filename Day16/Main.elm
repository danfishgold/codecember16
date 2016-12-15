module Tiles exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (width, height, transform)
import Day15.Polyomino as Poly exposing (Point)
import Day15.View exposing (polygon)
import Random
import Keyboard exposing (KeyCode)
import Color exposing (Color, white)
import Day2.Random exposing (ryb2v2)


type alias Tile =
    ( Poly.Word, Poly.Word, Poly.Word )


type alias Model =
    { width : Float
    , height : Float
    , tile : Tile
    , colors : ( Color, Color, Color, Color )
    }


randomize : Cmd Msg
randomize =
    let
        colors =
            Random.generate SetColors (ryb2v2 1 0.5 45)

        tile =
            Random.generate SetTile (Poly.randomBN 2 8)
    in
        Cmd.batch [ colors, tile ]


type Msg
    = SetTile Tile
    | SetColors ( Color, Color, Color, Color )
    | Key KeyCode


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , tile = ( [], [], [] )
      , colors = ( white, white, white, white )
      }
    , randomize
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups Key



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetTile tile ->
            ( { model | tile = tile |> Debug.log "tile" }, Cmd.none )

        SetColors colors ->
            ( { model | colors = colors }, Cmd.none )

        Key 32 ->
            ( model, randomize )

        Key _ ->
            ( model, Cmd.none )



--


axes : Tile -> ( Point, Point )
axes ( w1, w2, w3 ) =
    let
        vec word =
            word
                |> Poly.points ( 0, 0 )
                |> List.drop (List.length word)
                |> List.head
                |> Maybe.withDefault ( 0, 0 )
    in
        ( vec <| w1 ++ w2, vec <| w3 ++ w2 )


view : Float -> Model -> Svg Msg
view scale model =
    let
        ( u, v ) =
            axes model.tile

        p0 i j =
            ( i * Tuple.first u + j * Tuple.first v
            , i * Tuple.second u + j * Tuple.second v
            )

        word =
            Poly.bn model.tile

        ( c1, c2, c3, c4 ) =
            model.colors

        color i j =
            case ( i % 2 == 0, j % 2 == 0 ) of
                ( True, True ) ->
                    c1

                ( True, False ) ->
                    c2

                ( False, True ) ->
                    c3

                ( False, False ) ->
                    c4

        poly i j =
            polygon scale (color i j) (p0 i j) word

        row i =
            List.range -5 5
                |> List.map (poly i)

        grid =
            List.range -5 5
                |> List.concatMap row
    in
        [ g [ transform "translate(250, 250)" ] grid ]
            |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view 8
        }
