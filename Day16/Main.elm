module Tiles exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Svg exposing (Svg, svg, g)
import Svg.Attributes exposing (width, height, transform)
import Day15.Polyomino as Poly exposing (Point, Letter(..))
import Day15.View exposing (polygon)
import Random
import Keyboard exposing (KeyCode)
import Color exposing (Color, white)


type alias Tile =
    ( Poly.Word, Poly.Word, Poly.Word )


type alias Model =
    { width : Float
    , height : Float
    , tile : Tile
    }


randomize : Cmd Msg
randomize =
    Random.generate SetTile (Poly.randomBN 3 7)


type Msg
    = SetTile Tile
    | Key KeyCode


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , tile = ( [ D ], [ L ], [] )
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
            ( { model | tile = tile }, Cmd.none )

        Key 13 ->
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


prefactors : Float -> Float -> ( Point, Point ) -> ( Float, Float )
prefactors x y ( ( u1, u2 ), ( v1, v2 ) ) =
    let
        ( ux, uy, vx, vy ) =
            ( toFloat u1, toFloat u2, toFloat v1, toFloat v2 )

        det =
            vy * ux - vx * uy
    in
        if det == 0 then
            Debug.crash "base is colinear"
        else
            ( (vy * x - vx * y) / det, (ux * y - uy * x) / det )


ranges : Float -> Float -> ( Point, Point ) -> ( ( Int, Int ), ( Int, Int ) )
ranges wd ht vecs =
    let
        rangify xs =
            ( List.minimum xs |> Maybe.withDefault 0 |> floor
            , List.maximum xs |> Maybe.withDefault 0 |> ceiling
            )
    in
        [ ( 0, 0 ), ( wd, 0 ), ( wd, ht ), ( 0, ht ) ]
            |> List.map (\( x, y ) -> prefactors x y vecs)
            |> List.unzip
            |> \( i, j ) -> ( rangify i, rangify j )


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

        color i j =
            case ( i % 2 == 0, j % 2 == 0 ) of
                ( True, True ) ->
                    Color.white

                ( True, False ) ->
                    Color.lightGray

                ( False, True ) ->
                    Color.gray

                ( False, False ) ->
                    Color.darkGray

        poly i j =
            polygon scale (color i j) (p0 i j) word

        ( ( iMin, iMax ), ( jMin, jMax ) ) =
            ranges (model.width / scale) (model.height / scale) ( u, v )

        row i =
            List.range jMin jMax
                |> List.map (poly i)

        grid =
            List.range iMin iMax
                |> List.concatMap row
    in
        grid
            |> svg [ width <| toString model.width, height <| toString model.height ]



--


description : String
description =
    """
A continuation from the previous day.

I think this is nice.

## Instructions

Hit enter to randomize.
"""


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view 8 |> project 16 description
        }
