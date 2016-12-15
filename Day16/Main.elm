module Tiles exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height)
import Day15.Polyomino as Poly
import Random
import Keyboard exposing (KeyCode)


type alias Tile =
    ( Poly.Word, Poly.Word, Poly.Word )


type alias Model =
    { width : Float
    , height : Float
    , tile : Tile
    }


randomizeTile : Cmd Msg
randomizeTile =
    Random.generate SetTile (Poly.randomBN 1 5)


type Msg
    = SetTile Tile
    | Key KeyCode


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    ( { width = width
      , height = height
      , tile = ( [], [], [] )
      }
    , randomizeTile
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

        Key 32 ->
            ( model, randomizeTile )

        Key _ ->
            ( model, Cmd.none )



--


view : Model -> Svg Msg
view model =
    []
        |> svg [ width <| toString model.width, height <| toString model.height ]



--


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
