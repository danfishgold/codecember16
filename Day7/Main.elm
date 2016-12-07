module Loops exposing (..)

import Html exposing (program)
import Html exposing (Html, div, text)
import Random
import Random.Extra exposing (constant)


type alias Model =
    { minLength : Int
    , count : Int
    , loops : List (List Point)
    }


type Msg
    = Msg


type alias Point =
    { x : Int, y : Int }


init : ( Model, Cmd Msg )
init =
    ( { minLength = 10
      , count = 1
      , loops = Random.list 1 (loop 10) |> \loops -> Random.step loops (Random.initialSeed 0) |> Tuple.first
      }
    , Cmd.none
    )



--


loop : Int -> Random.Generator (List Point)
loop minLength =
    let
        diff =
            Random.Extra.sample [ Point 0 1, Point 0 -1, Point 1 0, Point -1 0 ]
                |> Random.map (Maybe.withDefault <| Point 0 1)

        nextPoint { x, y } =
            diff |> Random.map (\d -> Point (x + d.x) (y + d.y))

        aloop minLength prevs =
            prevs
                |> List.head
                |> Maybe.withDefault (Point 0 0)
                |> \p ->
                    if p == Point 0 0 && List.length prevs >= minLength then
                        constant prevs
                    else
                        nextPoint p |> Random.andThen (\q -> aloop minLength (q :: prevs))
    in
        aloop minLength []



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view model =
    div [] [ text <| toString model ]



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
