module Day3.Anatoly exposing (..)

import Html exposing (Html, program)
import Collage exposing (..)
import Element
import Time exposing (second, every, Time)
import Color exposing (Color)
import Random exposing (Generator, generate)
import Random.Array
import Random.List
import Array exposing (Array)
import Keyboard exposing (KeyCode)


type alias Model =
    { points : List ( Int, Int, Color )
    , side : Int
    , res : Float
    , symmetry : Symmetry
    }


init : Model
init =
    { points = [], side = 50, res = 5, symmetry = Mirror }


type Msg
    = Tick Time
    | Add ( Int, Int, Color )
    | Key KeyCode


type Symmetry
    = Mirror
    | Rotation



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add (( x, y, _ ) as pt) ->
            if abs x > model.side // 2 || abs y > model.side // 2 then
                ( { model | points = [] }, Cmd.none )
            else
                ( { model | points = pt :: model.points }, Cmd.none )

        Tick _ ->
            ( model, generate Add (randomPoint model) )

        Key 32 ->
            ( { model | points = [] }, Cmd.none )

        Key _ ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view { side, points, res, symmetry } =
    let
        bg =
            rect (toFloat side * res) (toFloat side * res) |> filled Color.black

        pts =
            points
                |> List.map mirroredPoint
                |> List.reverse

        mirroredPoint ( x, y, c ) =
            case symmetry of
                Rotation ->
                    group [ point ( x, y, c ), point ( -y, x, c ), point ( y, -x, c ), point ( -x, -y, c ) ]

                Mirror ->
                    group [ point ( x, y, c ), point ( -x, y, c ), point ( x, -y, c ), point ( -x, -y, c ) ]

        point ( x, y, c ) =
            rect res res |> filled c |> move ( toFloat x * res, toFloat y * res )
    in
        collage
            (side * ceiling res)
            (side * ceiling res)
            (bg :: pts)
            |> Element.toHtml



--


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ every (0.01 * second) Tick
        , Keyboard.ups Key
        ]



--


randomColor : Generator Color
randomColor =
    let
        colors =
            Array.fromList
                [ Color.white
                , Color.red
                , Color.blue
                , Color.green
                , Color.yellow
                , Color.purple
                ]
    in
        Random.Array.sample colors
            |> Random.map (Maybe.withDefault Color.white)


randomNeighbor : ( Int, Int ) -> Generator ( Int, Int )
randomNeighbor ( x, y ) =
    Random.List.choose [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
        |> Random.map Tuple.first
        |> Random.map (Maybe.withDefault ( x, y ))


randomPoint : Model -> Generator ( Int, Int, Color )
randomPoint model =
    case model.points of
        ( x, y, _ ) :: _ ->
            Random.map2 (\( x, y ) c -> ( x, y, c ))
                (randomNeighbor ( x, y ))
                (randomColor)

        [] ->
            randomColor |> Random.map (\c -> ( 0, 0, c ))



--


main : Program Never Model Msg
main =
    program
        { init = ( init, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
