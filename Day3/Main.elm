module Anatoly exposing (..)

import Html exposing (Html, program)
import Collage exposing (..)
import Element
import Time exposing (second, every, Time)
import Color exposing (Color)
import Random exposing (Generator, generate)
import Random.Extra
import Keyboard exposing (KeyCode)
import Svg exposing (Svg, svg, rect, g)
import Svg.Attributes exposing (x, y, width, height, fill)
import Color.Convert exposing (colorToCssRgb)


type alias Model =
    { points : List ( Int, Int, Color )
    , side : Int
    , res : Float
    , symmetry : Symmetry
    , paused : Bool
    }


init : Model
init =
    { points = [], side = 100, res = 5, symmetry = Mirror, paused = False }


type Msg
    = Tick Time
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
    let
        colors =
            [ Color.white
            , Color.red
            , Color.blue
            , Color.green
            , Color.yellow
            , Color.purple
            ]
    in
        Random.Extra.sample colors
            |> Random.map (Maybe.withDefault Color.white)


randomNeighbor : ( Int, Int ) -> Generator ( Int, Int )
randomNeighbor ( x, y ) =
    Random.Extra.sample [ ( x + 1, y ), ( x - 1, y ), ( x, y + 1 ), ( x, y - 1 ) ]
        |> Random.map (Maybe.withDefault ( x, y ))
        |> Random.map (\( x, y ) -> ( max x 0, max y 0 ))


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
            ( { model | paused = not model.paused }, Cmd.none )

        Key 49 ->
            ( { model | symmetry = Mirror, points = [] }, Cmd.none )

        Key 50 ->
            ( { model | symmetry = Rotation, points = [] }, Cmd.none )

        Key _ ->
            ( model, Cmd.none )



--


view : Model -> Html Msg
view { side, points, res, symmetry } =
    let
        bg =
            Collage.rect (toFloat side * res) (toFloat side * res) |> filled Color.black

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
            Collage.rect res res |> filled c |> move ( toFloat x * res, toFloat y * res )
    in
        collage
            (side * ceiling res)
            (side * ceiling res)
            (bg :: pts)
            |> Element.toHtml


svgView : Model -> Svg Msg
svgView { side, points, res, symmetry } =
    let
        bg =
            Svg.rect
                [ x "0"
                , y "0"
                , width <| toString <| toFloat side * res
                , height <| toString <| toFloat side * res
                , fill <| colorToCssRgb Color.black
                ]
                []

        pts =
            points
                |> List.map mirroredPoint
                |> List.reverse

        mirroredPoint ( x, y, c ) =
            case symmetry of
                Rotation ->
                    g [] [ point ( x, y, c ), point ( -y, x, c ), point ( y, -x, c ), point ( -x, -y, c ) ]

                Mirror ->
                    g [] [ point ( x, y, c ), point ( -x, y, c ), point ( x, -y, c ), point ( -x, -y, c ) ]

        point ( x0, y0, c ) =
            Svg.rect
                [ x <| toString <| toFloat (x0 + side // 2) * res
                , y <| toString <| toFloat (y0 + side // 2) * res
                , width <| toString res
                , height <| toString res
                , fill <| colorToCssRgb c
                ]
                []
    in
        svg [ width <| toString <| side * ceiling res, height <| toString <| side * ceiling res ]
            (bg :: pts)



--


main : Program Never Model Msg
main =
    program
        { init = ( init, Cmd.none )
        , update = update
        , view = svgView
        , subscriptions = subscriptions
        }
