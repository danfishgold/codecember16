module Areas exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg)
import Svg.Attributes exposing (width, height)
import Color exposing (Color)
import Mouse
import Day27.Area as Area exposing (..)


type alias Model =
    { size : Size
    , areas : List Area
    , mouseDown : Bool
    , mouseCenter : Maybe Center
    , mouseShape : Shape
    }


type alias Size =
    { rows : Int, columns : Int, scale : Int }


type Msg
    = Add Area
    | MouseMove (Maybe Center)
    | MouseDown Bool
    | MouseShape Shape



--


init : Int -> Int -> Int -> ( Model, Cmd Msg )
init n m scale =
    ( { size =
            { rows = n
            , columns = m
            , scale = scale
            }
      , mouseDown = False
      , mouseCenter = Nothing
      , mouseShape = Square
      , areas =
            [ shapeAround Color.red ( 25, 25 ) Square
            , shapeAround Color.blue ( 10, 25 ) Cross
            ]
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.moves (centerFromMouse model.size >> MouseMove)
        , Mouse.downs <| always <| MouseDown True
        , Mouse.ups <| always <| MouseDown False
        ]


centerFromMouse : Size -> Mouse.Position -> Maybe Center
centerFromMouse { rows, columns, scale } { x, y } =
    if 0 <= x && x <= scale * columns && 0 <= y && y <= scale * rows then
        Just
            ( x // scale, y // scale )
    else
        Nothing



--


update : Msg -> Model -> Model
update msg model =
    case msg of
        Add area ->
            model

        MouseMove center ->
            { model | mouseCenter = center }

        MouseDown isDown ->
            { model | mouseDown = isDown }

        MouseShape shape ->
            { model | mouseShape = shape }



--


view : Model -> Svg Msg
view { size, areas } =
    let
        { rows, columns, scale } =
            size

        areaView area =
            Area.view scale Color.lightGray area
    in
        areas
            |> List.map areaView
            |> svg
                [ width <| toString <| scale * columns
                , height <| toString <| scale * rows
                ]



--


main : Program Never Model Msg
main =
    program
        { init = init 50 50 10
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        }
