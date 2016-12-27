module Areas exposing (..)

import Html exposing (program)
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Svg exposing (svg)
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
            [ shapeAround Color.red Square ( 25, 25 )
            , shapeAround Color.blue Cross ( 10, 25 )
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
            { model | areas = addToAreas model.areas area }

        MouseMove center ->
            if center /= model.mouseCenter then
                { model | mouseCenter = center } |> update (MouseDown model.mouseDown)
            else
                model

        MouseDown True ->
            case model.mouseCenter of
                Just center ->
                    center
                        |> shapeAround Color.black model.mouseShape
                        |> Add
                        |> (flip update)
                            { model | mouseDown = True }

                Nothing ->
                    { model | mouseDown = True }

        MouseDown False ->
            { model | mouseDown = False }

        MouseShape shape ->
            { model | mouseShape = shape }



--


view : Model -> Html Msg
view { size, areas, mouseShape, mouseCenter } =
    let
        { rows, columns, scale } =
            size

        areaView area =
            Area.view scale area

        mouseAreaView =
            case mouseCenter of
                Just center ->
                    shapeAround (Color.rgba 0 0 0 0.5) mouseShape center |> areaView

                Nothing ->
                    Svg.text ""

        areaViews =
            areas |> List.map areaView

        svg =
            (areaViews ++ [ mouseAreaView ])
                |> Svg.svg
                    [ width <| toString <| scale * columns
                    , height <| toString <| scale * rows
                    ]
    in
        div []
            [ svg
            , button [ onClick <| MouseShape Square ] [ text "Square" ]
            , button [ onClick <| MouseShape Cross ] [ text "Cross" ]
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
