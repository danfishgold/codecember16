module Borders exposing (..)

import Html exposing (program)
import Helper exposing (project)
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Svg exposing (svg)
import Svg.Attributes exposing (width, height)
import Color exposing (Color)
import Pointer
import Day27.Area as Area exposing (..)
import Day28.Border as Border exposing (..)


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
            []
            -- [ shapeAround Color.red Square ( 25, 25 )
            -- , shapeAround Color.blue Cross ( 10, 25 )
            -- ]
      }
    , Cmd.none
    )



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


centerFromMouse : Size -> Pointer.Position -> Maybe Center
centerFromMouse { rows, columns, scale } ( x, y ) =
    if 0 <= x && floor x <= scale * columns && 0 <= y && floor y <= scale * rows then
        Just
            ( floor x // scale, floor y // scale )
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

        areaView fillColor area =
            Border.view scale fillColor area

        mouseAreaView =
            case mouseCenter of
                Just center ->
                    shapeAround Color.black mouseShape center |> areaView (Color.rgba 0 0 0 0)

                Nothing ->
                    Svg.text ""

        areaViews =
            areas |> List.map (areaView Color.lightGray)

        svg =
            (areaViews ++ [ mouseAreaView ])
                |> Svg.svg
                    [ width <| toString <| scale * columns
                    , height <| toString <| scale * rows
                    , Pointer.move (centerFromMouse size >> MouseMove)
                    , Pointer.down <| always <| MouseDown True
                    , Pointer.up <| always <| MouseDown False
                    ]
    in
        div []
            [ svg
            , button [ onClick <| MouseShape Square ] [ text "Square" ]
            , button [ onClick <| MouseShape Cross ] [ text "Cross" ]
            ]



--


description : String
description =
    """
In my opinion this is cooler than yesterday's project.

## Instructions

Click and drag to make shape outlines.
You can use the "Cross" option to make this faster.
Disconnected areas automatically get a different color. When two outlines meet
they merge.
"""


main : Program Never Model Msg
main =
    program
        { init = init 50 50 10
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view |> project 28 description
        }
