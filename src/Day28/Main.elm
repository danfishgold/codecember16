module Day28.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Color exposing (Color)
import Day27.Area as Area exposing (Area, Center, Shape(..))
import Day28.Border as Border
import Helper exposing (projectSvg)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Pointer
import Svg exposing (svg)
import Svg.Attributes exposing (height, width)


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
            { model | areas = Area.addToAreas model.areas area }

        MouseMove center ->
            if center /= model.mouseCenter then
                { model | mouseCenter = center } |> update (MouseDown model.mouseDown)

            else
                model

        MouseDown True ->
            case model.mouseCenter of
                Just center ->
                    center
                        |> Area.shapeAround Color.black model.mouseShape
                        |> Add
                        |> (\b a -> update a b)
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
                    Area.shapeAround Color.black mouseShape center
                        |> areaView (Color.rgba 0 0 0 0)

                Nothing ->
                    Svg.text ""

        areaViews =
            areas |> List.map (areaView Color.lightGray)

        svg =
            (areaViews ++ [ mouseAreaView ])
                |> projectSvg
                    ( toFloat <| scale * columns
                    , toFloat <| scale * rows
                    )
                    [ Pointer.move (centerFromMouse size >> MouseMove)
                    , Pointer.down <| always <| MouseDown True
                    , Pointer.up <| always <| MouseDown False
                    ]
    in
    div []
        [ svg
        , div []
            [ button [ onClick <| MouseShape Square ] [ text "Square" ]
            , button [ onClick <| MouseShape Cross ] [ text "Cross" ]
            ]
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


page =
    { init = always <| init 50 50 10
    , subscriptions = always Sub.none
    , update = \msg model -> ( update msg model, Cmd.none )
    , title = "Borders"
    , body = view
    , description = description
    }
