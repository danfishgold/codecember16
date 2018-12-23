module Day27.Main278 exposing (Model, Msg, init, subscriptions, update, view)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Day27.Area as Area exposing (..)
import Helper exposing (Size, getViewport, projectSvg)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Pointer
import Svg exposing (svg)
import Svg.Attributes exposing (height, width)


type alias Model =
    { gridSize : GridSize
    , size : Size
    , areas : List Area
    , mouseDown : Bool
    , hideCursor : Bool
    , mouseCenter : Maybe Center
    , mouseShape : Shape
    }


type alias GridSize =
    { rows : Int
    , columns : Int
    , scale : Int
    }


type Msg
    = MouseMove Pointer.Position
    | SetMouseDown Bool Pointer.Position
    | ShouldHideCursor
    | MouseShape Shape
    | SetSize Size
    | GetViewport
    | NoOp



--


init : String -> Int -> Int -> Int -> ( Model, Cmd Msg )
init svgId n m scale =
    ( { gridSize =
            { rows = n
            , columns = m
            , scale = scale
            }
      , mouseDown = False
      , hideCursor = False
      , size = { width = 500, height = 500 }
      , mouseCenter = Nothing
      , mouseShape = Square
      , areas =
            [ { color = Color.red
              , points = shapeAround Square ( 25, 25 )
              }
            , { color = Color.blue
              , points = shapeAround Cross ( 10, 25 )
              }
            ]
      }
    , getSvgViewport svgId
    )


getSvgViewport svgId =
    getViewport SetSize NoOp svgId



--


centerFromMouse : GridSize -> Size -> Pointer.Position -> Maybe Center
centerFromMouse { rows, columns } { width, height } ( x, y ) =
    if 0 <= x && x <= width && 0 <= y && y <= height then
        Just
            ( floor (x / width * toFloat columns)
            , floor (y / height * toFloat rows)
            )

    else
        Nothing



--


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update svgId msg model =
    case msg of
        MouseMove position ->
            let
                maybeCenter =
                    centerFromMouse model.gridSize model.size position

                newModel =
                    { model | mouseCenter = maybeCenter }
            in
            case maybeCenter of
                Just center ->
                    if Just center /= model.mouseCenter && model.mouseDown then
                        ( addShapeAroundCenter center newModel
                        , Cmd.none
                        )

                    else
                        ( newModel, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetMouseDown isDown position ->
            let
                newModel =
                    { model | mouseDown = isDown }
            in
            if isDown then
                case centerFromMouse model.gridSize model.size position of
                    Just center ->
                        ( addShapeAroundCenter center newModel, Cmd.none )

                    Nothing ->
                        ( newModel, Cmd.none )

            else
                ( newModel, Cmd.none )

        ShouldHideCursor ->
            ( { model | hideCursor = True }, Cmd.none )

        MouseShape shape ->
            ( { model | mouseShape = shape }, Cmd.none )

        SetSize sz ->
            ( { model | size = sz }, Cmd.none )

        GetViewport ->
            ( model, getSvgViewport svgId )

        NoOp ->
            ( model, Cmd.none )


addShapeAroundCenter : Center -> Model -> Model
addShapeAroundCenter center model =
    { model
        | areas =
            Area.addToAreas model.areas (shapeAround model.mouseShape center)
    }



--


view : String -> (Model -> Color -> Area -> Svg.Svg Msg) -> Model -> Html Msg
view svgId areaView model =
    let
        { rows, columns, scale } =
            model.gridSize

        mouseAreaView =
            if not model.hideCursor then
                case model.mouseCenter of
                    Just center ->
                        { points = shapeAround model.mouseShape center
                        , color = Color.rgba 0 0 0 0.5
                        }
                            |> areaView model Color.white

                    Nothing ->
                        Svg.text ""

            else
                Svg.text ""

        areaViews =
            model.areas |> List.map (areaView model Color.white)

        svg =
            (areaViews ++ [ mouseAreaView ])
                |> projectSvg
                    ( toFloat <| scale * columns
                    , toFloat <| scale * rows
                    )
                    [ Pointer.onMove MouseMove
                    , Pointer.onDown <| SetMouseDown True
                    , Pointer.onUp <| SetMouseDown False
                    , Pointer.onTouchUp <| ShouldHideCursor
                    , id svgId
                    ]
    in
    div []
        [ svg
        , div []
            [ button [ onClick <| MouseShape Square ] [ text "Square" ]
            , button [ onClick <| MouseShape Cross ] [ text "Cross" ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Browser.Events.onResize (\_ _ -> GetViewport)
