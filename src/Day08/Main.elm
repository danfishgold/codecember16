module Day08.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Dom as Dom
import Browser.Events
import Helper exposing (projectSvg)
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (id)
import Html.Events exposing (onClick)
import Pointer
import Svg exposing (Svg, circle, polyline, svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, strokeWidth, width)
import Task


type alias Point =
    ( Float, Float )


type MouseState
    = Up Point
    | Down Point
    | Moved Point Point


type Closeness
    = None
    | Edge Int
    | Middle Int Float


type alias Model =
    { line : List Point
    , previousClose : Closeness
    , mouse : MouseState
    , size : { width : Float, height : Float }
    }


type Msg
    = ChangedState MouseState
    | Reset
    | SetSize { width : Float, height : Float }
    | GetViewport


init : ( Model, Cmd Msg )
init =
    ( { line = [ ( 0.1, 0.5 ), ( 0.9, 0.5 ) ]
      , previousClose = None
      , mouse = Up ( 0, 0 )
      , size = { width = 500, height = 500 }
      }
    , getSvgViewport
    )


getSvgViewport =
    Dom.getViewportOf "day08"
        |> Task.map (\{ viewport } -> Debug.log "viewport" { width = viewport.width, height = viewport.height })
        |> Task.attempt
            (\res ->
                case res of
                    Ok sz ->
                        SetSize sz

                    Err error ->
                        Reset
            )



--


normalizeEventPoint : Model -> Point -> Point
normalizeEventPoint { size } ( x, y ) =
    ( x / size.width, y / size.height )


events : Model -> List (Svg.Attribute Msg)
events model =
    let
        toMsg : (Point -> MouseState) -> Point -> Msg
        toMsg change pt =
            pt
                |> normalizeEventPoint model
                |> change
                |> ChangedState
    in
    case model.mouse of
        Up _ ->
            [ Pointer.onDown (toMsg Down) ]

        _ ->
            [ Pointer.onUp (toMsg Up)
            , Pointer.onMove (toMsg (Moved (mousePosition model.mouse)))
            ]


mousePosition : MouseState -> Point
mousePosition mouse =
    case mouse of
        Up p ->
            p

        Down p ->
            p

        Moved _ p ->
            p



--


pairs : List a -> List ( a, a )
pairs xs =
    case xs of
        fst :: snd :: rest ->
            ( fst, snd ) :: pairs (snd :: rest)

        _ ->
            []


insert : Int -> a -> List a -> List a
insert idx x xs =
    List.take (idx + 1) xs ++ [ x ] ++ List.drop (idx + 1) xs


modify : Int -> (a -> a) -> List a -> List a
modify idx fn xs =
    let
        start =
            List.take idx xs

        xend =
            List.drop idx xs
    in
    case xend of
        x :: end ->
            start ++ [ fn x ] ++ end

        [] ->
            start


moveBy : Int -> Point -> List Point -> List Point
moveBy idx ( dx, dy ) pts =
    pts
        |> modify idx (\( x, y ) -> ( x + dx, y + dy ))


moveTo : Int -> Point -> List Point -> List Point
moveTo idx pt pts =
    pts |> modify idx (always pt)


delta : Point -> Point -> Point
delta ( x1, y1 ) ( x2, y2 ) =
    ( x2 - x1, y2 - y1 )


fraction : Float -> Point -> Point -> Point
fraction t ( x1, y1 ) ( x2, y2 ) =
    ( x1 + t * (x2 - x1), y1 + t * (y2 - y1) )


{-| nearest p (p1, p2) returns (t, d2)
where
t is the parameter of p1 + t\*(p2-p1) of the nearest point to p is on the p1-p2 line
d2 is the distance between that point and p, squared
-}
nearest : Point -> ( Point, Point ) -> ( Float, Float )
nearest ( a, b ) ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        ( dx, dy ) =
            ( x2 - x1, y2 - y1 )

        m =
            dx * dx + dy * dy

        ( dx1, dy1 ) =
            ( a - x1, b - y1 )
    in
    ( (dx * dx1 + dy * dy1) / m, (dx1 * dy - dx * dy1) ^ 2 / m )


minimumBy : (a -> comparable) -> List a -> Maybe a
minimumBy fn xs =
    let
        step x minX =
            if fn x < fn minX then
                x

            else
                minX
    in
    case xs of
        [] ->
            Nothing

        hd :: tl ->
            List.foldl step hd tl
                |> Just


parametersToLocation : ( Int, ( Float, Float ) ) -> ( Float, Closeness )
parametersToLocation ( i, ( t, d2 ) ) =
    if d2 <= 0.001 then
        if abs t <= 0.2 then
            ( d2, Edge i )

        else if abs (t - 1) <= 0.2 then
            ( d2, Edge (i + 1) )

        else if 0 <= t && t <= 1 then
            ( d2, Middle i t )

        else
            ( d2, None )

    else
        ( d2, None )


locationsForPoints : Point -> List Point -> List ( Float, Closeness )
locationsForPoints p pts =
    pts
        |> pairs
        |> List.indexedMap (\i ln -> ( i, nearest p ln ))
        |> List.map parametersToLocation
        |> List.filter (\( _, closeness ) -> closeness /= None)


currentClose : Closeness -> Point -> List Point -> Closeness
currentClose prev p pts =
    let
        locations =
            locationsForPoints p pts
    in
    if List.any (\( _, closeness ) -> closeness == prev) locations then
        prev

    else
        locations
            |> minimumBy Tuple.first
            |> Maybe.map Tuple.second
            |> Maybe.withDefault None


updateOnState : MouseState -> Model -> Model
updateOnState state model =
    let
        close =
            currentClose
                model.previousClose
                (mousePosition state)
                model.line

        newModel =
            { model | previousClose = close }
    in
    case state of
        Up p ->
            newModel

        Down p ->
            newModel

        Moved p1 p2 ->
            case ( model.previousClose, close ) of
                ( prev, Edge i ) ->
                    { newModel | line = model.line |> moveTo i p2 }

                ( Middle i _, Middle j t ) ->
                    if i == j then
                        { newModel
                            | line =
                                model.line
                                    |> insert i (fraction t p1 p2)
                            , previousClose = Edge (i + 1)
                        }

                    else
                        newModel

                _ ->
                    newModel


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangedState state ->
            ( model |> updateOnState state |> setMouse state, Cmd.none )

        Reset ->
            ( { model
                | line = [ ( 0.1, 0.5 ), ( 0.9, 0.5 ) ]
                , previousClose = None
              }
            , Cmd.none
            )

        SetSize sz ->
            ( { model | size = sz }, Cmd.none )

        GetViewport ->
            ( model, getSvgViewport )


setMouse mouse model =
    { model | mouse = mouse }



--


view : Model -> Svg Msg
view model =
    let
        marker color ( x, y ) =
            circle
                [ cx <| String.fromFloat (x * model.size.width)
                , cy <| String.fromFloat (y * model.size.height)
                , r "2"
                , fill color
                ]
                []
    in
    div []
        [ projectSvg ( model.size.width, model.size.height )
            (id "day08" :: events model)
            ([ polyline
                [ strokeWidth "2"
                , stroke "black"
                , fill "none"
                , model.line
                    |> List.map
                        (\( x, y ) ->
                            String.fromFloat (x * model.size.width)
                                ++ ","
                                ++ String.fromFloat (y * model.size.height)
                        )
                    |> String.join " "
                    |> points
                ]
                []
             , mousePosition model.mouse |> marker "red"
             ]
                ++ (model.line |> List.map (marker "black"))
            )
        , div [] [ button [ onClick Reset ] [ text "Reset" ] ]
        ]



--


description : String
description =
    """
In 2014 I went to a hackathon with a friend.
We made a tool that helped teach high-school students geometry using gamification (ugh.)
I was in charge of the graphic part, where you could see the diagram,
highlight parts of it (vertices, edges and angles,) and solve the given problem.

The vertices' positions and the nodes connecting them were defined in Python,
and then a horrible, disgusting, hacky JS code was generated to actually display it.
It was honestly just horrible. I used Canvas instead of SVG and wrote so many hacks.

This was the first time I wrote JS code and it actually went fine.
We were among the 10 best groups, but that was probably because we did something
educational and not because it was good.

When I showed my horrible hack to someone, he asked my why I didn't use [D3](https://d3js.org).
I probably kept a link to the D3 homepage open on my phone for a few months,
but in 2015 I finally got into D3 and reimplemented the whole thing with much fewer hacks.

Anyway, my point is that I wanted to make it easier to create these diagrams.
That idea evolved into whatever this is.

## Instructions

Use the mouse

* When you hover near a vertex you move it.
* When you hover near an edge away from a vertex you split it.
* When you hold the mouse button the above rules are ignored.
"""


page =
    { init = always <| init
    , subscriptions = subscriptions
    , update = update
    , title = "Cradle"
    , body = view
    , description = description
    }


subscriptions model =
    Browser.Events.onResize (\_ _ -> GetViewport)
