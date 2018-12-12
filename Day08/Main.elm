module Cradle exposing (main)

import Browser exposing (document)
import Helper exposing (project)
import Pointer
import Svg exposing (Svg, circle, polyline, svg)
import Svg.Attributes exposing (cx, cy, fill, height, points, r, stroke, strokeWidth, style, width)


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
    }


type Msg
    = ChangedState MouseState


init : ( Model, Cmd Msg )
init =
    ( { line = [ ( 50, 250 ), ( 450, 250 ) ]
      , previousClose = None
      , mouse = Up ( 0, 0 )
      }
    , Cmd.none
    )



--


events : Model -> List (Svg.Attribute Msg)
events model =
    case model.mouse of
        Down _ ->
            [ Pointer.up (ChangedState << Up) ]

        _ ->
            [ Pointer.down (ChangedState << Down)
            , Pointer.move (ChangedState << Moved (mousePosition model.mouse))
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
    if d2 <= 30 then
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedState state ->
            model |> updateOnState state |> setMouse state


setMouse mouse model =
    { model | mouse = mouse }



--


view : Model -> Svg Msg
view model =
    let
        marker color ( x, y ) =
            circle
                [ cx <| String.fromFloat x
                , cy <| String.fromFloat y
                , r "2"
                , fill color
                ]
                []
    in
    svg ([ width "500", height "500" ] ++ events model)
        ([ polyline
            [ strokeWidth "2"
            , stroke "black"
            , fill "none"
            , model.line
                |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
                |> String.join " "
                |> points
            ]
            []
         , mousePosition model.mouse |> marker "red"
         ]
            ++ (model.line |> List.map (marker "black"))
        )



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


main : Program () Model Msg
main =
    document
        { init = always <| init
        , subscriptions = always Sub.none
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view |> project 8 description
        }