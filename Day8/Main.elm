module Cradle exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, polyline, circle)
import Svg.Attributes exposing (width, height, points, stroke, strokeWidth, fill, style, cx, cy, r)
import Mouse


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


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.mouse of
        Down _ ->
            Mouse.ups <| \{ x, y } -> ChangedState <| Up ( toFloat x, toFloat y )

        _ ->
            Sub.batch
                [ Mouse.downs <| \{ x, y } -> ChangedState <| Down ( toFloat x, toFloat y )
                , Mouse.moves <| \{ x, y } -> ChangedState <| Moved (mousePosition model.mouse) ( toFloat x, toFloat y )
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
    (List.take (idx + 1) xs) ++ [ x ] ++ (List.drop (idx + 1) xs)


modify : Int -> (a -> a) -> List a -> List a
modify idx fn xs =
    let
        start =
            List.take (idx) xs

        xend =
            List.drop (idx) xs
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


{-|
nearest p (p1, p2) returns (t, d2)
where
  t is the parameter of p1 + t*(p2-p1) of the nearest point to p is on the p1-p2 line
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


minimumBy : (a -> comparable) -> List a -> Maybe ( Int, a )
minimumBy fn xs =
    let
        step x ( i, minI, minX, minV ) =
            if fn x < minV then
                ( i + 1, i, x, fn x )
            else
                ( i + 1, minI, minX, minV )
    in
        case xs of
            [] ->
                Nothing

            hd :: tl ->
                List.foldl step ( 1, 0, hd, fn hd ) tl
                    |> \( _, i, x, _ ) -> Just ( i, x )


nearestForPoints : Point -> List Point -> Maybe ( Int, ( Float, Float ) )
nearestForPoints p pts =
    pts
        |> pairs
        |> List.map (nearest p)
        |> minimumBy Tuple.second


currentClose : Point -> List Point -> Closeness
currentClose p pts =
    let
        actualI ( i, ( t, d2 ) ) =
            if d2 <= 20 then
                if abs (t) <= 0.2 then
                    Edge i
                else if abs (t - 1) <= 0.2 then
                    Edge (i + 1)
                else if 0 <= t && t <= 1 then
                    Middle i t
                else
                    None
            else
                None
    in
        nearestForPoints p pts
            |> Maybe.map actualI
            |> Maybe.withDefault None


updateOnState : MouseState -> Model -> Model
updateOnState state model =
    case state of
        Up p ->
            { model | previousClose = currentClose p model.line }

        Down p ->
            { model | previousClose = currentClose p model.line }

        Moved p1 p2 ->
            let
                close =
                    currentClose p2 model.line

                newModel =
                    { model | previousClose = close }
            in
                case ( model.previousClose, close ) of
                    ( prev, Edge i ) ->
                        if prev == Edge i then
                            { newModel | line = model.line |> moveBy i (delta p1 p2) }
                        else
                            { newModel | line = model.line |> moveTo i p2 }

                    ( Middle i _, Middle j t ) ->
                        if i == j then
                            { newModel | line = model.line |> insert i (fraction t p1 p2), previousClose = Edge (i + 1) }
                        else
                            newModel

                    _ ->
                        newModel


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangedState state ->
            model |> updateOnState state |> \model -> { model | mouse = state }



--


view : Model -> Svg Msg
view model =
    let
        marker color ( x, y ) =
            circle
                [ cx <| toString x
                , cy <| toString y
                , r "2"
                , fill color
                ]
                []
    in
        svg [ width "500", height "500", style "pointer-events: none" ]
            ([ polyline
                [ strokeWidth "2"
                , stroke "black"
                , fill "none"
                , model.line
                    |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                    |> String.join " "
                    |> points
                ]
                []
             , mousePosition model.mouse |> marker "red"
             ]
                ++ (model.line |> List.map (marker "black"))
            )



--


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = \msg model -> ( update msg model, Cmd.none )
        , view = view
        }
