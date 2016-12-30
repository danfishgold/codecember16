module Lightning exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, rect, polyline)
import Svg.Attributes exposing (x, y, width, height, points, fill, stroke, strokeWidth)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Keyboard exposing (KeyCode)
import Random
import Random.Extra exposing (constant, sample, combine)


type alias Model =
    { width : Float
    , height : Float
    , lightnings : List Lightning
    , maxLevel : Level
    , iterations : Int
    }


type alias Lightning =
    ( List Point, Level )


level : Lightning -> Level
level ( _, lvl ) =
    lvl


pointCount : Lightning -> Int
pointCount ( pts, _ ) =
    List.length pts


type alias Point =
    ( Float, Float )


type alias Level =
    Int


type Msg
    = Key KeyCode
    | Refine Lightning
    | Add Lightning


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    { width = width
    , height = height
    , lightnings = []
    , maxLevel = 6
    , iterations = 4
    }
        |> restart


restart : Model -> ( Model, Cmd Msg )
restart ({ width, height } as model) =
    Refine ( [ ( width / 2, 0 ), ( width / 2, height ) ], 1 )
        |> flip update model


main : Program Never Model Msg
main =
    program
        { init = init 500 500
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups Key



--


randomElement : List a -> Random.Generator a
randomElement xs =
    Random.Extra.sample xs
        |> Random.map
            (\maybeX ->
                case maybeX of
                    Nothing ->
                        Debug.crash "random sample from an empty list"

                    Just x ->
                        x
            )


firstAndLast : List a -> ( a, a )
firstAndLast xs =
    case ( List.head xs, List.head <| List.drop (List.length xs - 1) xs ) of
        ( Just first, Just last ) ->
            ( first, last )

        _ ->
            Debug.crash "list was empty"


pairs : List a -> List ( a, a )
pairs xs =
    case xs of
        fst :: snd :: rest ->
            ( fst, snd ) :: pairs (snd :: rest)

        _ ->
            []


dist : Point -> Point -> Float
dist ( x1, y1 ) ( x2, y2 ) =
    sqrt <| (x2 - x1) ^ 2 + (y2 - y1) ^ 2


angle : Point -> Point -> Float
angle ( x1, y1 ) ( x2, y2 ) =
    atan2 (y2 - y1) (x2 - x1)


endPoint : Point -> Float -> Float -> Point
endPoint ( x0, y0 ) r theta =
    ( x0 + r * cos theta, y0 + r * sin theta )



--


refine : Lightning -> Random.Generator Lightning
refine ( pts, lvl ) =
    let
        last =
            firstAndLast pts |> Tuple.second

        refinePair ( p1, p2 ) =
            let
                ( r, theta ) =
                    ( dist p1 p2, angle p1 p2 )

                mid p theta0 =
                    Random.map2 (endPoint p)
                        (Random.float (r / 4) (r / 3))
                        (Random.float (theta0 - 0.5) (theta0 + 0.5))

                refined mid1 mid2 =
                    [ p1, mid1, mid2 ]
            in
                Random.map2 refined (mid p1 theta) (mid p2 (pi + theta))
    in
        pts
            |> pairs
            |> List.map refinePair
            |> combine
            |> Random.map List.concat
            |> Random.map (\pts -> ( pts ++ [ last ], lvl ))


branchOut : Float -> Float -> Lightning -> Random.Generator Lightning
branchOut wd ht ( pts, lvl ) =
    let
        diam =
            pts |> firstAndLast |> \( p1, p2 ) -> dist p1 p2

        branchStart =
            randomElement pts

        withinBounds ( x, y ) =
            0 <= x && x <= wd && 0 <= y && y <= ht

        branch start r theta =
            ( [ start, endPoint start r theta ], lvl + 1 )
    in
        branchStart
            |> Random.andThen
                (\start ->
                    Random.map2 (branch start)
                        (Random.float 0.5 1 |> Random.map ((*) diam))
                        (Random.float (0) (pi))
                )
            |> Random.Extra.filter (\( pts, _ ) -> List.all withinBounds pts)



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Key 32 ->
            { model | lightnings = [] } |> restart

        Key _ ->
            ( model, Cmd.none )

        Add lightning ->
            ( { model | lightnings = lightning :: model.lightnings }
            , if level lightning < model.maxLevel then
                Random.generate Refine (branchOut model.width model.height lightning)
              else
                Cmd.none
            )

        Refine lightning ->
            if pointCount lightning < 3 ^ model.iterations then
                ( model, Random.generate Refine (refine lightning) )
            else
                update (Add lightning) model



--


view : Model -> Svg Msg
view model =
    let
        bg =
            Svg.rect
                [ x "0"
                , y "0"
                , width <| toString model.width
                , height <| toString model.height
                , fill "black"
                ]
                []

        lightning ( pts, level ) =
            Svg.polyline
                [ pts
                    |> List.map (\( x, y ) -> toString x ++ "," ++ toString y)
                    |> String.join " "
                    |> points
                , fill "none"
                , stroke <| colorToCssRgb <| color level
                , strokeWidth "1"
                ]
                []
    in
        (bg :: List.map lightning model.lightnings)
            |> svg [ width <| toString model.width, height <| toString model.height ]


color : Level -> Color
color lvl =
    case lvl of
        1 ->
            Color.white

        2 ->
            Color.white

        3 ->
            Color.lightBlue

        4 ->
            Color.blue

        _ ->
            Color.darkBlue
