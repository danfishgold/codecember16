module Day30.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Helper exposing (onEnter)
import Random exposing (constant)
import Random.Extra exposing (combine, rangeLengthList, sample)
import Svg exposing (Svg, polyline, rect, svg)
import Svg.Attributes exposing (fill, height, points, stroke, strokeWidth, width, x, y)
import Task


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
    = Add Lightning
    | BranchOut (List Lightning)
    | Refine Lightning
    | Randomize


init : Float -> Float -> ( Model, Cmd Msg )
init width height =
    { width = width
    , height = height
    , lightnings = []
    , maxLevel = 5
    , iterations = 5
    }
        |> restart


restart : Model -> ( Model, Cmd Msg )
restart ({ width, height } as model) =
    Refine ( [ ( width / 2, 0 ), ( width / 2, height ) ], 1 )
        |> (\a -> update a model)


description : String
description =
    """
This is based on
[another article](http://10tv.nana10.co.il/Article/?ArticleID=552882)
by [Ido Gendel](http://www.idogendel.com/). It was almost certainly my favorite.

This article introduced me to the concept of
[fractals](https://en.wikipedia.org/wiki/Fractal),
which was one of the reasons I wanted a degree in Math.
"""


page =
    { init = always <| init 500 500
    , subscriptions = subscriptions
    , update = update
    , title = "Lightning"
    , body = view
    , description = description
    }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    onEnter Randomize



--


randomElement : List a -> Random.Generator a
randomElement xs =
    Random.Extra.sample xs
        |> Random.map
            (\maybeX ->
                case maybeX of
                    Nothing ->
                        Debug.todo "random sample from an empty list"

                    Just x ->
                        x
            )


firstAndLast : List a -> ( a, a )
firstAndLast xs =
    case ( List.head xs, List.head <| List.drop (List.length xs - 1) xs ) of
        ( Just first, Just last ) ->
            ( first, last )

        _ ->
            Debug.todo "list was empty"


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


message : Msg -> Cmd Msg
message msg =
    Task.perform identity (Task.succeed msg)



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
        |> Random.map (\pts_ -> ( pts_ ++ [ last ], lvl ))


branchOut : Float -> Float -> Lightning -> Random.Generator Lightning
branchOut wd ht ( pts, lvl ) =
    let
        diam =
            pts |> firstAndLast |> (\( p1, p2 ) -> dist p1 p2)

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
                    (Random.float 0 pi)
            )
        |> Random.Extra.filter (\( pts_, _ ) -> List.all withinBounds pts_)



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Randomize ->
            { model | lightnings = [] } |> restart

        Add lightning ->
            ( { model | lightnings = lightning :: model.lightnings }
            , if level lightning < model.maxLevel then
                branchOut model.width model.height lightning
                    |> rangeLengthList 1 2
                    |> Random.generate BranchOut

              else
                Cmd.none
            )

        BranchOut branches ->
            ( model, branches |> List.map (message << Refine) |> Cmd.batch )

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
                , width <| String.fromFloat model.width
                , height <| String.fromFloat model.height
                , fill "black"
                ]
                []

        lightning ( pts, level_ ) =
            Svg.polyline
                [ pts
                    |> List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y)
                    |> String.join " "
                    |> points
                , fill "none"
                , stroke <| Color.toCssString <| color level_
                , strokeWidth "1"
                ]
                []
    in
    (bg :: List.map lightning model.lightnings)
        |> svg [ width <| String.fromFloat model.width, height <| String.fromFloat model.height ]


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
