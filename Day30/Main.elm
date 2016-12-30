module Lightning exposing (..)

import Html exposing (program)
import Svg exposing (Svg, svg, rect, polyline)
import Svg.Attributes exposing (x, y, width, height, points, fill, stroke, strokeWidth)
import Color exposing (Color)
import Color.Convert exposing (colorToCssRgb)
import Keyboard exposing (KeyCode)
import Random
import Random.Extra exposing (constant)


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
        |> update (Refine ( [ ( width / 2, 0 ), ( width / 2, height ) ], 1 ))


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



--


refine : Lightning -> Random.Generator Lightning
refine ( pts, lvl ) =
    constant ( pts ++ pts ++ pts, lvl )


branchOut : Lightning -> Random.Generator Lightning
branchOut ( pts, lvl ) =
    case ( List.head pts, List.head <| List.drop (List.length pts - 1) pts ) of
        ( Just first, Just last ) ->
            constant ( [], lvl + 1 )

        _ ->
            constant ( [], lvl + 1 )



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Key 32 ->
            ( { model | lightnings = [] }, Cmd.none )

        Key _ ->
            ( model, Cmd.none )

        Add lightning ->
            ( { model | lightnings = lightning :: model.lightnings }
            , if level lightning < model.maxLevel then
                Cmd.none
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
            Color.red

        3 ->
            Color.green

        4 ->
            Color.blue

        _ ->
            Color.purple
