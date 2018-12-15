module Day03.Main exposing (Model, Msg, page)

import Browser exposing (document)
import Browser.Events
import Color exposing (Color)
import Day02.Random exposing (ryb1)
import Dict exposing (Dict)
import Helper exposing (onEnterOrSpace)
import Html exposing (Html)
import Json.Decode as Json
import Random exposing (Generator, generate)
import Random.Extra
import Svg exposing (Svg, g, rect, svg)
import Svg.Attributes exposing (fill, height, width, x, y)
import Svg.Keyed exposing (node)
import Time exposing (Posix, every)


type alias Model =
    { previous : Dict ( Int, Int ) Point
    , current : Point
    , side : Int
    , paused : Bool
    }


type alias Point =
    ( Int, Int, Color )


init : Int -> Model
init side =
    { previous = Dict.empty
    , current = ( 0, 0, Color.white )
    , side = side
    , paused = False
    }


initWithShape : Int -> ( Model, Cmd Msg )
initWithShape side =
    ( { previous = Dict.empty
      , current = ( 0, 0, Color.white )
      , side = side
      , paused = True
      }
    , generate SetShape (randomShape side)
    )


shape : Int -> Int -> Model
shape seed side =
    seed
        |> Random.initialSeed
        |> Random.step (randomShape side)
        |> Tuple.first
        |> separateList
        |> (\( curr, prev ) ->
                { previous = prev
                , current = curr
                , side = side
                , paused = True
                }
           )


separateList : List Point -> ( Point, Dict ( Int, Int ) Point )
separateList pts =
    case pts of
        [] ->
            ( ( 0, 0, Color.white ), Dict.empty )

        current :: previous ->
            ( current
            , previous
                |> List.map (\( x, y, c ) -> ( ( x, y ), ( x, y, c ) ))
                |> Dict.fromList
            )


type Msg
    = ResetPoints
    | Tick Posix
    | Add Point
    | SetShape (List Point)
    | SkipAnimation



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ if model.paused then
            Sub.none

          else
            every 1 Tick
        , onEnterOrSpace SkipAnimation ResetPoints
        ]



--


randomNeighbor : Point -> Generator Point
randomNeighbor ( x0, y0, _ ) =
    let
        neighbor ( x, y ) =
            let
                up =
                    Random.constant ( x, y + 1 )

                down =
                    Random.constant ( x, y - 1 )

                left =
                    Random.constant ( x - 1, y )

                right =
                    Random.constant ( x + 1, y )
            in
            Random.Extra.frequency
                ( 0.335, up )
                [ ( 0.28, down )
                , ( 0.3, left )
                , ( 0.37, right )
                ]

        color =
            ryb1 1 0.35
    in
    Random.map2 (\( x1, y1 ) c -> ( x1, y1, c ))
        (neighbor ( x0, y0 ))
        color


randomShape : Int -> Generator (List Point)
randomShape side =
    let
        process previous (( x, y, _ ) as pt) =
            if abs x > side || abs y > side then
                Random.constant previous

            else
                randomNeighbor pt
                    |> Random.andThen (process (pt :: previous))
    in
    process [] ( 0, 0, Color.white )



--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetPoints ->
            ( { model
                | current = ( 0, 0, Color.white )
                , previous = Dict.empty
                , paused = False
              }
            , Cmd.none
            )

        Add (( x, y, _ ) as pt) ->
            if abs x > model.side // 3 || abs y > model.side // 3 then
                ( { model | paused = True }, Cmd.none )

            else
                let
                    addToDict ( x_, y_, c ) dict =
                        dict |> Dict.insert ( x_, y_ ) ( x_, y_, c )
                in
                ( { model
                    | previous = model.previous |> addToDict model.current
                    , current = pt
                  }
                , Cmd.none
                )

        Tick _ ->
            ( model, generate Add (randomNeighbor model.current) )

        SkipAnimation ->
            ( model, generate SetShape (randomShape (model.side // 3)) )

        SetShape [] ->
            update ResetPoints model

        SetShape pts ->
            pts
                |> separateList
                |> (\( curr, prev ) ->
                        ( { model
                            | current = curr
                            , previous = prev
                            , paused = True
                          }
                        , Cmd.none
                        )
                   )



--


view : Float -> Model -> Svg Msg
view res { side, current, previous } =
    let
        bg =
            rect
                [ x "0"
                , y "0"
                , width <| String.fromFloat <| toFloat side * res
                , height <| String.fromFloat <| toFloat side * res
                , fill <| Color.toCssString Color.black
                ]
                []

        pts =
            (Dict.values previous ++ [ current ])
                |> List.indexedMap (\i pt -> ( String.fromInt i, mirroredPoint pt ))
                |> node "g" []

        mirroredPoint ( x, y, c ) =
            g []
                [ point ( x, y, c )
                , point ( y, x, c )
                , point ( -x, y, c )
                , point ( -y, x, c )
                , point ( x, -y, c )
                , point ( y, -x, c )
                , point ( -x, -y, c )
                , point ( -y, -x, c )
                ]

        point ( x0, y0, c ) =
            rect
                [ x <| String.fromFloat <| toFloat (x0 + side // 2) * res
                , y <| String.fromFloat <| toFloat (y0 + side // 2) * res
                , width <| String.fromFloat res
                , height <| String.fromFloat res
                , fill <| Color.toCssString c
                ]
                []
    in
    svg
        [ width <| String.fromInt <| side * ceiling res
        , height <| String.fromInt <| side * ceiling res
        ]
        [ bg, pts ]



--


description : String
description =
    """
**Originally from 2007**

This was inspired by some coding example by my programming teacher from seventh grade, Anatoly Peymer.
That example (of random numbers) was the first computer generated art I probably ever saw,
and it really stuck with me.

This is a little different from what Anatoly made (8-way symmetry instead of 4-)
but credit goes to him.

## Instructions
Hit space to restart the process.
Hit enter to skip to the final result.
"""


page =
    { init = always ( init 100, Cmd.none )
    , update = update
    , title = "Anatoly"
    , body = view 5
    , description = description
    , subscriptions = subscriptions
    }
